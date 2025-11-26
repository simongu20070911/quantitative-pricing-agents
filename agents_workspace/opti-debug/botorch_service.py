#!/usr/bin/env python3
"""
BoTorch TuRBO-m microservice (HTTP/JSON).

Endpoints:
  POST /init
    { "bounds": [{"name": str, "lower": float, "upper": float}, ...],
      "batch_size": int,
      "n_regions": int }
  POST /suggest
    { "X": [[...]], "Y": [ ... ] }   # arrays aligned with bounds order
    returns { "candidates": [[...]], "region_ids": [int], "state": <opaque> }
  POST /update
    { "state": <state>, "Y_new": [ ... ] }  # optional; updates success/failure

Notes:
  - Uses BoTorch SingleTaskGP + qEI inside TuRBO regions.
  - Trust-region defaults per TuRBO: length init 0.8, bounds [0.5^7,1.6],
    success_tol=10, failure_tol=ceil(max(4/batch, d/batch)), epsilon=1e-3|best|.
  - Candidate pool: min(5000, max(2000, 200*d)) Sobol samples per region; picks q=batch_size with qEI.
"""

from __future__ import annotations

import math
from typing import Dict, List, Optional, Tuple

import torch
from fastapi import FastAPI
from pydantic import BaseModel
from botorch import fit_gpytorch_mll
from botorch.acquisition import qNoisyExpectedImprovement
from botorch.models import SingleTaskGP
from botorch.optim import optimize_acqf
from botorch.utils.sampling import draw_sobol_samples
from botorch.utils.transforms import normalize, unnormalize
from gpytorch.mlls import ExactMarginalLogLikelihood

app = FastAPI()


class Bounds(BaseModel):
    name: str
    lower: float
    upper: float
    integer: bool = False
    domain: dict


class InitRequest(BaseModel):
    bounds: List[Bounds]
    batch_size: int = 5
    n_regions: int = 3


class SuggestRequest(BaseModel):
    X: List[List[float]]
    Y: List[float]
    state: Optional[dict] = None


class UpdateRequest(BaseModel):
    state: dict
    Y_new: List[float]


class TurboState:
    def __init__(self, dim: int, batch_size: int):
        self.dim = dim
        self.batch_size = batch_size
        self.length = 0.8
        self.length_min = 0.5 ** 7
        self.length_max = 1.6
        self.success_t = 10
        self.failure_t = math.ceil(max(4 / batch_size, dim / batch_size))
        self.successes = 0
        self.failures = 0
        self.best = -1e30
        self.center = torch.full((dim,), 0.5, dtype=torch.double)

    def to_json(self):
        return {
            "dim": self.dim,
            "batch_size": self.batch_size,
            "length": self.length,
            "length_min": self.length_min,
            "length_max": self.length_max,
            "success_t": self.success_t,
            "failure_t": self.failure_t,
            "successes": self.successes,
            "failures": self.failures,
            "best": self.best,
            "center": self.center.tolist(),
        }

    @classmethod
    def from_json(cls, d):
        obj = cls(d["dim"], d["batch_size"])
        obj.length = d["length"]
        obj.length_min = d["length_min"]
        obj.length_max = d["length_max"]
        obj.success_t = d["success_t"]
        obj.failure_t = d["failure_t"]
        obj.successes = d["successes"]
        obj.failures = d["failures"]
        obj.best = d["best"]
        obj.center = torch.tensor(d["center"], dtype=torch.double)
        return obj


class ServiceState:
    def __init__(self, bounds: List[Bounds], batch_size: int, n_regions: int):
        self.bounds = bounds
        self.lower = torch.tensor([b.lower for b in bounds], dtype=torch.double)
        self.upper = torch.tensor([b.upper for b in bounds], dtype=torch.double)
        self.integer_mask = torch.tensor([1 if b.integer else 0 for b in bounds], dtype=torch.bool)
        self.domains = [b.domain for b in bounds]
        self.batch_size = batch_size
        self.states = [TurboState(len(bounds), batch_size) for _ in range(n_regions)]

    def to_json(self):
        return {
            "bounds": [b.dict() for b in self.bounds],
            "batch_size": self.batch_size,
            "states": [s.to_json() for s in self.states],
        }

    @classmethod
    def from_json(cls, d):
        bounds = [Bounds(**b) for b in d["bounds"]]
        obj = cls(bounds, d["batch_size"], len(d["states"]))
        obj.states = [TurboState.from_json(s) for s in d["states"]]
        return obj


svc_state: Optional[ServiceState] = None


def fit_model(X: torch.Tensor, Y: torch.Tensor) -> SingleTaskGP:
    gp = SingleTaskGP(X, Y)
    mll = ExactMarginalLogLikelihood(gp.likelihood, gp)
    fit_gpytorch_mll(mll)
    return gp


def generate_batch(
    state: TurboState,
    X: torch.Tensor,
    Y: torch.Tensor,
    bounds: torch.Tensor,
    domains: List[dict],
    integer_mask: torch.Tensor,
    batch_size: int,
):
    # Encode discrete/categorical to indices
    X_enc, enc_meta = encode_X(X, domains)
    # Normalize to [0,1]^d
    X_norm = normalize(X_enc, bounds=bounds)
    Y_centered = Y

    # Update center to incumbent for this region
    best_val, best_idx = Y_centered.max(dim=0)
    state.best = max(state.best, best_val.item())
    state.center = X_norm[best_idx].squeeze(0).detach()

    # Define trust region box
    half = state.length / 2.0
    tr_lb = torch.clamp(state.center - half, 0.0, 1.0)
    tr_ub = torch.clamp(state.center + half, 0.0, 1.0)

    # Candidate pool (Sobol in TR)
    d = X.shape[1]
    n_candidates = min(5000, max(2000, 200 * d))
    sobol = draw_sobol_samples(torch.stack([tr_lb, tr_ub]), n_candidates, 1).squeeze(1)

    gp = fit_model(X_norm, Y_centered)
    qnei = qNoisyExpectedImprovement(model=gp, X_baseline=X_norm)
    # optimize qNEI in the TR
    candidates, _ = optimize_acqf(
        acq_function=qnei,
        bounds=torch.stack([tr_lb, tr_ub]),
        q=batch_size,
        num_restarts=10,
        raw_samples=128,
    )
    # Unnormalize and decode
    X_cand = unnormalize(candidates, bounds=bounds)
    X_cand = decode_X(X_cand, domains, enc_meta, bounds)
    X_cand = snap_integers(X_cand, bounds, integer_mask)
    return X_cand, None


def update_state(state: TurboState, y_new: torch.Tensor):
    best_new = y_new.max().item()
    eps = 1e-3 * abs(state.best)
    improved = best_new > state.best + eps
    if improved:
        state.successes += 1
        state.failures = 0
    else:
        state.failures += 1
        state.successes = 0
    if state.successes >= state.success_t:
        state.length = min(state.length * 2.0, state.length_max)
        state.successes = 0
    elif state.failures >= state.failure_t:
        state.length = max(state.length / 2.0, state.length_min)
        state.failures = 0
    if state.length < state.length_min:
        state.length = 0.8
        state.successes = 0
        state.failures = 0
        state.center = torch.rand_like(state.center)
        state.best = -1e30
    else:
        state.best = max(state.best, best_new)


@app.post("/init")
def init(req: InitRequest):
    global svc_state
    svc_state = ServiceState(req.bounds, req.batch_size, req.n_regions)
    return {"state": svc_state.to_json()}


@app.post("/suggest")
def suggest(req: SuggestRequest):
    global svc_state
    if svc_state is None:
        raise ValueError("Service not initialized; call /init first.")
    if req.state:
        svc_state = ServiceState.from_json(req.state)

    bounds = torch.stack([svc_state.lower, svc_state.upper])
    X = torch.tensor(req.X, dtype=torch.double)
    X = snap_integers(X, bounds, svc_state.integer_mask)
    Y = torch.tensor(req.Y, dtype=torch.double).unsqueeze(-1)

    candidates = []
    region_ids = []
    # Round-robin regions
    for rid, state in enumerate(svc_state.states):
        X_cand, _ = generate_batch(state, X, Y, bounds, svc_state.domains, svc_state.integer_mask, svc_state.batch_size)
        candidates.append(X_cand)
        region_ids.extend([rid] * X_cand.shape[0])
    out = torch.cat(candidates, dim=0).tolist()
    return {"candidates": out, "region_ids": region_ids, "state": svc_state.to_json()}


@app.post("/update")
def update(req: UpdateRequest):
    global svc_state
    if svc_state is None:
        raise ValueError("Service not initialized; call /init first.")
    svc_state = ServiceState.from_json(req.state)
    y_new = torch.tensor(req.Y_new, dtype=torch.double)
    for s in svc_state.states:
        update_state(s, y_new.unsqueeze(-1))
    return {"state": svc_state.to_json()}


if __name__ == "__main__":
    import uvicorn
    uvicorn.run("botorch_service:app", host="127.0.0.1", port=10001, reload=False)
def snap_integers(x: torch.Tensor, bounds: torch.Tensor, int_mask: torch.Tensor) -> torch.Tensor:
    if x.numel() == 0 or not int_mask.any():
        return x
    snapped = x.clone()
    snapped[..., int_mask] = torch.round(snapped[..., int_mask])
    lb = bounds[0, int_mask]
    ub = bounds[1, int_mask]
    snapped[..., int_mask] = torch.max(torch.min(snapped[..., int_mask], ub), lb)
    return snapped


def encode_X(X_raw: torch.Tensor, domains: List[dict]) -> Tuple[torch.Tensor, List[Tuple[str, int]]]:
    """
    Encode categorical/discrete dimensions to numeric indices for the GP.
    Returns encoded X and a list of (kind, size) for decoding.
    """
    encoded_cols = []
    enc_meta = []
    for j, dom in enumerate(domains):
        kind = dom["kind"]
        if kind == "categorical":
            values = dom["values"]
            enc_meta.append(("categorical", len(values)))
            idx = X_raw[..., j].round().clamp(0, len(values) - 1)
            encoded_cols.append(idx.unsqueeze(-1))
        elif kind == "discrete":
            values = dom["values"]
            enc_meta.append(("discrete", len(values)))
            # map to nearest index
            vals = torch.tensor(values, dtype=X_raw.dtype, device=X_raw.device)
            diffs = (X_raw[..., j].unsqueeze(-1) - vals).abs()
            idx = diffs.argmin(dim=-1)
            encoded_cols.append(idx.unsqueeze(-1))
        elif kind == "integer":
            enc_meta.append(("integer", 1))
            encoded_cols.append(X_raw[..., j].unsqueeze(-1))
        else:
            enc_meta.append(("continuous", 1))
            encoded_cols.append(X_raw[..., j].unsqueeze(-1))
    return torch.cat(encoded_cols, dim=-1), enc_meta


def decode_X(X_encoded: torch.Tensor, domains: List[dict], enc_meta: List[Tuple[str, int]], bounds: torch.Tensor) -> torch.Tensor:
    cols = []
    for j, dom in enumerate(domains):
        kind = dom["kind"]
        if kind == "categorical":
            values = dom["values"]
            idx = X_encoded[..., j].round().long().clamp(0, len(values) - 1)
            cols.append(idx.to(torch.float))
        elif kind == "discrete":
            values = dom["values"]
            vals = torch.tensor(values, dtype=X_encoded.dtype, device=X_encoded.device)
            idx = X_encoded[..., j].round().long().clamp(0, len(values) - 1)
            cols.append(vals[idx])
        elif kind == "integer":
            cols.append(X_encoded[..., j].round())
        else:
            cols.append(X_encoded[..., j])
    return torch.stack(cols, dim=-1)
