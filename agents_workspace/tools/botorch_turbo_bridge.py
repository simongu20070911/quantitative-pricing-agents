#!/usr/bin/env python3
"""
Minimal BoTorch TuRBO bridge.

Usage (one-shot suggest):
  python tools/botorch_turbo_bridge.py --bounds bounds.json --data data.csv --state state.json --batch-size 5

Inputs:
  bounds.json : [{"name": "p1", "lower": 0.0, "upper": 1.0}, ...]
  data.csv    : header with param names + column "y". Provides observed points.
  state.json  : (optional) persisted TuRBO state; will be created/updated.

Outputs:
  Prints JSON with suggested candidates under "candidates": [{"name": v, ...}, ...]
  Updates state.json with new TuRBO state (center/length/success/failure/best).

Notes:
  - Uses BoTorch SingleTaskGP + qEI inside the current trust region.
  - Trust region follows TuRBO defaults: normalized [0,1]^d, length_init=0.8,
    length bounds [0.5^7, 1.6], success_t=10, failure_t=ceil(max(4/b, d/b)).
  - Improvement test: max(y_new) > best + 1e-3*|best|.
"""

from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
from typing import Dict, List, Tuple

import torch
from botorch import fit_gpytorch_mll
from botorch.acquisition import qExpectedImprovement
from botorch.models import SingleTaskGP
from botorch.optim import optimize_acqf
from botorch.utils.sampling import draw_sobol_samples
from botorch.utils.transforms import normalize, unnormalize
from gpytorch.mlls import ExactMarginalLogLikelihood


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
        self.center = torch.full((dim,), 0.5)
        self.best = -float("inf")

    def to_json(self) -> Dict:
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
            "center": self.center.tolist(),
            "best": self.best,
        }

    @classmethod
    def from_json(cls, d: Dict) -> "TurboState":
        obj = cls(d["dim"], d["batch_size"])
        obj.length = d["length"]
        obj.length_min = d["length_min"]
        obj.length_max = d["length_max"]
        obj.success_t = d["success_t"]
        obj.failure_t = d["failure_t"]
        obj.successes = d["successes"]
        obj.failures = d["failures"]
        obj.center = torch.tensor(d["center"], dtype=torch.double)
        obj.best = d["best"]
        return obj


def load_bounds(path: Path) -> Tuple[torch.Tensor, torch.Tensor, List[str]]:
    spec = json.loads(path.read_text())
    names = [b["name"] for b in spec]
    lower = torch.tensor([b["lower"] for b in spec], dtype=torch.double)
    upper = torch.tensor([b["upper"] for b in spec], dtype=torch.double)
    return lower, upper, names


def load_data(path: Path, names: List[str]) -> Tuple[torch.Tensor, torch.Tensor]:
    import pandas as pd

    df = pd.read_csv(path)
    x = torch.tensor(df[names].values, dtype=torch.double)
    y = torch.tensor(df["y"].values, dtype=torch.double).unsqueeze(-1)
    return x, y


def fit_model(x_norm: torch.Tensor, y: torch.Tensor) -> SingleTaskGP:
    gp = SingleTaskGP(x_norm, y)
    mll = ExactMarginalLogLikelihood(gp.likelihood, gp)
    fit_gpytorch_mll(mll)
    return gp


def suggest_batch(
    state: TurboState,
    x: torch.Tensor,
    y: torch.Tensor,
    lower: torch.Tensor,
    upper: torch.Tensor,
    batch_size: int,
    device: torch.device,
) -> Tuple[torch.Tensor, TurboState]:
    # Normalize data to [0,1]^d
    x_norm = normalize(x, bounds=torch.stack([lower, upper]).to(device))
    # Update center to incumbent
    best_val, best_idx = y.max(dim=0)
    state.best = max(state.best, best_val.item())
    state.center = x_norm[best_idx].squeeze(0).detach()

    # Trust region bounds
    half = state.length / 2.0
    tr_lb = torch.clamp(state.center - half, 0.0, 1.0)
    tr_ub = torch.clamp(state.center + half, 0.0, 1.0)
    tr_bounds = torch.stack([tr_lb, tr_ub]).to(device)

    # Fit GP on normalized data
    gp = fit_model(x_norm.to(device), y.to(device))

    ei = qExpectedImprovement(model=gp, best_f=state.best, maximize=True)
    candidates, _ = optimize_acqf(
        acq_function=ei,
        bounds=tr_bounds,
        q=batch_size,
        num_restarts=20,
        raw_samples=256,
    )
    cand_denorm = unnormalize(candidates, bounds=torch.stack([lower, upper]).to(device))
    return cand_denorm.cpu(), state


def update_state(state: TurboState, y_new: torch.Tensor):
    best_new = y_new.max().item()
    eps = 1e-3 * abs(state.best) if math.isfinite(state.best) else 0.0
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
        # restart
        state.length = 0.8
        state.successes = 0
        state.failures = 0
        state.center = torch.rand_like(state.center)
        state.best = -float("inf")
    else:
        state.best = max(state.best, best_new)


def main():
    parser = argparse.ArgumentParser(description="BoTorch TuRBO bridge (suggest-only).")
    parser.add_argument("--bounds", required=True, type=Path)
    parser.add_argument("--data", required=True, type=Path, help="CSV with params + y")
    parser.add_argument("--state", required=True, type=Path, help="JSON state path")
    parser.add_argument("--batch-size", type=int, default=5)
    parser.add_argument("--device", default="cpu")
    args = parser.parse_args()

    lower, upper, names = load_bounds(args.bounds)
    x, y = load_data(args.data, names)
    device = torch.device(args.device)

    if args.state.exists():
        state = TurboState.from_json(json.loads(args.state.read_text()))
    else:
        state = TurboState(dim=x.shape[1], batch_size=args.batch_size)

    cands, state = suggest_batch(state, x, y, lower, upper, args.batch_size, device)

    # Emit suggestions
    out = []
    for row in cands.tolist():
        out.append({n: v for n, v in zip(names, row)})
    print(json.dumps({"candidates": out}, indent=2))

    # Update state with imaginary feedback? We don’t have y_new here; caller should update after eval.
    # Persist current state (unchanged) so caller can load, update_state after observing y_new, and resave.
    args.state.write_text(json.dumps(state.to_json(), indent=2))


if __name__ == "__main__":
    main()
