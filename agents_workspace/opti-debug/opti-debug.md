# Optimization Debug Notes (Nov 26, 2025, failing long-run BO jobs)

Purpose: hand-off snapshot of the optimization path and current issues for expert review. Captures minimal but complete info to reproduce the long-running/“hanging” optimizer runs (Bayes/qNEI sidecar).

## Context (you can recreate without repo)
- Language split: OCaml driver calls a Python FastAPI sidecar for BoTorch TuRBO-m + qNEI suggestions.
- OCaml sends: bounds with domain info (continuous/integer/discrete/categorical), X (past points), Y (scores), gets back candidates.
- Sidecar does: normalize -> fit SingleTaskGP -> qNEI -> optimize_acqf (heavy) -> unnormalize -> snap integers/discretes.
- Benchmark data in the repo was `sample_es.csv` (~50k rows), but the slowdown is dominated by acquisition optimization, not data size.

## Symptoms
- Running Bayes or even random search with `samples=6` causes wall-clock > 2–5 minutes (appears to hang). Tiny jobs (`samples<=3`) finish fast (<1s). Single strategy run is ~1s.
- The sidecar now works (init/suggest ok) after fixing `integer_mask` bug, but long BO runs still very slow.
- Dune rebuild/caching was disabled during some runs (`DUNE_CACHE=disabled`), so rebuild overhead is not the current culprit (prebuilt binaries also slow for these BO runs).

- qNEI `optimize_acqf` with defaults: `num_restarts=10`, `raw_samples=128`, per TuRBO region. That dominates runtime even on tiny datasets; not a hang, just heavy optimization per iteration.
- Candidate evaluations are serial (no parallelism around `eval_batch`).

## Key code snippets (current state)

### Minimal code you can paste elsewhere to reproduce the behavior

**Sidecar (Python 3.10+, torch 2.9.1, botorch 0.16.1):**
```python
# save as service.py
import math, torch
from fastapi import FastAPI
from pydantic import BaseModel
from botorch import fit_gpytorch_mll
from botorch.acquisition import qNoisyExpectedImprovement
from botorch.models import SingleTaskGP
from botorch.optim import optimize_acqf
from botorch.utils.transforms import normalize, unnormalize
from gpytorch.mlls import ExactMarginalLogLikelihood

class Bounds(BaseModel):
    name: str; lower: float; upper: float; integer: bool=False; domain: dict
class InitRequest(BaseModel): bounds:list[Bounds]; batch_size:int=3; n_regions:int=1
class SuggestRequest(BaseModel): X:list[list[float]]; Y:list[float]; state:dict|None=None

app = FastAPI()
state = None

def encode_X(X, domains):
    X = torch.tensor(X, dtype=torch.double)
    meta=[]
    cols=[]
    for j,d in enumerate(domains):
        k=d["kind"]
        if k=="categorical":
            vals=d["values"]; meta.append(("categorical",len(vals)))
            idx=X[...,j].round().clamp(0,len(vals)-1); cols.append(idx.unsqueeze(-1))
        elif k=="discrete":
            vals=torch.tensor(d["values"],dtype=X.dtype)
            diffs=(X[...,j].unsqueeze(-1)-vals).abs(); idx=diffs.argmin(dim=-1)
            meta.append(("discrete",len(vals))); cols.append(idx.unsqueeze(-1))
        else:
            meta.append((k,1)); cols.append(X[...,j].unsqueeze(-1))
    return torch.cat(cols,dim=-1), meta

def decode_X(X, domains, meta, bounds):
    cols=[]
    for j,d in enumerate(domains):
        k=d["kind"]
        if k=="categorical":
            vals=d["values"]; idx=X[...,j].round().long().clamp(0,len(vals)-1)
            cols.append(idx.to(torch.float))
        elif k=="discrete":
            vals=torch.tensor(d["values"],dtype=X.dtype); idx=X[...,j].round().long().clamp(0,len(vals)-1)
            cols.append(vals[idx])
        elif k=="integer":
            cols.append(X[...,j].round())
        else:
            cols.append(X[...,j])
    return torch.stack(cols,dim=-1)

def snap_integers(x, bounds, int_mask):
    if x.numel()==0 or not int_mask.any(): return x
    x=x.clone(); lb=bounds[0,int_mask]; ub=bounds[1,int_mask]
    x[...,int_mask]=torch.max(torch.min(torch.round(x[...,int_mask]),ub),lb)
    return x

@app.post("/init")
def init(req:InitRequest):
    global state
    state = {
        "bounds": [b.dict() for b in req.bounds],
        "lower": torch.tensor([b.lower for b in req.bounds],dtype=torch.double),
        "upper": torch.tensor([b.upper for b in req.bounds],dtype=torch.double),
        "int_mask": torch.tensor([1 if b.integer else 0 for b in req.bounds],dtype=torch.bool),
        "domains": [b.domain for b in req.bounds],
        "length":0.8,"length_min":0.5**7,"length_max":1.6,
        "success_t":10,"failure_t":math.ceil(max(4/req.batch_size,len(req.bounds)/req.batch_size)),
        "successes":0,"failures":0,"best":-1e30,
        "center": torch.full((len(req.bounds),),0.5,dtype=torch.double),
        "batch_size": req.batch_size,
    }
    return {"state": state}

def fit_model(X,Y):
    gp=SingleTaskGP(X,Y)
    mll=ExactMarginalLogLikelihood(gp.likelihood,gp)
    fit_gpytorch_mll(mll); return gp

@app.post("/suggest")
def suggest(req:SuggestRequest):
    global state
    if req.state: state = req.state
    bounds = torch.stack([torch.tensor(state["lower"]), torch.tensor(state["upper"])])
    X = torch.tensor(req.X,dtype=torch.double); Y=torch.tensor(req.Y,dtype=torch.double).unsqueeze(-1)
    X_enc, meta = encode_X(X, state["domains"])
    X_norm = normalize(X_enc, bounds=bounds)
    gp = fit_model(X_norm, Y)
    qnei = qNoisyExpectedImprovement(model=gp, X_baseline=X_norm)
    tr_lb = torch.clamp(torch.tensor(state["center"]) - state["length"]/2, 0.0, 1.0)
    tr_ub = torch.clamp(torch.tensor(state["center"]) + state["length"]/2, 0.0, 1.0)
    cand, _ = optimize_acqf(qnei, bounds=torch.stack([tr_lb,tr_ub]), q=state["batch_size"],
                            num_restarts=10, raw_samples=128)
    X_cand = unnormalize(cand, bounds=bounds)
    X_cand = decode_X(X_cand, state["domains"], meta, bounds)
    X_cand = snap_integers(X_cand, bounds, torch.tensor(state["int_mask"]))
    return {"candidates": X_cand.tolist(), "region_ids": [0]*X_cand.shape[0], "state": state}

# run with: uvicorn service:app --host 127.0.0.1 --port 10001
```

**Client call pattern (language-agnostic):**
1) POST /init with bounds (include `domain` e.g., `{kind:"continuous"}`) -> get `state`.
2) POST /suggest with X, Y, state -> get candidates, new state. Repeat.

**Heavy bit:** `optimize_acqf(... num_restarts=10, raw_samples=128)` per suggest.

## Repro commands + observed runtime (shell)

Slow case:
```
cat > /tmp/random_vwap2.json <<'JSON'
{ "strategy": "vwap_revert", "data": "sample_es.csv",
  "search": { "type": "random", "samples": 6, "seed": 7 },
  "objective": "sharpe", "batch_size": 3, "restarts": 1,
  "shared_stream": true, "robustness": { "bumps": [] } }
JSON
./_build/default/bin/optimize_main.exe /tmp/random_vwap2.json
# actual: >60–120s, killed
```

Fast control:
```
cat > /tmp/random_vwap3.json <<'JSON'
{ "strategy": "vwap_revert", "data": "sample_es.csv",
  "search": { "type": "random", "samples": 3, "seed": 7 },
  "objective": "sharpe", "batch_size": 1, "restarts": 1,
  "shared_stream": true, "robustness": { "bumps": [] } }
JSON
./_build/default/bin/optimize_main.exe /tmp/random_vwap3.json
# output: Done... runtime ~0.28s
```

Quick timings (same machine, prebuilt binaries):
- Single run: `vwap_revert_main sample_es.csv` ~0.9s
- Grid 1-step: `optimize_main /tmp/one_vwap.json` ~0.17s
- Random samples=3: ~0.28s
- Random/Bayes samples=6: >60–120s (killed)

Log snippet (sidecar):
```
qNoisyExpectedImprovement has known numerical issues ...
```
Earlier bug fixed: AttributeError 'TurboState' object has no attribute 'integer_mask'.

## Minimal test data
You can use any dummy objective; data size doesn’t matter for the slowdown. The heavy part is the qNEI optimize_acqf call with 10 restarts/128 raw samples.

- Replace qNEI with qLogNEI; reduce num_restarts to 2–3 and raw_samples to 32–64.
- Or use Sobol pool + top-k qEI/qLogNEI instead of optimize_acqf.
- Add client-side HTTP timeouts.



## Repro steps
1) Ensure sidecar is running (current PID may vary):
   ```sh
   cd /Users/simongu/Documents/Library_QPA/QPA/agents_workspace
   source .venv/bin/activate
   nohup uvicorn tools.botorch_service:app --host 127.0.0.1 --port 10001 --log-level warning >/tmp/botorch_service.log 2>&1 &
   ```
2) Run a minimal job that hangs/slows:
   ```sh
   cat > /tmp/random_vwap2.json <<'JSON'
   {
     "strategy": "vwap_revert",
     "data": "sample_es.csv",
     "search": { "type": "random", "samples": 6, "seed": 7 },
     "objective": "sharpe",
     "batch_size": 3,
     "restarts": 1,
     "shared_stream": true,
     "robustness": { "bumps": [] }
   }
   JSON

   # Use prebuilt binary to avoid rebuild time:
   cd /Users/simongu/Documents/Library_QPA/QPA/agents_workspace
   ./_build/default/bin/optimize_main.exe /tmp/random_vwap2.json
   ```
   Expectation: should finish in seconds; actual: runs >60–120s (then killed).

3) Fast control that succeeds:
   ```sh
   cat > /tmp/random_vwap3.json <<'JSON'
   {
     "strategy": "vwap_revert",
     "data": "sample_es.csv",
     "search": { "type": "random", "samples": 3, "seed": 7 },
     "objective": "sharpe",
     "batch_size": 1,
     "restarts": 1,
     "shared_stream": true,
     "robustness": { "bumps": [] }
   }
   JSON
   ./_build/default/bin/optimize_main.exe /tmp/random_vwap3.json
   # finishes ~0.3s
   ```

## Sidecar acquisition settings (current)
- qNoisyExpectedImprovement (warning suggests using qLogNEI).
- `optimize_acqf` with `num_restarts=10`, `raw_samples=128`.
- Trust region candidate bounds come from TuRBO state; domains are encoded (continuous/integer/discrete/categorical).
- Integer/discrete snapping after unnormalize; categorical/discrete encoded/decoded in sidecar.

## Observed logs
- `/tmp/botorch_service.log` shows qNEI numerics warning; no further stack traces after integer_mask fix.

- Not a code hang; qNEI optimize_acqf is too heavy for small jobs. Each BO iteration spends most time in Python optimization, so total time is >> linear in #evaluations.

- Switch to qLogNEI; reduce `num_restarts` to 2–3 and `raw_samples` to 32–64.
- Provide a “fast mode” flag in sidecar; default to fast for small jobs.
- Add curl timeouts in OCaml client.
- Optional: parallelize candidate evaluations in OCaml.

## Environment
- Python: .venv/bin/python3 (3.10.19); torch 2.9.1; botorch 0.16.1.
- OCaml switch: 5.4.0.

## Current sidecar state
- Running PID may be stale; last started: `nohup uvicorn ... --port 10001` (check `lsof -i :10001`).

## Repro commands + observed runtime

1) Slow case (hang/kill):
```
cat > /tmp/random_vwap2.json <<'JSON'
{
  "strategy": "vwap_revert",
  "data": "sample_es.csv",
  "search": { "type": "random", "samples": 6, "seed": 7 },
  "objective": "sharpe",
  "batch_size": 3,
  "restarts": 1,
  "shared_stream": true,
  "robustness": { "bumps": [] }
}
JSON
cd /Users/simongu/Documents/Library_QPA/QPA/agents_workspace
./_build/default/bin/optimize_main.exe /tmp/random_vwap2.json
# actual: runs >60–120s, then killed (no output)
```

2) Fast control:
```
cat > /tmp/random_vwap3.json <<'JSON'
{
  "strategy": "vwap_revert",
  "data": "sample_es.csv",
  "search": { "type": "random", "samples": 3, "seed": 7 },
  "objective": "sharpe",
  "batch_size": 1,
  "restarts": 1,
  "shared_stream": true,
  "robustness": { "bumps": [] }
}
JSON
./_build/default/bin/optimize_main.exe /tmp/random_vwap3.json
# output example: Done. Best score -0.1566 (tested 4, guardrail_rej 3, robustness_rej 0, cache_hits 0).
# runtime ~0.28s
```

Quick timings (same machine)
- Single run: `./_build/default/bin/vwap_revert_main.exe sample_es.csv` -> ~0.9s
- Grid 1-step: `./_build/default/bin/optimize_main.exe /tmp/one_vwap.json` -> ~0.17s
- Random samples=3: ~0.28s
- Random/Bayes samples=6: >60–120s (killed)

Observed log snippet (/tmp/botorch_service.log):
```
qNoisyExpectedImprovement has known numerical issues ...
```
