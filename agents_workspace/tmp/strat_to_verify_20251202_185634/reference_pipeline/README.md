# Amp/Stop Edge Bundle

This folder collects the minimal OCaml + Python scripts involved in the
triple-barrier stop-label experiments and the two-stage "amp → stop"
models that produced the Sharpe ratios we explored.

## Files

- `ml_export_main.ml`
  - OCaml CLI that reads raw 1m ES data and produces ML exports.
  - In this context it is used (via `dune exec`) to generate
    `ml_exports/ml_export_es_3m_amp_50_20_full.csv`, a 3-minute ES
    feature file with:
    - Brooks-style features (atr10, z_vwap, rv10/rv60, range/leg stats, etc.).
    - Amplitude labels `label_amp` computed by `Amplitude_labeler`.

- `context.ml`
  - OCaml context manager that maintains the rolling Brooks "market
    state" across bars.
  - Owns the `Context.t` record, handles per-bar updates from raw
    bars (including day/session resets), and calls into
    `Features.snapshot` to produce the feature vector that
    `ml_export_main.ml` writes to CSV.
  - This is the glue that ensures features are computed causally from
    past and current bars only; it is crucial for understanding how
    the exported features align with the playback engine.

- `features.ml`
  - OCaml feature engine that maintains rolling state (VWAP, RV, OFI,
    EMA, intraday phase, etc.) and produces the Brooks feature snapshot
    used in the ML exports.
  - All numeric features referenced in the Python scripts (`atr10`,
    `z_vwap`, `rv10`, `rv60`, `rv_ratio`, `trend_feat`, `gap`,
    `dist_onh/onl`, `dist_ema20`, `ema20_slope`, leg/range stats, day
    context, etc.) ultimately come from this module.

- `amplitude_labeler.ml`
  - OCaml implementation of the amplitude-based labeler.
  - Computes `label_amp` by segmenting cumulative returns (in bps)
    into up / down / flat episodes based on minimum amplitude and
    inactivity parameters.
  - Used by `ml_export_main.ml` to write `label_amp` into the exports
    (e.g. up/down/flat for each 3m bar).

- `stop_label_hgb_analysis.py`
  - Python script that:
    - Loads an ML export (e.g. `ml_export_es_new.csv`) with Brooks
      features.
    - Uses `compute_stop_labels` to construct triple-barrier labels
      (`label_stop ∈ {up,down,None}`) from raw 1m ES data.
    - Builds a Brooks-only feature matrix via `build_features`.
    - Trains a HistGradientBoostingClassifier to predict the stop
      label on the up/down subset.
    - Runs a zero-cost triple-barrier backtest on **all** RTH test
      bars, using the model’s `p_hgb_stop` as the side signal.
  - This script produced the initial stop-model Sharpe (Brooks features
    only) and the random-side baseline (via slight modifications).

- `amp_two_stage_stop_xgb.py`
  - Python script that implements the two-stage pipeline:
    1. **Model A (amp)**: 3-class XGB on `label_amp` using Brooks
       features, outputs `p_amp_down/flat/up`.
    2. **Model B (stop)**: binary XGB on `label_stop` (triple-barrier
       up/down) using Brooks features **plus** `p_amp_*` as extra
       inputs.
  - For backtesting:
    - Applies Model B over all RTH test bars (no label-based gating).
    - For each bar, simulates a 1-tick stop entry in the predicted
      direction and a symmetric ATR-based bracket with 10-bar horizon.
    - Records zero PnL for no-fill / ambiguous cases, and closes at
      horizon or intraday day-end.
  - This script produced the Sharpe numbers for:
    - Brooks-only stop model.
    - Two-stage amp→stop model.
    - Comparison vs random-side triple-barrier baseline.

## How They Connect

End-to-end data flow for the experiments:

1. **Raw ES 1m → 3m export (OCaml)**
   - Run from `agents_workspace`:
     ```bash
     dune exec --root . bin/ml_export_main.exe -- \
       es.c.0-20100606-20251116.et.ohlcv-1m.csv \
       ml_exports/ml_export_es_3m_amp_50_20_full.csv \
       3m 50 20
     ```
   - This uses `features.ml` + `amplitude_labeler.ml` to produce a
     3m ES CSV with Brooks features + `label_amp`.

2. **Triple-barrier labels (Python)**
   - `stop_label_hgb_analysis.py` and `amp_two_stage_stop_xgb.py`
     both import `build_features` and `compute_stop_labels` from
     `stop_label_hgb_analysis.py`.
   - `compute_stop_labels` reads the raw ES 1m CSV, maps each 3m
     record by timestamp, and constructs a long-entry triple-barrier
     outcome (`label_stop`) using ATR10 and a 10-bar horizon.

3. **Stop-only model (Brooks features)**
   - `stop_label_hgb_analysis.py`:
     - Builds features with `build_features`.
     - Trains HGB to predict `label_stop` (up vs down) on the subset
       where the label is defined.
     - Backtests on the full test split using the model’s probability
       as the side for a triple-barrier trading rule.

4. **Two-stage amp → stop model**
   - `amp_two_stage_stop_xgb.py`:
     - Trains Model A (amp XGB) on `label_amp` and writes `p_amp_*`.
     - Computes `label_stop` via `compute_stop_labels`.
     - Trains Model B (stop XGB) on Brooks features + `p_amp_*`.
     - Backtests on the full test split (same triple-barrier engine),
       comparing Sharpe with and without the amp-derived features and
       against a random-side baseline.

## Running the Python Experiments

From `agents_workspace` (with the existing `.venv` activated):

```bash
cd agents_workspace
.venv/bin/python tmp/stop_label_hgb_analysis.py es
.venv/bin/python tmp/amp_two_stage_stop_xgb.py
```

Both scripts assume:
- `ml_exports/ml_export_es_3m_amp_50_20_full.csv` exists (from
  `ml_export_main.exe`).
- Raw ES 1m file is at `../es.c.0-20100606-20251116.et.ohlcv-1m.csv`.
