# Strategy_fast Modular Structure (agents_workspace)

### What lives where
- **core/**
  - `types.ml` — generic domain types (bars, trades, daily PnL). No strategy-specific fields.
  - `time_utils.ml` — session times, timestamp parsing, RTH helpers.
  - `csv_parser.ml` — 1m bar ingest (expects ES-style CSV; see `sample_es.csv`).
  - `summary.ml/.mli` — stats, CSV exporters, pretty prints.
  - `parameters.ml/.mli` — “magic number” registry: specs with bounds/scale/fixed/integer flags and helpers to merge overrides.
  - `guardrails.ml/.mli` — central statistical guardrail record + loader from `config/optimization_guardrails.yaml` (JSON-formatted YAML).
  - `position_sizing.ml/.mli` — simple vol-target sizing helpers (contracts per signal / vol).
  - `cost_model.ml/.mli` — slippage + fee deduction (tick- and USD-based knobs), equity_base-aware; fills `qty`, `pnl_usd`, `pnl_pct` on trades.

- **features/**
  - `indicators.ml/.mli` — rolling helpers (mean, stdev, EMA) shared by features.
  - `features.ml/.mli` — VWAP + zVWAP, OFI proxy, RV10/60, trend, gap, overnight high/low distances; produces a `Feature_snapshot` consumed by strategies.

- **engine/**
  - `policy_sig.ml/.mli` — public Strategy API (`init`, `on_bar`, `finalize`).
  - `engine.ml/.mli` — single-strategy runner on a CSV stream.
  - `multi_engine.ml/.mli` — shared-bar loop feeding multiple strategies (shared features, per-strategy state/trades) without unsafe Obj hacks.
  - `backtest.ml/.mli` — thin legacy wrapper retained for compatibility/tests.
  - `stat_tests.ml/.mli` — basic permutation p-value + bootstrap CI helpers.
  - `robustness.ml/.mli` — param-bump stability checks (stability fraction, cliff detection).
  - `optimizer.ml/.mli` — grid / random / LHS / Bayes (TPE-style) search driver with caching option, guardrails, and optional robustness bumps.

- **strategies/**
  - `setup_builder_b1b2.ml` — B1/B2 setup detection (kept strategy-specific, not in generic features).
  - `trade_logic.ml` — B1/B2 plan execution (entries/exits/stops in ticks). Uses integer ticks to avoid float edge drift.
  - `strategy_b1b2.ml` — wraps setup + trade logic into `Policy_sig` strategy.
  - `vwap_revert_strategy.ml` — VWAP-based mean-reversion policy built on shared `Feature_snapshot`, `position_sizing`, and `cost_model`.
  Each strategy now exposes `strategy_id` and `parameter_specs` to feed optimizers/guardrails.

- **plot/**
  - `plotter.ml/.mli` — OCaml Cairo plots (equity per trade, daily equity, histogram).

- **strategy_fast.ml** — umbrella module that re-exports curated submodules with `wrapped=true` dune library.

### How to run (OCaml)
```sh
eval "$(opam env --switch=5.4.0 --set-switch)"
dune build

# B1/B2 strategy on a CSV
dune exec bin/strategy_fast_main.exe -- es.c.0-20100606-20251116.et.ohlcv-1m.csv \
  --plot plots \
  --export-trades plots/b1b2_trades.csv \
  --export-daily plots/b1b2_daily.csv

# VWAP-revert strategy
dune exec bin/vwap_revert_main.exe -- es.c.0-20100606-20251116.et.ohlcv-1m.csv \
  --plot plots
```

### Optional Python plotting (kept outside hot path)
```sh
python3 -m venv .venv && . .venv/bin/activate
pip install -r tools/requirements.txt

dune exec bin/strategy_fast_main.exe -- sample_es.csv --plot-python plots_py/b1b2
dune exec bin/vwap_revert_main.exe -- sample_es.csv --plot-python plots_py/vwap
```
The executable exports trades/daily CSVs into the target dir and calls `tools/plot_trades.py` to render labeled PNGs.

### Optimizer driver (new)
```sh
dune exec bin/optimize_main.exe -- job.json
```
`job.json` schema is documented at the top of `bin/optimize_main.ml`. Search types: `grid`, `random`, `lhs`, `bayes`. Optional fields: `perm_reps`, `bootstrap_reps`, `robustness: {"bumps": [...]}`, `cache: true`. Outputs land in `runs/<strategy>/<timestamp>/` with `summary.json`, `trades.csv`, and `daily.csv`. Guardrails load from `config/optimization_guardrails.yaml`.

### Current safety/maintainability choices
- No Obj-based heterogeneous state; multi-strategy loop uses first-class modules to keep types intact.
- All public modules have `.mli` interfaces to avoid accidental namespace bleed; library is `wrapped=true`, so reference via `Strategy_fast.<Module>`.
- Tick/fee/slippage constants live in `cost_model` and configs passed to strategies, not scattered literals.
- CLI supports `--help`, and optional `--plot/--plot-python/--export-*` flags; filenames validated before use.
- Features module is purely feature math; strategy-specific setup building (B1/B2) stays in `strategies/`.
- Parameter registry + guardrails implemented; defaults live in `config/optimization_guardrails.yaml` with per-strategy overrides.
- Optimizer now supports Bayes/TPE sampling, caching of duplicate param evaluations, and optional robustness bumps; still intentionally small and typed. Capacity guardrail hook exists (stub until trade sizing is tracked).
- Trades now carry `qty`, `pnl_usd`, and optional `pnl_pct` (if equity_base provided); daily exports include R/USD/% columns.

### Roadmap (agreed but not yet implemented)
- **Macro/session filters:** configurable blackout windows around macro events and open/close; hook into engine entry gating.
- **Capacity/impact model:** extend `cost_model` with depth-aware impact curve, fed to backtests and optimizers.
- **Capacity/impact model:** extend `cost_model` with depth-aware impact curve, fed to backtests and optimizers.
- **Schema validation + richer artifacts:** validate job.json, add plots/manifests, parallel runs.

Until the roadmap items land, this README stays the single place to record the intended shape so we can implement without rediscovering the design.
