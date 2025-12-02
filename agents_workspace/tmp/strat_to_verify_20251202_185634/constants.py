from __future__ import annotations

import sys
from pathlib import Path


# Filesystem layout ---------------------------------------------------------

CURRENT_DIR: Path = Path(__file__).resolve().parent
TMP_DIR: Path = CURRENT_DIR.parent
AGENTS_ROOT: Path = TMP_DIR.parent
PROJECT_ROOT: Path = AGENTS_ROOT.parent

# Ensure we can import supporting tmp-level scripts such as
# stop_label_hgb_analysis.py and amp_two_stage_stop_xgb.py.
if str(TMP_DIR) not in sys.path:
    sys.path.insert(0, str(TMP_DIR))


# Data paths ----------------------------------------------------------------

ML_EXPORT_PATH: Path = AGENTS_ROOT / "ml_exports" / "ml_export_es_3m_amp_50_20_full.csv"
RAW_ES_1M_PATH: Path = PROJECT_ROOT / "es.c.0-20100606-20251116.et.ohlcv-1m.csv"


# Backtest configuration ----------------------------------------------------

TICK: float = 0.25
HORIZON_BARS: int = 10

# For the 3m export, each row at time t represents [t, t+2].
# We therefore shift the stop entry to the 1m bar at t+2 so that
# features and entry price are aligned at the 3m bar close.
ENTRY_SHIFT_MIN: int = 2


# Output locations ----------------------------------------------------------

# Replication-specific outputs (kept inside this strat_to_verify folder)
REPL_TRADES_CSV: Path = CURRENT_DIR / "stop_trades_equity_extreme_rep.csv"
REPL_DAILY_CSV: Path = CURRENT_DIR / "stop_daily_equity_extreme_rep.csv"

# Original script outputs (for comparison only; not written here)
ORIG_TRADES_CSV: Path = TMP_DIR / "stop_trades_equity_extreme.csv"
ORIG_DAILY_CSV: Path = TMP_DIR / "stop_daily_equity_extreme.csv"

