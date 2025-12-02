from dataclasses import dataclass


@dataclass(frozen=True)
class DataPaths:
    ml_export_path: str = "ml_exports/ml_export_es_3m_amp_50_20_full.csv"
    raw_1m_path: str = "../es.c.0-20100606-20251116.et.ohlcv-1m.csv"


@dataclass(frozen=True)
class AmpModelConfig:
    n_estimators: int = 300
    max_depth: int = 4
    learning_rate: float = 0.05
    subsample: float = 0.8
    colsample_bytree: float = 0.8
    random_state: int = 42


@dataclass(frozen=True)
class StopModelConfig:
    n_estimators: int = 300
    max_depth: int = 4
    learning_rate: float = 0.05
    subsample: float = 0.8
    colsample_bytree: float = 0.8
    random_state: int = 42


@dataclass(frozen=True)
class BacktestConfig:
    tick_size: float = 0.25
    horizon_bars: int = 10
    long_threshold: float = 0.7
    short_threshold: float = 0.3
    entry_shift_min: int = 2

