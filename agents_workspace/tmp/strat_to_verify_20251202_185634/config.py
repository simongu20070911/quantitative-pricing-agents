from __future__ import annotations

from dataclasses import dataclass

import constants


@dataclass(frozen=True)
class AmpModelConfig:
    """Hyperparameters for the amplitude (3-class) XGB model."""

    n_estimators: int = 300
    max_depth: int = 4
    learning_rate: float = 0.05
    subsample: float = 0.8
    colsample_bytree: float = 0.8
    random_state: int = 42


@dataclass(frozen=True)
class StopModelConfig:
    """Hyperparameters for the stop (binary) XGB model."""

    n_estimators: int = 300
    max_depth: int = 4
    learning_rate: float = 0.05
    subsample: float = 0.8
    colsample_bytree: float = 0.8
    random_state: int = 42


@dataclass(frozen=True)
class BacktestConfig:
    """Configuration for the naive triple-barrier backtest."""

    tick: float = constants.TICK
    horizon: int = constants.HORIZON_BARS
    entry_shift_min: int = constants.ENTRY_SHIFT_MIN
    prob_long_threshold: float = 0.7
    prob_short_threshold: float = 0.3

