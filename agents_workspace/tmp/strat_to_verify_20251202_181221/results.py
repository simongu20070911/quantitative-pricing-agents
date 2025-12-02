from dataclasses import dataclass
from typing import List, Sequence

import numpy as np


@dataclass
class AmpModelMetrics:
    class_counts: np.ndarray
    class_weights: np.ndarray
    acc_train: float
    acc_test: float


@dataclass
class StopModelMetrics:
    n_train_tb: int
    n_test_tb: int
    y_train_mean: float
    y_test_mean: float
    shape_train: tuple[int, int]
    shape_test: tuple[int, int]
    acc_train: float
    acc_test: float
    auc_train: float
    auc_test: float


@dataclass
class BacktestMetrics:
    trades_used: int
    mean_pnl: float
    std_pnl: float
    win_frac: float
    loss_frac: float
    flat_frac: float
    days_with_trades: int
    mean_daily: float
    std_daily: float
    sharpe_daily: float
    sharpe_annual: float
    pnls: np.ndarray
    trade_dates: Sequence[str]


@dataclass
class PipelineOutputs:
    amp_metrics: AmpModelMetrics
    stop_metrics: StopModelMetrics
    backtest_metrics: BacktestMetrics
