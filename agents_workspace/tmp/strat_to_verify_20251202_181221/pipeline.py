from typing import Tuple

import numpy as np
import pandas as pd

from amp_stage import train_amp_model
from constants import AMP_COLS
from config import AmpModelConfig, BacktestConfig, DataPaths, StopModelConfig
from results import PipelineOutputs
from stop_stage import (
    attach_stop_labels,
    make_updown_subsets,
    run_naive_backtest,
    train_stop_model,
)


def _load_data(paths: DataPaths) -> Tuple[pd.DataFrame, pd.DataFrame]:
    ml = pd.read_csv(paths.ml_export_path, low_memory=False)
    raw = pd.read_csv(paths.raw_1m_path)
    return ml, raw


def _split_amp_train_test(ml: pd.DataFrame) -> Tuple[pd.DataFrame, pd.DataFrame]:
    ml = ml[ml["label_amp"].isin(["up", "down", "flat"])].copy()
    train_df = ml[ml["split"] == "train"].copy().reset_index(drop=True)
    test_df = ml[ml["split"] == "test"].copy().reset_index(drop=True)
    return train_df, test_df


def _attach_amp_probabilities(
    train_df: pd.DataFrame,
    test_df: pd.DataFrame,
    proba_train_amp: np.ndarray,
    proba_test_amp: np.ndarray,
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    train_df = train_df.copy()
    test_df = test_df.copy()
    for idx, col in enumerate(AMP_COLS):
        train_df[col] = proba_train_amp[:, idx]
        test_df[col] = proba_test_amp[:, idx]
    return train_df, test_df


def run_pipeline(
    paths: DataPaths | None = None,
    amp_cfg: AmpModelConfig | None = None,
    stop_cfg: StopModelConfig | None = None,
    bt_cfg: BacktestConfig | None = None,
) -> PipelineOutputs:
    paths = paths or DataPaths()
    amp_cfg = amp_cfg or AmpModelConfig()
    stop_cfg = stop_cfg or StopModelConfig()
    bt_cfg = bt_cfg or BacktestConfig()

    ml, raw = _load_data(paths)
    train_df, test_df = _split_amp_train_test(ml)

    proba_train_amp, proba_test_amp, _, amp_metrics = train_amp_model(
        train_df, test_df, amp_cfg
    )
    train_df_amp, test_df_amp = _attach_amp_probabilities(
        train_df, test_df, proba_train_amp, proba_test_amp
    )

    train_with_stop, test_with_stop = attach_stop_labels(
        train_df_amp, test_df_amp, raw, bt_cfg
    )
    train_tb, test_tb = make_updown_subsets(train_with_stop, test_with_stop)

    _, _, clf_stop, feature_space, stop_metrics = train_stop_model(
        train_tb, test_tb, stop_cfg
    )

    backtest_metrics = run_naive_backtest(
        test_df_amp, raw, clf_stop, feature_space, bt_cfg
    )

    return PipelineOutputs(
        amp_metrics=amp_metrics,
        stop_metrics=stop_metrics,
        backtest_metrics=backtest_metrics,
    )
