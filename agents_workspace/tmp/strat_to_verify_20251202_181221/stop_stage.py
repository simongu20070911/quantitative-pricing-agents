from typing import List, Tuple

import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.metrics import accuracy_score, roc_auc_score

from config import BacktestConfig, StopModelConfig
from features_adapter import FeatureSpace, build_aligned_features, transform_full_test
from results import BacktestMetrics, StopModelMetrics
from stop_label_hgb_analysis import compute_stop_labels
from constants import AMP_COLS


def build_stop_labels_for_split(
    df: pd.DataFrame, raw: pd.DataFrame, cfg: BacktestConfig
) -> List[str]:
    """Wrapper over compute_stop_labels with explicit entry-shift semantics."""
    return compute_stop_labels(
        df,
        raw,
        tick=cfg.tick_size,
        horizon=cfg.horizon_bars,
        entry_shift_min=cfg.entry_shift_min,
    )


def attach_stop_labels(
    train_df: pd.DataFrame,
    test_df: pd.DataFrame,
    raw: pd.DataFrame,
    cfg: BacktestConfig,
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """Compute and attach stop-label targets to train/test splits."""
    train_labels = build_stop_labels_for_split(train_df, raw, cfg)
    test_labels = build_stop_labels_for_split(test_df, raw, cfg)

    train_df = train_df.copy()
    test_df = test_df.copy()
    train_df["label_stop"] = train_labels
    test_df["label_stop"] = test_labels
    return train_df, test_df


def make_updown_subsets(
    train_df: pd.DataFrame, test_df: pd.DataFrame
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """Restrict to definitive up/down stop hits, mirroring the original script."""
    train_tb = train_df[train_df["label_stop"].isin(["up", "down"])].copy()
    test_tb = test_df[test_df["label_stop"].isin(["up", "down"])].copy()
    train_tb["y_stop"] = (train_tb["label_stop"] == "up").astype(int)
    test_tb["y_stop"] = (test_tb["label_stop"] == "up").astype(int)
    return train_tb, test_tb


def train_stop_model(
    train_tb: pd.DataFrame,
    test_tb: pd.DataFrame,
    cfg: StopModelConfig,
) -> Tuple[np.ndarray, np.ndarray, xgb.XGBClassifier, FeatureSpace, StopModelMetrics]:
    """Train the binary stop-label classifier with Brooks features + amp probs."""
    X_train_base, X_test_base_tb, feature_space = build_aligned_features(train_tb, test_tb)

    for c in AMP_COLS:
        if c not in train_tb.columns or c not in test_tb.columns:
            raise ValueError(f"Missing amp probability column '{c}' in train/test.")

    amp_train = train_tb[AMP_COLS].values.astype(np.float32)
    amp_test_tb = test_tb[AMP_COLS].values.astype(np.float32)

    X_train = np.hstack([X_train_base, amp_train])
    X_test = np.hstack([X_test_base_tb, amp_test_tb])
    y_train = train_tb["y_stop"].values
    y_test = test_tb["y_stop"].values

    clf_stop = xgb.XGBClassifier(
        n_estimators=cfg.n_estimators,
        max_depth=cfg.max_depth,
        learning_rate=cfg.learning_rate,
        subsample=cfg.subsample,
        colsample_bytree=cfg.colsample_bytree,
        objective="binary:logistic",
        eval_metric="logloss",
        tree_method="hist",
        n_jobs=-1,
        random_state=cfg.random_state,
    )
    clf_stop.fit(X_train, y_train)

    proba_train_stop = clf_stop.predict_proba(X_train)[:, 1]
    proba_test_stop = clf_stop.predict_proba(X_test)[:, 1]

    pred_train_stop = (proba_train_stop >= 0.5).astype(int)
    pred_test_stop = (proba_test_stop >= 0.5).astype(int)

    acc_train = accuracy_score(y_train, pred_train_stop)
    acc_test = accuracy_score(y_test, pred_test_stop)
    auc_train = roc_auc_score(y_train, proba_train_stop)
    auc_test = roc_auc_score(y_test, proba_test_stop)

    metrics = StopModelMetrics(
        n_train_tb=len(train_tb),
        n_test_tb=len(test_tb),
        y_train_mean=float(y_train.mean()),
        y_test_mean=float(y_test.mean()),
        shape_train=X_train.shape,
        shape_test=X_test.shape,
        acc_train=acc_train,
        acc_test=acc_test,
        auc_train=auc_train,
        auc_test=auc_test,
    )
    return proba_train_stop, proba_test_stop, clf_stop, feature_space, metrics


def run_naive_backtest(
    test_df: pd.DataFrame,
    raw: pd.DataFrame,
    clf_stop: xgb.XGBClassifier,
    feature_space: FeatureSpace,
    cfg: BacktestConfig,
) -> BacktestMetrics:
    """Replicate the naive triple-barrier backtest driven by p_stop."""
    raw_rth = raw.copy()
    if "ET_datetime" in raw_rth.columns:
        dt_rth = pd.to_datetime(raw_rth["ET_datetime"], utc=True)
    elif "ts_event" in raw_rth.columns:
        dt_rth = pd.to_datetime(raw_rth["ts_event"])
    else:
        raise ValueError("raw file must contain ET_datetime or ts_event column")

    raw_rth["date_str"] = dt_rth.dt.strftime("%Y-%m-%d")
    raw_rth["time_str"] = dt_rth.dt.strftime("%H:%M")
    raw_rth["minute_of_day"] = dt_rth.dt.hour * 60 + dt_rth.dt.minute
    rth_mask = (raw_rth["minute_of_day"] >= 9 * 60 + 30) & (
        raw_rth["minute_of_day"] <= 16 * 60 + 15
    )
    raw_rth = raw_rth[rth_mask].reset_index(drop=True)
    key_series = raw_rth["date_str"] + " " + raw_rth["time_str"]
    index_map = {k: i for i, k in enumerate(key_series)}

    high = raw_rth["high"].values
    low = raw_rth["low"].values
    close = raw_rth["close"].values
    date_raw = raw_rth["date_str"].values

    X_test_base_full = transform_full_test(test_df, feature_space)

    test_full_sorted = test_df.sort_values(["date", "time"]).reset_index(drop=True)
    X_test_sorted = X_test_base_full[test_full_sorted.index.values]
    amp_sorted = test_full_sorted[AMP_COLS].values.astype(np.float32)
    proba_sorted = clf_stop.predict_proba(np.hstack([X_test_sorted, amp_sorted]))[:, 1]
    test_full_sorted["p_stop"] = proba_sorted

    pnls: List[float] = []
    trade_dates: List[str] = []

    entry_shift = cfg.entry_shift_min
    horizon = cfg.horizon_bars
    tick = cfg.tick_size

    for _, row in test_full_sorted.iterrows():
        time_str = str(row["time"])
        try:
            hh_str, mm_str = time_str.split(":")
            minute = int(hh_str) * 60 + int(mm_str) + entry_shift
            hh_new, mm_new = divmod(minute, 60)
            shifted_time = f"{hh_new:02d}:{mm_new:02d}"
        except Exception:
            shifted_time = time_str
        key = f"{row['date']} {shifted_time}"
        i = index_map.get(key)
        if i is None:
            continue
        atr = pd.to_numeric(row["atr10"], errors="coerce")
        if not np.isfinite(atr) or atr <= 0:
            continue

        p_sig = float(row["p_stop"])
        if p_sig >= cfg.long_threshold:
            side = 1.0
        elif p_sig <= cfg.short_threshold:
            side = -1.0
        else:
            continue

        p0 = close[i]
        stop_price = p0 + side * tick
        n = len(close)
        last_idx = min(n - 1, i + horizon)
        entry_idx = None
        for j in range(i + 1, last_idx + 1):
            if date_raw[j] != date_raw[i]:
                break
            if side > 0 and high[j] >= stop_price:
                entry_idx = j
                break
            if side < 0 and low[j] <= stop_price:
                entry_idx = j
                break
        if entry_idx is None:
            pnls.append(0.0)
            trade_dates.append(row["date"])
            continue

        entry_price = stop_price
        up_level = entry_price + atr
        dn_level = entry_price - atr
        hit_side = None
        last_exit_idx = min(n - 1, entry_idx + horizon)
        while last_exit_idx > entry_idx and date_raw[last_exit_idx] != date_raw[entry_idx]:
            last_exit_idx -= 1
        for j in range(entry_idx + 1, last_exit_idx + 1):
            if date_raw[j] != date_raw[i]:
                break
            hit_up = high[j] >= up_level
            hit_dn = low[j] <= dn_level
            if hit_up and not hit_dn:
                hit_side = 1
                break
            if hit_dn and not hit_up:
                hit_side = -1
                break
            if hit_up and hit_dn:
                hit_side = 0
                break

        if hit_side is None:
            exit_price = close[last_exit_idx]
            pnl_pts = (exit_price - entry_price) * side
        elif hit_side == 0:
            pnl_pts = 0.0
        else:
            pnl_pts = (up_level - entry_price) if hit_side == 1 else (dn_level - entry_price)
            pnl_pts *= side

        pnls.append(pnl_pts)
        trade_dates.append(row["date"])

    pnls_arr = np.array(pnls, dtype=float)
    if len(pnls_arr) == 0:
        return BacktestMetrics(
            trades_used=0,
            mean_pnl=float("nan"),
            std_pnl=float("nan"),
            win_frac=float("nan"),
            loss_frac=float("nan"),
            flat_frac=float("nan"),
            days_with_trades=0,
            mean_daily=float("nan"),
            std_daily=float("nan"),
            sharpe_daily=float("nan"),
            sharpe_annual=float("nan"),
            pnls=pnls_arr,
            trade_dates=trade_dates,
        )

    mean_pnl = float(pnls_arr.mean())
    std_pnl = float(pnls_arr.std())
    win_frac = float((pnls_arr > 0).mean())
    loss_frac = float((pnls_arr < 0).mean())
    flat_frac = float((pnls_arr == 0).mean())

    pnl_df = pd.DataFrame({"date": trade_dates, "pnl": pnls_arr})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = float(by_day.mean())
    std_daily = float(by_day.std())
    sharpe_daily = mean_daily / std_daily if std_daily > 0 else float("nan")
    sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else float("nan")

    metrics = BacktestMetrics(
        trades_used=len(pnls_arr),
        mean_pnl=mean_pnl,
        std_pnl=std_pnl,
        win_frac=win_frac,
        loss_frac=loss_frac,
        flat_frac=flat_frac,
        days_with_trades=len(by_day),
        mean_daily=mean_daily,
        std_daily=std_daily,
        sharpe_daily=sharpe_daily,
        sharpe_annual=sharpe_annual,
        pnls=pnls_arr,
        trade_dates=trade_dates,
    )
    return metrics
