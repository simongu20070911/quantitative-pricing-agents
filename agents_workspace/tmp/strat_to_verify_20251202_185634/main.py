from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Tuple

import numpy as np
import pandas as pd
from sklearn.metrics import accuracy_score, roc_auc_score
import xgboost as xgb

import constants
from config import AmpModelConfig, BacktestConfig, StopModelConfig

# Import feature/label helpers from the original analysis script.
from stop_label_hgb_analysis import build_features, compute_stop_labels


AMP_LABEL_MAP: Dict[str, int] = {"down": 0, "flat": 1, "up": 2}
AMP_PROBA_COLS: List[str] = ["p_amp_down", "p_amp_flat", "p_amp_up"]


@dataclass
class AmpModelArtifacts:
    clf: xgb.XGBClassifier
    proba_train: np.ndarray
    proba_test: np.ndarray
    y_train: np.ndarray
    y_test: np.ndarray
    num_cols: List[str]
    all_cat_cols: List[str]
    num_means: Dict[str, float]


@dataclass
class StopModelArtifacts:
    clf: xgb.XGBClassifier
    X_train: np.ndarray
    X_test_tb: np.ndarray
    y_train: np.ndarray
    y_test: np.ndarray
    num_cols: List[str]
    all_cat_cols: List[str]
    num_means: Dict[str, float]


@dataclass
class BacktestSummary:
    pnls: np.ndarray
    trade_dates: List[str]
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
    pnl_df_sorted: pd.DataFrame
    daily_equity: pd.DataFrame


def _align_onehots_train_test(
    X_train_df: pd.DataFrame,
    X_test_df: pd.DataFrame,
    num_cols: List[str],
    train_cat_cols: List[str],
    test_cat_cols: List[str],
) -> Tuple[pd.DataFrame, pd.DataFrame, List[str]]:
    """Align one-hot columns between train/test using the union of categories."""
    train_cat_set = set(train_cat_cols)
    test_cat_set = set(test_cat_cols)
    all_cat_cols = sorted(list(train_cat_set | test_cat_set))

    def align(X_df: pd.DataFrame) -> pd.DataFrame:
        missing = [c for c in all_cat_cols if c not in X_df.columns]
        for c in missing:
            X_df[c] = 0.0
        cols = num_cols + all_cat_cols
        return X_df[cols]

    return align(X_train_df), align(X_test_df), all_cat_cols


def _align_onehots_with_template(
    X_df: pd.DataFrame, num_cols: List[str], all_cat_cols: List[str]
) -> pd.DataFrame:
    """Align a feature frame to a fixed numeric + categorical template."""
    missing = [c for c in all_cat_cols if c not in X_df.columns]
    for c in missing:
        X_df[c] = 0.0
    cols = num_cols + all_cat_cols
    return X_df[cols]


def load_data() -> Tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """Load ML export and raw 1m ES data, return train/test splits."""
    ml = pd.read_csv(constants.ML_EXPORT_PATH, low_memory=False)
    # Keep only the three amplitude labels used by the model.
    ml = ml[ml["label_amp"].isin(AMP_LABEL_MAP.keys())].copy()

    train_df = ml[ml["split"] == "train"].copy().reset_index(drop=True)
    test_df = ml[ml["split"] == "test"].copy().reset_index(drop=True)

    raw = pd.read_csv(constants.RAW_ES_1M_PATH)
    return train_df, test_df, raw


def train_amp_model(
    train_df: pd.DataFrame, test_df: pd.DataFrame, cfg: AmpModelConfig
) -> AmpModelArtifacts:
    """Train 3-class XGB on amplitude labels and return artifacts."""
    train_df = train_df.copy()
    test_df = test_df.copy()

    train_df["y_amp"] = train_df["label_amp"].map(AMP_LABEL_MAP).astype(int)
    test_df["y_amp"] = test_df["label_amp"].map(AMP_LABEL_MAP).astype(int)

    X_train_df, num_cols, train_cat_cols, num_means = build_features(train_df)
    X_test_df, _, test_cat_cols, _ = build_features(test_df, num_means=num_means)

    X_train_df, X_test_df, all_cat_cols = _align_onehots_train_test(
        X_train_df, X_test_df, num_cols, train_cat_cols, test_cat_cols
    )

    X_train = X_train_df.values.astype(np.float32)
    X_test = X_test_df.values.astype(np.float32)
    y_train = train_df["y_amp"].values
    y_test = test_df["y_amp"].values

    # Class weights to de-emphasize flat vs up/down, identical to original.
    class_counts = np.bincount(y_train, minlength=3)
    baseline = class_counts[1]  # flat
    raw_w = baseline / class_counts.astype(float)
    raw_w = raw_w / raw_w.mean()
    class_weights = np.clip(raw_w, 0.5, 3.0)
    sample_weight = class_weights[y_train]

    print("Amp model class counts (down, flat, up):", class_counts.tolist())
    print("Amp model class weights (down, flat, up):", class_weights.tolist())

    clf_amp = xgb.XGBClassifier(
        n_estimators=cfg.n_estimators,
        max_depth=cfg.max_depth,
        learning_rate=cfg.learning_rate,
        subsample=cfg.subsample,
        colsample_bytree=cfg.colsample_bytree,
        objective="multi:softprob",
        num_class=3,
        eval_metric="mlogloss",
        tree_method="hist",
        n_jobs=-1,
        random_state=cfg.random_state,
    )
    clf_amp.fit(X_train, y_train, sample_weight=sample_weight)

    proba_train = clf_amp.predict_proba(X_train)
    proba_test = clf_amp.predict_proba(X_test)

    pred_train = np.argmax(proba_train, axis=1)
    pred_test = np.argmax(proba_test, axis=1)
    acc_train = accuracy_score(y_train, pred_train)
    acc_test = accuracy_score(y_test, pred_test)
    print("\nAmp model 3-class accuracy train/test:", acc_train, acc_test)

    return AmpModelArtifacts(
        clf=clf_amp,
        proba_train=proba_train,
        proba_test=proba_test,
        y_train=y_train,
        y_test=y_test,
        num_cols=num_cols,
        all_cat_cols=all_cat_cols,
        num_means=num_means,
    )


def attach_amp_probabilities(
    train_df: pd.DataFrame, test_df: pd.DataFrame, amp_artifacts: AmpModelArtifacts
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """Attach p_amp_* columns to train/test DataFrames."""
    train_df = train_df.copy()
    test_df = test_df.copy()
    for idx, col in enumerate(AMP_PROBA_COLS):
        train_df[col] = amp_artifacts.proba_train[:, idx]
        test_df[col] = amp_artifacts.proba_test[:, idx]
    return train_df, test_df


def build_stop_labels_for_split(
    df: pd.DataFrame,
    raw: pd.DataFrame,
    tick: float,
    horizon: int,
    entry_shift_min: int,
) -> List[str]:
    """Wrapper around compute_stop_labels to keep interface explicit."""
    return compute_stop_labels(
        df, raw, tick=tick, horizon=horizon, entry_shift_min=entry_shift_min
    )


def train_stop_model(
    train_df: pd.DataFrame,
    test_df: pd.DataFrame,
    raw: pd.DataFrame,
    cfg: StopModelConfig,
    bt_cfg: BacktestConfig,
) -> Tuple[StopModelArtifacts, pd.DataFrame, pd.DataFrame]:
    """Train the stop-label classifier (Model B) on Brooks + p_amp_* features."""
    train_df = train_df.copy()
    test_df = test_df.copy()

    print("\nComputing stop-based triple-barrier labels (up/down) for train/test...")
    train_labels = build_stop_labels_for_split(
        train_df, raw, tick=bt_cfg.tick, horizon=bt_cfg.horizon, entry_shift_min=bt_cfg.entry_shift_min
    )
    test_labels = build_stop_labels_for_split(
        test_df, raw, tick=bt_cfg.tick, horizon=bt_cfg.horizon, entry_shift_min=bt_cfg.entry_shift_min
    )

    train_df["label_stop"] = train_labels
    test_df["label_stop"] = test_labels

    # Keep only definitive up/down labels for training.
    train_tb = train_df[train_df["label_stop"].isin(["up", "down"])].copy()
    test_tb = test_df[test_df["label_stop"].isin(["up", "down"])].copy()

    print(
        "Triple-barrier labeled rows (up/down only):",
        len(train_tb),
        "train,",
        len(test_tb),
        "test",
    )

    train_tb["y_stop"] = (train_tb["label_stop"] == "up").astype(int)
    test_tb["y_stop"] = (test_tb["label_stop"] == "up").astype(int)

    # Brooks features.
    X_train_base_df, num_cols, train_cat_cols, num_means = build_features(train_tb)
    X_test_base_tb_df, _, test_cat_cols, _ = build_features(test_tb, num_means=num_means)

    X_train_base_df, X_test_base_tb_df, all_cat_cols = _align_onehots_train_test(
        X_train_base_df, X_test_base_tb_df, num_cols, train_cat_cols, test_cat_cols
    )

    X_train_base = X_train_base_df.values.astype(np.float32)
    X_test_base_tb = X_test_base_tb_df.values.astype(np.float32)

    # Amp probabilities as extra numeric features.
    for c in AMP_PROBA_COLS:
        if c not in train_tb.columns or c not in test_tb.columns:
            raise ValueError(f"Missing amp probability column '{c}' in train/test.")

    amp_train = train_tb[AMP_PROBA_COLS].values.astype(np.float32)
    amp_test_tb = test_tb[AMP_PROBA_COLS].values.astype(np.float32)

    X_train = np.hstack([X_train_base, amp_train])
    X_test_tb = np.hstack([X_test_base_tb, amp_test_tb])
    y_train = train_tb["y_stop"].values
    y_test = test_tb["y_stop"].values

    print("Stop model feature shapes train/test:", X_train.shape, X_test_tb.shape)
    print(
        "Stop model class balance train/test (up=1):",
        y_train.mean(),
        y_test.mean(),
    )

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
    proba_test_stop = clf_stop.predict_proba(X_test_tb)[:, 1]

    pred_train_stop = (proba_train_stop >= 0.5).astype(int)
    pred_test_stop = (proba_test_stop >= 0.5).astype(int)

    acc_train = accuracy_score(y_train, pred_train_stop)
    acc_test = accuracy_score(y_test, pred_test_stop)
    auc_train = roc_auc_score(y_train, proba_train_stop)
    auc_test = roc_auc_score(y_test, proba_test_stop)

    print("\nStop model (with amp p) performance:")
    print("  Accuracy train/test:", acc_train, acc_test)
    print("  AUC train/test     :", auc_train, auc_test)

    return (
        StopModelArtifacts(
            clf=clf_stop,
            X_train=X_train,
            X_test_tb=X_test_tb,
            y_train=y_train,
            y_test=y_test,
            num_cols=num_cols,
            all_cat_cols=all_cat_cols,
            num_means=num_means,
        ),
        train_tb,
        test_tb,
    )


def run_naive_triple_barrier_backtest(
    test_df: pd.DataFrame,
    stop_artifacts: StopModelArtifacts,
    raw: pd.DataFrame,
    bt_cfg: BacktestConfig,
) -> BacktestSummary:
    """Naive triple-barrier backtest on FULL test_df (no label-based gating)."""
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

    # Build Brooks features for FULL test_df with the same num_means / category set.
    X_test_base_full_df, _, test_cat_full, _ = build_features(
        test_df, num_means=stop_artifacts.num_means
    )
    X_test_base_full_df = _align_onehots_with_template(
        X_test_base_full_df, stop_artifacts.num_cols, stop_artifacts.all_cat_cols
    )

    # Align probabilities to time-sorted FULL test_df.
    test_full_sorted = test_df.sort_values(["date", "time"]).reset_index(drop=True)
    X_test_sorted = X_test_base_full_df.loc[test_full_sorted.index].values.astype(
        np.float32
    )
    proba_sorted = stop_artifacts.clf.predict_proba(
        np.hstack(
            [X_test_sorted, test_full_sorted[AMP_PROBA_COLS].values.astype(np.float32)]
        )
    )[:, 1]
    test_full_sorted["p_stop"] = proba_sorted

    pnls: List[float] = []
    trade_dates: List[str] = []

    ENTRY_SHIFT_MIN = bt_cfg.entry_shift_min

    for _, row in test_full_sorted.iterrows():
        time_str = str(row["time"])
        try:
            hh_str, mm_str = time_str.split(":")
            minute = int(hh_str) * 60 + int(mm_str) + ENTRY_SHIFT_MIN
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
        # Only trade when p_stop is in an extreme band:
        # go long if p_stop >= 0.7, short if p_stop <= 0.3, otherwise stay flat.
        p_sig = float(row["p_stop"])
        if p_sig >= bt_cfg.prob_long_threshold:
            side = 1.0
        elif p_sig <= bt_cfg.prob_short_threshold:
            side = -1.0
        else:
            continue
        p0 = close[i]
        stop_price = p0 + side * bt_cfg.tick
        n = len(close)
        last_idx = min(n - 1, i + bt_cfg.horizon)
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
        # If stop never fills within horizon/RTH, treat as a zero-PnL "no fill" attempt.
        if entry_idx is None:
            pnls.append(0.0)
            trade_dates.append(row["date"])
            continue
        entry_price = stop_price
        up_level = entry_price + atr
        dn_level = entry_price - atr
        hit_side = None
        last_exit_idx = min(n - 1, entry_idx + bt_cfg.horizon)
        # Ensure we don't exit past the RTH day of entry.
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
            # Ambiguous hit: treat as flat outcome with zero PnL.
            pnl_pts = 0.0
        else:
            pnl_pts = (up_level - entry_price) if hit_side == 1 else (dn_level - entry_price)
            pnl_pts *= side
        pnls.append(pnl_pts)
        trade_dates.append(row["date"])

    pnls_arr = np.array(pnls, dtype=float)
    print("\nNaive triple-barrier backtest using Stop model (with amp p), cost=0:")
    print("  trades_used", len(pnls_arr))

    if len(pnls_arr) == 0:
        # Degenerate case; should not happen with the current config.
        return BacktestSummary(
            pnls=pnls_arr,
            trade_dates=trade_dates,
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
            pnl_df_sorted=pd.DataFrame(),
            daily_equity=pd.DataFrame(),
        )

    mean_pnl = float(pnls_arr.mean())
    std_pnl = float(pnls_arr.std())
    win_frac = float((pnls_arr > 0).mean())
    loss_frac = float((pnls_arr < 0).mean())
    flat_frac = float((pnls_arr == 0).mean())
    print("  per-trade mean_pts", mean_pnl, "std_pts", std_pnl)
    print("  trade win/loss/flat fractions:", win_frac, loss_frac, flat_frac)

    pnl_df = pd.DataFrame({"date": trade_dates, "pnl": pnls_arr})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = float(by_day.mean())
    std_daily = float(by_day.std())
    sharpe_daily = mean_daily / std_daily if std_daily > 0 else float("nan")
    sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else float("nan")
    print("  days_with_trades", len(by_day))
    print("  mean_daily_pts", mean_daily, "std_daily_pts", std_daily)
    print("  daily_sharpe", sharpe_daily, "annualized_sharpe", sharpe_annual)

    pnl_df_sorted = pnl_df.sort_values("date").reset_index(drop=True)
    pnl_df_sorted["cum_pnl"] = pnl_df_sorted["pnl"].cumsum()
    daily_equity = by_day.sort_index().to_frame(name="daily_pnl")
    daily_equity["cum_pnl"] = daily_equity["daily_pnl"].cumsum()

    return BacktestSummary(
        pnls=pnls_arr,
        trade_dates=trade_dates,
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
        pnl_df_sorted=pnl_df_sorted,
        daily_equity=daily_equity,
    )


def save_replication_equity(summary: BacktestSummary) -> None:
    """Save replicated equity curves to CSV inside this folder."""
    summary.pnl_df_sorted.to_csv(constants.REPL_TRADES_CSV, index=False)
    summary.daily_equity.to_csv(constants.REPL_DAILY_CSV, index=True)


def main() -> None:
    amp_cfg = AmpModelConfig()
    stop_cfg = StopModelConfig()
    bt_cfg = BacktestConfig()

    train_df, test_df, raw = load_data()

    # Model A: amplitude labeler.
    amp_artifacts = train_amp_model(train_df, test_df, amp_cfg)
    train_df_amp, test_df_amp = attach_amp_probabilities(
        train_df, test_df, amp_artifacts
    )

    # Model B: stop-label classifier with Brooks + p_amp_* features.
    stop_artifacts, train_tb, test_tb = train_stop_model(
        train_df_amp, test_df_amp, raw, stop_cfg, bt_cfg
    )

    # Naive triple-barrier backtest on FULL test_df.
    summary = run_naive_triple_barrier_backtest(
        test_df_amp, stop_artifacts, raw, bt_cfg
    )
    save_replication_equity(summary)


if __name__ == "__main__":
    main()
