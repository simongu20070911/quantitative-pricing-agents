import os
from typing import Tuple, List

import numpy as np
import pandas as pd
from sklearn.metrics import roc_auc_score, accuracy_score
import xgboost as xgb

from stop_label_hgb_analysis import build_features, compute_stop_labels


def train_amp_model(
    train_df: pd.DataFrame, test_df: pd.DataFrame
) -> Tuple[np.ndarray, np.ndarray]:
    """Train 3-class XGB on amplitude labels and return per-row class probabilities."""
    label_map = {"down": 0, "flat": 1, "up": 2}
    train_df = train_df.copy()
    test_df = test_df.copy()

    train_df["y_amp"] = train_df["label_amp"].map(label_map).astype(int)
    test_df["y_amp"] = test_df["label_amp"].map(label_map).astype(int)

    X_train_df, num_cols, train_cat_cols, num_means = build_features(train_df)
    X_test_df, _, test_cat_cols, _ = build_features(test_df, num_means=num_means)

    # Align one-hot columns.
    train_cat_set = set(train_cat_cols)
    test_cat_set = set(test_cat_cols)
    all_cat_cols = sorted(list(train_cat_set | test_cat_set))

    def align_onehots(X_df: pd.DataFrame) -> pd.DataFrame:
        missing = [c for c in all_cat_cols if c not in X_df.columns]
        for c in missing:
            X_df[c] = 0.0
        cols = num_cols + all_cat_cols
        return X_df[cols]

    X_train_df = align_onehots(X_train_df)
    X_test_df = align_onehots(X_test_df)

    X_train = X_train_df.values.astype(np.float32)
    X_test = X_test_df.values.astype(np.float32)
    y_train = train_df["y_amp"].values
    y_test = test_df["y_amp"].values

    # Class weights to de-emphasize flat vs up/down.
    class_counts = np.bincount(y_train, minlength=3)
    baseline = class_counts[1]  # flat
    raw_w = baseline / class_counts.astype(float)
    raw_w = raw_w / raw_w.mean()
    class_weights = np.clip(raw_w, 0.5, 3.0)
    sample_weight = class_weights[y_train]

    print("Amp model class counts (down, flat, up):", class_counts.tolist())
    print("Amp model class weights (down, flat, up):", class_weights.tolist())

    clf_amp = xgb.XGBClassifier(
        n_estimators=300,
        max_depth=4,
        learning_rate=0.05,
        subsample=0.8,
        colsample_bytree=0.8,
        objective="multi:softprob",
        num_class=3,
        eval_metric="mlogloss",
        tree_method="hist",
        n_jobs=-1,
        random_state=42,
    )
    clf_amp.fit(X_train, y_train, sample_weight=sample_weight)

    proba_train = clf_amp.predict_proba(X_train)
    proba_test = clf_amp.predict_proba(X_test)

    pred_train = np.argmax(proba_train, axis=1)
    pred_test = np.argmax(proba_test, axis=1)
    acc_train = accuracy_score(y_train, pred_train)
    acc_test = accuracy_score(y_test, pred_test)
    print("\nAmp model 3-class accuracy train/test:", acc_train, acc_test)

    return proba_train, proba_test


def build_stop_labels(
    df: pd.DataFrame, raw: pd.DataFrame, tick: float, horizon: int
) -> List[str]:
    """Wrapper around compute_stop_labels to keep interface explicit."""
    return compute_stop_labels(df, raw, tick=tick, horizon=horizon)


def main():
    TICK = 0.25
    HORIZON = 10

    ml_path = os.path.join("ml_exports", "ml_export_es_3m_amp_50_20_full.csv")
    raw_path = os.path.join("..", "es.c.0-20100606-20251116.et.ohlcv-1m.csv")

    ml = pd.read_csv(ml_path, low_memory=False)
    # Keep all amplitude labels (down/flat/up).
    ml = ml[ml["label_amp"].isin(["up", "down", "flat"])].copy()

    train_df = ml[ml["split"] == "train"].copy().reset_index(drop=True)
    test_df = ml[ml["split"] == "test"].copy().reset_index(drop=True)

    # ----- Model A: amplitude labeler -----
    proba_train_amp, proba_test_amp = train_amp_model(train_df, test_df)

    # Attach amp probabilities as features.
    amp_cols = ["p_amp_down", "p_amp_flat", "p_amp_up"]
    for idx, col in enumerate(amp_cols):
        train_df[col] = proba_train_amp[:, idx]
        test_df[col] = proba_test_amp[:, idx]

    # ----- Triple-barrier trading labels (Model B target) -----
    raw = pd.read_csv(raw_path)

    print("\nComputing stop-based triple-barrier labels (up/down) for train/test...")
    train_labels = build_stop_labels(train_df, raw, tick=TICK, horizon=HORIZON)
    test_labels = build_stop_labels(test_df, raw, tick=TICK, horizon=HORIZON)

    train_df["label_stop"] = train_labels
    test_df["label_stop"] = test_labels

    # For training the stop model, we keep only bars with definitive up/down hits.
    # Evaluation/backtest will operate on the FULL test_df to avoid future-leak gating.
    train_tb = train_df[train_df["label_stop"].isin(["up", "down"])].copy()
    test_tb = test_df[test_df["label_stop"].isin(["up", "down"])].copy()

    print("Triple-barrier labeled rows (up/down only):", len(train_tb), "train,", len(test_tb), "test")

    train_tb["y_stop"] = (train_tb["label_stop"] == "up").astype(int)
    test_tb["y_stop"] = (test_tb["label_stop"] == "up").astype(int)

    # ----- Model B: stop-label classifier with Brooks + p_amp features -----
    X_train_base_df, num_cols, train_cat_cols, num_means = build_features(train_tb)
    X_test_base_tb_df, _, test_cat_cols, _ = build_features(test_tb, num_means=num_means)

    train_cat_set = set(train_cat_cols)
    test_cat_set = set(test_cat_cols)
    all_cat_cols = sorted(list(train_cat_set | test_cat_set))

    def align_onehots(X_df: pd.DataFrame) -> pd.DataFrame:
        missing = [c for c in all_cat_cols if c not in X_df.columns]
        for c in missing:
            X_df[c] = 0.0
        cols = num_cols + all_cat_cols
        return X_df[cols]

    X_train_base_df = align_onehots(X_train_base_df)
    X_test_base_tb_df = align_onehots(X_test_base_tb_df)

    X_train_base = X_train_base_df.values.astype(np.float32)
    X_test_base_tb = X_test_base_tb_df.values.astype(np.float32)

    # Append amp probabilities as additional numeric features.
    for c in amp_cols:
        if c not in train_tb.columns or c not in test_tb.columns:
            raise ValueError(f"Missing amp probability column '{c}' in train/test.")

    amp_train = train_tb[amp_cols].values.astype(np.float32)
    amp_test_tb = test_tb[amp_cols].values.astype(np.float32)

    X_train = np.hstack([X_train_base, amp_train])
    X_test = np.hstack([X_test_base_tb, amp_test_tb])
    y_train = train_tb["y_stop"].values
    y_test = test_tb["y_stop"].values

    print("Stop model feature shapes train/test:", X_train.shape, X_test.shape)
    print("Stop model class balance train/test (up=1):", y_train.mean(), y_test.mean())

    clf_stop = xgb.XGBClassifier(
        n_estimators=300,
        max_depth=4,
        learning_rate=0.05,
        subsample=0.8,
        colsample_bytree=0.8,
        objective="binary:logistic",
        eval_metric="logloss",
        tree_method="hist",
        n_jobs=-1,
        random_state=42,
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

    print("\nStop model (with amp p) performance:")
    print("  Accuracy train/test:", acc_train, acc_test)
    print("  AUC train/test     :", auc_train, auc_test)

    # ----- Naive triple-barrier trading backtest on FULL test_df (no label-based gating) -----
    # Reuse raw for 1m path and RTH gating, similar to stop_label_hgb_analysis.
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

    # Build features for the full test_df using the same num_means / category set.
    X_test_base_full_df, _, test_cat_full, _ = build_features(test_df, num_means=num_means)
    X_test_base_full_df = align_onehots(X_test_base_full_df)

    # Align probabilities to time-sorted FULL test_df.
    test_full_sorted = test_df.sort_values(["date", "time"]).reset_index(drop=True)
    X_test_sorted = X_test_base_full_df.loc[test_full_sorted.index].values.astype(np.float32)
    proba_sorted = clf_stop.predict_proba(
        np.hstack([X_test_sorted, test_full_sorted[amp_cols].values.astype(np.float32)])
    )[:, 1]
    test_full_sorted["p_stop"] = proba_sorted

    pnls = []
    trade_dates = []

    for _, row in test_full_sorted.iterrows():
        key = f"{row['date']} {row['time']}"
        i = index_map.get(key)
        if i is None:
            continue
        atr = pd.to_numeric(row["atr10"], errors="coerce")
        if not np.isfinite(atr) or atr <= 0:
            continue
        side = 1.0 if row["p_stop"] >= 0.5 else -1.0
        p0 = close[i]
        stop_price = p0 + side * TICK
        n = len(close)
        last_idx = min(n - 1, i + HORIZON)
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
        # If stop never fills within horizon/RTH, treat as a zero-PnL "no fill" attempt
        # rather than dropping the bar entirely.
        if entry_idx is None:
            pnls.append(0.0)
            trade_dates.append(row["date"])
            continue
        entry_price = stop_price
        up_level = entry_price + atr
        dn_level = entry_price - atr
        hit_side = None
        last_exit_idx = min(n - 1, entry_idx + HORIZON)
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

    pnls = np.array(pnls, dtype=float)
    print("\nNaive triple-barrier backtest using Stop model (with amp p), cost=0:")
    print("  trades_used", len(pnls))
    if len(pnls) > 0:
        print("  per-trade mean_pts", float(pnls.mean()), "std_pts", float(pnls.std()))
        pnl_df = pd.DataFrame({"date": trade_dates, "pnl": pnls})
        pnl_df["date"] = pd.to_datetime(pnl_df["date"])
        by_day = pnl_df.groupby("date")["pnl"].sum()
        mean_daily = float(by_day.mean())
        std_daily = float(by_day.std())
        sharpe_daily = mean_daily / std_daily if std_daily > 0 else float("nan")
        sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else float("nan")
        print("  days_with_trades", len(by_day))
        print("  mean_daily_pts", mean_daily, "std_daily_pts", std_daily)
        print("  daily_sharpe", sharpe_daily, "annualized_sharpe", sharpe_annual)


if __name__ == "__main__":
    main()
