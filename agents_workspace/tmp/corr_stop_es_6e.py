import pandas as pd
import numpy as np
from sklearn.ensemble import HistGradientBoostingClassifier


def build_features(df: pd.DataFrame, num_means=None):
    df = df.copy()
    num_cols = [
        "atr10",
        "z_vwap",
        "rv10",
        "rv60",
        "rv_ratio",
        "trend_feat",
        "gap",
        "dist_onh",
        "dist_onl",
        "dist_ema20",
        "ema20_slope",
        "leg_len_bars",
        "leg_range",
        "pos_in_range",
        "range_test_rate_high",
        "range_test_rate_low",
        "range_fail_rate_high",
        "range_fail_rate_low",
        "range_width_ratio",
        "micro_up_len",
        "micro_down_len",
        "soft_micro_up_len",
        "soft_micro_down_len",
        "soft_micro_bias",
        "bar_body_frac",
        "bar_close_pos",
        "micro_channel_slope",
        "major_channel_slope",
        "micro_channel_z",
        "major_channel_z",
        "leg_mm_up",
        "leg_mm_down",
        "recent_strength_score",
        "htf_leg_len_bars_5m",
    ]
    for c in num_cols:
        df[c] = pd.to_numeric(df[c], errors="coerce")
    cols_all_nan = [c for c in num_cols if df[c].isna().all()]
    num_cols = [c for c in num_cols if c not in cols_all_nan]
    if num_means is None:
        num_means = {c: df[c].mean() for c in num_cols}
    for c in num_cols:
        df[c] = df[c].fillna(num_means[c])
    cat_cols = ["leg_side", "bar_is_trend", "bar_is_doji", "always_in", "htf_leg_side_5m"]
    cat = pd.get_dummies(df[cat_cols], drop_first=False)
    X = pd.concat([df[num_cols].reset_index(drop=True), cat.reset_index(drop=True)], axis=1)
    return X, num_cols, list(cat.columns), num_means


def compute_stop_labels(df: pd.DataFrame, raw: pd.DataFrame, tick: float, horizon: int = 10):
    df = df.copy()
    raw = raw.copy()

    # Use ET_datetime if present (ES), otherwise ts_event (6E)
    ts_col = "ET_datetime" if "ET_datetime" in raw.columns else "ts_event"
    raw["ET_dt"] = pd.to_datetime(raw[ts_col], utc=True)
    raw["date_str"] = raw["ET_dt"].dt.strftime("%Y-%m-%d")
    raw["time_str"] = raw["ET_dt"].dt.strftime("%H:%M")
    raw["minute_of_day"] = raw["ET_dt"].dt.hour * 60 + raw["ET_dt"].dt.minute
    # approximate RTH 9:30-16:15
    rth_mask = (raw["minute_of_day"] >= 9 * 60 + 30) & (raw["minute_of_day"] <= 16 * 60 + 15)
    raw = raw[rth_mask].reset_index(drop=True)

    key_series = raw["date_str"] + " " + raw["time_str"]
    index_map = {k: i for i, k in enumerate(key_series)}

    high = raw["high"].values
    low = raw["low"].values
    close = raw["close"].values
    date_raw = raw["date_str"].values

    labels = []
    for _, row in df.iterrows():
        key = f"{row['date']} {row['time']}"
        i = index_map.get(key)
        if i is None:
            labels.append(None)
            continue
        atr = pd.to_numeric(row["atr10"], errors="coerce")
        if not np.isfinite(atr) or atr <= 0:
            labels.append(None)
            continue
        p0 = close[i]
        stop_price = p0 + tick
        n = len(close)
        last_idx = min(n - 1, i + horizon)
        entry_idx = None
        for j in range(i + 1, last_idx + 1):
            if date_raw[j] != date_raw[i]:
                break
            if high[j] >= stop_price:
                entry_idx = j
                break
        if entry_idx is None:
            labels.append(None)
            continue
        entry_price = stop_price
        up_level = entry_price + atr
        dn_level = entry_price - atr
        hit = None
        last_exit_idx = min(n - 1, entry_idx + horizon)
        for j in range(entry_idx + 1, last_exit_idx + 1):
            if date_raw[j] != date_raw[i]:
                break
            hit_up = high[j] >= up_level
            hit_dn = low[j] <= dn_level
            if hit_up and not hit_dn:
                hit = "up"
                break
            if hit_dn and not hit_up:
                hit = "down"
                break
            if hit_up and hit_dn:
                hit = None
                break
        labels.append(hit)
    return labels


def compute_daily_pnl_for_asset(ml_path: str, raw_path: str, asset_name: str, tick: float):
    COST = 0.0
    HORIZON = 10

    ml = pd.read_csv(ml_path, low_memory=False)
    ml = ml[ml["label_small"].isin(["up", "down"])].copy()

    train_df = ml[ml["split"] == "train"].copy()
    test_df = ml[ml["split"] == "test"].copy()

    raw = pd.read_csv(raw_path)

    # compute stop labels on full set, then keep train/test
    print(f"[{asset_name}] computing stop labels...")
    ml_labels = compute_stop_labels(ml, raw, tick=tick, horizon=HORIZON)
    ml["label_stop"] = ml_labels
    ml = ml[ml["label_stop"].isin(["up", "down"])].copy()

    train_df = ml[ml["split"] == "train"].copy()
    test_df = ml[ml["split"] == "test"].copy()

    train_df["y"] = (train_df["label_stop"] == "up").astype(int)
    test_df["y"] = (test_df["label_stop"] == "up").astype(int)

    X_train_df, num_cols, train_cat_cols, num_means = build_features(train_df)
    X_test_df, _, test_cat_cols, _ = build_features(test_df, num_means=num_means)

    train_cat_set = set(train_cat_cols)
    test_cat_set = set(test_cat_cols)
    all_cat_cols = sorted(list(train_cat_set | test_cat_set))

    def align_onehots(X_df, current_cat_cols):
        missing = [c for c in all_cat_cols if c not in X_df.columns]
        for c in missing:
            X_df[c] = 0.0
        cols = num_cols + all_cat_cols
        return X_df[cols]

    X_train_df = align_onehots(X_train_df, train_cat_cols)
    X_test_df = align_onehots(X_test_df, test_cat_cols)

    X_train = X_train_df.values.astype(np.float32)
    X_test = X_test_df.values.astype(np.float32)
    y_train = train_df["y"].values
    y_test = test_df["y"].values

    print(f"[{asset_name}] train/test shapes", X_train.shape, X_test.shape)

    clf = HistGradientBoostingClassifier(
        loss="log_loss",
        max_depth=3,
        learning_rate=0.05,
        max_iter=100,
        max_leaf_nodes=16,
        l2_regularization=0.1,
        validation_fraction=0.1,
        n_iter_no_change=10,
        random_state=42,
    )
    clf.fit(X_train, y_train)

    from sklearn.metrics import roc_auc_score

    proba_test = clf.predict_proba(X_test)[:, 1]
    auc_test = roc_auc_score(y_test, proba_test)
    print(f"[{asset_name}] stop-label test AUC: {auc_test:.4f}")

    # Stop-entry trading: long if p>=0.5, short if p<0.5, 1-tick stop, ±ATR from entry
    test_df_sorted = test_df.sort_values(["date", "time"]).reset_index(drop=True)
    X_test_sorted = X_test_df.loc[test_df_sorted.index].values.astype(np.float32)
    proba_sorted = clf.predict_proba(X_test_sorted)[:, 1]
    test_df_sorted["p_stop"] = proba_sorted

    # map ml date/time to raw indices
    raw_rth = raw.copy()
    ts_col = "ET_datetime" if "ET_datetime" in raw_rth.columns else "ts_event"
    raw_rth["ET_dt"] = pd.to_datetime(raw_rth[ts_col], utc=True)
    raw_rth["date_str"] = raw_rth["ET_dt"].dt.strftime("%Y-%m-%d")
    raw_rth["time_str"] = raw_rth["ET_dt"].dt.strftime("%H:%M")
    raw_rth["minute_of_day"] = raw_rth["ET_dt"].dt.hour * 60 + raw_rth["ET_dt"].dt.minute
    rth_mask = (raw_rth["minute_of_day"] >= 9 * 60 + 30) & (raw_rth["minute_of_day"] <= 16 * 60 + 15)
    raw_rth = raw_rth[rth_mask].reset_index(drop=True)
    key_series = raw_rth["date_str"] + " " + raw_rth["time_str"]
    index_map = {k: i for i, k in enumerate(key_series)}

    high = raw_rth["high"].values
    low = raw_rth["low"].values
    close = raw_rth["close"].values
    date_raw = raw_rth["date_str"].values

    atr_map = dict(
        zip(
            ml["date"].astype(str) + " " + ml["time"].astype(str),
            pd.to_numeric(ml["atr10"], errors="coerce"),
        )
    )

    pnls = []
    dates = []

    for _, row in test_df_sorted.iterrows():
        key = f"{row['date']} {row['time']}"
        i = index_map.get(key)
        if i is None:
            continue
        atr = atr_map.get(key)
        if atr is None or not np.isfinite(atr) or atr <= 0:
            continue
        side = 1.0 if row["p_stop"] >= 0.5 else -1.0
        p0 = close[i]
        stop_price = p0 + side * tick
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
        if entry_idx is None:
            continue
        entry_price = stop_price
        up_level = entry_price + atr
        dn_level = entry_price - atr
        hit_side = None
        last_exit_idx = min(n - 1, entry_idx + HORIZON)
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
            continue
        else:
            pnl_pts = (up_level - entry_price) if hit_side == 1 else (dn_level - entry_price)
            pnl_pts *= side
        pnl_pts -= COST
        pnls.append(pnl_pts)
        dates.append(row["date"])

    pnl_df = pd.DataFrame({"date": dates, "pnl": pnls})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()

    return by_day


def main():
    # ES
    es_daily = compute_daily_pnl_for_asset(
        ml_path="ml_export_es_new.csv",
        raw_path="../es.c.0-20100606-20251116.et.ohlcv-1m.csv",
        asset_name="ES",
        tick=0.25,
    )

    # 6E
    e6_daily = compute_daily_pnl_for_asset(
        ml_path="ml_export_6e.csv",
        raw_path="6e_clean.csv",
        asset_name="6E",
        tick=0.0001,
    )

    # Align dates and compute correlation over test period overlap
    common_dates = es_daily.index.intersection(e6_daily.index)
    es_aligned = es_daily.loc[common_dates]
    e6_aligned = e6_daily.loc[common_dates]

    corr = np.corrcoef(es_aligned.values, e6_aligned.values)[0, 1]
    print("Common days:", len(common_dates))
    print("ES daily mean/std:", es_aligned.mean(), es_aligned.std())
    print("6E daily mean/std:", e6_aligned.mean(), e6_aligned.std())
    print("Correlation of ES vs 6E stop-strategy daily PnL:", corr)


if __name__ == "__main__":
    main()
