import pandas as pd
import numpy as np
from sklearn.ensemble import HistGradientBoostingClassifier
from sklearn.metrics import roc_auc_score


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


def main():
    COST = 0.0
    TICK = 0.25
    HORIZON_BARS = 10

    # 1) Load ML export and split
    ml = pd.read_csv("ml_export_es_new.csv", low_memory=False)
    train_df = ml[ml["split"] == "train"].copy()
    test_df = ml[ml["split"] == "test"].copy()

    train_df = train_df[train_df["label_small"].isin(["up", "down"])].copy()
    test_df = test_df[test_df["label_small"].isin(["up", "down"])].copy()

    train_df["y"] = (train_df["label_small"] == "up").astype(int)
    test_df["y"] = (test_df["label_small"] == "up").astype(int)

    # 2) Build features
    X_train_df, num_cols, train_cat_cols, num_means = build_features(train_df)
    X_test_df, _, test_cat_cols, _ = build_features(test_df, num_means=num_means)

    # align cats
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

    # 3) Train HGB
    clf = HistGradientBoostingClassifier(
        loss="log_loss",
        max_depth=4,
        learning_rate=0.1,
        max_iter=200,
        max_leaf_nodes=31,
        validation_fraction=0.1,
        n_iter_no_change=10,
        random_state=42,
    )
    clf.fit(X_train, y_train)
    proba_test = clf.predict_proba(X_test)[:, 1]
    auc = roc_auc_score(y_test, proba_test)
    print("HGB test AUC (full context):", auc)

    # Attach probs and ATR to sorted test rows
    test_df = test_df.sort_values(["date", "time"]).reset_index(drop=True)
    X_test = X_test_df.loc[test_df.index].values.astype(np.float32)
    proba_test = clf.predict_proba(X_test)[:, 1]
    test_df["p_hgb"] = proba_test
    test_df["atr10"] = pd.to_numeric(test_df["atr10"], errors="coerce")

    # 4) Load raw ES 1m and build mapping date+time -> index (RTH only)
    raw = pd.read_csv("../es.c.0-20100606-20251116.et.ohlcv-1m.csv")
    raw["ET_datetime"] = pd.to_datetime(raw["ET_datetime"], utc=True)
    raw["date_str"] = raw["ET_datetime"].dt.strftime("%Y-%m-%d")
    raw["time_str"] = raw["ET_datetime"].dt.strftime("%H:%M")
    raw["minute_of_day"] = raw["ET_datetime"].dt.hour * 60 + raw["ET_datetime"].dt.minute
    # RTH mask
    # Note: we match ml_export which is already RTH-filtered
    from Time_utils import rth_start_min, rth_end_min  # type: ignore

    rth_mask = (raw["minute_of_day"] >= rth_start_min) & (raw["minute_of_day"] <= rth_end_min)
    raw = raw[rth_mask].reset_index(drop=True)

    key_series = raw["date_str"] + " " + raw["time_str"]
    index_map = {k: i for i, k in enumerate(key_series)}

    high = raw["high"].values
    low = raw["low"].values
    close = raw["close"].values
    date_raw = raw["date_str"].values

    # 5) Stop-entry + ATR bracket simulation
    pnls = []
    trade_dates = []
    n_signals = 0
    n_entries = 0

    for idx, row in test_df.iterrows():
        key = f"{row['date']} {row['time']}"
        i = index_map.get(key)
        if i is None:
            continue
        atr = row["atr10"]
        if not np.isfinite(atr) or atr <= 0:
            continue
        n_signals += 1
        side = 1.0 if row["p_hgb"] >= 0.5 else -1.0
        p0 = close[i]
        stop_price = p0 + side * TICK

        n = len(close)
        last_idx = min(n - 1, i + HORIZON_BARS)
        entry_idx = None

        # 5a. Find entry via stop
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
            continue  # no fill

        n_entries += 1
        entry_price = stop_price
        up_level = entry_price + atr
        dn_level = entry_price - atr

        # 5b. Scan for bracket hits after entry
        hit_side = None  # +1 for up, -1 for down, 0 for ambiguous
        exit_idx = None
        last_exit_idx = min(n - 1, entry_idx + HORIZON_BARS)
        for j in range(entry_idx + 1, last_exit_idx + 1):
            if date_raw[j] != date_raw[i]:
                break
            hit_up = high[j] >= up_level
            hit_dn = low[j] <= dn_level
            if hit_up and not hit_dn:
                hit_side = 1
                exit_idx = j
                break
            if hit_dn and not hit_up:
                hit_side = -1
                exit_idx = j
                break
            if hit_up and hit_dn:
                hit_side = 0
                exit_idx = j
                break

        if hit_side is None:
            # No bracket hit; exit at last close
            exit_price = close[last_exit_idx]
            pnl_pts = (exit_price - entry_price) * side
        elif hit_side == 0:
            # Ambiguous intrabar; skip this trade
            continue
        else:
            pnl_pts = (up_level - entry_price) if hit_side == 1 else (dn_level - entry_price)
            pnl_pts *= side  # align with trade direction (should already be +atr)

        pnl_pts -= COST
        pnls.append(pnl_pts)
        trade_dates.append(row["date"])

    pnls = np.array(pnls, dtype=float)
    print("signals", n_signals, "entries", n_entries, "trades used", len(pnls))

    if len(pnls) == 0:
        print("No trades triggered; something is off.")
        return

    print("per-trade mean_pts", float(pnls.mean()), "std_pts", float(pnls.std()))

    pnl_df = pd.DataFrame({"date": trade_dates, "pnl": pnls})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = by_day.mean()
    std_daily = by_day.std()
    sharpe_daily = mean_daily / std_daily
    sharpe_annual = sharpe_daily * np.sqrt(252)

    print("days_with_trades", len(by_day))
    print("mean_daily_pts", float(mean_daily), "std_daily_pts", float(std_daily))
    print("daily_sharpe", float(sharpe_daily), "annualized_sharpe", float(sharpe_annual))


if __name__ == "__main__":
    main()

