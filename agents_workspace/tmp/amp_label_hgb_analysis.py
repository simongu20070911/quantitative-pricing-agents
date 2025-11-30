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
        "day_net_change",
        "day_range",
        "day_pos_in_range",
        "day_trend_run_len",
        "dist_prev_day_high",
        "dist_prev_day_low",
        "range_mid",
        "range_mm_up",
        "range_mm_down",
        "soft_break_up_severity",
        "soft_break_down_severity",
        "recent_bull_count",
        "recent_bear_count",
        "recent_doji_count",
        "recent_body_sum",
    ]
    if num_means is not None:
        num_cols = list(num_means.keys())

    for c in num_cols:
        if c not in df.columns:
            df[c] = np.nan
        df[c] = pd.to_numeric(df[c], errors="coerce")
    cols_all_nan = [c for c in num_cols if df[c].isna().all()]
    if num_means is None:
        num_cols = [c for c in num_cols if c not in cols_all_nan]
        num_means = {c: df[c].mean() for c in num_cols}
    for c in num_cols:
        if c not in df.columns:
            df[c] = num_means.get(c, 0.0)
        df[c] = df[c].fillna(num_means[c])

    cat_cols = [
        "leg_side",
        "range_regime",
        "day_regime",
        "intraday_phase",
        "bar_is_trend",
        "bar_is_doji",
        "always_in",
        "htf_leg_side_5m",
        "range_tight",
        "micro_channel_overshoot",
        "major_channel_overshoot",
        "day_inside_prev_range",
        "soft_break_up_trend",
        "soft_break_down_trend",
    ]
    for c in cat_cols:
        if c not in df.columns:
            df[c] = ""
    cat = pd.get_dummies(df[cat_cols], drop_first=False)
    X = pd.concat([df[num_cols].reset_index(drop=True), cat.reset_index(drop=True)], axis=1)
    return X, num_cols, list(cat.columns), num_means


def main():
    HORIZON = 10

    ml = pd.read_csv("ml_export_es_new.csv", low_memory=False)
    ml = ml[ml["label_amp"].isin(["up", "down"])].copy()

    train_df = ml[ml["split"] == "train"].copy()
    test_df = ml[ml["split"] == "test"].copy()

    train_df["y"] = (train_df["label_amp"] == "up").astype(int)
    test_df["y"] = (test_df["label_amp"] == "up").astype(int)

    X_train_df, num_cols, train_cat_cols, num_means = build_features(train_df)
    X_test_df, _, test_cat_cols, _ = build_features(test_df, num_means=num_means)

    train_cat_set = set(train_cat_cols)
    test_cat_set = set(test_cat_cols)
    all_cat_cols = sorted(list(train_cat_set | test_cat_set))

    def align_onehots(X_df):
        missing = [c for c in all_cat_cols if c not in X_df.columns]
        for c in missing:
            X_df[c] = 0.0
        cols = num_cols + all_cat_cols
        return X_df[cols]

    X_train_df = align_onehots(X_train_df)
    X_test_df = align_onehots(X_test_df)

    X_train = X_train_df.values.astype(np.float32)
    X_test = X_test_df.values.astype(np.float32)
    y_train = train_df["y"].values
    y_test = test_df["y"].values

    print("Feature shapes train/test:", X_train.shape, X_test.shape)

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

    proba_train = clf.predict_proba(X_train)[:, 1]
    proba_test = clf.predict_proba(X_test)[:, 1]
    auc_train = roc_auc_score(y_train, proba_train)
    auc_test = roc_auc_score(y_test, proba_test)
    print("Amplitude label HGB train AUC:", auc_train)
    print("Amplitude label HGB test AUC:", auc_test)

    # Simple close-to-close payoff over HORIZON bars.
    raw = pd.read_csv("../es.c.0-20100606-20251116.et.ohlcv-1m.csv")
    raw["ET_datetime"] = pd.to_datetime(raw["ET_datetime"], utc=True)
    raw["date_str"] = raw["ET_datetime"].dt.strftime("%Y-%m-%d")
    raw["time_str"] = raw["ET_datetime"].dt.strftime("%H:%M")
    raw["minute_of_day"] = raw["ET_datetime"].dt.hour * 60 + raw["ET_datetime"].dt.minute
    rth_mask = (raw["minute_of_day"] >= 9 * 60 + 30) & (raw["minute_of_day"] <= 16 * 60 + 15)
    raw = raw[rth_mask].reset_index(drop=True)

    key_series = raw["date_str"] + " " + raw["time_str"]
    index_map = {k: i for i, k in enumerate(key_series)}
    close = raw["close"].values
    date_raw = raw["date_str"].values

    test_df_sorted = test_df.sort_values(["date", "time"]).reset_index(drop=True)
    X_test_sorted = X_test_df.loc[test_df_sorted.index].values.astype(np.float32)
    proba_sorted = clf.predict_proba(X_test_sorted)[:, 1]
    test_df_sorted["p_amp"] = proba_sorted

    pnls = []
    dates = []
    for _, row in test_df_sorted.iterrows():
        key = f"{row['date']} {row['time']}"
        i = index_map.get(key)
        if i is None:
            continue
        n = len(close)
        last_idx = min(n - 1, i + HORIZON)
        if date_raw[last_idx] != date_raw[i]:
            continue
        side = 1.0 if row["p_amp"] >= 0.5 else -1.0
        entry = close[i]
        exit_px = close[last_idx]
        pnl = (exit_px - entry) * side
        pnls.append(pnl)
        dates.append(row["date"])

    pnls = np.array(pnls, dtype=float)
    print("amp-model trades_used", len(pnls))
    print("amp-model per-trade mean_pts", float(pnls.mean()), "std_pts", float(pnls.std()))

    pnl_df = pd.DataFrame({"date": dates, "pnl": pnls})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = by_day.mean()
    std_daily = by_day.std()
    sharpe_daily = mean_daily / std_daily if std_daily > 0 else np.nan
    sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else np.nan
    print("amp-model days_with_trades", len(by_day))
    print("amp-model mean_daily_pts", float(mean_daily), "std_daily_pts", float(std_daily))
    print("amp-model daily_sharpe", float(sharpe_daily), "annualized_sharpe", float(sharpe_annual))


if __name__ == "__main__":
    main()

