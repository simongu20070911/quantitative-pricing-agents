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
    ml = pd.read_csv("ml_export_noise_3m.csv", low_memory=False)
    ml = ml[ml["label_amp"].isin(["up", "down"])].copy()
    ml["y"] = (ml["label_amp"] == "up").astype(int)

    train_df = ml[ml["split"] == "train"].copy().sort_values(["date", "time"]).reset_index(
        drop=True
    )
    test_df = ml[ml["split"] == "test"].copy().sort_values(["date", "time"]).reset_index(
        drop=True
    )

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

    p_test = clf.predict_proba(X_test)[:, 1]
    auc_test = roc_auc_score(y_test, p_test)
    print("Noise HGB amplitude label test AUC:", auc_test)

    # Sign-flip always-in strategy on noise using HGB predictions
    test_df_sorted = test_df.copy()
    test_df_sorted["p_amp"] = p_test
    test_df_sorted = test_df_sorted.sort_values(["date", "time"]).reset_index(drop=True)

    test_df_sorted["state_pred"] = np.where(test_df_sorted["p_amp"] >= 0.5, 1, -1)

    close = pd.to_numeric(
        test_df_sorted["close"].astype(str).str.replace("_", ""), errors="coerce"
    ).values
    dates = test_df_sorted["date"].values
    states = test_df_sorted["state_pred"].values

    n = len(test_df_sorted)
    state = 0
    entry_px = None

    trades_pnl = []
    trades_date = []

    for j in range(n):
        d = dates[j]
        px = close[j]
        desired = states[j]
        if desired != state:
            if state != 0 and entry_px is not None and np.isfinite(px) and np.isfinite(entry_px):
                pnl = (px - entry_px) * state
                trades_pnl.append(pnl)
                trades_date.append(d)
            if np.isfinite(px):
                state = desired
                entry_px = px
            else:
                state = 0
                entry_px = None

    if state != 0 and entry_px is not None and np.isfinite(close[-1]) and np.isfinite(entry_px):
        pnl = (close[-1] - entry_px) * state
        trades_pnl.append(pnl)
        trades_date.append(dates[-1])

    trades_pnl = np.array(trades_pnl, dtype=float)
    print("Noise HGB signflip trades_used", len(trades_pnl))
    if len(trades_pnl) == 0:
        return
    mean_trade = float(trades_pnl.mean())
    std_trade = float(trades_pnl.std())
    print(
        "Noise HGB signflip per-trade mean_pts",
        mean_trade,
        "std_pts",
        std_trade,
    )

    # Daily Sharpe on noise
    pnl_df = pd.DataFrame({"date": trades_date, "pnl": trades_pnl})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = float(by_day.mean())
    std_daily = float(by_day.std())
    sharpe_daily = mean_daily / std_daily if std_daily > 0 else float("nan")
    sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else float("nan")
    print(
        "Noise HGB signflip days_with_trades",
        len(by_day),
        "mean_daily_pts",
        mean_daily,
        "std_daily_pts",
        std_daily,
    )
    print(
        "Noise HGB signflip daily_sharpe",
        sharpe_daily,
        "annualized_sharpe",
        sharpe_annual,
    )


if __name__ == "__main__":
    main()
