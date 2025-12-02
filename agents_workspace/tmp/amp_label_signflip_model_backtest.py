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
    # Load 3-minute export with amplitude labels
    ml = pd.read_csv("ml_export_es_new_3m.csv", low_memory=False)
    ml = ml[ml["label_amp"].isin(["up", "down"])].copy()
    ml["y"] = (ml["label_amp"] == "up").astype(int)

    train_df = ml[ml["split"] == "train"].copy()
    test_df = ml[ml["split"] == "test"].copy()

    # Build features and train HGB on label_amp
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

    p_train = clf.predict_proba(X_train)[:, 1]
    p_test = clf.predict_proba(X_test)[:, 1]
    auc_train = roc_auc_score(y_train, p_train)
    auc_test = roc_auc_score(y_test, p_test)
    print("Amplitude label HGB train AUC:", auc_train)
    print("Amplitude label HGB test AUC:", auc_test)

    # Build predicted state on test (3m bars), using p_amp>=0.5 as up, <0.5 as down
    test_df_sorted = test_df.sort_values(["date", "time"]).reset_index(drop=True)
    X_test_sorted = X_test_df.loc[test_df_sorted.index].values.astype(np.float32)
    p_sorted = clf.predict_proba(X_test_sorted)[:, 1]
    test_df_sorted["p_amp"] = p_sorted

    # Map predictions to position state: +1 (long) if p>=0.5, -1 (short) if p<0.5
    test_df_sorted["state_pred"] = np.where(test_df_sorted["p_amp"] >= 0.5, 1, -1)

    # Use export close as 3m close
    close = pd.to_numeric(
        test_df_sorted["close"].astype(str).str.replace("_", ""), errors="coerce"
    ).values
    dates = test_df_sorted["date"].values
    states = test_df_sorted["state_pred"].values

    n = len(test_df_sorted)
    state = 0  # 0=flat, +1=long, -1=short
    entry_px = None

    # Total per-trade cost: 0.07 fees + 1 tick (0.25)
    COST = 0.07 + 0.25
    trades_pnl = []
    trades_date = []

    for j in range(n):
        d = dates[j]
        px = close[j]
        desired = states[j]

        # On predicted sign change, close old and/or open new at this bar's close
        if desired != state:
            # Close existing
            if state != 0 and entry_px is not None and np.isfinite(px) and np.isfinite(entry_px):
                pnl = (px - entry_px) * state - COST
                trades_pnl.append(pnl)
                trades_date.append(d)
            # Open new (always in) if desired != 0
            if np.isfinite(px):
                state = desired
                entry_px = px
            else:
                state = 0
                entry_px = None

    # Close any open position at last bar
    if state != 0 and entry_px is not None and np.isfinite(close[-1]) and np.isfinite(entry_px):
        pnl = (close[-1] - entry_px) * state - COST
        trades_pnl.append(pnl)
        trades_date.append(dates[-1])

    trades_pnl = np.array(trades_pnl, dtype=float)
    print("model signflip swing-model trades_used", len(trades_pnl))
    if len(trades_pnl) == 0:
        print("No trades.")
        return
    print(
        "model signflip swing-model per-trade mean_pts",
        float(trades_pnl.mean()),
        "std_pts",
        float(trades_pnl.std()),
    )

    pnl_df = pd.DataFrame({"date": trades_date, "pnl": trades_pnl})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = by_day.mean()
    std_daily = by_day.std()
    sharpe_daily = mean_daily / std_daily if std_daily > 0 else np.nan
    sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else np.nan

    print("model signflip swing-model days_with_trades", len(by_day))
    print("model signflip swing-model mean_daily_pts", float(mean_daily), "std_daily_pts", float(std_daily))
    print("model signflip swing-model daily_sharpe", float(sharpe_daily), "annualized_sharpe", float(sharpe_annual))

    print("\nPer-year Sharpe (model signflip swing-model, no cost):")
    pnl_df["year"] = pnl_df["date"].dt.year
    for year, grp in pnl_df.groupby("year"):
        by_day_y = grp.groupby("date")["pnl"].sum()
        m = by_day_y.mean()
        s = by_day_y.std()
        if s > 0:
            sd = m / s
            sa = sd * np.sqrt(252)
        else:
            sd = float("nan")
            sa = float("nan")
        print(f"  {year}: days={len(by_day_y)}, mean_daily={m:.3f}, std_daily={s:.3f}, ann_sharpe={sa:.3f}")


if __name__ == "__main__":
    main()
