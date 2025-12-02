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
    # Strategy parameters
    THRESH_HI = 0.65
    THRESH_LO = 0.35
    ALPHA = 1.0   # target = entry + ALPHA * ATR
    BETA = 0.5    # stop   = entry - BETA  * ATR
    HORIZON = 100  # max bars per swing (~100 minutes)

    ml = pd.read_csv("ml_export_es_new_3m.csv", low_memory=False)
    ml = ml[ml["label_amp"].isin(["up", "down"])].copy()
    ml["y"] = (ml["label_amp"] == "up").astype(int)

    train_df = ml[ml["split"] == "train"].copy()
    test_df = ml[ml["split"] == "test"].copy()

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

    p_train = clf.predict_proba(X_train)[:, 1]
    p_test = clf.predict_proba(X_test)[:, 1]
    auc_train = roc_auc_score(y_train, p_train)
    auc_test = roc_auc_score(y_test, p_test)
    print("Amplitude label HGB train AUC:", auc_train)
    print("Amplitude label HGB test AUC:", auc_test)

    # Build predictions aligned to raw RTH bars.
    raw = pd.read_csv("../es.c.0-20100606-20251116.et.ohlcv-1m.csv")
    raw["ET_datetime"] = pd.to_datetime(raw["ET_datetime"], utc=True)
    raw["date_str"] = raw["ET_datetime"].dt.strftime("%Y-%m-%d")
    raw["time_str"] = raw["ET_datetime"].dt.strftime("%H:%M")
    raw["minute_of_day"] = raw["ET_datetime"].dt.hour * 60 + raw["ET_datetime"].dt.minute
    rth_mask = (raw["minute_of_day"] >= 9 * 60 + 30) & (raw["minute_of_day"] <= 16 * 60 + 15)
    raw = raw[rth_mask].reset_index(drop=True)

    raw = raw.sort_values(["date_str", "time_str"]).reset_index(drop=True)
    raw["key"] = raw["date_str"] + " " + raw["time_str"]

    # Predictions only on test segment
    test_df_sorted = test_df.sort_values(["date", "time"]).reset_index(drop=True)
    X_test_sorted = X_test_df.loc[test_df_sorted.index].values.astype(np.float32)
    p_sorted = clf.predict_proba(X_test_sorted)[:, 1]
    test_df_sorted["p_amp"] = p_sorted

    test_df_sorted["key"] = test_df_sorted["date"] + " " + test_df_sorted["time"]
    preds = test_df_sorted[["key", "p_amp", "atr10"]].copy()
    # Align atr10 as numeric
    preds["atr10"] = pd.to_numeric(preds["atr10"], errors="coerce")

    raw = raw.merge(preds, on="key", how="left")

    close = raw["close"].values
    high = raw["high"].values
    low = raw["low"].values
    date_raw = raw["date_str"].values
    p_amp = raw["p_amp"].values
    atr10 = raw["atr10"].values

    n = len(raw)
    state = 0  # 0=flat, +1=long, -1=short
    entry_idx = None
    entry_px = None
    target_px = None
    stop_px = None
    expiry_idx = None

    trades_pnl = []
    trades_date = []

    last_date = date_raw[0] if n > 0 else None

    for j in range(n):
        d = date_raw[j]
        px_close = close[j]
        px_high = high[j]
        px_low = low[j]

        # If day changed, we don't force exit; swings can cross days.

        # Manage open position: check target/stop/horizon
        if state != 0 and entry_idx is not None:
            hit = False
            pnl = None
            # Horizon expiry
            if expiry_idx is not None and j >= expiry_idx:
                pnl = (px_close - entry_px) * state
                hit = True
            else:
                if state > 0:
                    hit_target = target_px is not None and px_high >= target_px
                    hit_stop = stop_px is not None and px_low <= stop_px
                else:
                    hit_target = target_px is not None and px_low <= target_px
                    hit_stop = stop_px is not None and px_high >= stop_px
                if hit_target and not hit_stop:
                    pnl = (target_px - entry_px) * state
                    hit = True
                elif hit_stop and not hit_target:
                    pnl = (stop_px - entry_px) * state
                    hit = True
                elif hit_target and hit_stop:
                    # Ambiguous bar: treat as flat outcome (zero PnL) and close.
                    pnl = 0.0
                    hit = True

            if hit:
                trades_pnl.append(pnl)
                trades_date.append(d)
                state = 0
                entry_idx = None
                entry_px = None
                target_px = None
                stop_px = None
                expiry_idx = None

        # Determine desired state from p_amp at this bar (if available)
        desired_state = state
        if not np.isnan(p_amp[j]):
            if p_amp[j] >= THRESH_HI:
                desired_state = 1
            elif p_amp[j] <= THRESH_LO:
                desired_state = -1
            # else keep current

        # Handle state transitions (flip / new entry)
        if desired_state != state:
            # Close existing position at this bar's close if any and not already closed
            if state != 0 and entry_px is not None:
                pnl = (px_close - entry_px) * state
                trades_pnl.append(pnl)
                trades_date.append(d)
            # Open new position if desired_state != 0 and we have a valid ATR
            if desired_state != 0 and np.isfinite(atr10[j]) and atr10[j] is not None and atr10[j] > 0:
                state = desired_state
                entry_idx = j
                entry_px = px_close
                h = float(atr10[j])
                if state > 0:
                    target_px = entry_px + ALPHA * h
                    stop_px = entry_px - BETA * h
                else:
                    target_px = entry_px - ALPHA * h
                    stop_px = entry_px + BETA * h
                expiry_idx = j + HORIZON
            else:
                state = 0
                entry_idx = None
                entry_px = None
                target_px = None
                stop_px = None
                expiry_idx = None

    trades_pnl = np.array(trades_pnl, dtype=float)
    print("swing-model trades_used", len(trades_pnl))
    if len(trades_pnl) > 0:
        print(
            "swing-model per-trade mean_pts",
            float(trades_pnl.mean()),
            "std_pts",
            float(trades_pnl.std()),
        )
    else:
        print("No trades; exiting.")
        return

    pnl_df = pd.DataFrame({"date": trades_date, "pnl": trades_pnl})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = by_day.mean()
    std_daily = by_day.std()
    sharpe_daily = mean_daily / std_daily if std_daily > 0 else np.nan
    sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else np.nan

    print("swing-model days_with_trades", len(by_day))
    print("swing-model mean_daily_pts", float(mean_daily), "std_daily_pts", float(std_daily))
    print("swing-model daily_sharpe", float(sharpe_daily), "annualized_sharpe", float(sharpe_annual))

    # Per-year Sharpe
    pnl_df["year"] = pnl_df["date"].dt.year
    print("\nPer-year Sharpe (swing-model, ATR bracket, no cost):")
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
