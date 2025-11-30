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
        # channel geometry
        "micro_channel_slope",
        "major_channel_slope",
        "micro_channel_z",
        "major_channel_z",
        # measured moves / strength
        "leg_mm_up",
        "leg_mm_down",
        "recent_strength_score",
        # higher-timeframe leg
        "htf_leg_len_bars_5m",
        # new day / magnet context
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
    # On test/validation calls, restrict numeric columns to the keys we saw on train.
    if num_means is not None:
        num_cols = list(num_means.keys())
    for c in num_cols:
        # If column missing (e.g. older exports), create it as NaN for now.
        if c not in df.columns:
            df[c] = np.nan
        df[c] = pd.to_numeric(df[c], errors="coerce")
    cols_all_nan = [c for c in num_cols if df[c].isna().all()]
    # On the training split, drop columns that are entirely NaN to avoid degenerate features.
    # On test/validation (when num_means is provided), keep the training-defined num_cols unchanged.
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
        # boolean options / flags treated as categorical
        "range_tight",
        "micro_channel_overshoot",
        "major_channel_overshoot",
        "day_inside_prev_range",
        "soft_break_up_trend",
        "soft_break_down_trend",
    ]
    cat = pd.get_dummies(df[cat_cols], drop_first=False)
    X = pd.concat([df[num_cols].reset_index(drop=True), cat.reset_index(drop=True)], axis=1)
    return X, num_cols, list(cat.columns), num_means


def compute_stop_labels(df: pd.DataFrame, raw: pd.DataFrame, tick: float = 0.25, horizon: int = 10):
    """Compute stop-based bracket labels for a *long* stop entry.

    For each bar t in df:
      - Map to raw index i via date+time.
      - Stop at close_t + tick.
      - If stop never hits within horizon: label = None.
      - Once filled at entry_price, bracket = entry ± atr10(t).
      - Over next horizon bars after entry, if up hits first -> 'up', if down hits first -> 'down',
        if both in same bar or no hit -> None.
    """
    df = df.copy()
    raw = raw.copy()

    # Use ET_datetime when present (ES .et.ohlcv), otherwise fall back to ts_event
    # (e.g., raw MDP3 GC file). In the fallback we treat timestamps as local and
    # compute minute-of-day directly; this matches the OCaml parser, which uses
    # ts_event (with fractional seconds stripped) as a local wall-clock time.
    if "ET_datetime" in raw.columns:
        dt = pd.to_datetime(raw["ET_datetime"], utc=True)
    elif "ts_event" in raw.columns:
        dt = pd.to_datetime(raw["ts_event"])
    else:
        raise ValueError("raw file must contain ET_datetime or ts_event column")

    raw["date_str"] = dt.dt.strftime("%Y-%m-%d")
    raw["time_str"] = dt.dt.strftime("%H:%M")
    raw["minute_of_day"] = dt.dt.hour * 60 + dt.dt.minute
    # approximate RTH 9:30-16:15, same gate as OCaml exporter
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


def main():
    import sys

    COST = 0.0
    HORIZON = 10

    # Instrument-specific config
    sym = sys.argv[1].lower() if len(sys.argv) > 1 else "es"
    if sym == "es":
        TICK = 0.25
        ml_path = "ml_export_es_new.csv"
        raw_path = "../es.c.0-20100606-20251116.et.ohlcv-1m.csv"
    elif sym == "gc":
        TICK = 0.1
        ml_path = "ml_export_gc_new.csv"
        raw_path = "../glbx-mdp3-20100606-20251117_gc.ohlcv-1m.csv"
    else:
        raise ValueError(f"Unknown symbol '{sym}'")

    # Load ml export
    ml = pd.read_csv(ml_path, low_memory=False)
    # Only use rows where original label was up/down (for consistency)
    ml = ml[ml["label_small"].isin(["up", "down"])].copy()

    train_df = ml[ml["split"] == "train"].copy()
    test_df = ml[ml["split"] == "test"].copy()

    # raw underlying for labels
    raw = pd.read_csv(raw_path)

    print("Computing stop-based labels for train...")
    train_labels = compute_stop_labels(train_df, raw, tick=TICK, horizon=HORIZON)
    print("Computing stop-based labels for test...")
    test_labels = compute_stop_labels(test_df, raw, tick=TICK, horizon=HORIZON)

    train_df["label_stop"] = train_labels
    test_df["label_stop"] = test_labels

    # Drop None labels
    train_df = train_df[train_df["label_stop"].isin(["up", "down"])].copy()
    test_df = test_df[test_df["label_stop"].isin(["up", "down"])].copy()

    print("Train stop-labeled rows:", len(train_df), "Test stop-labeled rows:", len(test_df))

    train_df["y"] = (train_df["label_stop"] == "up").astype(int)
    test_df["y"] = (test_df["label_stop"] == "up").astype(int)

    # Build features using same pipeline
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

    print("Feature shapes train/test:", X_train.shape, X_test.shape)

    # Smaller, more regularized HGB to reduce overfitting
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
    auc_train = roc_auc_score(y_train, proba_train)
    print("HGB on stop-label train AUC:", auc_train)

    proba_test = clf.predict_proba(X_test)[:, 1]
    auc = roc_auc_score(y_test, proba_test)
    print("HGB on stop-label test AUC:", auc)

    # Stop-entry trading aligned with this label:
    # long-stop only (label_stop is defined for long scenario),
    # we go long when p>=0.5, short when p<0.5 and flip label.
    test_df_sorted = test_df.sort_values(["date", "time"]).reset_index(drop=True)
    X_test_sorted = X_test_df.loc[test_df_sorted.index].values.astype(np.float32)
    proba_sorted = clf.predict_proba(X_test_sorted)[:, 1]
    test_df_sorted["p_hgb_stop"] = proba_sorted

    # Map date/time to raw indices again (same timestamp handling as in compute_stop_labels)
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
    rth_mask = (raw_rth["minute_of_day"] >= 9 * 60 + 30) & (raw_rth["minute_of_day"] <= 16 * 60 + 15)
    raw_rth = raw_rth[rth_mask].reset_index(drop=True)
    key_series = raw_rth["date_str"] + " " + raw_rth["time_str"]
    index_map = {k: i for i, k in enumerate(key_series)}

    high = raw_rth["high"].values
    low = raw_rth["low"].values
    close = raw_rth["close"].values
    date_raw = raw_rth["date_str"].values

    pnls = []
    trade_dates = []
    prob_trades = []

    for _, row in test_df_sorted.iterrows():
        key = f"{row['date']} {row['time']}"
        i = index_map.get(key)
        if i is None:
            continue
        atr = pd.to_numeric(row["atr10"], errors="coerce")
        if not np.isfinite(atr) or atr <= 0:
            continue
        side = 1.0 if row["p_hgb_stop"] >= 0.5 else -1.0
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
        trade_dates.append(row["date"])
        prob_trades.append(row["p_hgb_stop"])

    pnls = np.array(pnls, dtype=float)
    prob_trades = np.array(prob_trades, dtype=float)
    print("stop-model trades_used", len(pnls))
    print("stop-model per-trade mean_pts", float(pnls.mean()), "std_pts", float(pnls.std()))

    pnl_df = pd.DataFrame({"date": trade_dates, "pnl": pnls})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = by_day.mean()
    std_daily = by_day.std()
    sharpe_daily = mean_daily / std_daily
    sharpe_annual = sharpe_daily * np.sqrt(252)

    print("stop-model days_with_trades", len(by_day))
    print("stop-model mean_daily_pts", float(mean_daily), "std_daily_pts", float(std_daily))
    print("stop-model daily_sharpe", float(sharpe_daily), "annualized_sharpe", float(sharpe_annual))

    # High-confidence slices
    def analyze_band(lo, hi):
        mask = (prob_trades >= lo) & (prob_trades <= hi)
        if not mask.any():
            print(f"band {lo}-{hi}: no trades")
            return
        pnls_band = pnls[mask]
        pnl_df_b = pd.DataFrame({"date": np.array(trade_dates)[mask], "pnl": pnls_band})
        pnl_df_b["date"] = pd.to_datetime(pnl_df_b["date"])
        by_day_b = pnl_df_b.groupby("date")["pnl"].sum()
        mean_d = by_day_b.mean()
        std_d = by_day_b.std()
        sharpe_d = mean_d / std_d if std_d > 0 else np.nan
        sharpe_a = sharpe_d * np.sqrt(252) if not np.isnan(sharpe_d) else np.nan
        print(f"band {lo}-{hi}: n_trades={len(pnls_band)}, mean_pts={pnls_band.mean():.5f}, std_pts={pnls_band.std():.3f}, ann_sharpe={sharpe_a:.3f}")

    print("\nHigh-confidence bands (stop-model, cost=0):")
    analyze_band(0.0, 0.35)
    analyze_band(0.65, 1.0)
    analyze_band(0.0, 0.25)
    analyze_band(0.75, 1.0)

    # Short-only strategies
    def analyze_short(mask, name):
        if not mask.any():
            print(f"{name}: no trades")
            return
        pnls_s = pnls[mask]
        dates_s = np.array(trade_dates)[mask]
        pnl_df_s = pd.DataFrame({"date": dates_s, "pnl": pnls_s})
        pnl_df_s["date"] = pd.to_datetime(pnl_df_s["date"])
        by_day_s = pnl_df_s.groupby("date")["pnl"].sum()
        mean_d = by_day_s.mean()
        std_d = by_day_s.std()
        sharpe_d = mean_d / std_d if std_d > 0 else np.nan
        sharpe_a = sharpe_d * np.sqrt(252) if not np.isnan(sharpe_d) else np.nan
        print(f"{name}: n_trades={len(pnls_s)}, mean_pts={pnls_s.mean():.5f}, std_pts={pnls_s.std():.3f}, ann_sharpe={sharpe_a:.3f}")

    print("\nShort-only variants (stop-model, cost=0):")
    # short when p<0.5
    mask_short_all = prob_trades < 0.5
    analyze_short(mask_short_all, "short_all_p<0.5")
    # short when p<0.35
    mask_short_035 = prob_trades < 0.35
    analyze_short(mask_short_035, "short_p<0.35")

    # Long-only variants
    def analyze_long(mask, name):
        if not mask.any():
            print(f"{name}: no trades")
            return
        pnls_l = pnls[mask]
        dates_l = np.array(trade_dates)[mask]
        pnl_df_l = pd.DataFrame({"date": dates_l, "pnl": pnls_l})
        pnl_df_l["date"] = pd.to_datetime(pnl_df_l["date"])
        by_day_l = pnl_df_l.groupby("date")["pnl"].sum()
        mean_d = by_day_l.mean()
        std_d = by_day_l.std()
        sharpe_d = mean_d / std_d if std_d > 0 else np.nan
        sharpe_a = sharpe_d * np.sqrt(252) if not np.isnan(sharpe_d) else np.nan
        print(f"{name}: n_trades={len(pnls_l)}, mean_pts={pnls_l.mean():.5f}, std_pts={pnls_l.std():.3f}, ann_sharpe={sharpe_a:.3f}")

    print("\nLong-only variants (stop-model, cost=0):")
    mask_long_all = prob_trades >= 0.5
    analyze_long(mask_long_all, "long_all_p>=0.5")
    mask_long_065 = prob_trades >= 0.65
    analyze_long(mask_long_065, "long_p>=0.65")

    # Per-year Sharpe for short_p<0.35
    print("\nPer-year Sharpe for short_p<0.35 (stop-model, cost=0):")
    if mask_short_035.any():
        pnls_s = pnls[mask_short_035]
        dates_s = np.array(trade_dates)[mask_short_035]
        pnl_df_s = pd.DataFrame({"date": dates_s, "pnl": pnls_s})
        pnl_df_s["date"] = pd.to_datetime(pnl_df_s["date"])
        pnl_df_s["year"] = pnl_df_s["date"].dt.year
        for year, grp in pnl_df_s.groupby("year"):
            by_day_y = grp.groupby("date")["pnl"].sum()
            mean_d = by_day_y.mean()
            std_d = by_day_y.std()
            if std_d > 0:
                sharpe_d = mean_d / std_d
                sharpe_a = sharpe_d * np.sqrt(252)
            else:
                sharpe_d = float("nan")
                sharpe_a = float("nan")
            print(f"  {year}: days={len(by_day_y)}, mean_daily={mean_d:.3f}, std_daily={std_d:.3f}, ann_sharpe={sharpe_a:.3f}")


if __name__ == "__main__":
    main()
