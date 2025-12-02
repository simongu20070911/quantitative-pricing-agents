import pandas as pd
import numpy as np


def main():
    # Strategy parameters (same as model-based swing backtest)
    ALPHA = 1.0   # target = entry ± ALPHA * ATR
    BETA = 0.5    # stop   = entry ∓ BETA  * ATR
    HORIZON = 100  # max bars per swing

    # Load 3-minute ML export with amplitude labels and ATR
    ml = pd.read_csv("ml_export_es_new_3m.csv", low_memory=False)
    ml = ml[ml["label_amp"].isin(["up", "down"])].copy()

    # Use test segment only for PnL stats
    test_df = ml[ml["split"] == "test"].copy()

    # Oracle p_amp: 1 for up, 0 for down
    test_df["p_amp"] = (test_df["label_amp"] == "up").astype(float)

    # Load raw 1m ES and align to 3m keys
    raw = pd.read_csv("../es.c.0-20100606-20251116.et.ohlcv-1m.csv")
    raw["ET_datetime"] = pd.to_datetime(raw["ET_datetime"], utc=True)
    raw["date_str"] = raw["ET_datetime"].dt.strftime("%Y-%m-%d")
    raw["time_str"] = raw["ET_datetime"].dt.strftime("%H:%M")
    raw["minute_of_day"] = raw["ET_datetime"].dt.hour * 60 + raw["ET_datetime"].dt.minute
    rth_mask = (raw["minute_of_day"] >= 9 * 60 + 30) & (raw["minute_of_day"] <= 16 * 60 + 15)
    raw = raw[rth_mask].reset_index(drop=True)
    raw = raw.sort_values(["date_str", "time_str"]).reset_index(drop=True)

    # Build 3m buckets on raw to align exact bar prices with the export keys
    raw["bucket_min"] = (raw["minute_of_day"] // 3) * 3
    raw["bucket_time"] = (
        (raw["bucket_min"] // 60).astype(int).astype(str).str.zfill(2)
        + ":"
        + (raw["bucket_min"] % 60).astype(int).astype(str).str.zfill(2)
    )
    raw["key"] = raw["date_str"] + " " + raw["bucket_time"]

    # Aggregate raw to 3m OHLC (consistent with ml_export_main aggregation)
    def agg_group(g):
        return pd.Series(
            {
                "open": g.iloc[0]["open"],
                "high": g["high"].max(),
                "low": g["low"].min(),
                "close": g.iloc[-1]["close"],
            }
        )

    raw_3m = raw.groupby(["date_str", "bucket_min", "bucket_time"]).apply(agg_group).reset_index()
    raw_3m["key"] = raw_3m["date_str"] + " " + raw_3m["bucket_time"]

    # Merge oracle predictions onto 3m raw
    test_df["key"] = test_df["date"] + " " + test_df["time"]
    preds = test_df[["key", "p_amp", "atr10"]].copy()
    preds["atr10"] = pd.to_numeric(preds["atr10"], errors="coerce")

    df = raw_3m.merge(preds, on="key", how="left")
    df = df.sort_values(["date_str", "bucket_min"]).reset_index(drop=True)

    close = df["close"].values
    high = df["high"].values
    low = df["low"].values
    date_raw = df["date_str"].values
    p_amp = df["p_amp"].values
    atr10 = df["atr10"].values

    n = len(df)
    state = 0  # 0=flat, +1=long, -1=short
    entry_idx = None
    entry_px = None
    target_px = None
    stop_px = None
    expiry_idx = None

    trades_pnl = []
    trades_date = []

    for j in range(n):
        d = date_raw[j]
        px_close = close[j]
        px_high = high[j]
        px_low = low[j]

        # Manage open position
        if state != 0 and entry_idx is not None:
            hit = False
            pnl = None
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

        # Oracle desired state: up→long, down→short where label is defined
        desired_state = state
        if not np.isnan(p_amp[j]):
            desired_state = 1 if p_amp[j] >= 0.5 else -1

        if desired_state != state:
            # Close existing position at current close
            if state != 0 and entry_px is not None:
                pnl = (px_close - entry_px) * state
                trades_pnl.append(pnl)
                trades_date.append(d)

            # Open new position if desired_state != 0 and ATR is valid
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
    print("oracle swing-model trades_used", len(trades_pnl))
    if len(trades_pnl) == 0:
        print("No trades.")
        return
    print(
        "oracle swing-model per-trade mean_pts",
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

    print("oracle swing-model days_with_trades", len(by_day))
    print("oracle swing-model mean_daily_pts", float(mean_daily), "std_daily_pts", float(std_daily))
    print("oracle swing-model daily_sharpe", float(sharpe_daily), "annualized_sharpe", float(sharpe_annual))

    print("\nPer-year Sharpe (oracle swing-model, ATR bracket, no cost):")
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

