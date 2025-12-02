import pandas as pd
import numpy as np


def main():
    # Load 3-minute export with amplitude labels
    ml = pd.read_csv("ml_export_es_new_3m.csv", low_memory=False)
    ml = ml[ml["label_amp"].isin(["up", "down", "flat"])].copy()

    # Test segment only
    df = ml[ml["split"] == "test"].copy()
    df = df.sort_values(["date", "time"]).reset_index(drop=True)

    # Map labels to desired state: up=+1, down=-1, flat=0
    label_to_state = {"up": 1, "down": -1, "flat": 0}
    df["state_label"] = df["label_amp"].map(label_to_state)

    # Use export close as tradeable 3m close
    close = pd.to_numeric(df["close"].str.replace("_", ""), errors="coerce").values
    dates = df["date"].values
    states = df["state_label"].values

    n = len(df)
    state = 0  # 0=flat, +1=long, -1=short
    entry_px = None

    trades_pnl = []
    trades_date = []

    for j in range(n):
        d = dates[j]
        px = close[j]
        desired = states[j]

        # On label change, close old and/or open new at this bar's close
        if desired != state:
            # Close existing
            if state != 0 and entry_px is not None and np.isfinite(px) and np.isfinite(entry_px):
                pnl = (px - entry_px) * state
                trades_pnl.append(pnl)
                trades_date.append(d)
            # Open new if non-flat
            if desired != 0 and np.isfinite(px):
                state = desired
                entry_px = px
            else:
                state = 0
                entry_px = None

    # Close any open position at last bar
    if state != 0 and entry_px is not None and np.isfinite(close[-1]) and np.isfinite(entry_px):
        pnl = (close[-1] - entry_px) * state
        trades_pnl.append(pnl)
        trades_date.append(dates[-1])

    trades_pnl = np.array(trades_pnl, dtype=float)
    print("signflip swing-model trades_used", len(trades_pnl))
    if len(trades_pnl) == 0:
        print("No trades.")
        return
    print(
        "signflip swing-model per-trade mean_pts",
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

    print("signflip swing-model days_with_trades", len(by_day))
    print("signflip swing-model mean_daily_pts", float(mean_daily), "std_daily_pts", float(std_daily))
    print("signflip swing-model daily_sharpe", float(sharpe_daily), "annualized_sharpe", float(sharpe_annual))

    print("\nPer-year Sharpe (signflip swing-model, oracle labels, no cost):")
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

