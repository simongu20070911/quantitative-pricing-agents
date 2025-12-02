import os
from pathlib import Path

import numpy as np
import pandas as pd

from config import AmpModelConfig, BacktestConfig, DataPaths, StopModelConfig
from pipeline import run_pipeline


def _write_backtest_equity(out_dir: Path, trade_dates, pnls: np.ndarray) -> None:
    """Persist per-trade and daily equity curves, mirroring the original script."""
    pnl_df = pd.DataFrame({"date": trade_dates, "pnl": pnls})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    pnl_df_sorted = pnl_df.sort_values("date").reset_index(drop=True)
    pnl_df_sorted["cum_pnl"] = pnl_df_sorted["pnl"].cumsum()

    by_day = pnl_df.groupby("date")["pnl"].sum().sort_index()
    daily_equity = by_day.to_frame(name="daily_pnl")
    daily_equity["cum_pnl"] = daily_equity["daily_pnl"].cumsum()

    trades_path = out_dir / "stop_trades_equity_extreme_rep.csv"
    daily_path = out_dir / "stop_daily_equity_extreme_rep.csv"
    pnl_df_sorted.to_csv(trades_path, index=False)
    daily_equity.to_csv(daily_path)


def main() -> None:
    paths = DataPaths()
    amp_cfg = AmpModelConfig()
    stop_cfg = StopModelConfig()
    bt_cfg = BacktestConfig()

    outputs = run_pipeline(paths, amp_cfg, stop_cfg, bt_cfg)

    amp = outputs.amp_metrics
    stop = outputs.stop_metrics
    bt = outputs.backtest_metrics

    print("Amp model class counts (down, flat, up):", amp.class_counts.tolist())
    print("Amp model class weights (down, flat, up):", amp.class_weights.tolist())
    print("\nAmp model 3-class accuracy train/test:", amp.acc_train, amp.acc_test)

    print(
        "\nComputing stop-based triple-barrier labels (up/down) for train/test..."
    )
    print(
        "Triple-barrier labeled rows (up/down only):",
        stop.n_train_tb,
        "train,",
        stop.n_test_tb,
        "test",
    )

    print("Stop model feature shapes train/test:", stop.shape_train, stop.shape_test)
    print(
        "Stop model class balance train/test (up=1):",
        stop.y_train_mean,
        stop.y_test_mean,
    )

    print("\nStop model (with amp p) performance:")
    print("  Accuracy train/test:", stop.acc_train, stop.acc_test)
    print("  AUC train/test     :", stop.auc_train, stop.auc_test)

    print(
        "\nNaive triple-barrier backtest using Stop model (with amp p), cost=0:"
    )
    print("  trades_used", bt.trades_used)
    if bt.trades_used > 0:
        print(
            "  per-trade mean_pts",
            bt.mean_pnl,
            "std_pts",
            bt.std_pnl,
        )
        print(
            "  trade win/loss/flat fractions:",
            bt.win_frac,
            bt.loss_frac,
            bt.flat_frac,
        )
        print("  days_with_trades", bt.days_with_trades)
        print(
            "  mean_daily_pts",
            bt.mean_daily,
            "std_daily_pts",
            bt.std_daily,
        )
        print(
            "  daily_sharpe",
            bt.sharpe_daily,
            "annualized_sharpe",
            bt.sharpe_annual,
        )

    out_dir = Path(os.path.dirname(__file__))
    _write_backtest_equity(out_dir, bt.trade_dates, bt.pnls)


if __name__ == "__main__":
    main()
