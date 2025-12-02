import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score


def build_three_ret_features(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy().sort_values(["date", "time"]).reset_index(drop=True)
    close = pd.to_numeric(df["close"].astype(str).str.replace("_", ""), errors="coerce")

    # 10-bar log returns
    r1 = np.zeros(len(close), dtype=np.float64)
    r2 = np.zeros(len(close), dtype=np.float64)
    r3 = np.zeros(len(close), dtype=np.float64)

    close_np = close.values
    for i in range(len(close_np)):
        if i >= 10 and close_np[i - 10] > 0 and close_np[i] > 0:
            r1[i] = np.log(close_np[i] / close_np[i - 10])
        if i >= 20 and close_np[i - 20] > 0 and close_np[i - 10] > 0:
            r2[i] = np.log(close_np[i - 10] / close_np[i - 20])
        if i >= 30 and close_np[i - 30] > 0 and close_np[i - 20] > 0:
            r3[i] = np.log(close_np[i - 20] / close_np[i - 30])

    df["r1_0_10"] = r1
    df["r2_10_20"] = r2
    df["r3_20_30"] = r3
    return df


def main():
    path = "ml_export_es_new_3m.csv"
    df = pd.read_csv(path, low_memory=False)
    df = df[df["label_amp"].isin(["up", "down"])].copy()

    # Binary labels: up=1, down=0
    df["y"] = (df["label_amp"] == "up").astype(int)

    # Build features
    df_feat = build_three_ret_features(df)

    train_df = df_feat[df_feat["split"] == "train"].copy()
    test_df = df_feat[df_feat["split"] == "test"].copy()

    # Drop early rows where returns are zero because of insufficient history
    train_df = train_df[(train_df["r1_0_10"] != 0.0) | (train_df["r2_10_20"] != 0.0) | (train_df["r3_20_30"] != 0.0)]
    test_df = test_df[(test_df["r1_0_10"] != 0.0) | (test_df["r2_10_20"] != 0.0) | (test_df["r3_20_30"] != 0.0)]

    X_train = train_df[["r1_0_10", "r2_10_20", "r3_20_30"]].values
    y_train = train_df["y"].values
    X_test = test_df[["r1_0_10", "r2_10_20", "r3_20_30"]].values
    y_test = test_df["y"].values

    # Standardize features based on train
    mu = X_train.mean(axis=0, keepdims=True)
    sigma = X_train.std(axis=0, keepdims=True)
    sigma[sigma == 0.0] = 1.0
    X_train_std = (X_train - mu) / sigma
    X_test_std = (X_test - mu) / sigma

    clf = LogisticRegression(max_iter=1000)
    clf.fit(X_train_std, y_train)

    proba_test = clf.predict_proba(X_test_std)[:, 1]
    auc = roc_auc_score(y_test, proba_test)
    print("Three-ret logistic, test AUC:", auc)

    # Sign-flip always-in strategy on ES 3m using logistic predictions (no cost)
    test_df = test_df.copy().reset_index(drop=True)
    test_df["p_three"] = proba_test
    test_df["state_pred"] = np.where(test_df["p_three"] >= 0.5, 1, -1)

    close = pd.to_numeric(
        test_df["close"].astype(str).str.replace("_", ""), errors="coerce"
    ).values
    dates = test_df["date"].values
    states = test_df["state_pred"].values

    n = len(test_df)
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
        if desired != state:
            if state != 0 and entry_px is not None and np.isfinite(px) and np.isfinite(entry_px):
                pnl = (px - entry_px) * state - COST
                trades_pnl.append(pnl)
                trades_date.append(d)
            if np.isfinite(px):
                state = desired
                entry_px = px
            else:
                state = 0
                entry_px = None

    if state != 0 and entry_px is not None and np.isfinite(close[-1]) and np.isfinite(entry_px):
        pnl = (close[-1] - entry_px) * state - COST
        trades_pnl.append(pnl)
        trades_date.append(dates[-1])

    trades_pnl = np.array(trades_pnl, dtype=float)
    print("Three-ret signflip trades_used", len(trades_pnl))
    if len(trades_pnl) == 0:
        return
    mean_trade = float(trades_pnl.mean())
    std_trade = float(trades_pnl.std())
    print(
        "Three-ret signflip per-trade mean_pts",
        mean_trade,
        "std_pts",
        std_trade,
    )

    pnl_df = pd.DataFrame({"date": trades_date, "pnl": trades_pnl})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = float(by_day.mean())
    std_daily = float(by_day.std())
    sharpe_daily = mean_daily / std_daily if std_daily > 0 else float("nan")
    sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else float("nan")
    print(
        "Three-ret signflip days_with_trades",
        len(by_day),
        "mean_daily_pts",
        mean_daily,
        "std_daily_pts",
        std_daily,
    )
    print(
        "Three-ret signflip daily_sharpe",
        sharpe_daily,
        "annualized_sharpe",
        sharpe_annual,
    )


if __name__ == "__main__":
    main()
