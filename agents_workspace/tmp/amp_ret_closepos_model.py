import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score


def build_ret_closepos_features(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy().sort_values(["date", "time"]).reset_index(drop=True)
    close = pd.to_numeric(df["close"].astype(str).str.replace("_", ""), errors="coerce")

    ret1 = np.zeros(len(close), dtype=np.float64)
    close_np = close.values
    for i in range(1, len(close_np)):
        if close_np[i - 1] > 0 and close_np[i] > 0:
            ret1[i] = np.log(close_np[i] / close_np[i - 1])
        else:
            ret1[i] = 0.0

    df["ret1"] = ret1
    return df


def main():
    path = "ml_export_es_new_3m.csv"
    df = pd.read_csv(path, low_memory=False)
    df = df[df["label_amp"].isin(["up", "down"])].copy()
    df["y"] = (df["label_amp"] == "up").astype(int)

    df_feat = build_ret_closepos_features(df)

    # Keep only rows where bar_close_pos is defined
    df_feat = df_feat[~df_feat["bar_close_pos"].isna()].reset_index(drop=True)

    train_df = df_feat[df_feat["split"] == "train"].copy()
    test_df = df_feat[df_feat["split"] == "test"].copy()

    X_train = train_df[["ret1", "bar_close_pos"]].values
    y_train = train_df["y"].values
    X_test = test_df[["ret1", "bar_close_pos"]].values
    y_test = test_df["y"].values

    # Standardize using train stats
    mu = X_train.mean(axis=0, keepdims=True)
    sigma = X_train.std(axis=0, keepdims=True)
    sigma[sigma == 0.0] = 1.0
    X_train_std = (X_train - mu) / sigma
    X_test_std = (X_test - mu) / sigma

    clf = LogisticRegression(max_iter=1000)
    clf.fit(X_train_std, y_train)

    proba_test = clf.predict_proba(X_test_std)[:, 1]
    auc = roc_auc_score(y_test, proba_test)
    print("ret1+bar_close_pos logistic, test AUC:", auc)

    # Sign-flip always-in strategy with cost = 0.07 + 0.25
    COST = 0.07 + 0.25
    test_df = test_df.copy().reset_index(drop=True)
    test_df["p_rc"] = proba_test
    test_df["state_pred"] = np.where(test_df["p_rc"] >= 0.5, 1, -1)

    close = pd.to_numeric(
        test_df["close"].astype(str).str.replace("_", ""), errors="coerce"
    ).values
    dates = test_df["date"].values
    states = test_df["state_pred"].values

    n = len(test_df)
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
    print("ret1+bar_close_pos signflip trades_used", len(trades_pnl))
    if len(trades_pnl) == 0:
        return
    mean_trade = float(trades_pnl.mean())
    std_trade = float(trades_pnl.std())
    print(
        "ret1+bar_close_pos signflip per-trade mean_pts",
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
        "ret1+bar_close_pos signflip days_with_trades",
        len(by_day),
        "mean_daily_pts",
        mean_daily,
        "std_daily_pts",
        std_daily,
    )
    print(
        "ret1+bar_close_pos signflip daily_sharpe",
        sharpe_daily,
        "annualized_sharpe",
        sharpe_annual,
    )


if __name__ == "__main__":
    main()

