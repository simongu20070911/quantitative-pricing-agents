import pandas as pd
import numpy as np
from sklearn.metrics import roc_auc_score
from sklearn.linear_model import LogisticRegression


def main():
    # Load ML export with amplitude labels
    ml = pd.read_csv("ml_export_es_new.csv", low_memory=False)
    ml = ml[ml["label_amp"].isin(["up", "down"])].copy()
    ml["y"] = (ml["label_amp"] == "up").astype(int)

    # Load raw ES to build naive return features that don't look at patterns
    raw = pd.read_csv("../es.c.0-20100606-20251116.et.ohlcv-1m.csv")
    raw["ET_datetime"] = pd.to_datetime(raw["ET_datetime"], utc=True)
    raw["date"] = raw["ET_datetime"].dt.strftime("%Y-%m-%d")
    raw["time"] = raw["ET_datetime"].dt.strftime("%H:%M")
    raw["minute_of_day"] = raw["ET_datetime"].dt.hour * 60 + raw["ET_datetime"].dt.minute
    rth_mask = (raw["minute_of_day"] >= 9 * 60 + 30) & (raw["minute_of_day"] <= 16 * 60 + 15)
    raw = raw[rth_mask].reset_index(drop=True)

    raw = raw.sort_values(["date", "time"]).reset_index(drop=True)
    close = raw["close"].values

    # Naive cumulative returns over fixed horizons (in points)
    for h in [10, 30, 60]:
        raw[f"ret_{h}"] = pd.Series(close).diff(h)

    key = raw["date"] + " " + raw["time"]
    raw["key"] = key
    ml["key"] = ml["date"] + " " + ml["time"]

    df = ml.merge(raw[["key", "ret_10", "ret_30", "ret_60"]], on="key", how="left")
    df = df.dropna(subset=["ret_10", "ret_30", "ret_60"])

    train_df = df[df["split"] == "train"].copy()
    test_df = df[df["split"] == "test"].copy()

    y_train = train_df["y"].values
    y_test = test_df["y"].values

    # 1) Single-feature sign baselines
    for h in [10, 30, 60]:
        f = f"ret_{h}"
        for name, series in [("sign", np.sign), ("raw", lambda x: x)]:
            x_tr = series(train_df[f].values)
            x_te = series(test_df[f].values)
            # For constant predictions, AUC is undefined; guard with noise
            # but here we just use raw values for AUC.
            auc_tr = roc_auc_score(y_train, x_tr)
            auc_te = roc_auc_score(y_test, x_te)
            print(f"Naive {name}({f}) AUC train={auc_tr:.4f} test={auc_te:.4f}")

    # 2) Simple logistic regression on all three naive returns
    X_train = train_df[["ret_10", "ret_30", "ret_60"]].values
    X_test = test_df[["ret_10", "ret_30", "ret_60"]].values

    lr = LogisticRegression(max_iter=1000)
    lr.fit(X_train, y_train)
    p_tr = lr.predict_proba(X_train)[:, 1]
    p_te = lr.predict_proba(X_test)[:, 1]
    auc_tr_lr = roc_auc_score(y_train, p_tr)
    auc_te_lr = roc_auc_score(y_test, p_te)
    print(f"Logistic(ret_10,30,60) AUC train={auc_tr_lr:.4f} test={auc_te_lr:.4f}")


if __name__ == "__main__":
    main()

