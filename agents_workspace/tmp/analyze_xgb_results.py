import pandas as pd
import numpy as np
from sklearn.ensemble import HistGradientBoostingClassifier
from sklearn.metrics import accuracy_score, roc_auc_score, confusion_matrix


def load_and_prepare(path: str):
    df = pd.read_csv(path, low_memory=False)
    df = df[df["label_small"].isin(["up", "down"])].copy()
    df["y"] = (df["label_small"] == "up").astype(int)

    train_df = df[df["split"] == "train"].copy()
    test_df = df[df["split"] == "test"].copy()

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
    ]

    for c in num_cols:
        train_df[c] = pd.to_numeric(train_df[c], errors="coerce")
        test_df[c] = pd.to_numeric(test_df[c], errors="coerce")

    # Drop columns that are entirely NaN in train (carry no information)
    cols_all_nan = [c for c in num_cols if train_df[c].isna().all()]
    num_cols = [c for c in num_cols if c not in cols_all_nan]

    cat_cols = ["leg_side", "bar_is_trend", "bar_is_doji", "always_in", "htf_leg_side_5m"]
    train_cat = pd.get_dummies(train_df[cat_cols], drop_first=False)
    test_cat = pd.get_dummies(test_df[cat_cols], drop_first=False)
    train_cat, test_cat = train_cat.align(test_cat, join="outer", axis=1, fill_value=0)

    X_train = pd.concat(
        [train_df[num_cols].reset_index(drop=True), train_cat.reset_index(drop=True)],
        axis=1,
    )
    X_test = pd.concat(
        [test_df[num_cols].reset_index(drop=True), test_cat.reset_index(drop=True)],
        axis=1,
    )

    return train_df, test_df, X_train, X_test


def bucket_confidence(y_true, proba, extra_df, n_bins=10):
    df = pd.DataFrame({"y": y_true, "p": proba})
    for col in [
        "range_width_ratio",
        "pos_in_range",
        "micro_channel_z",
        "major_channel_z",
        "soft_micro_up_len",
        "soft_micro_down_len",
        "soft_micro_bias",
        "bar_body_frac",
        "bar_close_pos",
    ]:
        if col in extra_df.columns:
            df[col] = extra_df[col].values
    df["bin"] = pd.qcut(df["p"], q=n_bins, labels=False, duplicates="drop")
    rows = []
    for b in sorted(df["bin"].dropna().unique()):
        sub = df[df["bin"] == b]
        n = len(sub)
        if n == 0:
            continue
        up_rate = sub["y"].mean()
        row = {
            "bin": int(b),
            "n": int(n),
            "avg_p": float(sub["p"].mean()),
            "realized_up_rate": float(up_rate),
        }
        for col in [
            "range_width_ratio",
            "pos_in_range",
            "micro_channel_z",
            "major_channel_z",
            "soft_micro_up_len",
            "soft_micro_down_len",
            "soft_micro_bias",
            "bar_body_frac",
            "bar_close_pos",
        ]:
            if col in df.columns:
                row[f"mean_{col}"] = float(sub[col].mean())
        rows.append(row)
    return pd.DataFrame(rows).sort_values("bin")


def main():
    path = "ml_export_es_new.csv"
    train_df, test_df, X_train, X_test = load_and_prepare(path)

    y_train = train_df["y"].values
    y_test = test_df["y"].values

    print("Train base up rate:", y_train.mean())
    print("Test base up rate :", y_test.mean())

    clf = HistGradientBoostingClassifier(
        loss="log_loss",
        max_depth=4,
        learning_rate=0.1,
        max_iter=200,
        max_leaf_nodes=31,
        l2_regularization=0.0,
        validation_fraction=0.1,
        n_iter_no_change=10,
        random_state=42,
    )
    clf.fit(X_train, y_train)

    proba_train = clf.predict_proba(X_train)[:, 1]
    proba_test = clf.predict_proba(X_test)[:, 1]

    pred_train = (proba_train >= 0.5).astype(int)
    pred_test = (proba_test >= 0.5).astype(int)

    print("\nAccuracy train/test:", accuracy_score(y_train, pred_train), accuracy_score(y_test, pred_test))
    print("AUC train/test      :", roc_auc_score(y_train, proba_train), roc_auc_score(y_test, proba_test))
    print("\nTest confusion matrix:")
    print(confusion_matrix(y_test, pred_test))

    # Confidence buckets on test
    print("\nTest probability deciles (HGB) and realized up-rates:")
    bucket_df = bucket_confidence(y_test, proba_test, test_df, n_bins=10)
    print(bucket_df.to_string(index=False))


if __name__ == "__main__":
    main()
