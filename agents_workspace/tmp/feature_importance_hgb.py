import pandas as pd
import numpy as np
from sklearn.ensemble import HistGradientBoostingClassifier
from sklearn.inspection import permutation_importance


def load_train_test(path: str):
    df = pd.read_csv(path, low_memory=False)
    train_df = df[df["split"] == "train"].copy()
    test_df = df[df["split"] == "test"].copy()

    # keep only up/down labels
    train_df = train_df[train_df["label_small"].isin(["up", "down"])].copy()
    test_df = test_df[test_df["label_small"].isin(["up", "down"])].copy()

    train_df["y"] = (train_df["label_small"] == "up").astype(int)
    test_df["y"] = (test_df["label_small"] == "up").astype(int)

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

    cols_all_nan = [c for c in num_cols if train_df[c].isna().all()]
    num_cols = [c for c in num_cols if c not in cols_all_nan]

    # simple fill on train; apply same means to test
    for c in num_cols:
        m = train_df[c].mean()
        train_df[c] = train_df[c].fillna(m)
        test_df[c] = test_df[c].fillna(m)

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


def main():
    train_df, test_df, X_train, X_test = load_train_test("ml_export_es_new.csv")
    y_train = train_df["y"].values
    y_test = test_df["y"].values

    clf = HistGradientBoostingClassifier(
        loss="log_loss",
        max_depth=4,
        learning_rate=0.1,
        max_iter=200,
        max_leaf_nodes=31,
        validation_fraction=0.1,
        n_iter_no_change=10,
        random_state=42,
    )
    clf.fit(X_train, y_train)

    print("Train size:", X_train.shape, "Test size:", X_test.shape)

    # Permutation importance on test set using ROC-AUC as the score
    print("Computing permutation importance (this can take a bit)...")
    result = permutation_importance(
        clf,
        X_test,
        y_test,
        n_repeats=5,
        scoring="roc_auc",
        n_jobs=-1,
        random_state=42,
    )

    feature_names = list(X_test.columns)
    importances = result.importances_mean
    stds = result.importances_std

    rows = []
    for name, mean_imp, std_imp in zip(feature_names, importances, stds):
        rows.append((name, float(mean_imp), float(std_imp)))

    df_imp = (
        pd.DataFrame(rows, columns=["feature", "mean_importance", "std_importance"])
        .sort_values("mean_importance", ascending=False)
        .reset_index(drop=True)
    )

    print("\nTop 30 features by permutation importance (ROC-AUC drop):")
    print(df_imp.head(30).to_string(index=False))


if __name__ == "__main__":
    main()

