import pandas as pd
import numpy as np
from lightgbm import LGBMClassifier
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
    ]
    for c in num_cols:
        df[c] = pd.to_numeric(df[c], errors="coerce")
    cols_all_nan = [c for c in num_cols if df[c].isna().all()]
    num_cols = [c for c in num_cols if c not in cols_all_nan]

    if num_means is None:
        num_means = {c: df[c].mean() for c in num_cols}
    for c in num_cols:
        df[c] = df[c].fillna(num_means[c])

    cat_cols = ["leg_side", "bar_is_trend", "bar_is_doji", "always_in", "htf_leg_side_5m"]
    cat = pd.get_dummies(df[cat_cols], drop_first=False)

    X = pd.concat([df[num_cols].reset_index(drop=True), cat.reset_index(drop=True)], axis=1)
    return X, num_cols, list(cat.columns), num_means


def main():
    ml = pd.read_csv("ml_export_es_new.csv", low_memory=False)

    train_df = ml[ml["split"] == "train"].copy()
    test_df = ml[ml["split"] == "test"].copy()

    train_df = train_df[train_df["label_small"].isin(["up", "down"])].copy()
    test_df = test_df[test_df["label_small"].isin(["up", "down"])].copy()

    train_df["y"] = (train_df["label_small"] == "up").astype(int)
    test_df["y"] = (test_df["label_small"] == "up").astype(int)

    X_train_df, num_cols, train_cat_cols, num_means = build_features(train_df)
    X_test_df, _, test_cat_cols, _ = build_features(test_df, num_means=num_means)

    # align one-hot columns
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

    print("LightGBM feature shape train/test:", X_train.shape, X_test.shape)

    clf = LGBMClassifier(
        n_estimators=300,
        max_depth=-1,
        learning_rate=0.05,
        subsample=0.8,
        colsample_bytree=0.8,
        min_child_samples=200,
        objective="binary",
        n_jobs=-1,
        random_state=42,
    )
    clf.fit(X_train, y_train)

    proba_test = clf.predict_proba(X_test)[:, 1]
    auc = roc_auc_score(y_test, proba_test)
    print("LightGBM test AUC:", auc)

    # Always-in bracket Sharpe with zero cost
    COST = 0.0
    sign = np.where(proba_test >= 0.5, 1.0, -1.0)
    atr = pd.to_numeric(test_df["atr10"], errors="coerce").values
    label_up = (test_df["label_small"].values == "up")
    side_match = np.where(label_up, sign, -sign)
    pnl = side_match * atr - COST

    mask_valid = np.isfinite(atr) & (atr > 0)
    pnl = pnl[mask_valid]
    valid_dates = test_df["date"].values[mask_valid]

    print("LightGBM n_trades", len(pnl), "mean_pts", float(pnl.mean()), "std_pts", float(pnl.std()))

    pnl_df = pd.DataFrame({"date": valid_dates, "pnl": pnl})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = by_day.mean()
    std_daily = by_day.std()
    sharpe_daily = mean_daily / std_daily
    sharpe_annual = sharpe_daily * np.sqrt(252)

    print("LightGBM days_with_trades", len(by_day))
    print("LightGBM mean_daily_pts", float(mean_daily), "std_daily_pts", float(std_daily))
    print("LightGBM daily_sharpe", float(sharpe_daily), "annualized_sharpe", float(sharpe_annual))


if __name__ == "__main__":
    main()

