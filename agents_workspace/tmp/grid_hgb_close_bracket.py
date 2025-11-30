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


def sharpe_from_pnl(dates, pnls):
    pnl_df = pd.DataFrame({"date": dates, "pnl": pnls})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    if len(by_day) == 0:
        return np.nan, np.nan, 0
    mean_d = by_day.mean()
    std_d = by_day.std()
    if std_d <= 0:
        return np.nan, np.nan, len(by_day)
    sharpe_d = mean_d / std_d
    sharpe_a = sharpe_d * np.sqrt(252)
    return mean_d, sharpe_a, len(by_day)


def main():
    COST = 0.0

    ml = pd.read_csv("ml_export_es_new.csv", low_memory=False)
    ml = ml[ml["label_small"].isin(["up", "down"])].copy()

    train_df = ml[ml["split"] == "train"].copy()
    test_df = ml[ml["split"] == "test"].copy()

    train_df["y"] = (train_df["label_small"] == "up").astype(int)
    test_df["y"] = (test_df["label_small"] == "up").astype(int)

    X_train_df, num_cols, train_cat_cols, num_means = build_features(train_df)
    X_test_df, _, test_cat_cols, _ = build_features(test_df, num_means=num_means)

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

    atr_train = pd.to_numeric(train_df["atr10"], errors="coerce").values
    atr_test = pd.to_numeric(test_df["atr10"], errors="coerce").values

    configs = [
        {"name": "depth2_iter50", "max_depth": 2, "max_iter": 50, "max_leaf_nodes": 16, "lr": 0.05},
        {"name": "depth3_iter50", "max_depth": 3, "max_iter": 50, "max_leaf_nodes": 16, "lr": 0.05},
        {"name": "depth3_iter100", "max_depth": 3, "max_iter": 100, "max_leaf_nodes": 16, "lr": 0.05},
        {"name": "depth4_iter100", "max_depth": 4, "max_iter": 100, "max_leaf_nodes": 32, "lr": 0.05},
        {"name": "depth3_iter150", "max_depth": 3, "max_iter": 150, "max_leaf_nodes": 32, "lr": 0.05},
    ]

    results = []

    for cfg in configs:
        clf = HistGradientBoostingClassifier(
            loss="log_loss",
            max_depth=cfg["max_depth"],
            learning_rate=cfg["lr"],
            max_iter=cfg["max_iter"],
            max_leaf_nodes=cfg["max_leaf_nodes"],
            validation_fraction=0.1,
            n_iter_no_change=10,
            random_state=42,
        )
        clf.fit(X_train, y_train)

        proba_train = clf.predict_proba(X_train)[:, 1]
        proba_test = clf.predict_proba(X_test)[:, 1]

        auc_train = roc_auc_score(y_train, proba_train)
        auc_test = roc_auc_score(y_test, proba_test)

        # Close-entry bracket PnL: immediate entry at close, ±ATR around close.
        def compute_pnl(atr, labels, proba, dates):
            sign = np.where(proba >= 0.5, 1.0, -1.0)
            label_up = labels == 1
            side_match = np.where(label_up, sign, -sign)
            pnl = side_match * atr - COST
            mask = np.isfinite(atr) & (atr > 0)
            return pnl[mask], dates[mask]

        labels_train = y_train
        labels_test = y_test

        pnl_train, dates_train = compute_pnl(atr_train, labels_train, proba_train, train_df["date"].values)
        pnl_test, dates_test = compute_pnl(atr_test, labels_test, proba_test, test_df["date"].values)

        mean_tr, sharpe_tr, days_tr = sharpe_from_pnl(dates_train, pnl_train)
        mean_te, sharpe_te, days_te = sharpe_from_pnl(dates_test, pnl_test)

        results.append(
            {
                "name": cfg["name"],
                "max_depth": cfg["max_depth"],
                "max_iter": cfg["max_iter"],
                "max_leaf_nodes": cfg["max_leaf_nodes"],
                "lr": cfg["lr"],
                "auc_train": auc_train,
                "auc_test": auc_test,
                "mean_daily_train": mean_tr,
                "sharpe_train": sharpe_tr,
                "days_train": days_tr,
                "mean_daily_test": mean_te,
                "sharpe_test": sharpe_te,
                "days_test": days_te,
            }
        )

    print("name,max_depth,max_iter,max_leaf,lr,auc_tr,auc_te,sharpe_tr,sharpe_te,mean_tr,mean_te")
    for r in results:
        print(
            f"{r['name']},{r['max_depth']},{r['max_iter']},{r['max_leaf_nodes']},{r['lr']},"
            f"{r['auc_train']:.4f},{r['auc_test']:.4f},"
            f"{(r['sharpe_train'] if not np.isnan(r['sharpe_train']) else float('nan')):.3f},"
            f"{(r['sharpe_test'] if not np.isnan(r['sharpe_test']) else float('nan')):.3f},"
            f"{r['mean_daily_train']:.3f},{r['mean_daily_test']:.3f}"
        )


if __name__ == "__main__":
    main()

