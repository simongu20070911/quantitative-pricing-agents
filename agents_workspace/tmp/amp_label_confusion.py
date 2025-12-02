import pandas as pd
import numpy as np
from sklearn.ensemble import HistGradientBoostingClassifier
from sklearn.metrics import roc_auc_score, confusion_matrix


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
        "day_net_change",
        "day_range",
        "day_pos_in_range",
        "day_trend_run_len",
        "dist_prev_day_high",
        "dist_prev_day_low",
        "range_mid",
        "range_mm_up",
        "range_mm_down",
        "soft_break_up_severity",
        "soft_break_down_severity",
        "recent_bull_count",
        "recent_bear_count",
        "recent_doji_count",
        "recent_body_sum",
    ]
    if num_means is not None:
        num_cols = list(num_means.keys())

    for c in num_cols:
        if c not in df.columns:
            df[c] = np.nan
        df[c] = pd.to_numeric(df[c], errors="coerce")
    cols_all_nan = [c for c in num_cols if df[c].isna().all()]
    if num_means is None:
        num_cols = [c for c in num_cols if c not in cols_all_nan]
        num_means = {c: df[c].mean() for c in num_cols}
    for c in num_cols:
        if c not in df.columns:
            df[c] = num_means.get(c, 0.0)
        df[c] = df[c].fillna(num_means[c])

    cat_cols = [
        "leg_side",
        "range_regime",
        "day_regime",
        "intraday_phase",
        "bar_is_trend",
        "bar_is_doji",
        "always_in",
        "htf_leg_side_5m",
        "range_tight",
        "micro_channel_overshoot",
        "major_channel_overshoot",
        "day_inside_prev_range",
        "soft_break_up_trend",
        "soft_break_down_trend",
    ]
    for c in cat_cols:
        if c not in df.columns:
            df[c] = ""
    cat = pd.get_dummies(df[cat_cols], drop_first=False)
    X = pd.concat([df[num_cols].reset_index(drop=True), cat.reset_index(drop=True)], axis=1)
    return X, num_cols, list(cat.columns), num_means


def main():
    ml = pd.read_csv("ml_export_es_new_3m.csv", low_memory=False)
    ml = ml[ml["label_amp"].isin(["up", "down"])].copy()
    ml["y"] = (ml["label_amp"] == "up").astype(int)

    train_df = ml[ml["split"] == "train"].copy().sort_values(["date", "time"]).reset_index(
        drop=True
    )
    test_df = ml[ml["split"] == "test"].copy().sort_values(["date", "time"]).reset_index(
        drop=True
    )

    X_train_df, num_cols, train_cat_cols, num_means = build_features(train_df)
    X_test_df, _, test_cat_cols, _ = build_features(test_df, num_means=num_means)

    train_cat_set = set(train_cat_cols)
    test_cat_set = set(test_cat_cols)
    all_cat_cols = sorted(list(train_cat_set | test_cat_set))

    def align_onehots(X_df):
        missing = [c for c in all_cat_cols if c not in X_df.columns]
        for c in missing:
            X_df[c] = 0.0
        cols = num_cols + all_cat_cols
        return X_df[cols]

    X_train_df = align_onehots(X_train_df)
    X_test_df = align_onehots(X_test_df)

    X_train = X_train_df.values.astype(np.float32)
    X_test = X_test_df.values.astype(np.float32)
    y_train = train_df["y"].values
    y_test = test_df["y"].values

    clf = HistGradientBoostingClassifier(
        loss="log_loss",
        max_depth=3,
        learning_rate=0.05,
        max_iter=100,
        max_leaf_nodes=16,
        l2_regularization=0.1,
        validation_fraction=0.1,
        n_iter_no_change=10,
        random_state=42,
    )
    clf.fit(X_train, y_train)

    p_test = clf.predict_proba(X_test)[:, 1]
    auc_test = roc_auc_score(y_test, p_test)
    print("HGB amplitude label test AUC:", auc_test)

    y_pred = (p_test >= 0.5).astype(int)
    cm = confusion_matrix(y_test, y_pred, labels=[0, 1])
    tn, fp, fn, tp = cm.ravel()
    print("Confusion matrix (down=0, up=1):")
    print(cm)
    acc = (tp + tn) / cm.sum()
    print(f"Overall accuracy: {acc:.4f}")
    print(f"TPR (recall up): {tp / (tp + fn):.4f}")
    print(f"TNR (recall down): {tn / (tn + fp):.4f}")

    # Persistence baseline: predict y_t = y_{t-1}
    y_test_seq = y_test
    same = y_test_seq[1:] == y_test_seq[:-1]
    baseline_acc = same.mean()  # accuracy if always predict previous label
    print(f"Persistence baseline accuracy (predict y_t=y_(t-1)): {baseline_acc:.4f}")

    # Accuracy conditional on no-switch vs switch
    # For bars 1..N-1, we compare y_pred vs y_test, and look at whether y_test changed from previous.
    y_true_sub = y_test_seq[1:]
    y_pred_sub = y_pred[1:]
    same_sub = same  # same length
    acc_noswitch = (y_pred_sub[same_sub] == y_true_sub[same_sub]).mean()
    acc_switch = (y_pred_sub[~same_sub] == y_true_sub[~same_sub]).mean()
    frac_switch = (~same_sub).mean()
    print(f"Fraction of switch bars: {frac_switch:.4f}")
    print(f"Accuracy on non-switch bars: {acc_noswitch:.4f}")
    print(f"Accuracy on switch bars: {acc_switch:.4f}")


if __name__ == "__main__":
    main()

