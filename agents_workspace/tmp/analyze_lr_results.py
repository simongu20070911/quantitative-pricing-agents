import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LogisticRegression


def load_and_prepare(path: str):
    df = pd.read_csv(path, low_memory=False)
    # Keep only up/down labels
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

    # Drop almost-empty numeric columns (meant to be optional magnets)
    cols_to_drop = [c for c in num_cols if train_df[c].isna().mean() > 0.99]
    num_cols = [c for c in num_cols if c not in cols_to_drop]

    for c in num_cols:
        mean_val = train_df[c].mean()
        train_df[c] = train_df[c].fillna(mean_val)
        test_df[c] = test_df[c].fillna(mean_val)

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

    return train_df, test_df, X_train, X_test, num_cols, list(train_cat.columns)


def fit_lr(X_train, y_train):
    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)
    clf = LogisticRegression(max_iter=1000, n_jobs=-1)
    clf.fit(X_train_scaled, y_train)
    return clf, scaler


def feature_importance(clf, feature_names, top_k=30):
    coef = clf.coef_[0]
    data = []
    for name, w in zip(feature_names, coef):
        data.append((name, float(w), float(abs(w))))
    df_coef = pd.DataFrame(data, columns=["feature", "coef", "abs_coef"])
    df_coef = df_coef.sort_values("abs_coef", ascending=False)
    return df_coef.head(top_k)


def bucket_confidence(y_true, proba, extra_df, n_bins=10):
    """
    Group by deciles of predicted probability and summarize realized hit rate
    and a few context features.
    """
    df = pd.DataFrame(
        {
            "y": y_true,
            "p": proba,
        }
    )
    # attach a few key context fields
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
        avg_p = sub["p"].mean()
        row = {
            "bin": int(b),
            "n": int(n),
            "avg_p": float(avg_p),
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


def high_confidence_slices(test_df, proba, thresh_hi=0.6, thresh_lo=0.4):
    df = test_df.copy()
    df["p"] = proba
    hi = df[df["p"] >= thresh_hi]
    lo = df[df["p"] <= thresh_lo]
    return hi, lo


def summarize_slice(name, slice_df):
    if slice_df.empty:
        print(f"{name}: empty")
        return
    print(f"\n{name}: n={len(slice_df)}, up_rate={slice_df['y'].mean():.4f}")
    # Discrete context distributions
    for col in ["leg_side", "always_in", "bar_is_trend", "bar_is_doji", "htf_leg_side_5m"]:
        if col in slice_df.columns:
            vc = slice_df[col].value_counts(normalize=True).head(5)
            print(f"  {col} dist (top 5):")
            for k, v in vc.items():
                print(f"    {k}: {v:.3f}")
    # Key numeric means
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
        if col in slice_df.columns:
            print(f"  mean {col}: {slice_df[col].mean():.3f}")


def main():
    path = "ml_export_es_new.csv"
    train_df, test_df, X_train, X_test, num_cols, cat_cols = load_and_prepare(path)
    feature_names = list(X_train.columns)

    clf, scaler = fit_lr(X_train, train_df["y"].values)

    X_train_scaled = scaler.transform(X_train)
    X_test_scaled = scaler.transform(X_test)

    proba_train = clf.predict_proba(X_train_scaled)[:, 1]
    proba_test = clf.predict_proba(X_test_scaled)[:, 1]

    print("Train base up rate:", train_df["y"].mean())
    print("Test base up rate :", test_df["y"].mean())

    # 1) Feature importance
    print("\nTop 30 features by |coef|:")
    top_feats = feature_importance(clf, feature_names, top_k=30)
    print(top_feats.to_string(index=False))

    # 2) Probability deciles on test
    print("\nTest probability deciles and realized up-rates:")
    bucket_df = bucket_confidence(
        test_df["y"].values,
        proba_test,
        test_df,
        n_bins=10,
    )
    print(bucket_df.to_string(index=False))

    # 3) Slices where model is relatively confident
    hi, lo = high_confidence_slices(test_df, proba_test, thresh_hi=0.6, thresh_lo=0.4)
    summarize_slice("High-probability up slice (p >= 0.6)", hi)
    summarize_slice("Low-probability up slice (p <= 0.4)", lo)


if __name__ == "__main__":
    main()

