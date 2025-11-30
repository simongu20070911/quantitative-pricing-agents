import pandas as pd
import numpy as np
from sklearn.ensemble import HistGradientBoostingClassifier


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


def compute_stop_labels(df: pd.DataFrame, raw: pd.DataFrame, tick: float = 0.25, horizon: int = 10):
    df = df.copy()
    raw = raw.copy()
    raw["ET_datetime"] = pd.to_datetime(raw["ET_datetime"], utc=True)
    raw["date_str"] = raw["ET_datetime"].dt.strftime("%Y-%m-%d")
    raw["time_str"] = raw["ET_datetime"].dt.strftime("%H:%M")
    raw["minute_of_day"] = raw["ET_datetime"].dt.hour * 60 + raw["ET_datetime"].dt.minute
    rth_mask = (raw["minute_of_day"] >= 9 * 60 + 30) & (raw["minute_of_day"] <= 16 * 60 + 15)
    raw = raw[rth_mask].reset_index(drop=True)

    key_series = raw["date_str"] + " " + raw["time_str"]
    index_map = {k: i for i, k in enumerate(key_series)}

    high = raw["high"].values
    low = raw["low"].values
    close = raw["close"].values
    date_raw = raw["date_str"].values

    labels = []
    for _, row in df.iterrows():
        key = f"{row['date']} {row['time']}"
        i = index_map.get(key)
        if i is None:
            labels.append(None)
            continue
        atr = pd.to_numeric(row["atr10"], errors="coerce")
        if not np.isfinite(atr) or atr <= 0:
            labels.append(None)
            continue
        p0 = close[i]
        stop_price = p0 + tick
        n = len(close)
        last_idx = min(n - 1, i + horizon)
        entry_idx = None
        for j in range(i + 1, last_idx + 1):
            if date_raw[j] != date_raw[i]:
                break
            if high[j] >= stop_price:
                entry_idx = j
                break
        if entry_idx is None:
            labels.append(None)
            continue
        entry_price = stop_price
        up_level = entry_price + atr
        dn_level = entry_price - atr
        hit = None
        last_exit_idx = min(n - 1, entry_idx + horizon)
        for j in range(entry_idx + 1, last_exit_idx + 1):
            if date_raw[j] != date_raw[i]:
                break
            hit_up = high[j] >= up_level
            hit_dn = low[j] <= dn_level
            if hit_up and not hit_dn:
                hit = "up"
                break
            if hit_dn and not hit_up:
                hit = "down"
                break
            if hit_up and hit_dn:
                hit = None
                break
        labels.append(hit)
    return labels


def main():
    TICK = 0.25
    HORIZON = 10

    ml = pd.read_csv("ml_export_es_new.csv", low_memory=False)
    ml = ml[ml["label_small"].isin(["up", "down"])].copy()

    train_df = ml[ml["split"] == "train"].copy()
    test_df = ml[ml["split"] == "test"].copy()

    raw = pd.read_csv("../es.c.0-20100606-20251116.et.ohlcv-1m.csv")

    print("Computing stop labels (train)...")
    train_labels = compute_stop_labels(train_df, raw, tick=TICK, horizon=HORIZON)
    print("Computing stop labels (test)...")
    test_labels = compute_stop_labels(test_df, raw, tick=TICK, horizon=HORIZON)

    train_df["label_stop"] = train_labels
    test_df["label_stop"] = test_labels

    train_df = train_df[train_df["label_stop"].isin(["up", "down"])].copy()
    test_df = test_df[test_df["label_stop"].isin(["up", "down"])].copy()

    train_df["y"] = (train_df["label_stop"] == "up").astype(int)
    test_df["y"] = (test_df["label_stop"] == "up").astype(int)

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

    # Predict on all stop-labeled rows (train + test)
    train_df = train_df.sort_values(["date", "time"]).reset_index(drop=True)
    test_df = test_df.sort_values(["date", "time"]).reset_index(drop=True)

    X_train_sorted = X_train_df.loc[train_df.index].values.astype(np.float32)
    X_test_sorted = X_test_df.loc[test_df.index].values.astype(np.float32)

    proba_train = clf.predict_proba(X_train_sorted)[:, 1]
    proba_test = clf.predict_proba(X_test_sorted)[:, 1]

    preds = pd.concat(
        [
            pd.DataFrame(
                {
                    "date": train_df["date"],
                    "time": train_df["time"],
                    "split": "train",
                    "label_stop": train_df["label_stop"],
                    "y": train_df["y"],
                    "p_stop": proba_train,
                }
            ),
            pd.DataFrame(
                {
                    "date": test_df["date"],
                    "time": test_df["time"],
                    "split": "test",
                    "label_stop": test_df["label_stop"],
                    "y": test_df["y"],
                    "p_stop": proba_test,
                }
            ),
        ],
        ignore_index=True,
    )

    preds.to_csv("stop_predictions_hgb.csv", index=False)
    print("Wrote stop_predictions_hgb.csv with", len(preds), "rows")


if __name__ == "__main__":
    main()
