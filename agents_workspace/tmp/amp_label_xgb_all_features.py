import os
import numpy as np
import pandas as pd
from sklearn.metrics import accuracy_score
import xgboost as xgb

from amp_label_hgb_analysis import build_features


def main():
    # Use the full 3m amplitude export with 50bps/20 params.
    path = os.path.join("ml_exports", "ml_export_es_3m_amp_50_20_full.csv")
    ml = pd.read_csv(path, low_memory=False)

    # Keep all amplitude labels: up, down, flat.
    ml = ml[ml["label_amp"].isin(["up", "down", "flat"])].copy()
    label_map = {"down": 0, "flat": 1, "up": 2}
    ml["y"] = ml["label_amp"].map(label_map).astype(int)

    train_df = ml[ml["split"] == "train"].copy()
    test_df = ml[ml["split"] == "test"].copy()

    X_train_df, num_cols, train_cat_cols, num_means = build_features(train_df)
    X_test_df, _, test_cat_cols, _ = build_features(test_df, num_means=num_means)

    # Align one-hot columns between train and test.
    train_cat_set = set(train_cat_cols)
    test_cat_set = set(test_cat_cols)
    all_cat_cols = sorted(list(train_cat_set | test_cat_set))

    def align_onehots(X_df: pd.DataFrame) -> pd.DataFrame:
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

    def class_dist(y):
        vals, cnts = np.unique(y, return_counts=True)
        return {int(v): int(c) for v, c in zip(vals, cnts)}

    print("XGB feature shapes train/test:", X_train.shape, X_test.shape)
    print("Train class counts (down=0, flat=1, up=2):", class_dist(y_train))
    print("Test class counts  (down=0, flat=1, up=2):", class_dist(y_test))

    # Class-reweighting: make errors on down/up more expensive than flat.
    class_counts = np.bincount(y_train, minlength=3)
    # Use flat (class 1) as baseline; inverse-frequency style.
    baseline = class_counts[1]
    raw_w = baseline / class_counts.astype(float)
    # Normalize to mean 1 to keep effective learning rate sane.
    raw_w = raw_w / raw_w.mean()
    # Clip to avoid extreme weights.
    class_weights = np.clip(raw_w, 0.5, 3.0)
    sample_weight = class_weights[y_train]

    print("XGB class weights (down, flat, up):", class_weights.tolist())

    clf = xgb.XGBClassifier(
        n_estimators=300,
        max_depth=4,
        learning_rate=0.05,
        subsample=0.8,
        colsample_bytree=0.8,
        objective="multi:softprob",
        num_class=3,
        eval_metric="mlogloss",
        tree_method="hist",
        n_jobs=-1,
        random_state=42,
    )
    clf.fit(X_train, y_train, sample_weight=sample_weight)

    proba_train = clf.predict_proba(X_train)
    proba_test = clf.predict_proba(X_test)

    pred_train = np.argmax(proba_train, axis=1)
    pred_test = np.argmax(proba_test, axis=1)

    acc_train = accuracy_score(y_train, pred_train)
    acc_test = accuracy_score(y_test, pred_test)

    print("\nXGB 3-class amplitude label results (down/flat/up):")
    print("  Accuracy train/test:", acc_train, acc_test)

    # Per-class accuracy for diagnostics.
    for cls, name in [(0, "down"), (1, "flat"), (2, "up")]:
        mask_tr = y_train == cls
        mask_te = y_test == cls
        acc_tr_c = accuracy_score(y_train[mask_tr], pred_train[mask_tr]) if mask_tr.any() else float("nan")
        acc_te_c = accuracy_score(y_test[mask_te], pred_test[mask_te]) if mask_te.any() else float("nan")
        print(f"  Class {cls} ({name}) accuracy train/test: {acc_tr_c:.4f} {acc_te_c:.4f}")

    # --- Zero-cost intraday backtest on test split (RTH only), flatten at RTH close ---
    # Map predicted classes on test to trading states: down=-1, flat=0, up=+1.
    test_df = test_df.copy()
    test_df["pred_class"] = pred_test
    test_df["state_pred"] = test_df["pred_class"].map({0: -1, 1: 0, 2: 1}).astype(int)

    # Restrict to regular trading hours (09:30–16:15 ET).
    def minute_of_day(tstr: str) -> int:
        hh, mm = tstr.split(":")
        return int(hh) * 60 + int(mm)

    test_df["minute_of_day"] = test_df["time"].astype(str).str.slice(0, 5).apply(minute_of_day)
    rth_start = 9 * 60 + 30
    rth_end = 16 * 60 + 15
    test_rth = test_df[
        (test_df["minute_of_day"] >= rth_start) & (test_df["minute_of_day"] <= rth_end)
    ].copy()
    test_rth = test_rth.sort_values(["date", "time"]).reset_index(drop=True)

    if test_rth.empty:
        print("\n[Backtest] No RTH bars found; skipping trading simulation.")
        return

    close = pd.to_numeric(
        test_rth["close"].astype(str).str.replace("_", ""), errors="coerce"
    ).values.astype(float)
    dates = test_rth["date"].values
    states = test_rth["state_pred"].values

    trades_pnl = []
    trades_date = []

    current_date = None
    state = 0
    entry_px = None

    for px, desired, d in zip(close, states, dates):
        # New day: ensure we start flat.
        if current_date is None or d != current_date:
            current_date = d
            state = 0
            entry_px = None

        if not np.isfinite(px):
            continue

        if desired != state:
            # Close existing position at this bar's close.
            if state != 0 and entry_px is not None and np.isfinite(entry_px):
                pnl = (px - entry_px) * state
                trades_pnl.append(pnl)
                trades_date.append(d)
            # Open new position if desired is non-flat.
            if desired != 0:
                state = desired
                entry_px = px
            else:
                state = 0
                entry_px = None

    # Flatten at end of each RTH day: since we reset at day boundary above,
    # any open position will already have been closed on the last state change.
    # To be safe, close any residual open position at the very last bar.
    if state != 0 and entry_px is not None and np.isfinite(close[-1]) and np.isfinite(entry_px):
        pnl = (close[-1] - entry_px) * state
        trades_pnl.append(pnl)
        trades_date.append(dates[-1])

    trades_pnl = np.array(trades_pnl, dtype=float)
    print("\n[Backtest] Zero-cost XGB state trading (RTH, flatten EOD):")
    print("  trades_used:", len(trades_pnl))
    if len(trades_pnl) == 0:
        return
    mean_trade = float(trades_pnl.mean())
    std_trade = float(trades_pnl.std())
    print("  per-trade mean_pts", mean_trade, "std_pts", std_trade)

    pnl_df = pd.DataFrame({"date": trades_date, "pnl": trades_pnl})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = float(by_day.mean())
    std_daily = float(by_day.std())
    sharpe_daily = mean_daily / std_daily if std_daily > 0 else float("nan")
    sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else float("nan")
    print("  days_with_trades", len(by_day))
    print("  mean_daily_pts", mean_daily, "std_daily_pts", std_daily)
    print("  daily_sharpe", sharpe_daily, "annualized_sharpe", sharpe_annual)

    # Per-year daily Sharpe.
    pnl_df["year"] = pnl_df["date"].dt.year
    print("\n  Per-year Sharpe (XGB state, zero cost, RTH):")
    for year, grp in pnl_df.groupby("year"):
        by_day_y = grp.groupby("date")["pnl"].sum()
        m_y = float(by_day_y.mean())
        s_y = float(by_day_y.std())
        if s_y > 0:
            sd_y = m_y / s_y
            sa_y = sd_y * np.sqrt(252)
        else:
            sd_y = float("nan")
            sa_y = float("nan")
        print(
            f"    {year}: days={len(by_day_y)}, "
            f"mean_daily={m_y:.3f}, std_daily={s_y:.3f}, ann_sharpe={sa_y:.3f}"
        )


if __name__ == "__main__":
    main()
