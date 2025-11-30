import pandas as pd
import numpy as np
import torch
from torch import nn
from torch.utils.data import Dataset, DataLoader
from sklearn.metrics import roc_auc_score


class SeqDataset(Dataset):
    def __init__(self, features: np.ndarray, labels: np.ndarray, seq_len: int):
        self.features = features  # shape (N, D)
        self.labels = labels      # shape (N,)
        self.seq_len = seq_len
        self.valid_idx = np.arange(seq_len - 1, len(labels))

    def __len__(self):
        return len(self.valid_idx)

    def __getitem__(self, i):
        idx = self.valid_idx[i]
        start = idx - self.seq_len + 1
        x = self.features[start:idx + 1]  # (seq_len, D)
        y = self.labels[idx]
        return torch.from_numpy(x).float(), torch.tensor(y, dtype=torch.float32)


class LSTMFullContext(nn.Module):
    def __init__(self, input_dim: int, hidden_dim: int = 32, num_layers: int = 1):
        super().__init__()
        self.lstm = nn.LSTM(
            input_size=input_dim,
            hidden_size=hidden_dim,
            num_layers=num_layers,
            batch_first=True,
        )
        self.fc = nn.Linear(hidden_dim, 1)

    def forward(self, x):
        # x: (batch, seq_len, D)
        out, _ = self.lstm(x)
        last = out[:, -1, :]  # (batch, hidden_dim)
        logits = self.fc(last).squeeze(-1)
        return logits


def build_features(df: pd.DataFrame, num_means=None):
    """Build numeric + one-hot categorical features for a split."""
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

    df = df.copy()
    for c in num_cols:
        df[c] = pd.to_numeric(df[c], errors="coerce")

    # drop all-NaN numeric columns
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
    path = "ml_export_es_new.csv"
    df = pd.read_csv(path, low_memory=False)
    df = df[df["label_small"].isin(["up", "down"])].copy()

    train_df = df[df["split"] == "train"].copy().sort_values(["date", "time"]).reset_index(drop=True)
    test_df = df[df["split"] == "test"].copy().sort_values(["date", "time"]).reset_index(drop=True)

    # Build train features and record numeric means + cat columns
    X_train_df, num_cols, train_cat_cols, num_means = build_features(train_df)
    # Build test features using same means and cat columns
    X_test_df, _, test_cat_cols, _ = build_features(test_df, num_means=num_means)

    # Align one-hot columns
    train_cat_set = set(train_cat_cols)
    test_cat_set = set(test_cat_cols)
    all_cat_cols = sorted(list(train_cat_set | test_cat_set))

    def align_onehots(X_df, current_cat_cols):
        # X_df has numeric + its own cats; we need to ensure all_cat_cols exist
        missing = [c for c in all_cat_cols if c not in X_df.columns]
        for c in missing:
            X_df[c] = 0.0
        # numeric + all_cat_cols
        cols = num_cols + all_cat_cols
        return X_df[cols]

    X_train_df = align_onehots(X_train_df, train_cat_cols)
    X_test_df = align_onehots(X_test_df, test_cat_cols)

    # Convert to numpy and standardize
    X_train = X_train_df.values.astype(np.float32)
    X_test = X_test_df.values.astype(np.float32)
    y_train = (train_df["label_small"] == "up").astype(float).values
    y_test = (test_df["label_small"] == "up").astype(float).values

    mean_feat = X_train.mean(axis=0, keepdims=True)
    std_feat = X_train.std(axis=0, keepdims=True)
    std_feat[std_feat == 0.0] = 1.0

    X_train = (X_train - mean_feat) / std_feat
    X_test = (X_test - mean_feat) / std_feat

    seq_len = 20
    train_ds = SeqDataset(X_train, y_train, seq_len)
    test_ds = SeqDataset(X_test, y_test, seq_len)

    batch_size = 256
    train_loader = DataLoader(train_ds, batch_size=batch_size, shuffle=True)
    test_loader = DataLoader(test_ds, batch_size=batch_size, shuffle=False)

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    model = LSTMFullContext(input_dim=X_train.shape[1], hidden_dim=64, num_layers=1).to(device)
    criterion = nn.BCEWithLogitsLoss()
    optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)

    n_epochs = 2
    model.train()
    for epoch in range(n_epochs):
        total_loss = 0.0
        n_batches = 0
        for x_batch, y_batch in train_loader:
            x_batch = x_batch.to(device)
            y_batch = y_batch.to(device)
            optimizer.zero_grad()
            logits = model(x_batch)
            loss = criterion(logits, y_batch)
            loss.backward()
            optimizer.step()
            total_loss += loss.item()
            n_batches += 1
        avg_loss = total_loss / max(1, n_batches)
        print(f"Epoch {epoch+1}/{n_epochs}, train loss={avg_loss:.4f}")

    # Evaluate on test
    model.eval()
    all_logits = []
    with torch.no_grad():
        for x_batch, _ in test_loader:
            x_batch = x_batch.to(device)
            logits = model(x_batch)
            all_logits.append(logits.cpu().numpy())
    all_logits = np.concatenate(all_logits, axis=0)
    proba = 1 / (1 + np.exp(-all_logits))

    # Align with y_test indices
    y_test_valid = y_test[seq_len - 1 :]
    proba_valid = proba[: len(y_test_valid)]

    auc = roc_auc_score(y_test_valid, proba_valid)
    print(f"LSTM(full Brooks context, seq_len={seq_len}) test AUC: {auc:.6f}")

    # Always-in bracket Sharpe (zero cost) on test
    COST = 0.0
    test_df_valid = test_df.iloc[seq_len - 1 :].reset_index(drop=True)
    test_df_valid = test_df_valid.iloc[: len(proba_valid)].copy()
    test_df_valid["p_lstm"] = proba_valid

    sign = np.where(test_df_valid["p_lstm"].values >= 0.5, 1.0, -1.0)
    atr = pd.to_numeric(test_df_valid["atr10"], errors="coerce").values
    label_up = (test_df_valid["label_small"].values == "up")
    side_match = np.where(label_up, sign, -sign)
    pnl = side_match * atr - COST

    mask_valid = np.isfinite(atr) & (atr > 0)
    pnl = pnl[mask_valid]
    valid_dates = test_df_valid["date"].values[mask_valid]

    print("LSTM(full) n_trades", len(pnl), "mean_pts", float(pnl.mean()), "std_pts", float(pnl.std()))

    pnl_df = pd.DataFrame({"date": valid_dates, "pnl": pnl})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = by_day.mean()
    std_daily = by_day.std()
    sharpe_daily = mean_daily / std_daily
    sharpe_annual = sharpe_daily * np.sqrt(252)

    print("LSTM(full) days_with_trades", len(by_day))
    print("LSTM(full) mean_daily_pts", float(mean_daily), "std_daily_pts", float(std_daily))
    print("LSTM(full) daily_sharpe", float(sharpe_daily), "annualized_sharpe", float(sharpe_annual))


if __name__ == "__main__":
    main()

