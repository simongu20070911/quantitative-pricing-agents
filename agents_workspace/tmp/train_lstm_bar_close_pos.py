import pandas as pd
import numpy as np
import torch
from torch import nn
from torch.utils.data import Dataset, DataLoader
from sklearn.metrics import roc_auc_score


class SeqDataset(Dataset):
    def __init__(self, features: np.ndarray, labels: np.ndarray, seq_len: int):
        self.features = features  # shape (N, 1)
        self.labels = labels      # shape (N,)
        self.seq_len = seq_len
        self.valid_idx = np.arange(seq_len - 1, len(labels))

    def __len__(self):
        return len(self.valid_idx)

    def __getitem__(self, i):
        idx = self.valid_idx[i]
        start = idx - self.seq_len + 1
        x = self.features[start:idx + 1]  # (seq_len, 1)
        y = self.labels[idx]
        return torch.from_numpy(x).float(), torch.tensor(y, dtype=torch.float32)


class LSTMBarClosePos(nn.Module):
    def __init__(self, hidden_dim: int = 16, num_layers: int = 1):
        super().__init__()
        self.lstm = nn.LSTM(
            input_size=1,
            hidden_size=hidden_dim,
            num_layers=num_layers,
            batch_first=True,
        )
        self.fc = nn.Linear(hidden_dim, 1)

    def forward(self, x):
        # x: (batch, seq_len, 1)
        out, _ = self.lstm(x)
        last = out[:, -1, :]  # (batch, hidden_dim)
        logits = self.fc(last).squeeze(-1)  # (batch,)
        return logits


def prepare_split(df: pd.DataFrame, seq_len: int):
    df = df.sort_values(["date", "time"]).reset_index(drop=True)
    # single feature: bar_close_pos
    f = pd.to_numeric(df["bar_close_pos"], errors="coerce")
    mean_f = f.mean()
    f = f.fillna(mean_f).values.reshape(-1, 1)
    y = (df["label_small"] == "up").astype(float).values
    return f, y


def main():
    path = "ml_export_es_new.csv"
    df = pd.read_csv(path, low_memory=False)
    df = df[df["label_small"].isin(["up", "down"])].copy()

    train_df = df[df["split"] == "train"].copy()
    test_df = df[df["split"] == "test"].copy()

    seq_len = 20

    X_train_np, y_train_np = prepare_split(train_df, seq_len)
    X_test_np, y_test_np = prepare_split(test_df, seq_len)

    train_ds = SeqDataset(X_train_np, y_train_np, seq_len)
    test_ds = SeqDataset(X_test_np, y_test_np, seq_len)

    batch_size = 256
    train_loader = DataLoader(train_ds, batch_size=batch_size, shuffle=True)
    test_loader = DataLoader(test_ds, batch_size=batch_size, shuffle=False)

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    model = LSTMBarClosePos(hidden_dim=16, num_layers=1).to(device)
    criterion = nn.BCEWithLogitsLoss()
    optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)

    n_epochs = 3
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

    # Note: y_test_np aligned with full test series; we only have predictions for indices >= seq_len-1
    y_test_valid = y_test_np[seq_len - 1 :]
    proba_valid = proba[: len(y_test_valid)]

    auc = roc_auc_score(y_test_valid, proba_valid)
    print(f"LSTM(bar_close_pos seq, seq_len={seq_len}) test AUC: {auc:.6f}")

    # Compute always-in bracket Sharpe on test (zero cost, same ±atr10 label)
    COST = 0.0
    # attach predictions back to sorted test_df
    test_df_sorted = test_df.sort_values(["date", "time"]).reset_index(drop=True)
    # trim to valid range (seq_len-1 .. end)
    test_df_valid = test_df_sorted.iloc[seq_len - 1 :].reset_index(drop=True)
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

    print("LSTM n_trades", len(pnl), "mean_pts", float(pnl.mean()), "std_pts", float(pnl.std()))

    pnl_df = pd.DataFrame({"date": valid_dates, "pnl": pnl})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = by_day.mean()
    std_daily = by_day.std()
    sharpe_daily = mean_daily / std_daily
    sharpe_annual = sharpe_daily * np.sqrt(252)

    print("LSTM days_with_trades", len(by_day))
    print("LSTM mean_daily_pts", float(mean_daily), "std_daily_pts", float(std_daily))
    print("LSTM daily_sharpe", float(sharpe_daily), "annualized_sharpe", float(sharpe_annual))


if __name__ == "__main__":
    main()
