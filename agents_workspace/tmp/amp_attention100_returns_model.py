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


class AttentionReturns(nn.Module):
    def __init__(self, input_dim: int = 1, d_model: int = 32):
        super().__init__()
        self.proj = nn.Linear(input_dim, d_model)
        self.scale = d_model ** 0.5
        self.out = nn.Linear(d_model, 1)

    def forward(self, x):
        # x: (batch, seq_len, 1)
        h = self.proj(x)  # (B, T, D)
        # Self-attention with Q=K=V=h
        Q = h
        K = h
        V = h
        attn_scores = torch.matmul(Q, K.transpose(1, 2)) / self.scale  # (B, T, T)
        attn_weights = torch.softmax(attn_scores, dim=-1)
        ctx = torch.matmul(attn_weights, V)  # (B, T, D)
        # Pool over time (mean)
        pooled = ctx.mean(dim=1)  # (B, D)
        logits = self.out(pooled).squeeze(-1)  # (B,)
        return logits


def build_returns(df: pd.DataFrame) -> np.ndarray:
    df = df.copy().sort_values(["date", "time"]).reset_index(drop=True)
    close = pd.to_numeric(df["close"].astype(str).str.replace("_", ""), errors="coerce")
    ret = np.zeros(len(close), dtype=np.float32)
    close_np = close.values.astype(np.float64)
    for i in range(1, len(close_np)):
        if close_np[i - 1] > 0 and close_np[i] > 0:
            ret[i] = np.log(close_np[i] / close_np[i - 1])
        else:
            ret[i] = 0.0
    # Standardize within segment
    mu = ret.mean()
    sigma = ret.std()
    if sigma > 0:
        ret = (ret - mu) / sigma
    else:
        ret = ret * 0.0
    return ret.reshape(-1, 1)


def main():
    path = "ml_export_es_new_3m.csv"
    df = pd.read_csv(path, low_memory=False)
    df = df[df["label_amp"].isin(["up", "down"])].copy()
    df["y"] = (df["label_amp"] == "up").astype(float)

    train_df = df[df["split"] == "train"].copy().sort_values(["date", "time"]).reset_index(
        drop=True
    )
    test_df = df[df["split"] == "test"].copy().sort_values(["date", "time"]).reset_index(
        drop=True
    )

    X_train = build_returns(train_df)
    X_test = build_returns(test_df)
    y_train = train_df["y"].values.astype(np.float32)
    y_test = test_df["y"].values.astype(np.float32)

    seq_len = 100
    train_ds = SeqDataset(X_train, y_train, seq_len)
    test_ds = SeqDataset(X_test, y_test, seq_len)

    batch_size = 256
    train_loader = DataLoader(train_ds, batch_size=batch_size, shuffle=True)
    test_loader = DataLoader(test_ds, batch_size=batch_size, shuffle=False)

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    model = AttentionReturns(input_dim=1, d_model=32).to(device)
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
        print(f"Epoch {epoch+1}/{n_epochs}, train loss={total_loss / max(1, n_batches):.4f}")

    # Evaluate AUC on test
    model.eval()
    all_logits = []
    with torch.no_grad():
        for x_batch, _ in test_loader:
            x_batch = x_batch.to(device)
            logits = model(x_batch)
            all_logits.append(logits.cpu().numpy())
    all_logits = np.concatenate(all_logits, axis=0)
    proba = 1.0 / (1.0 + np.exp(-all_logits))

    y_test_valid = y_test[seq_len - 1 :]
    proba_valid = proba[: len(y_test_valid)]
    auc = roc_auc_score(y_test_valid, proba_valid)
    print(f"Attention(100 returns) test AUC: {auc:.6f}")

    # Sign-flip always-in backtest with cost = 0.07 + 0.25
    COST = 0.07 + 0.25
    test_df_valid = test_df.iloc[seq_len - 1 :].reset_index(drop=True)
    test_df_valid = test_df_valid.iloc[: len(proba_valid)].copy()
    test_df_valid["p_attn"] = proba_valid
    test_df_valid["state_pred"] = np.where(test_df_valid["p_attn"] >= 0.5, 1, -1)

    close = pd.to_numeric(
        test_df_valid["close"].astype(str).str.replace("_", ""), errors="coerce"
    ).values
    dates = test_df_valid["date"].values
    states = test_df_valid["state_pred"].values

    n = len(test_df_valid)
    state = 0
    entry_px = None
    trades_pnl = []
    trades_date = []

    for j in range(n):
        d = dates[j]
        px = close[j]
        desired = states[j]
        if desired != state:
            if state != 0 and entry_px is not None and np.isfinite(px) and np.isfinite(entry_px):
                pnl = (px - entry_px) * state - COST
                trades_pnl.append(pnl)
                trades_date.append(d)
            if np.isfinite(px):
                state = desired
                entry_px = px
            else:
                state = 0
                entry_px = None

    if state != 0 and entry_px is not None and np.isfinite(close[-1]) and np.isfinite(entry_px):
        pnl = (close[-1] - entry_px) * state - COST
        trades_pnl.append(pnl)
        trades_date.append(dates[-1])

    trades_pnl = np.array(trades_pnl, dtype=float)
    print("Attention-100 signflip trades_used", len(trades_pnl))
    if len(trades_pnl) == 0:
        return
    mean_trade = float(trades_pnl.mean())
    std_trade = float(trades_pnl.std())
    print(
        "Attention-100 signflip per-trade mean_pts",
        mean_trade,
        "std_pts",
        std_trade,
    )

    pnl_df = pd.DataFrame({"date": trades_date, "pnl": trades_pnl})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = float(by_day.mean())
    std_daily = float(by_day.std())
    sharpe_daily = mean_daily / std_daily if std_daily > 0 else float("nan")
    sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else float("nan")
    print(
        "Attention-100 signflip days_with_trades",
        len(by_day),
        "mean_daily_pts",
        mean_daily,
        "std_daily_pts",
        std_daily,
    )
    print(
        "Attention-100 signflip daily_sharpe",
        sharpe_daily,
        "annualized_sharpe",
        sharpe_annual,
    )


if __name__ == "__main__":
    main()

