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


class LSTMReturns(nn.Module):
    def __init__(self, input_dim: int = 1, hidden_dim: int = 32, num_layers: int = 1):
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


def build_returns(df: pd.DataFrame) -> np.ndarray:
    """Build a 1D feature: log returns of close, per split segment."""
    df = df.copy().sort_values(["date", "time"]).reset_index(drop=True)
    close = pd.to_numeric(df["close"].astype(str).str.replace("_", ""), errors="coerce")
    # log returns; first return is 0
    ret = np.zeros(len(close), dtype=np.float32)
    close_np = close.values.astype(np.float64)
    for i in range(1, len(close_np)):
        if close_np[i - 1] > 0 and close_np[i] > 0:
            ret[i] = np.log(close_np[i] / close_np[i - 1])
        else:
            ret[i] = 0.0
    # standardize within segment
    mu = ret.mean()
    sigma = ret.std()
    if sigma > 0:
        ret = (ret - mu) / sigma
    else:
        ret = ret * 0.0
    # features shape (N,1)
    return ret.reshape(-1, 1)


def main():
    # Use full 3m amplitude export with 50bps/20 settings.
    path = "ml_exports/ml_export_es_3m_amp_50_20_full.csv"
    df = pd.read_csv(path, low_memory=False)

    # Binary labels: up=1, down=0
    df["y"] = (df["label_amp"] == "up").astype(float)

    # Train only on bars whose ex-post amplitude label is up/down,
    # but evaluate/trade on all test bars (including flat).
    train_df = (
        df[(df["split"] == "train") & (df["label_amp"].isin(["up", "down"]))]
        .copy()
        .sort_values(["date", "time"])
        .reset_index(drop=True)
    )
    test_df = (
        df[df["split"] == "test"]
        .copy()
        .sort_values(["date", "time"])
        .reset_index(drop=True)
    )

    # Build 1D return features separately for train and test to avoid leakage
    X_train = build_returns(train_df)
    X_test = build_returns(test_df)
    y_train = train_df["y"].values.astype(np.float32)
    y_test = test_df["y"].values.astype(np.float32)

    seq_len = 30
    train_ds = SeqDataset(X_train, y_train, seq_len)
    test_ds = SeqDataset(X_test, y_test, seq_len)

    batch_size = 256
    train_loader = DataLoader(train_ds, batch_size=batch_size, shuffle=True)
    test_loader = DataLoader(test_ds, batch_size=batch_size, shuffle=False)

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    model = LSTMReturns(input_dim=1, hidden_dim=32, num_layers=1).to(device)
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
    proba = 1.0 / (1.0 + np.exp(-all_logits))

    # Align with y_test indices
    y_test_valid = y_test[seq_len - 1 :]
    proba_valid = proba[: len(y_test_valid)]

    auc = roc_auc_score(y_test_valid, proba_valid)
    print(f"LSTM(returns only, seq_len={seq_len}) test AUC: {auc:.6f}")

    # Sign-flip swing backtest on test using LSTM predictions (3m bars, no cost)
    test_df_valid = test_df.iloc[seq_len - 1 :].reset_index(drop=True)
    test_df_valid = test_df_valid.iloc[: len(proba_valid)].copy()
    test_df_valid["p_lstm"] = proba_valid

    # Predicted state: +1 if p>=0.5 (up), -1 if p<0.5 (down)
    test_df_valid["state_pred"] = np.where(test_df_valid["p_lstm"] >= 0.5, 1, -1)

    # Use export close as 3m close
    # For this diagnostic run, assume zero per-trade cost.
    COST = 0.0
    close = pd.to_numeric(
        test_df_valid["close"].astype(str).str.replace("_", ""), errors="coerce"
    ).values
    dates = test_df_valid["date"].values
    states = test_df_valid["state_pred"].values

    n = len(test_df_valid)
    state = 0  # 0=flat, +1=long, -1=short
    entry_px = None

    trades_pnl = []
    trades_date = []

    for j in range(n):
        d = dates[j]
        px = close[j]
        desired = states[j]

        if desired != state:
            # Close existing
            if state != 0 and entry_px is not None and np.isfinite(px) and np.isfinite(entry_px):
                pnl = (px - entry_px) * state - COST
                trades_pnl.append(pnl)
                trades_date.append(d)
            # Open new (always in) if desired != 0
            if np.isfinite(px):
                state = desired
                entry_px = px
            else:
                state = 0
                entry_px = None

    # Close any open position at last bar
    if state != 0 and entry_px is not None and np.isfinite(close[-1]) and np.isfinite(entry_px):
        pnl = (close[-1] - entry_px) * state - COST
        trades_pnl.append(pnl)
        trades_date.append(dates[-1])

    trades_pnl = np.array(trades_pnl, dtype=float)
    print("LSTM signflip swing-model trades_used", len(trades_pnl))
    if len(trades_pnl) == 0:
        return
    # Trade-level statistics
    wins = np.sum(trades_pnl > 0.0)
    losses = np.sum(trades_pnl < 0.0)
    win_rate = wins / len(trades_pnl)
    print("LSTM signflip trade_win_rate", float(win_rate), "wins", int(wins), "losses", int(losses))
    print(
        "LSTM signflip per-trade mean_pts",
        float(trades_pnl.mean()),
        "std_pts",
        float(trades_pnl.std()),
    )

    pnl_df = pd.DataFrame({"date": trades_date, "pnl": trades_pnl})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = by_day.mean()
    std_daily = by_day.std()
    sharpe_daily = mean_daily / std_daily if std_daily > 0 else np.nan
    sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else np.nan
    print(
        "LSTM signflip days_with_trades",
        len(by_day),
        "mean_daily_pts",
        float(mean_daily),
        "std_daily_pts",
        float(std_daily),
    )
    print(
        "LSTM signflip daily_sharpe",
        float(sharpe_daily),
        "annualized_sharpe",
        float(sharpe_annual),
    )

    # Save equity curve (daily cumulative PnL) for plotting
    by_day_sorted = by_day.sort_index()
    equity = by_day_sorted.cumsum()
    equity.to_csv("lstm30_equity_curve.csv", header=["equity"])
    try:
        import matplotlib.pyplot as plt  # type: ignore

        plt.figure(figsize=(10, 4))
        equity.plot()
        plt.title("LSTM 30-returns sign-flip equity curve (2019–2025, cost=0.32)")
        plt.xlabel("Date")
        plt.ylabel("Cumulative PnL (pts)")
        plt.tight_layout()
        plt.savefig("lstm30_equity_curve.png")
        plt.close()
    except Exception as e:
        print("Could not generate PNG plot:", e)

    # Per-year Sharpe
    pnl_df["year"] = pnl_df["date"].dt.year
    print("\nPer-year Sharpe (LSTM signflip, cost=0.07):")
    for year, grp in pnl_df.groupby("year"):
        by_day_y = grp.groupby("date")["pnl"].sum()
        m = float(by_day_y.mean())
        s = float(by_day_y.std())
        if s > 0:
            sd = m / s
            sa = sd * np.sqrt(252)
        else:
            sd = float("nan")
            sa = float("nan")
        print(f"  {year}: days={len(by_day_y)}, mean_daily={m:.3f}, std_daily={s:.3f}, ann_sharpe={sa:.3f}")


if __name__ == "__main__":
    main()
