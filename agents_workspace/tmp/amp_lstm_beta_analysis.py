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
        x = self.features[start:idx + 1]
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
        out, _ = self.lstm(x)
        last = out[:, -1, :]
        logits = self.fc(last).squeeze(-1)
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
    df = df.sort_values(["date", "time"]).reset_index(drop=True)
    df["y"] = (df["label_amp"] == "up").astype(float)

    # Fixed split: train <= 2018, test >= 2019 (same as earlier LSTM)
    year = df["date"].str[:4].astype(int)
    train_mask = year <= 2018
    test_mask = year >= 2019
    train_df = df[train_mask].copy()
    test_df = df[test_mask].copy()

    print(f"Train rows: {len(train_df)}, Test rows: {len(test_df)}")

    X_train = build_returns(train_df)
    X_test = build_returns(test_df)
    y_train = train_df["y"].values.astype(np.float32)
    y_test = test_df["y"].values.astype(np.float32)

    seq_len = 30
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    train_ds = SeqDataset(X_train, y_train, seq_len)
    train_loader = DataLoader(train_ds, batch_size=256, shuffle=True)

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
        print(f"Epoch {epoch+1}/{n_epochs}, train loss={total_loss / max(1, n_batches):.4f}")

    # Evaluate on test for AUC and build sign-flip PnL
    test_ds = SeqDataset(X_test, y_test, seq_len)
    test_loader = DataLoader(test_ds, batch_size=256, shuffle=False)
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
    print(f"LSTM(30 returns) test AUC: {auc:.6f}")

    # Sign-flip backtest on test with cost
    COST = 0.07 + 0.25
    test_df_valid = test_df.iloc[seq_len - 1 :].reset_index(drop=True)
    test_df_valid = test_df_valid.iloc[: len(proba_valid)].copy()
    test_df_valid["p_lstm"] = proba_valid
    test_df_valid["state_pred"] = np.where(test_df_valid["p_lstm"] >= 0.5, 1, -1)

    close = pd.to_numeric(
        test_df_valid["close"].astype(str).str.replace("_", ""), errors="coerce"
    ).values
    dates = pd.to_datetime(test_df_valid["date"].values)
    states = test_df_valid["state_pred"].values

    state = 0
    entry_px = None
    trades_pnl = []
    trades_date = []

    for j in range(len(test_df_valid)):
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
    pnl_df = pd.DataFrame({"date": trades_date, "pnl": trades_pnl})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()

    mean_daily = float(by_day.mean())
    std_daily = float(by_day.std())
    sharpe_daily = mean_daily / std_daily if std_daily > 0 else float("nan")
    sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else float("nan")
    print(
        "LSTM signflip days_with_trades",
        len(by_day),
        "mean_daily_pts",
        mean_daily,
        "std_daily_pts",
        std_daily,
    )
    print(
        "LSTM signflip daily_sharpe",
        sharpe_daily,
        "annualized_sharpe",
        sharpe_annual,
    )

    # Compute ES daily returns over same dates from test_df_valid
    # Use last close per date in test_df_valid
    close_series = pd.to_numeric(
        test_df_valid["close"].astype(str).str.replace("_", ""), errors="coerce"
    )
    df_closes = pd.DataFrame({"date": dates, "close": close_series})
    df_closes["date"] = pd.to_datetime(df_closes["date"])
    daily_close = df_closes.groupby("date")["close"].last().sort_index()
    es_ret = daily_close.pct_change().dropna()

    # Align PnL and ES returns
    common_dates = by_day.index.intersection(es_ret.index)
    pnl_aligned = by_day.loc[common_dates]
    ret_aligned = es_ret.loc[common_dates]

    corr = float(pnl_aligned.corr(ret_aligned))
    cov = float(np.cov(pnl_aligned.values, ret_aligned.values, ddof=1)[0, 1])
    var_ret = float(ret_aligned.var())
    beta = cov / var_ret if var_ret > 0 else float("nan")

    print("\nBeta analysis (test 2019+):")
    print(f"  Corr(strategy daily PnL, ES daily return): {corr:.3f}")
    print(f"  Beta (pts per 1.0 ES return unit): {beta:.3f}")


if __name__ == "__main__":
    main()

