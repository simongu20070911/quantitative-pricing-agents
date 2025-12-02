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


def train_one_lstm(X_train, y_train, seq_len: int, n_epochs: int = 2) -> LSTMReturns:
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    train_ds = SeqDataset(X_train, y_train, seq_len)
    train_loader = DataLoader(train_ds, batch_size=256, shuffle=True)

    model = LSTMReturns(input_dim=1, hidden_dim=32, num_layers=1).to(device)
    criterion = nn.BCEWithLogitsLoss()
    optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)

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
        print(f"  Epoch {epoch+1}/{n_epochs}, train loss={avg_loss:.4f}")
    return model


def eval_year(model, X_test, y_test, df_test, seq_len: int, cost: float):
    device = next(model.parameters()).device
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

    # sign-flip backtest for that year
    df_valid = df_test.iloc[seq_len - 1 :].reset_index(drop=True)
    df_valid = df_valid.iloc[: len(proba_valid)].copy()
    df_valid["p_lstm"] = proba_valid
    df_valid["state_pred"] = np.where(df_valid["p_lstm"] >= 0.5, 1, -1)

    close = pd.to_numeric(
        df_valid["close"].astype(str).str.replace("_", ""), errors="coerce"
    ).values
    dates = df_valid["date"].values
    states = df_valid["state_pred"].values

    state = 0
    entry_px = None
    trades_pnl = []
    trades_date = []

    for j in range(len(df_valid)):
        d = dates[j]
        px = close[j]
        desired = states[j]
        if desired != state:
            if state != 0 and entry_px is not None and np.isfinite(px) and np.isfinite(entry_px):
                pnl = (px - entry_px) * state - cost
                trades_pnl.append(pnl)
                trades_date.append(d)
            if np.isfinite(px):
                state = desired
                entry_px = px
            else:
                state = 0
                entry_px = None

    if state != 0 and entry_px is not None and np.isfinite(close[-1]) and np.isfinite(entry_px):
        pnl = (close[-1] - entry_px) * state - cost
        trades_pnl.append(pnl)
        trades_date.append(dates[-1])

    trades_pnl = np.array(trades_pnl, dtype=float)
    pnl_df = pd.DataFrame({"date": trades_date, "pnl": trades_pnl})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean_daily = float(by_day.mean()) if len(by_day) > 0 else 0.0
    std_daily = float(by_day.std()) if len(by_day) > 0 else 0.0
    sharpe_daily = mean_daily / std_daily if std_daily > 0 else float("nan")
    sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else float("nan")

    return {
        "auc": float(auc),
        "trades": int(len(trades_pnl)),
        "mean_daily": mean_daily,
        "std_daily": std_daily,
        "sharpe_annual": sharpe_annual,
        "by_day": by_day,
    }


def main():
    path = "ml_export_es_new_3m.csv"
    df = pd.read_csv(path, low_memory=False)
    df = df[df["label_amp"].isin(["up", "down"])].copy()
    df = df.sort_values(["date", "time"]).reset_index(drop=True)
    df["y"] = (df["label_amp"] == "up").astype(float)

    seq_len = 30
    cost = 0.07 + 0.25

    years = sorted(df["date"].unique())
    years_int = sorted({int(str(d).split("-")[0]) for d in df["date"].unique()})

    results = []
    all_by_day = []

    for test_year in range(2017, 2026):  # 2017..2025
        train_mask = df["date"].str[:4].astype(int) < test_year
        test_mask = df["date"].str[:4].astype(int) == test_year
        if not test_mask.any() or train_mask.sum() < 5000:
            continue
        train_df = df[train_mask].copy()
        test_df = df[test_mask].copy()

        print(f"\n=== Walk-forward: train <= {test_year-1}, test = {test_year} ===")

        X_train = build_returns(train_df)
        X_test = build_returns(test_df)
        y_train = train_df["y"].values.astype(np.float32)
        y_test = test_df["y"].values.astype(np.float32)

        model = train_one_lstm(X_train, y_train, seq_len=seq_len, n_epochs=2)
        res = eval_year(model, X_test, y_test, test_df, seq_len=seq_len, cost=cost)
        print(
            f"  Year {test_year}: AUC={res['auc']:.3f}, trades={res['trades']}, "
            f"ann_sharpe={res['sharpe_annual']:.3f}"
        )
        res["year"] = test_year
        results.append(res)
        all_by_day.append(res["by_day"])

    if not results:
        print("No walk-forward results.")
        return

    # Aggregate across all test years
    all_by_day_series = pd.concat(all_by_day)
    all_by_day_agg = all_by_day_series.groupby(all_by_day_series.index).sum()
    mean_daily = float(all_by_day_agg.mean())
    std_daily = float(all_by_day_agg.std())
    sharpe_daily = mean_daily / std_daily if std_daily > 0 else float("nan")
    sharpe_annual = sharpe_daily * np.sqrt(252) if std_daily > 0 else float("nan")
    print(
        f"\nWalk-forward overall: mean_daily={mean_daily:.3f}, std_daily={std_daily:.3f}, "
        f"ann_sharpe={sharpe_annual:.3f}"
    )


if __name__ == "__main__":
    main()

