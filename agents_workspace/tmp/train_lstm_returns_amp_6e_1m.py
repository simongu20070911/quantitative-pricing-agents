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
        return self.fc(last).squeeze(-1)


def build_returns(df: pd.DataFrame, mu: float, sigma: float) -> np.ndarray:
    df = df.copy().sort_values(["date", "time"]).reset_index(drop=True)
    close = pd.to_numeric(df["close"].astype(str).str.replace("_", ""), errors="coerce")
    ret = np.zeros(len(close), dtype=np.float32)
    close_np = close.values.astype(np.float64)
    for i in range(1, len(close_np)):
        if close_np[i - 1] > 0 and close_np[i] > 0:
            ret[i] = np.log(close_np[i] / close_np[i - 1])
        else:
            ret[i] = 0.0
    if sigma > 0:
        ret = (ret - mu) / sigma
    else:
        ret = ret * 0.0
    return ret.reshape(-1, 1)


def main():
    path = "ml_export_6e_1m_amp.csv"
    df = pd.read_csv(path, low_memory=False)
    df = df[df["label_amp"].isin(["up", "down"])].copy()
    df["y"] = (df["label_amp"] == "up").astype(float)
    df["date_dt"] = pd.to_datetime(df["date"])

    # Time-based splits: train/val on 2010–2022, final holdout 2023+
    train_mask = df["date_dt"] < pd.Timestamp("2021-01-01")
    val_mask = (df["date_dt"] >= pd.Timestamp("2021-01-01")) & (
        df["date_dt"] < pd.Timestamp("2023-01-01")
    )
    test_mask = df["date_dt"] >= pd.Timestamp("2023-01-01")

    train_df = df[train_mask].copy().sort_values(["date", "time"]).reset_index(drop=True)
    val_df = df[val_mask].copy().sort_values(["date", "time"]).reset_index(drop=True)
    test_df = df[test_mask].copy().sort_values(["date", "time"]).reset_index(drop=True)

    # Compute returns on train to fit scaler
    close_train = pd.to_numeric(
        train_df["close"].astype(str).str.replace("_", ""), errors="coerce"
    ).values.astype(np.float64)
    ret_train = np.zeros_like(close_train, dtype=np.float32)
    for i in range(1, len(close_train)):
        if close_train[i - 1] > 0 and close_train[i] > 0:
            ret_train[i] = np.log(close_train[i] / close_train[i - 1])
        else:
            ret_train[i] = 0.0
    mu = float(ret_train.mean())
    sigma = float(ret_train.std())

    X_train = build_returns(train_df, mu, sigma)
    X_val = build_returns(val_df, mu, sigma)
    X_test = build_returns(test_df, mu, sigma)

    y_train = train_df["y"].values.astype(np.float32)
    y_val = val_df["y"].values.astype(np.float32)
    y_test = test_df["y"].values.astype(np.float32)

    seq_len = 120
    train_ds = SeqDataset(X_train, y_train, seq_len)
    val_ds = SeqDataset(X_val, y_val, seq_len)
    test_ds = SeqDataset(X_test, y_test, seq_len)

    batch_size = 256
    train_loader = DataLoader(train_ds, batch_size=batch_size, shuffle=True)
    val_loader = DataLoader(val_ds, batch_size=batch_size, shuffle=False)
    test_loader = DataLoader(test_ds, batch_size=batch_size, shuffle=False)

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    model = LSTMReturns(input_dim=1, hidden_dim=32, num_layers=1).to(device)
    criterion = nn.BCEWithLogitsLoss()
    optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)

    n_epochs = 3
    for epoch in range(n_epochs):
        model.train()
        total_loss = 0.0
        n_batches = 0
        for xb, yb in train_loader:
            xb = xb.to(device)
            yb = yb.to(device)
            optimizer.zero_grad()
            logits = model(xb)
            loss = criterion(logits, yb)
            loss.backward()
            optimizer.step()
            total_loss += loss.item()
            n_batches += 1
        avg_loss = total_loss / max(1, n_batches)

        # quick validation AUC each epoch
        model.eval()
        val_logits = []
        with torch.no_grad():
            for xb, _ in val_loader:
                xb = xb.to(device)
                lg = model(xb)
                val_logits.append(lg.cpu().numpy())
        if len(val_logits) > 0:
            val_logits = np.concatenate(val_logits, axis=0)
            val_proba = 1.0 / (1.0 + np.exp(-val_logits))
            y_val_valid = y_val[seq_len - 1 :]
            val_proba_valid = val_proba[: len(y_val_valid)]
            val_auc = roc_auc_score(y_val_valid, val_proba_valid)
        else:
            val_auc = float("nan")
        print(f"Epoch {epoch+1}/{n_epochs}, train loss={avg_loss:.4f}, val AUC={val_auc:.6f}")

    # Final AUC on train/val/test
    def eval_auc(ds, y_full, name):
        model.eval()
        logits_list = []
        with torch.no_grad():
            for xb, _ in DataLoader(ds, batch_size=256, shuffle=False):
                xb = xb.to(device)
                lg = model(xb)
                logits_list.append(lg.cpu().numpy())
        if not logits_list:
            print(f"{name} AUC: NA (no samples)")
            return None
        logits = np.concatenate(logits_list, axis=0)
        proba = 1.0 / (1.0 + np.exp(-logits))
        y_valid = y_full[seq_len - 1 :]
        proba_valid = proba[: len(y_valid)]
        auc = roc_auc_score(y_valid, proba_valid)
        print(f"{name} AUC: {auc:.6f}")
        return proba_valid

    proba_train = eval_auc(train_ds, y_train, "Train")
    proba_val = eval_auc(val_ds, y_val, "Val")
    proba_test = eval_auc(test_ds, y_test, "Test")

    def signflip_backtest(segment_df: pd.DataFrame, proba: np.ndarray, label: str):
        if proba is None:
            print(f"{label} signflip: NA (no probabilities)")
            return
        seg_valid = segment_df.iloc[seq_len - 1 :].reset_index(drop=True)
        seg_valid = seg_valid.iloc[: len(proba)].copy()
        seg_valid["p_lstm"] = proba
        seg_valid["state_pred"] = np.where(seg_valid["p_lstm"] >= 0.5, 1, -1)

        close = pd.to_numeric(
            seg_valid["close"].astype(str).str.replace("_", ""), errors="coerce"
        ).values
        dates = seg_valid["date"].values
        states = seg_valid["state_pred"].values

        COST = 0.07 + 0.25
        n = len(seg_valid)
        state = 0
        entry_px = None
        trades_pnl = []
        trades_date = []

        for j in range(n):
            d = dates[j]
            px = close[j]
            desired = states[j]
            if desired != state:
                if (
                    state != 0
                    and entry_px is not None
                    and np.isfinite(px)
                    and np.isfinite(entry_px)
                ):
                    pnl = (px - entry_px) * state - COST
                    trades_pnl.append(pnl)
                    trades_date.append(d)
                if np.isfinite(px):
                    state = desired
                    entry_px = px
                else:
                    state = 0
                    entry_px = None

        if (
            state != 0
            and entry_px is not None
            and np.isfinite(close[-1])
            and np.isfinite(entry_px)
        ):
            pnl = (close[-1] - entry_px) * state - COST
            trades_pnl.append(pnl)
            trades_date.append(dates[-1])

        trades_pnl = np.array(trades_pnl, dtype=float)
        print(f"6E {label} LSTM-120 signflip trades_used", len(trades_pnl))
        if len(trades_pnl) == 0:
            return
        mean_trade = float(trades_pnl.mean())
        std_trade = float(trades_pnl.std())
        print(
            f"6E {label} LSTM-120 per-trade mean_pts",
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
            f"6E {label} LSTM-120 days_with_trades",
            len(by_day),
            "mean_daily_pts",
            mean_daily,
            "std_daily_pts",
            std_daily,
        )
        print(
            f"6E {label} LSTM-120 daily_sharpe",
            sharpe_daily,
            "annualized_sharpe",
            sharpe_annual,
        )

    # Backtest on train/val/test with same signflip rule
    signflip_backtest(train_df, proba_train, "Train")
    signflip_backtest(val_df, proba_val, "Val")
    if proba_test is not None:
        signflip_backtest(test_df, proba_test, "Test")


if __name__ == "__main__":
    main()
