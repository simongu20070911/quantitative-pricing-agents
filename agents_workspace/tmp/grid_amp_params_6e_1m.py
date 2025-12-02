import subprocess
import pandas as pd
import numpy as np
import torch
from torch import nn
from torch.utils.data import Dataset, DataLoader
from sklearn.metrics import roc_auc_score


class SeqDataset(Dataset):
    def __init__(self, X, y, seq_len: int):
        self.X = X
        self.y = y
        self.seq_len = seq_len
        self.valid_idx = np.arange(seq_len - 1, len(y))

    def __len__(self):
        return len(self.valid_idx)

    def __getitem__(self, i):
        idx = self.valid_idx[i]
        start = idx - self.seq_len + 1
        x = self.X[start : idx + 1]
        y = self.y[idx]
        return torch.from_numpy(x).float(), torch.tensor(y, dtype=torch.float32)


class LSTMReturns(nn.Module):
    def __init__(self, input_dim=1, hidden_dim=32):
        super().__init__()
        self.lstm = nn.LSTM(input_size=input_dim, hidden_size=hidden_dim, num_layers=1, batch_first=True)
        self.fc = nn.Linear(hidden_dim, 1)

    def forward(self, x):
        out, _ = self.lstm(x)
        last = out[:, -1, :]
        return self.fc(last).squeeze(-1)


def build_returns(df: pd.DataFrame, mu: float, sigma: float) -> np.ndarray:
    df = df.sort_values(["date", "time"]).reset_index(drop=True)
    close = pd.to_numeric(df["close"].astype(str).str.replace("_", ""), errors="coerce").values.astype(
        np.float64
    )
    ret = np.zeros_like(close, dtype=np.float32)
    for i in range(1, len(close)):
        if close[i - 1] > 0 and close[i] > 0:
            ret[i] = np.log(close[i] / close[i - 1])
        else:
            ret[i] = 0.0
    if sigma > 0:
        ret = (ret - mu) / sigma
    else:
        ret *= 0.0
    return ret.reshape(-1, 1)


def signflip_sharpe(seg_df: pd.DataFrame, proba: np.ndarray, seq_len: int, cost: float) -> float:
    if proba is None or len(proba) == 0:
        return float("nan")
    seg_valid = seg_df.sort_values(["date", "time"]).reset_index(drop=True).iloc[seq_len - 1 :].copy()
    seg_valid = seg_valid.iloc[: len(proba)].copy()
    seg_valid["p"] = proba
    seg_valid["state_pred"] = np.where(seg_valid["p"] >= 0.5, 1, -1)

    close = pd.to_numeric(
        seg_valid["close"].astype(str).str.replace("_", ""), errors="coerce"
    ).values.astype(np.float64)
    dates = seg_valid["date"].values
    states = seg_valid["state_pred"].values

    state = 0
    entry_px = None
    pnl_list = []
    pnl_dates = []
    for px, desired, d in zip(close, states, dates):
        if desired != state:
            if state != 0 and entry_px is not None and np.isfinite(px) and np.isfinite(entry_px):
                pnl = (px - entry_px) * state - cost
                pnl_list.append(pnl)
                pnl_dates.append(d)
            if np.isfinite(px):
                state = desired
                entry_px = px
            else:
                state = 0
                entry_px = None
    if state != 0 and entry_px is not None and np.isfinite(close[-1]) and np.isfinite(entry_px):
        pnl = (close[-1] - entry_px) * state - cost
        pnl_list.append(pnl)
        pnl_dates.append(dates[-1])

    if not pnl_list:
        return float("nan")
    pnl_df = pd.DataFrame({"date": pnl_dates, "pnl": pnl_list})
    pnl_df["date"] = pd.to_datetime(pnl_df["date"])
    by_day = pnl_df.groupby("date")["pnl"].sum()
    mean = float(by_day.mean())
    std = float(by_day.std())
    if std <= 0:
        return float("nan")
    return (mean / std) * np.sqrt(252.0)


def main():
    amps = [30.0, 50.0]
    tinactives = [10, 20, 40]
    seq_len = 120
    cost = 0.0  # for tuning, start without cost to see raw structure

    results = []
    for amp in amps:
        for tin in tinactives:
            print(f"\n=== minamp_bps={amp}, tinactive={tin} ===")
            # Regenerate 6E export with this labeler setting
            cmd = [
                "dune",
                "exec",
                "--root",
                ".",
                "bin/ml_export_main.exe",
                "--",
                "6e_clean.csv",
                "ml_export_6e_1m_amp.csv",
                "1m",
                str(amp),
                str(tin),
            ]
            subprocess.run(cmd, check=True)

            df = pd.read_csv("ml_export_6e_1m_amp.csv", low_memory=False)
            df = df[df["label_amp"].isin(["up", "down"])].copy()
            df["y"] = (df["label_amp"] == "up").astype(float)
            df["date_dt"] = pd.to_datetime(df["date"])

            train_mask = df["date_dt"] < pd.Timestamp("2021-01-01")
            val_mask = (df["date_dt"] >= pd.Timestamp("2021-01-01")) & (
                df["date_dt"] < pd.Timestamp("2023-01-01")
            )
            train_df = df[train_mask].copy()
            val_df = df[val_mask].copy()

            # Fit scaler on train returns
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
            y_train = train_df["y"].values.astype(np.float32)
            y_val = val_df["y"].values.astype(np.float32)

            train_ds = SeqDataset(X_train, y_train, seq_len)
            val_ds = SeqDataset(X_val, y_val, seq_len)

            device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
            model = LSTMReturns(input_dim=1, hidden_dim=32).to(device)
            crit = nn.BCEWithLogitsLoss()
            opt = torch.optim.Adam(model.parameters(), lr=1e-3)

            train_loader = DataLoader(train_ds, batch_size=256, shuffle=True)

            # 3 epochs for a bit more fit quality
            for epoch in range(3):
                model.train()
                tot = 0.0
                nb = 0
                for xb, yb in train_loader:
                    xb = xb.to(device)
                    yb = yb.to(device)
                    opt.zero_grad()
                    logits = model(xb)
                    loss = crit(logits, yb)
                    loss.backward()
                    opt.step()
                    tot += loss.item()
                    nb += 1
                print(f" epoch {epoch+1}, loss={tot/max(1,nb):.4f}")

            def eval_auc(ds, y_full):
                model.eval()
                logits_list = []
                with torch.no_grad():
                    for xb, _ in DataLoader(ds, batch_size=256, shuffle=False):
                        xb = xb.to(device)
                        lg = model(xb)
                        logits_list.append(lg.cpu().numpy())
                if not logits_list:
                    return float("nan"), None
                logits = np.concatenate(logits_list, axis=0)
                proba = 1.0 / (1.0 + np.exp(-logits))
                y_valid = y_full[seq_len - 1 :]
                proba_valid = proba[: len(y_valid)]
                try:
                    auc = roc_auc_score(y_valid, proba_valid)
                except ValueError:
                    auc = float("nan")
                return auc, proba_valid

            train_auc, proba_train = eval_auc(train_ds, y_train)
            val_auc, proba_val = eval_auc(val_ds, y_val)

            train_sharpe = signflip_sharpe(train_df, proba_train, seq_len, cost)
            val_sharpe = signflip_sharpe(val_df, proba_val, seq_len, cost)

            print(
                f"  train_auc={train_auc:.3f}, val_auc={val_auc:.3f}, "
                f"train_sharpe={train_sharpe:.3f}, val_sharpe={val_sharpe:.3f}"
            )
            results.append(
                {
                    "minamp_bps": amp,
                    "tinactive": tin,
                    "train_auc": train_auc,
                    "val_auc": val_auc,
                    "train_sharpe": train_sharpe,
                    "val_sharpe": val_sharpe,
                }
            )

    res_df = pd.DataFrame(results)
    print("\nGrid results:")
    print(res_df.sort_values("val_sharpe", ascending=False))
    res_df.to_csv("grid_amp_params_6e_1m_results.csv", index=False)


if __name__ == "__main__":
    main()
