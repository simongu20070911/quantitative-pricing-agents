import numpy as np
import torch
from torch import nn
from torch.utils.data import Dataset, DataLoader
from sklearn.metrics import roc_auc_score


def label_cumr(cumr: np.ndarray, minamp: float, tinactive: int) -> np.ndarray:
    """Amplitude-based labeler on cumulative returns (simplified Python port)."""
    n = len(cumr)
    if n == 0:
        return np.zeros(0, dtype=np.float32)
    labels = np.zeros(n, dtype=np.float64)

    # Pass 1: segmentation
    istart = 0
    icursor = 0
    imin = 0
    imax = 0
    vmin = cumr[0]
    vmax = cumr[0]
    while icursor < n:
        v = cumr[icursor]
        if (vmax - vmin) >= minamp and imin > imax and (v - vmin) >= minamp:
            labels[istart:imax] = 0.0
            labels[imax : imin + 1] = -1.0
            istart = imin
            imax = icursor
            vmax = v
        elif (vmax - vmin) >= minamp and imax > imin and (vmax - v) >= minamp:
            labels[istart:imin] = 0.0
            labels[imin : imax + 1] = 1.0
            istart = imax
            imin = icursor
            vmin = v
        elif imax > imin and (icursor - imax) >= tinactive and v <= vmax:
            if (vmax - vmin) >= minamp:
                labels[istart:imin] = 0.0
                labels[imin : imax + 1] = 1.0
                labels[imax + 1 : icursor + 1] = 0.0
            else:
                labels[istart : icursor + 1] = 0.0
            istart = icursor
            imax = icursor
            imin = icursor
            vmax = v
            vmin = v
        elif imin > imax and (icursor - imin) >= tinactive and v >= vmin:
            if (vmax - vmin) >= minamp:
                labels[istart:imax] = 0.0
                labels[imax : imin + 1] = -1.0
                labels[imin + 1 : icursor + 1] = 0.0
            else:
                labels[istart : icursor + 1] = 0.0
            istart = icursor
            imax = icursor
            imin = icursor
            vmax = v
            vmin = v

        if v >= vmax:
            imax = icursor
            vmax = v
        if v <= vmin:
            imin = icursor
            vmin = v
        icursor += 1

    if (vmax - vmin) >= minamp and imin > imax:
        labels[istart:imax] = 0.0
        labels[imax : imin + 1] = -1.0
        labels[imin + 1 : icursor] = 0.0
    elif (vmax - vmin) >= minamp and imax > imin:
        labels[istart:imin] = 0.0
        labels[imin : imax + 1] = 1.0
        labels[imax + 1 : icursor] = 0.0
    else:
        labels[istart:icursor] = 0.0

    # Pass 2 (simplified): keep as-is; enough for train/test comparison
    return labels.astype(np.float32)


def label_prices(prices: np.ndarray, minamp_bps: float, tinactive: int) -> np.ndarray:
    n = len(prices)
    if n == 0:
        return np.zeros(0, dtype=np.float32)
    base = prices[0]
    cumr = np.zeros(n, dtype=np.float64)
    for i in range(n):
        p = prices[i]
        if p <= 0:
            p = base
        cumr[i] = np.log(p / base) * 1e4
    return label_cumr(cumr, minamp_bps, tinactive)


class SeqDataset(Dataset):
    def __init__(self, features: np.ndarray, labels: np.ndarray, seq_len: int):
        self.features = features
        self.labels = labels
        self.seq_len = seq_len
        self.valid_idx = np.arange(seq_len - 1, len(labels))

    def __len__(self):
        return len(self.valid_idx)

    def __getitem__(self, i):
        idx = self.valid_idx[i]
        start = idx - self.seq_len + 1
        x = self.features[start : idx + 1]
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


def simulate_noise_series(n_total: int, mu: float, sigma: float, seed: int):
    rng = np.random.default_rng(seed)
    rets = rng.normal(loc=mu, scale=sigma, size=n_total)
    price = np.zeros(n_total, dtype=np.float64)
    price[0] = 100.0
    for i in range(1, n_total):
        price[i] = price[i - 1] * np.exp(rets[i])
    return price, rets


def main():
    np.random.seed(123)
    torch.manual_seed(123)

    # Match ES 3m return scale roughly (reusing sigma from earlier noise script)
    mu = 0.0
    sigma = 0.5 / 100.0  # ~0.5% per bar as before
    n_total = 100_000

    # Simulate two independent noise series
    price_train, ret_train = simulate_noise_series(n_total, mu, sigma, seed=1)
    price_test, ret_test = simulate_noise_series(n_total, mu, sigma, seed=2)

    # Label swings on each series with same amplitude params
    # Smaller amplitude threshold for noise test (5 bps)
    minamp_bps = 5.0
    tinactive = 20
    labels_train_raw = label_prices(price_train, minamp_bps=minamp_bps, tinactive=tinactive)
    labels_test_raw = label_prices(price_test, minamp_bps=minamp_bps, tinactive=tinactive)

    # Keep only up/down, drop 0 labels
    mask_train = labels_train_raw != 0.0
    mask_test = labels_test_raw != 0.0
    price_train = price_train[mask_train]
    ret_train = ret_train[mask_train]
    labels_train_raw = labels_train_raw[mask_train]
    price_test = price_test[mask_test]
    ret_test = ret_test[mask_test]
    labels_test_raw = labels_test_raw[mask_test]

    y_train = (labels_train_raw > 0).astype(np.float32)
    y_test = (labels_test_raw > 0).astype(np.float32)

    # Standardize returns per series
    def build_X(ret):
        mu_r = ret.mean()
        sigma_r = ret.std()
        if sigma_r > 0:
            ret_std = (ret - mu_r) / sigma_r
        else:
            ret_std = ret * 0.0
        return ret_std.reshape(-1, 1).astype(np.float32)

    X_train = build_X(ret_train)
    X_test = build_X(ret_test)

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
        print(f"Epoch {epoch+1}/{n_epochs}, train loss={total_loss / max(1, n_batches):.4f}")

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
    print(f"Independent-noise LSTM(returns, amplitude labels) test AUC: {auc:.6f}")

    # Backtest sign-flip always-in strategy on independent test noise
    state_pred = np.where(proba_valid >= 0.5, 1, -1)
    price_valid = price_test[seq_len - 1 :]

    state = 0
    entry_px = None
    trades_pnl = []

    for j in range(len(price_valid)):
        px = price_valid[j]
        desired = state_pred[j]
        if desired != state:
            if state != 0 and entry_px is not None:
                pnl = (px - entry_px) * state
                trades_pnl.append(pnl)
            state = desired
            entry_px = px

    if state != 0 and entry_px is not None:
        pnl = (price_valid[-1] - entry_px) * state
        trades_pnl.append(pnl)

    trades_pnl = np.array(trades_pnl, dtype=float)
    print("Independent-noise LSTM signflip trades_used", len(trades_pnl))
    if len(trades_pnl) > 0:
        mean_trade = float(trades_pnl.mean())
        std_trade = float(trades_pnl.std())
        sharpe_trade = mean_trade / std_trade if std_trade > 0 else float("nan")
        print(
            "Independent-noise LSTM signflip per-trade mean_pts",
            mean_trade,
            "std_pts",
            std_trade,
        )
        print("Independent-noise LSTM signflip per-trade Sharpe", sharpe_trade)


if __name__ == "__main__":
    main()
