import numpy as np
import torch
from torch import nn
from torch.utils.data import Dataset, DataLoader
from sklearn.metrics import roc_auc_score


def label_cumr(cumr: np.ndarray, minamp: float, tinactive: int) -> np.ndarray:
    """Python port of the amplitude-based labeler on cumulative returns."""
    n = len(cumr)
    if n == 0:
        return np.zeros(0, dtype=np.float32)
    labels = np.zeros(n, dtype=np.float64)

    # Pass 1: brute-force segmentation
    istart = 0
    icursor = 0
    imin = 0
    imax = 0
    vmin = cumr[0]
    vmax = cumr[0]
    while icursor < n:
        v = cumr[icursor]
        if (vmax - vmin) >= minamp and imin > imax and (v - vmin) >= minamp:
            labels[istart : imax] = 0.0
            labels[imax : imin + 1] = -1.0
            istart = imin
            imax = icursor
            vmax = v
        elif (vmax - vmin) >= minamp and imax > imin and (vmax - v) >= minamp:
            labels[istart : imin] = 0.0
            labels[imin : imax + 1] = 1.0
            istart = imax
            imin = icursor
            vmin = v
        elif imax > imin and (icursor - imax) >= tinactive and v <= vmax:
            if (vmax - vmin) >= minamp:
                labels[istart : imin] = 0.0
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
                labels[istart : imax] = 0.0
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
        labels[istart : imax] = 0.0
        labels[imax : imin + 1] = -1.0
        labels[imin + 1 : icursor] = 0.0
    elif (vmax - vmin) >= minamp and imax > imin:
        labels[istart : imin] = 0.0
        labels[imin : imax + 1] = 1.0
        labels[imax + 1 : icursor] = 0.0
    else:
        labels[istart:icursor] = 0.0

    # Pass 2: OLS filtering (simplified to keep consistency of main structure)
    ipos = 0
    while ipos < n:
        dir_ = labels[ipos]
        if dir_ == 0.0:
            ipos += 1
            continue
        istart = ipos
        iend = ipos
        while iend < n and labels[iend] == dir_:
            iend += 1
        iend -= 1

        imaxfwd = istart
        imaxback = iend
        vmaxfwd = 0.0
        vmaxback = 0.0

        fexy = fexx = fex = fey = 0.0
        for i in range(istart, iend + 1):
            xc = float(i - istart)
            yc = cumr[i]
            fexy += xc * yc
            fexx += xc * xc
            fex += xc
            fey += yc
            if xc > 0.0:
                denom = fexx - fex * fex / (xc + 1.0)
                if denom != 0.0:
                    beta = (fexy - fex * fey / (xc + 1.0)) / denom
                    distance = dir_ * beta * xc
                    if distance > vmaxfwd:
                        vmaxfwd = distance
                        imaxfwd = i

        bexy = bexx = bex = bey = 0.0
        for i in range(iend, istart - 1, -1):
            xc = float(iend - i)
            yc = cumr[i]
            bexy += xc * yc
            bexx += xc * xc
            bex += xc
            bey += yc
            if xc > 0.0:
                denom = bexx - bex * bex / (xc + 1.0)
                if denom != 0.0:
                    beta = (bexy - bex * bey / (xc + 1.0)) / denom
                    distance = -dir_ * beta * xc
                    if distance > vmaxback:
                        vmaxback = distance
                        imaxback = i

        if vmaxfwd < minamp and vmaxback < minamp:
            labels[istart : iend + 1] = 0.0
        else:
            if vmaxfwd >= minamp:
                labels[istart : imaxfwd + 1] = dir_
                labels[imaxfwd + 1 : imaxback] = 0.0
            else:
                labels[istart : imaxback + 1] = 0.0

            if vmaxback >= minamp:
                labels[imaxback : iend + 1] = dir_
            else:
                start0 = max(imaxback, imaxfwd + 1)
                labels[start0 : iend + 1] = 0.0

        ipos = iend + 1

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


def main():
    np.random.seed(42)
    torch.manual_seed(42)

    n_total = 100_000
    # Simulate Gaussian returns and build a random walk
    ret = np.random.normal(loc=0.0, scale=0.5 / 100.0, size=n_total)  # ~0.5% vol
    price = np.zeros(n_total, dtype=np.float64)
    price[0] = 100.0
    for i in range(1, n_total):
        price[i] = price[i - 1] * np.exp(ret[i])

    # Train/test split on the *realization*: we will only label the train segment.
    n_train = int(0.6 * n_total)
    price_train = price[:n_train]
    ret_train = ret[:n_train]
    price_test = price[n_train:]
    ret_test = ret[n_train:]

    # Label swings on train noise only (smaller amplitude threshold, 5 bps)
    minamp_bps = 5.0
    tinactive = 20
    labels_train_raw = label_prices(price_train, minamp_bps=minamp_bps, tinactive=tinactive)
    # Keep only up/down on train, drop 0 labels
    mask_train = labels_train_raw != 0.0
    price_train = price_train[mask_train]
    ret_train = ret_train[mask_train]
    labels_train_raw = labels_train_raw[mask_train]

    y_train = (labels_train_raw > 0).astype(np.float32)

    # Build standardized returns as features
    mu = ret_train.mean()
    sigma = ret_train.std()
    if sigma > 0:
        ret_train_std = (ret_train - mu) / sigma
        ret_test_std = (ret_test - mu) / sigma
    else:
        ret_train_std = ret_train * 0.0
        ret_test_std = ret_test * 0.0

    X_train = ret_train_std.reshape(-1, 1).astype(np.float32)
    X_test = ret_test_std.reshape(-1, 1).astype(np.float32)

    # Dummy labels for test (not used in training)
    y_test_dummy = np.zeros(len(ret_test), dtype=np.float32)

    seq_len = 30
    train_ds = SeqDataset(X_train, y_train, seq_len)
    test_ds = SeqDataset(X_test, y_test_dummy, seq_len)

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

    # Predictions on test; no labels, so no AUC here
    proba_valid = proba  # corresponds to indices from seq_len-1 onward

    # Sign-flip always-in strategy on test noise using LSTM predictions
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
    print("Noise LSTM signflip trades_used (train-labeled only)", len(trades_pnl))
    if len(trades_pnl) > 0:
        mean = float(trades_pnl.mean())
        std = float(trades_pnl.std())
        sharpe = mean / std if std > 0 else float("nan")
        print(
            "Noise LSTM signflip per-trade mean_pts",
            mean,
            "std_pts",
            std,
        )
        print("Noise LSTM signflip per-trade Sharpe", sharpe)


if __name__ == "__main__":
    main()
