import os
import pandas as pd
import numpy as np


def main():
    path = "ml_exports/ml_export_es_1m_amp_30_20.csv"
    df = pd.read_csv(path, low_memory=False)
    # Keep all labels so we can see flats as well.
    df = df[df["label_amp"].isin(["up", "down", "flat"])].copy()

    # Build timestamp and sort
    df["ts"] = pd.to_datetime(df["date"] + " " + df["time"])
    df = df.sort_values("ts").reset_index(drop=True)

    # Take first 30 RTH dates
    unique_dates = df["date"].drop_duplicates().sort_values().tolist()
    sel_dates = unique_dates[:30]

    out_dir = "plots_py/es_1m_labels_rth"
    os.makedirs(out_dir, exist_ok=True)

    try:
        import matplotlib.pyplot as plt  # type: ignore
        import matplotlib.dates as mdates  # type: ignore
    except Exception as e:
        print("matplotlib not available:", e)
        return

    for d in sel_dates:
        day_df = df[df["date"] == d].copy()
        if day_df.empty:
            continue

        day_df = day_df.sort_values("ts").reset_index(drop=True)

        times = day_df["ts"]
        close = pd.to_numeric(
            day_df["close"].astype(str).str.replace("_", ""), errors="coerce"
        ).values.astype(float)

        labs = day_df["label_amp"].values
        colors = np.where(
            labs == "up",
            "tab:green",
            np.where(labs == "down", "tab:red", "0.6"),
        )

        fig, ax = plt.subplots(figsize=(11, 4))
        ax.plot(times, close, color="black", linewidth=0.8, alpha=0.8)
        ax.scatter(times, close, c=colors, s=10, alpha=0.9)

        ax.set_ylabel("Close")
        ax.set_xlabel("Time (ET)")
        ax.grid(True, alpha=0.3)
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%H:%M"))

        fig.suptitle(f"ES 1m amplitude labels (30bps/20, RTH) – {d}")
        fig.tight_layout(rect=[0, 0, 1, 0.95])

        out_path = os.path.join(out_dir, f"es_1m_labels_rth_{d}.png")
        fig.savefig(out_path, dpi=150)
        plt.close(fig)
        print("saved", out_path)


if __name__ == "__main__":
    main()

