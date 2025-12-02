import os
import pandas as pd
import numpy as np


def main():
    path = "ml_exports/ml_export_6e_1m_amp_20_40.csv"
    df = pd.read_csv(path, low_memory=False)
    # Keep all labels (up / down / flat) so we can see neutral regions too.

    # Build timestamp for sorting / filtering
    df["ts"] = pd.to_datetime(df["date"] + " " + df["time"])
    df = df.sort_values("ts").reset_index(drop=True)

    # Choose a 30-day window from the early part of the sample (just for inspection).
    unique_dates = df["date"].drop_duplicates().sort_values().tolist()
    sel_dates = unique_dates[:30]

    out_dir = "plots_py/6e_1m_labels_3h"
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

        # minute-of-day for 1-minute bars
        day_df["mod"] = day_df["time"].str.slice(0, 2).astype(int) * 60 + day_df[
            "time"
        ].str.slice(3, 5).astype(int)

        # Build 3 contiguous time windows that partition the day's data range,
        # so that we do not end up with empty middle panels.
        mod_min = int(day_df["mod"].min())
        mod_max = int(day_df["mod"].max())
        if mod_max <= mod_min:
            edges = [mod_min, mod_min, mod_min, mod_min]
        else:
            edges = np.linspace(mod_min, mod_max, 4).astype(int).tolist()
        windows = [(edges[i], edges[i + 1]) for i in range(3)]

        # Two 12-hour windows over the full 24h day: [00:00,12:00), [12:00,24:00).
        windows = [(0, 12 * 60), (12 * 60, 24 * 60)]

        fig, axes = plt.subplots(len(windows), 1, figsize=(11, 7), sharex=False)
        if len(windows) == 1:
            axes = [axes]

        for ax, (start_min, end_min) in zip(axes, windows):
            seg = day_df[(day_df["mod"] >= start_min) & (day_df["mod"] < end_min)].copy()

            if seg.empty:
                ax.set_visible(False)
                continue

            times = seg["ts"]
            close = pd.to_numeric(
                seg["close"].astype(str).str.replace("_", ""), errors="coerce"
            ).values.astype(float)

            # Color map: up=green, down=red, flat=grey
            labs = seg["label_amp"].values
            colors = np.where(
                labs == "up",
                "tab:green",
                np.where(labs == "down", "tab:red", "0.6"),
            )

            ax.plot(times, close, color="black", linewidth=0.8, alpha=0.8)
            ax.scatter(times, close, c=colors, s=10, alpha=0.9)
            ax.set_ylabel("Close")
            ax.grid(True, alpha=0.3)

            # Format each subplot's x-axis as time; do NOT share limits so that
            # each 12h window has its own readable time scale.
            ax.xaxis.set_major_formatter(mdates.DateFormatter("%H:%M"))

        axes[-1].set_xlabel("Time (ET)")
        fig.suptitle("6E 1m amplitude labels (30bps/20) – " + d)
        fig.tight_layout(rect=[0, 0, 1, 0.95])

        out_path = os.path.join(out_dir, f"6e_1m_labels_{d}.png")
        fig.savefig(out_path, dpi=150)
        plt.close(fig)
        print("saved", out_path)


if __name__ == "__main__":
    main()
