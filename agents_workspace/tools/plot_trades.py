#!/usr/bin/env python3
import argparse
import os
import pandas as pd
import matplotlib.pyplot as plt


def plot_equity(df, outfile, label, col, ylab):
    if col not in df:
        return
    plt.figure(figsize=(12, 6))
    df[f'cum_{col}'] = df[col].cumsum()
    plt.plot(df[f'cum_{col}'], label=label, color='#1f77b4')
    plt.axhline(0, color='gray', linewidth=0.8)
    plt.title(f'Cumulative {ylab} ({label})')
    plt.xlabel('Trades')
    plt.ylabel(f'Cumulative {ylab}')
    plt.grid(alpha=0.3)
    plt.legend()
    plt.tight_layout()
    plt.savefig(outfile)
    plt.close()


def plot_daily(df, outfile, col, label):
    if col not in df:
        return
    plt.figure(figsize=(12, 6))
    df[f'cum_{col}'] = df[col].cumsum()
    plt.plot(df['date'], df[f'cum_{col}'], color='#2ca02c', label=label)
    plt.axhline(0, color='gray', linewidth=0.8)
    plt.xticks(rotation=45, ha='right')
    plt.xlabel('Date')
    plt.ylabel(label)
    plt.title(f'{label} by Day')
    plt.grid(alpha=0.3)
    plt.tight_layout()
    plt.savefig(outfile)
    plt.close()


def plot_hist(df, outfile, col, title, xlabel):
    if col not in df:
        return
    plt.figure(figsize=(8, 5))
    plt.hist(df[col], bins=40, color='#ff7f0e', edgecolor='white', alpha=0.8)
    plt.axvline(0, color='gray', linewidth=0.8)
    plt.xlabel(xlabel)
    plt.ylabel('Count')
    plt.title(title)
    plt.tight_layout()
    plt.savefig(outfile)
    plt.close()


def main():
    ap = argparse.ArgumentParser(description='Plot trades/daily PnL exported from OCaml backtests')
    ap.add_argument('--trades', required=True, help='trades CSV from --export-trades')
    ap.add_argument('--daily', required=True, help='daily CSV from --export-daily')
    ap.add_argument('--outdir', required=True, help='output directory for PNGs')
    args = ap.parse_args()

    os.makedirs(args.outdir, exist_ok=True)
    trades = pd.read_csv(args.trades, parse_dates=['date'])
    daily = pd.read_csv(args.daily, parse_dates=['date'])

    plot_equity(trades, os.path.join(args.outdir, 'equity_trades_R.png'), label='Trades', col='pnl_R', ylab='R')
    plot_hist(trades, os.path.join(args.outdir, 'pnl_hist_R.png'), col='pnl_R', title='Trade PnL Distribution (R)', xlabel='PnL (R)')

    # USD and pct if available
    plot_equity(trades, os.path.join(args.outdir, 'equity_trades_usd.png'), label='Trades', col='pnl_usd', ylab='USD')
    plot_hist(trades, os.path.join(args.outdir, 'pnl_hist_usd.png'), col='pnl_usd', title='Trade PnL Distribution (USD)', xlabel='PnL (USD)')

    plot_daily(daily, os.path.join(args.outdir, 'equity_daily_R.png'), col='pnl_R', label='Cumulative R')
    plot_daily(daily, os.path.join(args.outdir, 'equity_daily_usd.png'), col='pnl_usd', label='Cumulative USD')
    plot_daily(daily, os.path.join(args.outdir, 'equity_daily_pct.png'), col='pnl_pct', label='Cumulative pct')


if __name__ == '__main__':
    main()
