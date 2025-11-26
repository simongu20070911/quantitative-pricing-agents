#!/usr/bin/env python3
"""
Stream-convert Databento OHLCV CSVs to the engine-friendly ET format.

Input files (e.g., *.ohlcv-1m.csv) often have:
  ts_event,rtype,publisher_id,instrument_id,open,high,low,close,volume
with nanosecond precision and no ET column.

Output is written with the columns expected by Csv_parser.parse_bar_1m:
  ts_event,rtype,publisher_id,instrument_id,open,high,low,close,volume,symbol,ET_datetime
where ts_event is normalized to minute precision with timezone offset,
and ET_datetime is the same instant converted to America/New_York.

Usage:
  python3 tools/convert_to_et.py --input data/file.csv [--output data/file.et.ohlcv-1m.csv] [--symbol ES.c.0]
If --symbol is omitted, a symbol is derived from the filename prefix
before the date range (e.g., glbx-mdp3-gc.v.0).
"""

from __future__ import annotations

import argparse
import csv
import re
from datetime import datetime, timezone
from pathlib import Path
from zoneinfo import ZoneInfo

ET_TZ = ZoneInfo("America/New_York")


def _strip_fractional(ts: str) -> str:
    """Drop fractional seconds while keeping any timezone offset."""
    if "." not in ts:
        return ts
    main, rest = ts.split(".", 1)
    offset = ""
    for i, ch in enumerate(rest):
        if not ch.isdigit():
            offset = rest[i:]
            break
    return main + offset


def parse_ts(ts_raw: str) -> datetime:
    """
    Accepts forms like:
      2010-06-07T00:00:00.000000000Z
      2010-06-07T00:00:00+00:00
      2010-06-07 00:00:00+00:00
    Returns timezone-aware datetime in UTC.
    """
    s = ts_raw.strip()
    if s.endswith("Z"):
        s = s[:-1] + "+00:00"
    s = _strip_fractional(s)
    # datetime.fromisoformat accepts both 'T' and space separators.
    dt = datetime.fromisoformat(s)
    if dt.tzinfo is None:
        dt = dt.replace(tzinfo=timezone.utc)
    else:
        dt = dt.astimezone(timezone.utc)
    return dt


def fmt_ts(dt: datetime) -> str:
    """Format with colon in offset to match existing ET CSVs."""
    s = dt.strftime("%Y-%m-%d %H:%M:%S%z")
    return s[:-2] + ":" + s[-2:]


def derive_symbol(path: Path) -> str:
    """
    Use the filename prefix before the date range as a stable symbol.
    Examples:
      glbx-mdp3-gc.v.0.20100606-20251117 -> glbx-mdp3-gc.v.0
      es_full-mdp3-20100606-20251117    -> es_full-mdp3
    """
    stem = path.name
    if stem.endswith(".csv"):
        stem = stem[:-4]
    if stem.endswith(".ohlcv-1m"):
        stem = stem[: -len(".ohlcv-1m")]
    m = re.match(r"(.+?)[.-]\d{8}-\d{8}$", stem)
    return m.group(1) if m else stem


def default_output_path(in_path: Path) -> Path:
    name = in_path.name
    suffix = ".ohlcv-1m.csv"
    if name.endswith(suffix):
        base = name[: -len(suffix)]
        return in_path.with_name(f"{base}.et{suffix}")
    return in_path.with_name(name + ".et")


def convert_file(in_path: Path, out_path: Path, symbol: str | None) -> None:
    expected_prefix = [
        "ts_event",
        "rtype",
        "publisher_id",
        "instrument_id",
        "open",
        "high",
        "low",
        "close",
        "volume",
    ]
    derived_symbol = symbol or derive_symbol(in_path)

    with in_path.open() as fin, out_path.open("w", newline="") as fout:
        reader = csv.reader(fin)
        writer = csv.writer(fout, lineterminator="\n")

        header = next(reader, None)
        if header is None:
            raise RuntimeError(f"{in_path} is empty")
        if header[: len(expected_prefix)] != expected_prefix:
            raise RuntimeError(
                f"{in_path} has unexpected header {header[:len(expected_prefix)]}"
            )

        writer.writerow(expected_prefix + ["symbol", "ET_datetime"])

        for row in reader:
            if not row:
                continue
            if len(row) < len(expected_prefix):
                raise RuntimeError(f"Row too short in {in_path}: {row}")

            ts_raw = row[0]
            dt_utc = parse_ts(ts_raw)
            ts_out = fmt_ts(dt_utc)
            et_out = fmt_ts(dt_utc.astimezone(ET_TZ))

            sym = derived_symbol
            # If input already carries a symbol column, prefer it.
            if len(row) >= 10 and row[9]:
                sym = row[9]

            writer.writerow(
                [
                    ts_out,
                    row[1],
                    row[2],
                    row[3],
                    row[4],
                    row[5],
                    row[6],
                    row[7],
                    row[8],
                    sym,
                    et_out,
                ]
            )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", required=True, type=Path)
    parser.add_argument("--output", type=Path, help="Optional output path")
    parser.add_argument("--symbol", type=str, help="Override symbol column")
    args = parser.parse_args()

    in_path: Path = args.input
    out_path: Path = args.output or default_output_path(in_path)
    convert_file(in_path, out_path, args.symbol)


if __name__ == "__main__":
    main()
