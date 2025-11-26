import csv
from collections import deque, defaultdict
from datetime import datetime

tick_size = 0.25
rth_start_min = 9*60 + 30
rth_end_min = 16*60 + 15
b1_min = 9*60 + 30
b2_min = 9*60 + 35
abr_eod_min = 16*60 + 10
b2_end_minute = b2_min + 4  # 09:39


def parse_bar(row):
    # row fields: ts_event,...,ET_datetime maybe last; prefer ET_datetime if present
    if len(row) == 11:
        ts_str = row[10]
    else:
        ts_str = row[0]
    date = ts_str[:10]
    minute = int(ts_str[11:13]) * 60 + int(ts_str[14:16])
    open_, high, low, close = map(float, row[4:8])
    volume = float(row[8])
    return date, minute, open_, high, low, close, volume


def stream_bars(filename):
    with open(filename, newline='') as f:
        reader = csv.reader(f)
        next(reader)  # header
        for row in reader:
            yield parse_bar(row)


def pass1(filename):
    day_macro = {}
    b1 = {}
    b2 = {}
    eod_abr = {}
    abr_window = deque()
    current_5m = None  # (date, bucket_min, open, high, low, close)

    def flush_5m(bar):
        if bar is None:
            return
        date, minute, o, h, l, c = bar
        rng = h - l
        abr_window.append(rng)
        if len(abr_window) > 8:
            abr_window.popleft()
        if len(abr_window) >= 8 and minute == abr_eod_min:
            eod_abr[date] = sum(abr_window) / len(abr_window)
        if minute == b1_min:
            b1[date] = (o, h, l, c)
        if minute == b2_min:
            b2[date] = (o, h, l, c)

    for date, minute, o, h, l, c, _ in stream_bars(filename):
        if rth_start_min <= minute <= rth_end_min:
            macro = day_macro.setdefault(date, {
                'rth_high': float('-inf'), 'rth_low': float('inf'), 'rth_close': None, 'has_rth': False
            })
            macro['rth_high'] = max(macro['rth_high'], h)
            macro['rth_low'] = min(macro['rth_low'], l)
            macro['rth_close'] = c
            macro['has_rth'] = True
        bucket = (minute // 5) * 5
        if current_5m is None:
            current_5m = (date, bucket, o, h, l, c)
        else:
            cd, cbucket, co, ch, cl, cc = current_5m
            if cd == date and cbucket == bucket:
                ch = max(ch, h)
                cl = min(cl, l)
                cc = c
                current_5m = (cd, cbucket, co, ch, cl, cc)
            else:
                flush_5m(current_5m)
                current_5m = (date, bucket, o, h, l, c)
    flush_5m(current_5m)
    return day_macro, b1, b2, eod_abr


def build_setups(day_macro, b1, b2, eod_abr):
    setups = {}
    adr_window = deque()
    prev_close = None
    prev_eod_abr = None
    dates = sorted(day_macro.keys())
    for date in dates:
        macro = day_macro[date]
        if not macro['has_rth']:
            continue
        daily_range = macro['rth_high'] - macro['rth_low']
        adr21 = sum(adr_window)/len(adr_window) if len(adr_window) >= 21 else None
        abr_prev = prev_eod_abr
        if date in b1 and date in b2 and prev_close is not None and adr21 is not None and abr_prev is not None:
            b1o, b1h, b1l, b1c = b1[date]
            b2o, b2h, b2l, b2c = b2[date]
            gap_pts = b1o - prev_close
            gap_pct_adr = abs(gap_pts) / adr21 * 100.0
            rng = b1h - b1l
            if rng > 0:
                body = abs(b1c - b1o)
                body_pct = body / rng
                ibs = (b1c - b1l) / rng
                f_gap = 11.0 <= gap_pct_adr <= 60.0
                f_trend = body_pct >= 0.5
                f_clim = rng <= 2.5 * abr_prev
                f_bull = b1c > b1o and ibs > 0.69
                f_bear = b1c < b1o and ibs < 0.31
                direction = None
                if f_gap and f_trend and f_clim and f_bull:
                    direction = 'Long'
                elif f_gap and f_trend and f_clim and f_bear:
                    direction = 'Short'
                if direction:
                    setups[date] = {
                        'direction': direction,
                        'b1': (b1o, b1h, b1l, b1c),
                        'b2': (b2o, b2h, b2l, b2c),
                        'abr_prev': abr_prev,
                        'prev_close': prev_close,
                        'adr21': adr21,
                    }
        adr_window.append(daily_range)
        if len(adr_window) > 21:
            adr_window.popleft()
        prev_close = macro['rth_close']
        prev_eod_abr = eod_abr.get(date)
    return setups


def build_plan(setup):
    direction = setup['direction']
    b1o, b1h, b1l, b1c = setup['b1']
    b2o, b2h, b2l, b2c = setup['b2']
    b1_range = b1h - b1l
    if direction == 'Long':
        entry = b1h + tick_size
        cancel = b1l
        stop = b1l - tick_size
        b2_good = b2c > b2o
    else:
        entry = b1l - tick_size
        cancel = b1h
        stop = b1h + tick_size
        b2_good = b2c < b2o
    r_pts = abs(entry - stop)
    can_two_r = b1_range <= 1.5 * setup['abr_prev']
    target_mult = 2.0 if can_two_r else 1.0
    target = entry + target_mult * r_pts if direction == 'Long' else entry - target_mult * r_pts
    be_trigger = entry + 0.8 * r_pts if direction == 'Long' else entry - 0.8 * r_pts
    return {
        'direction': direction,
        'entry': entry,
        'cancel': cancel,
        'stop': stop,
        'r_pts': r_pts,
        'target_mult': target_mult,
        'target': target,
        'be': be_trigger,
        'downgrade_after_b2': can_two_r and (not b2_good),
        'b2_follow': 'good' if b2_good else 'poor',
    }


def backtest(filename, setups):
    trades = []
    current_date = None
    plan = None
    state = 'No_trade'
    moved_to_be = False
    entry_ts = None
    last_rth_bar = None

    def finalize_day():
        nonlocal trades, plan, state, moved_to_be, entry_ts, last_rth_bar
        if state == 'Active' and plan and last_rth_bar and last_rth_bar[0] == current_date:
            _, minute, _, _, _, close, _ = last_rth_bar
            trades.append({
                'date': current_date,
                'entry_time': entry_ts,
                'exit_time': minute,
                'direction': plan['direction'],
                'entry_price': plan['entry'],
                'exit_price': close,
                'exit_reason': 'Eod_flat',
                'pnl_R': (close - plan['entry'])/plan['r_pts'] if plan['direction']=='Long' else (plan['entry'] - close)/plan['r_pts'],
            })
        plan = None
        state = 'No_trade'
        moved_to_be = False
        entry_ts = None
        last_rth_bar = None

    for date, minute, o, h, l, c, _ in stream_bars(filename):
        if current_date != date:
            if current_date is not None:
                finalize_day()
            current_date = date
            if date in setups:
                plan = build_plan(setups[date])
                state = 'Pending'
                moved_to_be = False
                entry_ts = None
            else:
                plan = None
                state = 'No_trade'
        if rth_start_min <= minute <= rth_end_min:
            last_rth_bar = (date, minute, o, h, l, c, _)
        if plan is None:
            continue
        if minute < b2_min or minute > rth_end_min:
            continue
        if plan['downgrade_after_b2'] and plan['target_mult'] == 2.0 and minute > b2_end_minute:
            plan['target_mult'] = 1.0
            plan['target'] = plan['entry'] + plan['r_pts'] if plan['direction']=='Long' else plan['entry'] - plan['r_pts']
        if state == 'Pending':
            if plan['direction']=='Long':
                if h >= plan['entry']:
                    state = 'Active'
                    moved_to_be = False
                    entry_ts = minute
                elif l <= plan['cancel']:
                    state = 'Done'
            else:
                if l <= plan['entry']:
                    state = 'Active'
                    moved_to_be = False
                    entry_ts = minute
                elif h >= plan['cancel']:
                    state = 'Done'
        elif state == 'Active':
            stopped = (l <= plan['stop']) if plan['direction']=='Long' else (h >= plan['stop'])
            if stopped:
                trades.append({
                    'date': date,
                    'entry_time': entry_ts,
                    'exit_time': minute,
                    'direction': plan['direction'],
                    'entry_price': plan['entry'],
                    'exit_price': plan['stop'],
                    'exit_reason': 'Stop',
                    'pnl_R': (plan['stop'] - plan['entry'])/plan['r_pts'] if plan['direction']=='Long' else (plan['entry'] - plan['stop'])/plan['r_pts'],
                })
                state = 'Done'
            else:
                if not moved_to_be:
                    if (plan['direction']=='Long' and h >= plan['be']) or (plan['direction']=='Short' and l <= plan['be']):
                        plan['stop'] = plan['entry']
                        moved_to_be = True
                hit_target = (plan['direction']=='Long' and h >= plan['target']) or (plan['direction']=='Short' and l <= plan['target'])
                if hit_target:
                    trades.append({
                        'date': date,
                        'entry_time': entry_ts,
                        'exit_time': minute,
                        'direction': plan['direction'],
                        'entry_price': plan['entry'],
                        'exit_price': plan['target'],
                        'exit_reason': 'Target',
                        'pnl_R': (plan['target'] - plan['entry'])/plan['r_pts'] if plan['direction']=='Long' else (plan['entry'] - plan['target'])/plan['r_pts'],
                    })
                    state = 'Done'
    finalize_day()
    return trades


def load_engine_trades(path):
    trades = []
    with open(path) as f:
        reader = csv.DictReader(f)
        for row in reader:
            trades.append({
                'date': row['date'],
                'entry_time': row['entry_time'],
                'exit_time': row['exit_time'],
                'direction': row['direction'],
                'exit_reason': row['exit_reason'],
            })
    return trades


def main():
    import argparse
    ap = argparse.ArgumentParser()
    ap.add_argument('--csv', required=True, help='path to ES 1m csv')
    ap.add_argument('--engine-trades', required=True)
    args = ap.parse_args()

    day_macro, b1, b2, eod_abr = pass1(args.csv)
    setups = build_setups(day_macro, b1, b2, eod_abr)
    legacy_trades = backtest(args.csv, setups)
    engine_trades = load_engine_trades(args.engine_trades)

    legacy_dates = set(t['date'] for t in legacy_trades)
    engine_dates = set(t['date'] for t in engine_trades)
    only_legacy = sorted(legacy_dates - engine_dates)
    only_engine = sorted(engine_dates - legacy_dates)

    print(f"Legacy trades: {len(legacy_trades)} dates: {len(legacy_dates)}")
    print(f"Engine trades: {len(engine_trades)} dates: {len(engine_dates)}")
    print("Only in legacy (dates):", only_legacy)
    print("Only in engine (dates):", only_engine)
    # show details for only_legacy dates
    if only_legacy:
        by_date = {t['date']: t for t in legacy_trades}
        for d in only_legacy:
            t = by_date[d]
            print("Legacy-only", d, t)
    if only_engine:
        by_date_e = {t['date']: t for t in engine_trades}
        for d in only_engine:
            print("Engine-only", d, by_date_e[d])

if __name__ == '__main__':
    main()
