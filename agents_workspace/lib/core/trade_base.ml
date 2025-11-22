open Core
open Types

let make_raw ~qty ~r_pts ~(direction : direction)
    ~(entry_ts : timestamp) ~entry_px ~(exit_ts : timestamp) ~exit_px
    ~(exit_reason : exit_reason) ~(meta : (string * string) list) : trade =
  let pnl_pts =
    match direction with
    | Long  -> exit_px -. entry_px
    | Short -> entry_px -. exit_px
  in
  let pnl_R = pnl_pts /. r_pts in
  let day_diff = Date.diff exit_ts.date entry_ts.date in
  let duration_min =
    let minutes =
      (day_diff * Time_utils.minutes_per_day)
      + (exit_ts.minute_of_day - entry_ts.minute_of_day)
    in
    if minutes < 0 then
      invalid_arg "make_raw: exit timestamp precedes entry timestamp"
    else
      Float.of_int minutes
  in
  {
    date = exit_ts.date;
    direction;
    entry_ts;
    exit_ts;
    entry_price = entry_px;
    exit_price = exit_px;
    qty;
    r_pts;
    pnl_pts;
    pnl_R;
    pnl_usd = 0.0;
    pnl_pct = None;
    duration_min;
    exit_reason;
    meta;
  }

let apply_costs ~qty (cost : Cost_model.config) (t : trade) =
  Cost_model.apply ~qty cost t
