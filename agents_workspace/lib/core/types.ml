open Core

[@@@warning "-27-32-69"]

(* Basic categorical types *)
type direction = Long | Short

let string_of_direction = function
  | Long -> "LONG"
  | Short -> "SHORT"

type timestamp = {
  date : Date.t;
  minute_of_day : int;  (* 0 .. 1439 *)
}

(* Market data *)
type bar_1m = {
  ts     : timestamp;
  open_  : float;
  high   : float;
  low    : float;
  close  : float;
  volume : float;
}

type bar_5m = {
  date          : Date.t;
  minute_of_day : int;   (* start minute of the 5-min bar *)
  open_         : float;
  mutable high  : float;
  mutable low   : float;
  mutable close : float;
}

(* Daily context *)
type day_macro = {
  mutable rth_high  : float;
  mutable rth_low   : float;
  mutable rth_close : float option;
  mutable has_rth   : bool;
}

type setup = {
  date       : Date.t;
  direction  : direction;
  b1         : bar_5m;
  b2         : bar_5m;
  abr_prev   : float;   (* ABR_8_From_Prev_RTH *)
  prev_close : float;
  adr21      : float;
}

(* Trade lifecycle *)
type exit_reason =
  | Stop
  | Target
  | Eod_flat

let string_of_exit_reason = function
  | Stop     -> "stop"
  | Target   -> "target"
  | Eod_flat -> "eod_flat"

type b2_follow =
  | Follow_good
  | Follow_poor

type trade = {
  date         : Date.t;
  direction    : direction;
  entry_ts     : timestamp;
  exit_ts      : timestamp;
  entry_price  : float;
  exit_price   : float;
  qty          : float;   (** number of contracts, can be fractional *)
  r_pts        : float;
  pnl_pts      : float;   (** per-contract PnL in index points, net if costs applied *)
  pnl_R        : float;   (** per-contract R PnL *)
  pnl_usd      : float;   (** total PnL in USD for the whole trade (qty included) *)
  pnl_pct      : float option; (** PnL as fraction of equity base, if provided *)
  duration_min : float;
  exit_reason  : exit_reason;
  meta         : (string * string) list;
}

type trade_plan = {
  direction         : direction;
  entry_price       : float;
  cancel_level      : float;
  stop_init         : float;
  r_pts             : float;
  target_mult       : float;
  target_price      : float;
  be_trigger        : float;
  b2_end_minute     : int;
  downgrade_after_b2 : bool;
  abr_prev          : float;
  b1_range          : float;
  b2_follow         : b2_follow;
}

type active_state = {
  mutable stop_price : float;
  mutable moved_to_be : bool;
  entry_ts           : timestamp;
  mutable entry_price : float;
  mutable entry_value : float;
  mutable qty : float;
  mutable pending_entry_qty : float;       (** additional qty still trying to fill *)
  mutable pending_entry_cancel_after : int; (** bars left to keep pending entry; -1 => no timeout *)
}

type pending_state = {
  qty_remaining : float;
  cancel_after  : int;   (** bars left before cancel; -1 => never cancel *)
  latency_remaining : int; (** bars of latency before entry eligible; 0 => live now *)
}

type trade_state =
  | No_trade
  | Pending of pending_state
  | Active of active_state
  | Done

(* Order/position layer for more complex engines *)

type order_side =
  | Buy
  | Sell

type order_status =
  | Working
  | Filled
  | Cancelled

type order_kind =
  | Bracket of trade_plan

type order = {
  id : int;
  parent_id : int option;
  side : order_side;
  qty : float;
  kind : order_kind;
  trade_state : trade_state;
  created_ts : timestamp;
  updated_ts : timestamp;
  status : order_status;
  meta : (string * string) list;
}

type book_position = {
  id : int;
  direction : direction;
  qty : float;
  entry_ts : timestamp;
  entry_price : float;
  meta : (string * string) list;
}

(* Aggregate performance outputs for reuse (printers can format as needed) *)
type perf_stats = {
  n_trades     : int;
  n_days       : int;
  win_rate     : float option;
  expectancy_R : float option;
  avg_R_per_day: float option;
  ann_sharpe   : float option;
  expectancy_usd : float option;
  avg_usd_per_day: float option;
  ann_sharpe_usd : float option;
  avg_pct_per_day: float option;
  ann_sharpe_pct : float option;
  max_drawdown_R : float option;
  max_drawdown_usd : float option;
  skew_R : float option;
}
