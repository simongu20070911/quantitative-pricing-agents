open Core
open Types

val compute_stats :
  ?daily_usd:(Date.t * float) list ->
  ?daily_pct:(Date.t * float) list ->
  trade list -> (Date.t * float) list -> perf_stats
val print_stats : perf_stats -> unit
val print_sample_trades : trade list -> int -> unit

val export_trades_csv : outfile:string -> trades:trade list -> unit
(** Write trades to CSV with columns:
    date,entry_time,exit_time,direction,entry_price,exit_price,qty,r_pts,pnl_pts,pnl_R,pnl_usd,pnl_pct,
    duration_min,exit_reason,meta *)

val export_daily_csv :
  outfile:string ->
  daily:(Date.t * float) list ->
  ?daily_usd:(Date.t * float) list ->
  ?daily_pct:(Date.t * float) list ->
  unit -> unit
(** Write daily PnL to CSV with columns: date,pnl_R,pnl_usd,pnl_pct. Optional USD/pct are zero if absent. *)
