(** Core trade construction helpers with centralized invariants. *)

open Types

val make_raw :
  qty:float ->
  r_pts:float ->
  direction:direction ->
  entry_ts:timestamp ->
  entry_px:float ->
  exit_ts:timestamp ->
  exit_px:float ->
  exit_reason:exit_reason ->
  meta:(string * string) list ->
  trade
(** Build a trade record with pnl_pts/pnl_R/pnl_usd/pnl_pct computed; cost
    adjustments are not applied. *)

val apply_costs : qty:float -> Cost_model.config -> trade -> trade
(** Apply slippage/fees/equity adjustments to a trade. *)
