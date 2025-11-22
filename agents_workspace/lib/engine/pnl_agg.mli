(** Shared daily PnL accumulator for engines. *)

open Core
open Types

type t = {
  trades : trade list;
  daily_pnl : float Date.Map.t;
  daily_pnl_usd : float Date.Map.t;
  daily_pnl_pct : float Date.Map.t;
}

val empty : t
val add_trade : t -> trade -> t
val add_trades : t -> trade list -> t

val to_alists_unsorted :
  t -> (Date.t * float) list * (Date.t * float) list * (Date.t * float) list
