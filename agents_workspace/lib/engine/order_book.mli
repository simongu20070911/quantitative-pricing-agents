open Types

(** Lightweight order/position book used by execution engines.
    This keeps engine state reusable across strategies. *)

type t = {
  next_order_id : int;
  orders : order list;
  positions : book_position list;
}

val empty : unit -> t

(** Apply a batch of high-level order commands at the timestamp of [bar_ts]. *)
val apply_cmds :
  t ->
  ts:timestamp ->
  Strategy_sig.order_cmd list ->
  t

(** Replace orders and positions; helper for execution engine. *)
val with_orders_positions : t -> orders:order list -> positions:book_position list -> t
