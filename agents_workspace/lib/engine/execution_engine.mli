open Types

type config = {
  cost : Cost_model.config;
  exec : Execution_params.t;
  build_trade :
    plan:trade_plan ->
    active:active_state ->
    exit_ts:timestamp ->
    exit_price:float ->
    exit_qty:float ->
    exit_reason:exit_reason ->
    trade;
}

(** Single-bar book transition: consume [bar], update orders/positions, and emit trades. *)
val step :
  config:config ->
  book:Order_book.t ->
  bar:bar_1m ->
  Order_book.t * trade list

(** Immediate flatten commands (e.g., time/feature exits) at the current bar close. *)
val apply_flatten_cmds :
  config:config ->
  book:Order_book.t ->
  bar:bar_1m ->
  Strategy_sig.order_cmd list ->
  Order_book.t * trade list

(** End-of-session hook: flatten any active positions at [last_bar] using [Eod_flat]. *)
val on_session_end :
  config:config ->
  book:Order_book.t ->
  last_bar:bar_1m option ->
  Order_book.t * trade list
