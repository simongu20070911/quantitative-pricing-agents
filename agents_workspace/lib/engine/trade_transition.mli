open Types

(** Stateless trade lifecycle transition for a single trade_plan.
    Invariants/responsibilities:
    - Caller gates on session windows; [step] assumes the bar is eligible.
    - [plan] is treated as immutable; [step] will not mutate it.
    - At most one trade is emitted per bar.
    - Stop has precedence over target when both are touched within a bar. *)

val step :
  plan:trade_plan ->
  state:trade_state ->
  bar:bar_1m ->
  record_trade:(active:active_state -> exit_ts:timestamp -> exit_price:float -> exit_reason:exit_reason -> 'a) ->
  trade_state * 'a list
(**
    [step] consumes one bar and returns the next [trade_state] plus any emitted
    trades (at most one). The [record_trade] callback builds domain-specific
    trade records (e.g., with costs/qty). It must be pure; callers handle
    accumulation.
*)
