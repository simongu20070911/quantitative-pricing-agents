open Types

(** Stateless trade lifecycle transition for a single trade_plan.
    Invariants/responsibilities:
    - Caller gates on session windows; [step] assumes the bar is eligible.
    - [plan] is treated as immutable; [step] will not mutate it.
    - Deterministic intrabar path (O/H/L/C or O/L/H/C) with optional
      latency/volume/slip modeled via [Execution_params].
    - At most one trade is emitted per bar. *)

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

val step_with_exec :
  exec:Execution_params.t ->
  qty:float ->
  plan:trade_plan ->
  state:trade_state ->
  bar:bar_1m ->
  record_trade:(active:active_state -> exit_ts:timestamp -> exit_price:float -> exit_reason:exit_reason -> 'a) ->
  trade_state * 'a list
(** Same as [step] but allowing custom execution parameters and position size. *)
