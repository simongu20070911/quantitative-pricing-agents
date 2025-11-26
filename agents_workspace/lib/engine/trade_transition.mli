open Types

(** Stateless trade lifecycle transition for a single trade_plan.
    Invariants/responsibilities:
    - Caller gates on session windows; [step] assumes the bar is eligible.
    - [plan] is treated as immutable; [step] will not mutate it.
    - Deterministic intrabar path (O/H/L/C or O/L/H/C) with optional
      latency/volume/slip modeled via [Execution_params].
    - Supports partial fills and latency; multiple partial trades may emit per bar. *)

val init_pending :
  qty:float ->
  latency_bars:int ->
  cancel_after:int ->
  trade_state

val step :
  exec:Execution_params.t ->
  plan:trade_plan ->
  state:trade_state ->
  bar:bar_1m ->
  record_trade:(active:active_state -> exit_ts:timestamp -> exit_price:float -> exit_qty:float -> exit_reason:exit_reason -> 'a) ->
  trade_state * 'a list
(**
    [step] consumes one bar and returns the next [trade_state] plus emitted
    trades (possibly multiple partials). The [record_trade] callback builds domain-specific
    trade records (e.g., with costs/qty). It must be pure; callers handle accumulation.
*)
