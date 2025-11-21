open Core
open Types

type strategy = {
  id : string;
  session_start_min : int;
  session_end_min   : int;
  build_setups : (string -> setup Date.Table.t) option;
  policy : (module Policy_sig.S);
}

type pure_strategy = {
  _id : string;
  env : Strategy_sig.env;
  build_setups : (string -> setup Date.Table.t) option;
  strategy : (module Strategy_sig.S);
}

type run_result = {
  setups     : setup Date.Table.t;
  trades     : trade list;
  daily_pnl  : (Date.t * float) list;          (** R per day *)
  daily_pnl_usd : (Date.t * float) list;       (** USD per day, includes qty and costs *)
  daily_pnl_pct : (Date.t * float) list;       (** Pct of equity base per day, if available *)
}

val run : strategy -> filename:string -> run_result

val run_pure : pure_strategy -> filename:string -> run_result
