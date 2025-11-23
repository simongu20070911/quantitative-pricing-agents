open Core
open Types

type pure_strategy = {
  _id : string;
  env : Strategy_sig.env;
  build_setups : (string -> setup Date.Table.t) option;
  strategy : (module Strategy_sig.V2);
}

type run_result = {
  setups     : setup Core.Date.Table.t;
  trades     : trade list;
  daily_pnl  : (Core.Date.t * float) list;
  daily_pnl_usd : (Core.Date.t * float) list;
  daily_pnl_pct : (Core.Date.t * float) list;
}

val run_pure : pure_strategy -> filename:string -> run_result
