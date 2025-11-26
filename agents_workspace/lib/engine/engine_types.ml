open Core
open Types

type pure_strategy = {
  _id : string;
  env : Strategy_sig.env;
  build_setups : (string -> setup Date.Table.t) option;
  strategy : (module Strategy_sig.V2);
}

type run_result = {
  setups     : setup Date.Table.t;
  trades     : trade list;
  daily_pnl  : (Date.t * float) list;
  daily_pnl_usd : (Date.t * float) list;
  daily_pnl_pct : (Date.t * float) list;
}
