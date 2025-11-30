open Core
open Types

type setup_stream = {
  on_bar : bar_1m -> setup option;
  finalize : unit -> unit;
}

type pure_strategy = {
  _id : string;
  env : Strategy_sig.env;
  build_setups : (string -> setup Date.Table.t) option;
  build_setups_stream : (unit -> setup_stream) option;
  strategy : (module Strategy_sig.V2);
}

type run_result = {
  setups     : setup Date.Table.t;
  trades     : trade list;
  daily_pnl  : (Date.t * float) list;
  daily_pnl_usd : (Date.t * float) list;
  daily_pnl_pct : (Date.t * float) list;
}
