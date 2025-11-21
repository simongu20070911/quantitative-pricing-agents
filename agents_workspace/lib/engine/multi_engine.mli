open Core
open Types

type run_result = {
  strategy_id : string;
  setups      : setup Date.Table.t;
  trades      : trade list;
  daily_pnl   : (Date.t * float) list;
  daily_pnl_usd : (Date.t * float) list;
  daily_pnl_pct : (Date.t * float) list;
}

val run_all : Engine.strategy list -> filename:string -> run_result list

val run_shared
  :  Engine.strategy list
  -> filename:string
  -> run_result list
