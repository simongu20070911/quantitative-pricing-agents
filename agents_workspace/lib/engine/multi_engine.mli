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

module type BAR_STREAM = sig
  val iter : f:(bar_1m -> unit) -> unit
end

val run_all : Engine_v2.pure_strategy list -> filename:string -> run_result list
val run_all_pure : Engine_v2.pure_strategy list -> filename:string -> run_result list

val run_shared_with_stream
  :  stream:(module BAR_STREAM)
  -> make_setups:(Engine_v2.pure_strategy -> setup Date.Table.t)
  -> Engine_v2.pure_strategy list
  -> run_result list

val run_shared
  :  Engine_v2.pure_strategy list
  -> filename:string
  -> run_result list

val run_shared_pure
  :  Engine_v2.pure_strategy list
  -> filename:string
  -> run_result list
