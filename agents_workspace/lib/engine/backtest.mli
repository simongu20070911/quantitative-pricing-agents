open Core
open Types

type engine_config = {
  session_start_min : int;
  session_end_min   : int;
  trade_start_min   : int;
  trade_end_min     : int;
  build_trade_plan  : setup -> trade_plan option;
  on_plan_bar       : trade_plan -> bar_1m -> unit;
}

val run : config:engine_config -> string -> setup Date.Table.t
  -> trade list * (Date.t * float) list
