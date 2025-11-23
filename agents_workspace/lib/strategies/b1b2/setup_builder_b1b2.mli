open Core
open Types

type agg_tables = {
  day_macro_tbl : day_macro Date.Table.t;
  b1_tbl : bar_5m Date.Table.t;
  b2_tbl : bar_5m Date.Table.t;
  eod_abr_tbl : float Date.Table.t;
}

val aggregate_bars : B1b2_params.Setup.t -> string -> agg_tables
val build_setups : B1b2_params.Setup.t -> agg_tables -> setup Date.Table.t
val build : B1b2_params.Setup.t -> string -> setup Date.Table.t
val build_with_defaults : string -> setup Date.Table.t
