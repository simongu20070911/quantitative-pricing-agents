[@@@warning "-32"]
open Core
open Types

val compute_daily_context_and_setups : string -> setup Date.Table.t

val compute_daily_context_and_setups_with_params
  : B1b2_params.t -> string -> setup Date.Table.t
