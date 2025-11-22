[@@@warning "-32"]
open Core
open Types

val build : B1b2_params.Setup.t -> string -> setup Date.Table.t
val build_with_defaults : string -> setup Date.Table.t
