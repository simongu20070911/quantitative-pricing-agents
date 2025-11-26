open Core
open Types

type config = {
  batch_size : int;
  n_regions : int;
  max_evals : int;
  init_samples : int option;
  shared_stream : bool;
  objective : Optimizer.objective;
  seed : int;
  host : string option;
  port : int option;
}

type eval = {
  params : Parameters.value_map;
  score : float;
  trades : trade list;
  daily_pnl : (Date.t * float) list;
  region_id : int option;
}

type result = {
  best : eval;
  history : eval list;
  tested : int;
  state : Botorch_client.state;
}

val run :
  ?log:(string -> unit) ->
  strat_pack:Strategy_registry.pack ->
  datafile:string ->
  config:config ->
  unit ->
  result
