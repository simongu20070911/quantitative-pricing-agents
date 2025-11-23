open Core
open Types

module Params = Execution_params

type side = Buy | Sell
type rng = Random.State.t

val make_rng : ?seed:int -> unit -> rng

val path_prices : bar_1m -> float array
(** Deterministic 4-point path: up/doji => O,H,L,C; down => O,L,H,C. *)

val volume_slices : params:Params.t -> bar_1m -> float array
(** Returns 4 slices matching path points. *)

val adjust_price : params:Params.t -> side:side -> rng:rng -> float -> float
(** Apply half-spread plus slip (in ticks) to a raw touch price. *)
