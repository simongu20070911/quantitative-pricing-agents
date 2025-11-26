open Core

type slip_model =
  | No_slip
  | Constant_ticks of float
  | Prob_one_tick of { prob : float }

type volume_model =
  | Equal_quarters
  | Range_weighted

type exit_priority =
  | Path_order  (** exit decided by first touch along the intrabar path *)
  | Stop_first  (** stop chosen whenever both stop and target touch in same bar *)

type t = {
  continuous_path : bool;  (** true => continuous first-cross along each OHLC segment; false => discrete touch-only processing *)
  tick_size : float;
  spread_ticks : float;
  slip_model : slip_model;
  volume_model : volume_model;
  volume_aware : bool;
  allow_partial_fills : bool;
  allow_same_bar_entry : bool;
  break_even_intrabar : bool;
  exit_priority : exit_priority;
  rng_state : Random.State.t;
}

val default : tick_size:float -> ?continuous:bool -> unit -> t
(** Realistic-ish defaults: continuous intrabar path, 1 tick spread, intrabar BE, stop-first exits, partial fills on, volume-aware. *)

val apply_tick : t -> float -> float
(** Convert ticks to absolute price increment. *)

val copy_rng : t -> t
(** Copy rng_state so independent runs don’t share entropy. *)

val with_seed : t -> int -> t
(** Replace rng_state with a fresh PRNG seeded deterministically. *)
