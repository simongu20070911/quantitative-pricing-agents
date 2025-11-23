open Core

type slip_model =
  | No_slip
  | Constant_ticks of float
  | Prob_one_tick of { prob : float }

type volume_model =
  | Equal_quarters
  | Range_weighted

type t = {
  tick_size : float;
  spread_ticks : float;
  slip_model : slip_model;
  volume_model : volume_model;
  cancel_after_bars : int;
  allow_partial_fills : bool;
  allow_same_bar_entry : bool;
  rng_state : Random.State.t;
  break_even_intrabar : bool;
  volume_aware : bool;
}

val default : tick_size:float -> t
(** Realistic-ish defaults: 1 tick spread, 0 slip, 1-bar latency. *)

val legacy : tick_size:float -> t
(** Legacy optimistic model: zero spread/slip, same-bar entry, equal volume splits. *)

val apply_tick : t -> float -> float
(** Convert ticks to absolute price increment. *)

val copy_rng : t -> t
(** Copy rng_state so independent runs don’t share entropy. *)

val with_seed : t -> int -> t
(** Replace rng_state with a fresh PRNG seeded deterministically. *)
