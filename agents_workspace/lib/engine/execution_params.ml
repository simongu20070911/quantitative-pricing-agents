open Core

type slip_model =
  | No_slip
  | Constant_ticks of float
  | Prob_one_tick of { prob : float }

type volume_model =
  | Equal_quarters
  | Range_weighted

type exit_priority =
  | Path_order
  | Stop_first

type t = {
  continuous_path : bool;
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

let default ~tick_size ?(continuous = true) () =
  {
    continuous_path = continuous;
    tick_size;
    spread_ticks = 0.0;   (* consolidated into slip_model *)
    slip_model = No_slip;
    volume_model = Equal_quarters;
    volume_aware = true;
    allow_partial_fills = true;
    allow_same_bar_entry = true;
    break_even_intrabar = true;
    exit_priority = Stop_first;
    rng_state = Random.State.make [| 12345 |];
  }

let apply_tick (t : t) n_ticks = n_ticks *. t.tick_size

let copy_rng t = { t with rng_state = Random.State.copy t.rng_state }

let with_seed t seed = { t with rng_state = Random.State.make [| seed |] }
