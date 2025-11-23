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

let default ~tick_size =
  {
    tick_size;
    spread_ticks = 1.0;
    slip_model = No_slip;
    volume_model = Equal_quarters;
    cancel_after_bars = -1;
    allow_partial_fills = true;
    allow_same_bar_entry = false;
    rng_state = Random.State.make [| 12345 |];
    break_even_intrabar = true;
    volume_aware = true;
  }

let legacy ~tick_size =
  {
    tick_size;
    spread_ticks = 0.0;
    slip_model = No_slip;
    volume_model = Equal_quarters;
    cancel_after_bars = -1;
    allow_partial_fills = false;
    allow_same_bar_entry = true;
    rng_state = Random.State.make [| 42 |];
    break_even_intrabar = false;
    volume_aware = false;
  }

let apply_tick (t : t) n_ticks = n_ticks *. t.tick_size

let copy_rng t = { t with rng_state = Random.State.copy t.rng_state }

let with_seed t seed = { t with rng_state = Random.State.make [| seed |] }
