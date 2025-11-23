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
  tick_size : float;
  spread_ticks : float;
  slip_model : slip_model;
  volume_model : volume_model;
  volume_aware : bool;
  allow_partial_fills : bool;
  allow_same_bar_entry : bool;
  latency_bars : int;
  cancel_latency_bars : int;
  break_even_intrabar : bool;
  exit_priority : exit_priority;
  rng_state : Random.State.t;
}

let default ~tick_size =
  {
    tick_size;
    spread_ticks = 1.0;
    slip_model = No_slip;
    volume_model = Equal_quarters;
    volume_aware = true;
    allow_partial_fills = true;
    allow_same_bar_entry = true;
    latency_bars = 0;
    cancel_latency_bars = -1;
    break_even_intrabar = true;
    exit_priority = Stop_first;
    rng_state = Random.State.make [| 12345 |];
  }

let legacy ~tick_size =
  {
    tick_size;
    spread_ticks = 0.0;
    slip_model = No_slip;
    volume_model = Equal_quarters;
    volume_aware = false;
    allow_partial_fills = false;
    allow_same_bar_entry = true;
    latency_bars = 0;
    cancel_latency_bars = -1;
    break_even_intrabar = false;
    exit_priority = Stop_first;
    rng_state = Random.State.make [| 42 |];
  }

let apply_tick (t : t) n_ticks = n_ticks *. t.tick_size

let copy_rng t = { t with rng_state = Random.State.copy t.rng_state }

let with_seed t seed = { t with rng_state = Random.State.make [| seed |] }
