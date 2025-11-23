open Core

type slip_model =
  | No_slip
  | Constant_ticks of float
  | Prob_one_tick of { prob : float }

type exit_priority =
  | Path_order
  | Stop_first

type t = {
  tick_size : float;
  spread_ticks : float;
  slip_model : slip_model;
  break_even_intrabar : bool;
  exit_priority : exit_priority;
  rng_state : Random.State.t;
}

let default ~tick_size =
  {
    tick_size;
    spread_ticks = 1.0;
    slip_model = No_slip;
    break_even_intrabar = true;
    exit_priority = Path_order;
    rng_state = Random.State.make [| 12345 |];
  }

let legacy ~tick_size =
  {
    tick_size;
    spread_ticks = 0.0;
    slip_model = No_slip;
    break_even_intrabar = false;
    exit_priority = Stop_first;
    rng_state = Random.State.make [| 42 |];
  }

let apply_tick (t : t) n_ticks = n_ticks *. t.tick_size

let copy_rng t = { t with rng_state = Random.State.copy t.rng_state }

let with_seed t seed = { t with rng_state = Random.State.make [| seed |] }
