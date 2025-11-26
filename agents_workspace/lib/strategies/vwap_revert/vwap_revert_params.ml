open Core
module T = Strategy_common.Tunables

type t = {
  a1 : float;
  a2 : float;
  b1 : float;
  b2 : float;
  s_entry : float;
  z_exit  : float;
  time_stop_min : int;
  stop_ticks : float;
  max_units : int;
}

type field = t T.param_field

let param_table : field list =
  let int = true and fl = false in
  let clamp_int ~min ~max v = v |> Int.of_float |> Int.clamp_exn ~min ~max in
  [
    { name = "a1"; default = 1.0; bounds = (0., 5.); integer = fl; tunable = true;
      description = "scale on z-score distance from VWAP";
      set = (fun p v -> { p with a1 = v }) };
    { name = "a2"; default = 0.5; bounds = (0., 5.); integer = fl; tunable = true;
      description = "OFI exhaustion term weight";
      set = (fun p v -> { p with a2 = v }) };
    { name = "b1"; default = 0.5; bounds = (0., 5.); integer = fl; tunable = true;
      description = "volatility penalty";
      set = (fun p v -> { p with b1 = v }) };
    { name = "b2"; default = 0.5; bounds = (0., 5.); integer = fl; tunable = true;
      description = "trend penalty";
      set = (fun p v -> { p with b2 = v }) };
    { name = "s_entry"; default = 0.7; bounds = (0.1, 3.0); integer = fl; tunable = true;
      description = "signal threshold to open a trade";
      set = (fun p v -> { p with s_entry = v }) };
    { name = "z_exit"; default = 0.2; bounds = (0., 1.0); integer = fl; tunable = true;
      description = "VWAP reversion exit band";
      set = (fun p v -> { p with z_exit = v }) };
    { name = "time_stop_min"; default = 10.; bounds = (1., 240.); integer = int; tunable = true;
      description = "time-based exit in minutes";
      set = (fun p v -> { p with time_stop_min = clamp_int ~min:1 ~max:240 v }) };
    { name = "stop_ticks"; default = 6.0; bounds = (1., 30.); integer = fl; tunable = true;
      description = "hard stop distance in ticks";
      set = (fun p v -> { p with stop_ticks = v }) };
    { name = "max_units"; default = 3.; bounds = (1., 10.); integer = int; tunable = false;
      description = "cap on volatility-targeted units (fixed)";
      set = (fun p v -> { p with max_units = clamp_int ~min:1 ~max:100 v }) };
  ]

let defaults =
  let zero = {
    a1 = 0.; a2 = 0.; b1 = 0.; b2 = 0.;
    s_entry = 0.; z_exit = 0.;
    time_stop_min = 0; stop_ticks = 0.; max_units = 0;
  } in
  List.fold param_table ~init:zero ~f:(fun acc f -> f.set acc f.default)

let parameter_specs = T.parameter_specs_of_table param_table

let apply_overrides (m : Parameters.value_map) (base : t) : t =
  T.params_of_table ~defaults:base ~param_table m
