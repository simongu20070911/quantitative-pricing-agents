open Core

module T = Strategy_common.Tunables

let int_flag = true
let fl_flag = false

let clamp_int ~min ~max v =
  v |> Int.of_float |> Int.clamp_exn ~min ~max

module Setup = struct
  type t = {
    climactic_range_factor : float;
    gap_min_pct_adr : float;
    gap_max_pct_adr : float;
    body_pct_min : float;
    ibs_bull_min : float;
    ibs_bear_max : float;
    abr_window_n : int;
    adr_window_n : int;
  }

  type field = t T.param_field

  let param_table : field list =
    [
      { name = "climactic_range_factor"; default = 2.5; bounds = (1.0, 5.0); integer = fl_flag; tunable = true;
        description = "max B1 range / ABR_prev to be valid";
        set = (fun p v -> { p with climactic_range_factor = v }) };
      { name = "gap_min_pct_adr"; default = 11.0; bounds = (0., 100.); integer = fl_flag; tunable = true;
        description = "min |gap| %% ADR to qualify";
        set = (fun p v -> { p with gap_min_pct_adr = v }) };
      { name = "gap_max_pct_adr"; default = 60.0; bounds = (0., 100.); integer = fl_flag; tunable = true;
        description = "max |gap| %% ADR to qualify";
        set = (fun p v -> { p with gap_max_pct_adr = v }) };
      { name = "body_pct_min"; default = 0.5; bounds = (0., 1.); integer = fl_flag; tunable = true;
        description = "min body/range for trend";
        set = (fun p v -> { p with body_pct_min = v }) };
      { name = "ibs_bull_min"; default = 0.69; bounds = (0., 1.); integer = fl_flag; tunable = true;
        description = "IBS threshold bullish";
        set = (fun p v -> { p with ibs_bull_min = v }) };
      { name = "ibs_bear_max"; default = 0.31; bounds = (0., 1.); integer = fl_flag; tunable = true;
        description = "IBS threshold bearish";
        set = (fun p v -> { p with ibs_bear_max = v }) };
      { name = "abr_window_n"; default = 8.; bounds = (4., 50.); integer = int_flag; tunable = false;
        description = "ABR rolling window length";
        set = (fun p v -> { p with abr_window_n = clamp_int ~min:1 ~max:120 v }) };
      { name = "adr_window_n"; default = 21.; bounds = (4., 50.); integer = int_flag; tunable = false;
        description = "ADR rolling window length";
        set = (fun p v -> { p with adr_window_n = clamp_int ~min:1 ~max:120 v }) };
    ]

  let defaults =
    let zero = {
      climactic_range_factor = 0.;
      gap_min_pct_adr = 0.;
      gap_max_pct_adr = 0.;
      body_pct_min = 0.;
      ibs_bull_min = 0.;
      ibs_bear_max = 0.;
      abr_window_n = 0;
      adr_window_n = 0;
    } in
    List.fold param_table ~init:zero ~f:(fun acc f -> f.set acc f.default)

  let parameter_specs = T.parameter_specs_of_table param_table

  let apply_overrides (m : Parameters.value_map) (base : t) : t =
    T.params_of_table ~defaults:base ~param_table m
end

module Exec = struct
  type t = {
    tick_size : float;
    tick_value : float;
    be_trigger_mult : float;
    downgrade_cutoff_offset_min : int;
    two_r_range_factor : float;
  }

  type field = t T.param_field

  let param_table : field list =
    [
      { name = "tick_size"; default = 0.25; bounds = (0.01, 1.); integer = fl_flag; tunable = false;
        description = "minimum price increment";
        set = (fun p v -> { p with tick_size = v }) };
      { name = "tick_value"; default = 12.5; bounds = (1., 100.); integer = fl_flag; tunable = false;
        description = "USD value per tick per contract";
        set = (fun p v -> { p with tick_value = v }) };
      { name = "be_trigger_mult"; default = 0.8; bounds = (0.1, 1.0); integer = fl_flag; tunable = true;
        description = "break-even trigger in R";
        set = (fun p v -> { p with be_trigger_mult = v }) };
      { name = "downgrade_cutoff_offset_min"; default = 4.; bounds = (0., 30.); integer = int_flag; tunable = true;
        description = "minutes after B2 before downgrade ends";
        set = (fun p v -> { p with downgrade_cutoff_offset_min = clamp_int ~min:0 ~max:120 v }) };
      { name = "two_r_range_factor"; default = 1.5; bounds = (0.5, 3.0); integer = fl_flag; tunable = true;
        description = "max B1 range / ABR_prev to allow 2R target";
        set = (fun p v -> { p with two_r_range_factor = v }) };
    ]

  let defaults =
    let zero = {
      tick_size = 0.;
      tick_value = 0.;
      be_trigger_mult = 0.;
      downgrade_cutoff_offset_min = 0;
      two_r_range_factor = 0.;
    } in
    List.fold param_table ~init:zero ~f:(fun acc f -> f.set acc f.default)

  let parameter_specs = T.parameter_specs_of_table param_table

  let apply_overrides (m : Parameters.value_map) (base : t) : t =
    T.params_of_table ~defaults:base ~param_table m
end

type t = {
  setup : Setup.t;
  exec : Exec.t;
}

type field = t T.param_field

let defaults = {
  setup = Setup.defaults;
  exec = Exec.defaults;
}

let param_table : field list =
  let wrap_setup (f : Setup.field) : field =
    { name = "setup." ^ f.name;
      default = f.default;
      bounds = f.bounds;
      integer = f.integer;
      tunable = f.tunable;
      description = "setup: " ^ f.description;
      set = (fun p v -> { p with setup = f.set p.setup v }) }
  in
  let wrap_exec (f : Exec.field) : field =
    { name = "exec." ^ f.name;
      default = f.default;
      bounds = f.bounds;
      integer = f.integer;
      tunable = f.tunable;
      description = "exec: " ^ f.description;
      set = (fun p v -> { p with exec = f.set p.exec v }) }
  in
  List.map Setup.param_table ~f:wrap_setup
  @ List.map Exec.param_table ~f:wrap_exec

let parameter_specs = T.parameter_specs_of_table param_table

let apply_overrides (m : Parameters.value_map) (base : t) : t =
  T.params_of_table ~defaults:base ~param_table m
