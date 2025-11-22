open Core

type t = {
  tick_size : float;
  tick_value : float;
  be_trigger_mult : float;
  downgrade_cutoff_offset_min : int;
  two_r_range_factor : float;
  climactic_range_factor : float;
  gap_min_pct_adr : float;
  gap_max_pct_adr : float;
  body_pct_min : float;
  ibs_bull_min : float;
  ibs_bear_max : float;
  abr_window_n : int;
  adr_window_n : int;
}

let default = {
  tick_size = 0.25;
  tick_value = 12.5;
  be_trigger_mult = 0.8;
  downgrade_cutoff_offset_min = 4;
  two_r_range_factor = 1.5;
  climactic_range_factor = 2.5;
  gap_min_pct_adr = 11.0;
  gap_max_pct_adr = 60.0;
  body_pct_min = 0.5;
  ibs_bull_min = 0.69;
  ibs_bear_max = 0.31;
  abr_window_n = 8;
  adr_window_n = 21;
}

type field = {
  name : string;
  default : float;
  bounds : float * float;
  integer : bool;
  tunable : bool;
  description : string;
  set : t -> float -> t;
}

let param_table : field list =
  let int = true and fl = false in
  [
    { name = "be_trigger_mult"; default = default.be_trigger_mult; bounds = (0.1, 1.0); integer = fl; tunable = true;
      description = "break-even trigger in R";
      set = (fun p v -> { p with be_trigger_mult = v }) };
    { name = "downgrade_cutoff_offset_min"; default = Float.of_int default.downgrade_cutoff_offset_min; bounds = (0., 30.); integer = int; tunable = true;
      description = "minutes after B2 before downgrade ends";
      set = (fun p v -> { p with downgrade_cutoff_offset_min = Int.of_float v |> Int.clamp_exn ~min:0 ~max:120 }) };
    { name = "two_r_range_factor"; default = default.two_r_range_factor; bounds = (0.5, 3.0); integer = fl; tunable = true;
      description = "max B1 range / ABR_prev to allow 2R target";
      set = (fun p v -> { p with two_r_range_factor = v }) };
    { name = "climactic_range_factor"; default = default.climactic_range_factor; bounds = (1.0, 5.0); integer = fl; tunable = true;
      description = "max B1 range / ABR_prev to be valid";
      set = (fun p v -> { p with climactic_range_factor = v }) };
    { name = "gap_min_pct_adr"; default = default.gap_min_pct_adr; bounds = (0., 100.); integer = fl; tunable = true;
      description = "min |gap| %% ADR to qualify";
      set = (fun p v -> { p with gap_min_pct_adr = v }) };
    { name = "gap_max_pct_adr"; default = default.gap_max_pct_adr; bounds = (0., 100.); integer = fl; tunable = true;
      description = "max |gap| %% ADR to qualify";
      set = (fun p v -> { p with gap_max_pct_adr = v }) };
    { name = "body_pct_min"; default = default.body_pct_min; bounds = (0., 1.); integer = fl; tunable = true;
      description = "min body/range for trend";
      set = (fun p v -> { p with body_pct_min = v }) };
    { name = "ibs_bull_min"; default = default.ibs_bull_min; bounds = (0., 1.); integer = fl; tunable = true;
      description = "IBS threshold bullish";
      set = (fun p v -> { p with ibs_bull_min = v }) };
    { name = "ibs_bear_max"; default = default.ibs_bear_max; bounds = (0., 1.); integer = fl; tunable = true;
      description = "IBS threshold bearish";
      set = (fun p v -> { p with ibs_bear_max = v }) };
    { name = "abr_window_n"; default = Float.of_int default.abr_window_n; bounds = (4., 50.); integer = int; tunable = false;
      description = "ABR rolling window length"; set = (fun p v -> { p with abr_window_n = Int.of_float v |> Int.max 1 }) };
    { name = "adr_window_n"; default = Float.of_int default.adr_window_n; bounds = (4., 50.); integer = int; tunable = false;
      description = "ADR rolling window length"; set = (fun p v -> { p with adr_window_n = Int.of_float v |> Int.max 1 }) };
  ]

let parameter_specs =
  param_table
  |> List.filter ~f:(fun f -> f.tunable)
  |> List.map ~f:(fun f ->
      Parameters.make ~name:f.name ~default:f.default ~bounds:f.bounds
        ~integer:f.integer ~description:f.description ())

let apply_overrides (m : Parameters.value_map) (base : t) : t =
  List.fold param_table ~init:base ~f:(fun acc f ->
      match Map.find m f.name with
      | None -> acc
      | Some v -> f.set acc v)
