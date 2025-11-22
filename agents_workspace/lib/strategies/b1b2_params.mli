(** Single source of truth for B1/B2 strategy constants and tunables. *)

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

val default : t

type field = {
  name : string;
  default : float;
  bounds : float * float;
  integer : bool;
  tunable : bool;
  description : string;
  set : t -> float -> t;
}

val param_table : field list

val parameter_specs : Parameters.t list

val apply_overrides : Parameters.value_map -> t -> t
