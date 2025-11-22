(** Single source of truth for B1/B2 setup and execution tunables. *)

module Setup : sig
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

  type field = t Strategy_common.Tunables.param_field

  val param_table : field list
  val defaults : t
  val parameter_specs : Parameters.t list
  val apply_overrides : Parameters.value_map -> t -> t
end

module Exec : sig
  type t = {
    tick_size : float;
    tick_value : float;
    be_trigger_mult : float;
    downgrade_cutoff_offset_min : int;
    two_r_range_factor : float;
  }

  type field = t Strategy_common.Tunables.param_field

  val param_table : field list
  val defaults : t
  val parameter_specs : Parameters.t list
  val apply_overrides : Parameters.value_map -> t -> t
end

type t = {
  setup : Setup.t;
  exec : Exec.t;
}

type field = t Strategy_common.Tunables.param_field

val defaults : t
val param_table : field list
val parameter_specs : Parameters.t list
val apply_overrides : Parameters.value_map -> t -> t
