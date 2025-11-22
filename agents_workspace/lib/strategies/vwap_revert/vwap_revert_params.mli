(** VWAP reversion strategy tunables. *)

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

type field = t Strategy_common.Tunables.param_field

val param_table : field list
val defaults : t
val parameter_specs : Parameters.t list
val apply_overrides : Parameters.value_map -> t -> t
