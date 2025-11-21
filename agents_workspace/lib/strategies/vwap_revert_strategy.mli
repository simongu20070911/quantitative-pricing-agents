type config = {
  a1 : float;
  a2 : float;
  b1 : float;
  b2 : float;
  s_entry : float;
  z_exit  : float;
  time_stop_min : int;
  stop_ticks : float;
  max_units : int;
  cost : Cost_model.config;
}

module Pure (_ : sig val cfg : config end) : Strategy_sig.S

val default_config : config
val strategy_id : string
val parameter_specs : Parameters.t list
val config_of_params : Parameters.value_map -> config

val make_pure_strategy : config -> Engine.pure_strategy
val strategy_pure : Engine.pure_strategy

val make_strategy : config -> Engine.strategy
val strategy : Engine.strategy
