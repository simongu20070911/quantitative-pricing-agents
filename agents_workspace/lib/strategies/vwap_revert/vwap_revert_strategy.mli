type config

val default_config : config
val strategy_id : string
val parameter_specs : Parameters.t list
val config_of_params : Parameters.value_map -> config

val session_window : config -> int * int
val qty : config -> float
val cost : config -> Cost_model.config
val with_cost : cost:Cost_model.config -> config -> config
val with_s_entry : s_entry:float -> config -> config
val with_z_exit : z_exit:float -> config -> config
val with_session : start:int -> end_:int -> config -> config
val with_qty : qty:float -> config -> config

module Pure (_ : sig val cfg : config end) : Strategy_sig.S

val pure_strategy : config -> Engine.pure_strategy
val strategy_pure : Engine.pure_strategy
