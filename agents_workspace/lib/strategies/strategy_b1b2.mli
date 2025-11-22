type config

val default_config : config

val strategy_id : string

val parameter_specs : Parameters.t list

val config_of_params : Parameters.value_map -> config

val session_window : config -> int * int
val qty : config -> float
val cost : config -> Cost_model.config
val with_cost : cost:Cost_model.config -> config -> config
val with_qty : qty:float -> config -> config
val with_session : start:int -> end_:int -> config -> config
val params : config -> B1b2_params.t

val pure_strategy : config -> Engine.pure_strategy
val strategy_pure : Engine.pure_strategy

val legacy_strategy : config -> Engine.strategy
val strategy : Engine.strategy
