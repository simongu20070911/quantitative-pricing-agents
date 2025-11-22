open Types

module Trade : sig
  val make
    :  qty:float
    -> ?r_pts:float
    -> Cost_model.config
    -> direction
    -> entry_ts:timestamp
    -> entry_px:float
    -> exit_ts:timestamp
    -> exit_px:float
    -> reason:exit_reason
    -> meta:(string * string) list
    -> trade
end

module Config : sig
  val cost_params : Cost_model.config -> Parameters.t list

  val cost_of_params
    :  defaults:Cost_model.config
    -> Parameters.value_map
    -> Cost_model.config

  val session_params : default_start:int -> default_end:int -> Parameters.t list

  val session_of_params
    :  defaults:(int * int)
    -> Parameters.value_map
    -> int * int
end

module Tunables : sig
  type env_defaults = {
    session_start_min : int;
    session_end_min   : int;
    qty : float;
    cost : Cost_model.config;
  }

  val env_specs : defaults:env_defaults -> Parameters.t list
  val env_of_params : defaults:env_defaults -> Parameters.value_map -> env_defaults

  type 'p param_field = {
    name : string;
    default : float;
    bounds : float * float;
    integer : bool;
    tunable : bool;
    description : string;
    set : 'p -> float -> 'p;
  }

  val parameter_specs_of_table : 'p param_field list -> Parameters.t list
  val params_of_table
    :  defaults:'p
    -> param_table:'p param_field list
    -> Parameters.value_map
    -> 'p

  (** Combine env specs and strategy-specific param table into a single surface. *)
  val make
    :  defaults_env:env_defaults
    -> defaults_params:'p
    -> param_table:'p param_field list
    -> Parameters.t list * (Parameters.value_map -> env_defaults * 'p)
end

module Env : sig
  val of_config
    :  session_start_min:int
    -> session_end_min:int
    -> qty:float
    -> cost:Cost_model.config
    -> Strategy_sig.env
end

module Session : sig
  val within : start:int -> end_:int -> bar_1m -> bool

  val eod_flat
    :  qty:float
    -> ?r_pts:float
    -> Cost_model.config
    -> direction:direction
    -> entry_ts:timestamp
    -> entry_px:float
    -> last_bar:bar_1m
    -> meta:(string * string) list
    -> trade
end

module Setup : sig
  val noop : string -> setup Core.Date.Table.t
end

module Setups : sig
  val with_params
    :  params:'p
    -> ('p -> string -> setup Core.Date.Table.t)
    -> string
    -> setup Core.Date.Table.t
end

module Strategy_builder : sig
  val make_pure
    :  id:string
    -> env:Strategy_sig.env
    -> ?build_setups:(string -> setup Core.Date.Table.t)
    -> (module Strategy_sig.S)
    -> Engine.pure_strategy
end

module Trade_common : sig
  val with_strategy : string -> (string * string) list -> (string * string) list
end
