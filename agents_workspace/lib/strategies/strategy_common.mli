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
