open Types

type env = {
  session_start_min : int;
  session_end_min   : int;
  qty : float;
  cost : Cost_model.config;
}

module type S = sig
  type state

  val init : setup option -> state
  val step : env -> state -> bar_1m -> state * trade list
  val finalize_day : env -> state -> bar_1m option -> state * trade list
end
