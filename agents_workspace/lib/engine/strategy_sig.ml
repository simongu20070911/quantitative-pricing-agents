open Types

type env = {
  session_start_min : int;
  session_end_min   : int;
  qty : float;
  cost : Cost_model.config;
  exec : Execution_params.t;
}

(** Order-intent commands emitted by V2 strategies. *)
type order_cmd =
  | Submit_bracket of {
      plan : trade_plan;
      qty : float;
      meta : (string * string) list;
    }
  | Update_all of (trade_plan -> trade_plan)
  | Update_stop of (trade_plan -> float)
  | Update_target of (trade_plan -> float)
  | Cancel_all
  | Flatten_all of {
      reason : exit_reason;
      meta : (string * string) list;
    }

module type V2 = sig
  type state

  val init : setup option -> state
  val step : env -> state -> bar_1m -> state * order_cmd list
  val finalize_day : env -> state -> bar_1m option -> state * order_cmd list
end
