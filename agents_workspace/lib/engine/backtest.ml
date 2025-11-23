open Core
open Types

type engine_config = {
  session_start_min : int;
  session_end_min   : int;
  trade_start_min   : int;
  trade_end_min     : int;
  build_trade_plan  : setup -> trade_plan option;
  on_plan_bar       : trade_plan -> bar_1m -> unit;
}

module Intent (Cfg : sig val cfg : engine_config end) : Strategy_sig.V2 = struct
  type state = {
    plan : trade_plan option;
    submitted : bool;
  }

  let init setup_opt =
    match setup_opt with
    | None -> { plan = None; submitted = false }
    | Some s ->
        match Cfg.cfg.build_trade_plan s with
        | None -> { plan = None; submitted = false }
        | Some plan -> { plan = Some plan; submitted = false }

  let step (env : Strategy_sig.env) state (bar : bar_1m) =
    match state.plan with
    | None -> state, []
    | Some plan ->
        let minute = bar.ts.minute_of_day in
        if minute < Cfg.cfg.trade_start_min || minute > Cfg.cfg.trade_end_min then
          state, []
        else begin
          Cfg.cfg.on_plan_bar plan bar;
          if state.submitted then state, []
          else
            let meta = [ ("strategy", "backtest") ] in
            let cmds = [ Strategy_sig.Submit_bracket { plan; qty = 1.0 *. env.qty; meta } ] in
            { state with submitted = true }, cmds
        end

  let finalize_day _env state _last_bar = state, []
end

let default_cost : Cost_model.config = {
  tick_size = 0.25;
  tick_value = 12.5;
  slippage_roundtrip_ticks = 0.0;
  fee_per_contract = 0.0;
  equity_base = None;
}

let run ~(config : engine_config) filename setups_tbl
  : trade list * (Date.t * float) list =
  let env : Strategy_sig.env = {
    session_start_min = config.session_start_min;
    session_end_min = config.session_end_min;
    qty = 1.0;
    cost = default_cost;
    exec = Execution_params.legacy ~tick_size:default_cost.tick_size;
  } in
  let module S = Intent(struct let cfg = config end) in
  let strategy : Engine_v2.pure_strategy = {
    Engine_v2._id = "backtest";
    env;
    build_setups = Some (fun _ -> setups_tbl);
    strategy = (module S);
  } in
  let r = Engine_v2.run_pure strategy ~filename in
  r.trades, r.daily_pnl
