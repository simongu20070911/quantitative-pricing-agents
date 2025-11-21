open Core
open Types
open Time_utils
module SC = Strategy_common
module TT = Trade_transition

module Setup_builder = Setup_builder_b1b2
module SS = Strategy_sig

type config = {
  session_start_min : int;
  session_end_min   : int;
  qty : float;
  cost : Cost_model.config;
}

let default_config = {
  session_start_min = rth_start_min;
  session_end_min = rth_end_min;
  qty = 1.0;
  cost = {
    tick_size = 0.25;
    tick_value = 12.5;
    slippage_roundtrip_ticks = 1.0;
    fee_per_contract = 4.0;
    equity_base = None;
  };
}

let strategy_id = "b1b2"

let parameter_specs =
  SC.Config.session_params ~default_start:default_config.session_start_min
    ~default_end:default_config.session_end_min
  @ [
      Parameters.make ~name:"qty" ~default:1.0 ~bounds:(0.1, 20.)
        ~description:"contracts per trade" ();
    ]
  @ SC.Config.cost_params default_config.cost

let config_of_params (m : Parameters.value_map) : config =
  let session_start_min, session_end_min =
    SC.Config.session_of_params
      ~defaults:(default_config.session_start_min, default_config.session_end_min)
      m
  in
  let qty = Map.find m "qty" |> Option.value ~default:default_config.qty in
  let cost = SC.Config.cost_of_params ~defaults:default_config.cost m in
  { session_start_min; session_end_min; qty; cost }

let record_trade ~(cfg : config) ~(plan : trade_plan) ~(active : active_state)
    ~(exit_ts : timestamp) ~(exit_price : float) ~(reason : exit_reason) =
  let meta = [
    ("strategy", strategy_id);
    ("target_mult", Float.to_string plan.target_mult);
    ("abr_prev", Float.to_string plan.abr_prev);
    ("b1_range", Float.to_string plan.b1_range);
    ("b2_follow",
     match plan.b2_follow with Follow_good -> "good" | Follow_poor -> "poor");
  ] in
  SC.Trade.make ~qty:cfg.qty ~r_pts:plan.r_pts cfg.cost plan.direction
    ~entry_ts:active.entry_ts ~entry_px:plan.entry_price
    ~exit_ts ~exit_px:exit_price ~reason ~meta

let downgrade_if_needed plan ~minute_of_day =
  if plan.downgrade_after_b2 && Float.(plan.target_mult = 2.0) && minute_of_day > plan.b2_end_minute then
    let target_price =
      match plan.direction with
      | Long  -> plan.entry_price +. plan.r_pts
      | Short -> plan.entry_price -. plan.r_pts
    in
    { plan with target_mult = 1.0; target_price }
  else plan

module Pure (Cfg : sig val cfg : config end) : SS.S = struct
  type state = {
    plan : trade_plan option;
    trade_state : trade_state;
  }

  let init setup_opt =
    match setup_opt with
    | None -> { plan = None; trade_state = No_trade }
    | Some s ->
        (match Trade_logic.build_trade_plan s with
         | None -> { plan = None; trade_state = No_trade }
         | Some plan -> { plan = Some plan; trade_state = Pending })

  let on_trade plan trade_state bar =
    let plan = downgrade_if_needed plan ~minute_of_day:bar.ts.minute_of_day in
    let trade_state', trades =
      TT.step ~plan ~state:trade_state ~bar
        ~record_trade:(fun ~active ~exit_ts ~exit_price ~exit_reason ->
            record_trade ~cfg:Cfg.cfg ~plan ~active ~exit_ts ~exit_price ~reason:exit_reason)
    in
    { plan = Some plan; trade_state = trade_state' }, trades

  let step env state (bar : bar_1m) =
    let open SS in
    let in_session =
      bar.ts.minute_of_day >= env.session_start_min
      && bar.ts.minute_of_day <= env.session_end_min
    in
    match state.plan with
    | None -> state, []
    | Some plan ->
        if bar.ts.minute_of_day < b2_min || not in_session then state, []
        else on_trade plan state.trade_state bar

  let finalize_day _env state last_bar =
    match state.plan, state.trade_state, last_bar with
    | Some plan, Active active, Some lb ->
        let trade =
          SC.Session.eod_flat ~qty:Cfg.cfg.qty ~r_pts:plan.r_pts Cfg.cfg.cost
            ~direction:plan.direction ~entry_ts:active.entry_ts
            ~entry_px:plan.entry_price ~last_bar:lb
            ~meta:[
              ("strategy", strategy_id);
              ("target_mult", Float.to_string plan.target_mult);
              ("abr_prev", Float.to_string plan.abr_prev);
              ("b1_range", Float.to_string plan.b1_range);
              ("b2_follow",
               match plan.b2_follow with
               | Follow_good -> "good"
               | Follow_poor -> "poor");
            ]
        in
        ({ plan = state.plan; trade_state = Done }, [ trade ])
    | _ -> (state, [])
end

module Policy_of_pure (Cfg : sig val cfg : config end) : Policy_sig.S = struct
  type t = Pure(Cfg).state
  module PS = Pure(Cfg)

  let env = {
    SS.session_start_min = Cfg.cfg.session_start_min;
    session_end_min = Cfg.cfg.session_end_min;
    qty = Cfg.cfg.qty;
    cost = Cfg.cfg.cost;
  }

  let init_day setup_opt = PS.init setup_opt
  let on_bar st bar =
    let st', trades = PS.step env st bar in
    st', trades
  let on_session_end st last_bar = PS.finalize_day env st last_bar
end

let make_strategy cfg =
  let module P = Policy_of_pure(struct let cfg = cfg end) in
  {
    Engine.id = strategy_id;
    session_start_min = cfg.session_start_min;
    session_end_min = cfg.session_end_min;
    build_setups = Some Setup_builder.compute_daily_context_and_setups;
    policy = (module P);
  }

let strategy = make_strategy default_config

let make_pure_strategy cfg =
  let env = {
    SS.session_start_min = cfg.session_start_min;
    session_end_min = cfg.session_end_min;
    qty = cfg.qty;
    cost = cfg.cost;
  } in
  let module S = Pure(struct let cfg = cfg end) in
  {
    Engine._id = strategy_id;
    env;
    build_setups = Some Setup_builder.compute_daily_context_and_setups;
    strategy = (module S);
  }

let strategy_pure = make_pure_strategy default_config
