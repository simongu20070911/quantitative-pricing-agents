open Core
open Types
open Time_utils
module SC = Strategy_common
module TT = Trade_transition
module P = B1b2_params

module Setup_builder = Setup_builder_b1b2
module SS = Strategy_sig

type config = {
  session_start_min : int;
  session_end_min   : int;
  qty : float;
  cost : Cost_model.config;
  exec : Execution_params.t;
  params : P.t;
}

let default_params = P.defaults

let default_env =
  { SC.Tunables.session_start_min = rth_start_min;
    session_end_min = rth_end_min;
    qty = 1.0;
    cost = {
      tick_size = default_params.exec.tick_size;
      tick_value = default_params.exec.tick_value;
      slippage_roundtrip_ticks = 1.0;
      fee_per_contract = 4.0;
      equity_base = None;
    }; }

let default_config = {
  session_start_min = default_env.session_start_min;
  session_end_min = default_env.session_end_min;
  qty = default_env.qty;
  cost = default_env.cost;
  exec = Execution_params.default ~tick_size:default_env.cost.tick_size;
  params = default_params;
}

let strategy_id = "b1b2"

let param_table =
  List.map P.param_table ~f:(fun (f : P.field) ->
      { SC.Tunables.name = f.name;
        default = f.default;
        bounds = f.bounds;
        integer = f.integer;
        tunable = f.tunable;
        description = f.description;
        set = f.set; })

let parameter_specs, config_parts_of_params =
  SC.Tunables.make
    ~defaults_env:default_env
    ~defaults_params:default_params
    ~param_table

let config_of_params (m : Parameters.value_map) : config =
  let env, params = config_parts_of_params m in
  let cost =
    { env.cost with
      tick_size = params.exec.tick_size;
      tick_value = params.exec.tick_value; }
  in
  { session_start_min = env.session_start_min;
    session_end_min = env.session_end_min;
    qty = env.qty;
    cost;
    exec = Execution_params.default ~tick_size:cost.tick_size;
    params; }

let session_window cfg = cfg.session_start_min, cfg.session_end_min
let qty cfg = cfg.qty
let cost cfg = cfg.cost
let exec cfg = cfg.exec
let params cfg = cfg.params
let with_cost ~cost cfg = { cfg with cost }
let with_qty ~qty cfg = { cfg with qty }
let with_params ~params cfg = { cfg with params }
let with_session ~start ~end_ cfg = { cfg with session_start_min = start; session_end_min = end_ }
let with_exec ~exec cfg = { cfg with exec }

let record_trade ~(cfg : config) ~(plan : trade_plan) ~(active : active_state)
    ~(exit_ts : timestamp) ~(exit_price : float) ~(reason : exit_reason) =
  let meta =
    SC.Trade_common.with_strategy strategy_id [
      ("target_mult", Float.to_string plan.target_mult);
      ("abr_prev", Float.to_string plan.abr_prev);
      ("b1_range", Float.to_string plan.b1_range);
      ("b2_follow",
       match plan.b2_follow with Follow_good -> "good" | Follow_poor -> "poor");
    ]
  in
  let qty = active.qty in
  SC.Trade.make ~qty ~r_pts:plan.r_pts cfg.cost plan.direction
    ~entry_ts:active.entry_ts ~entry_px:active.entry_price
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
        (match Trade_logic.build_trade_plan ~params:Cfg.cfg.params.exec s with
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
          SC.Session.eod_flat ~qty:active.qty ~r_pts:plan.r_pts Cfg.cfg.cost
            ~direction:plan.direction ~entry_ts:active.entry_ts
            ~entry_px:active.entry_price ~last_bar:lb
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

let make_pure_strategy cfg =
  let env =
    SC.Env.of_config
      ~session_start_min:cfg.session_start_min
      ~session_end_min:cfg.session_end_min
      ~qty:cfg.qty
      ~cost:cfg.cost
      ~exec:cfg.exec
      ()
  in
  let module S = Pure(struct let cfg = cfg end) in
  SC.Strategy_builder.make_pure ~id:strategy_id ~env
    ~build_setups:(SC.Setups.with_params ~params:cfg.params.setup Setup_builder.build)
    (module S)

let strategy_pure = make_pure_strategy default_config

let pure_strategy = make_pure_strategy
