open Core
open Types
open Time_utils
module SC = Strategy_common
module TT = Trade_transition
module C = B1b2_constants

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
    tick_size = C.tick_size;
    tick_value = C.tick_value;
    slippage_roundtrip_ticks = 1.0;
    fee_per_contract = 4.0;
    equity_base = None;
  };
}

let strategy_id = "b1b2"

type param_field = {
  name : string;
  default : float;
  bounds : float * float;
  integer : bool;
  tunable : bool;
  description : string;
  set : config -> float -> config;
}

let param_table : param_field list =
  let int = true and fl = false in
  [
    { name = "session_start_min"; default = Float.of_int default_config.session_start_min;
      bounds = (0., 1440.); integer = int; tunable = true; description = "session start minute-of-day (ET)";
      set = (fun cfg v -> { cfg with session_start_min = Int.of_float v |> Int.clamp_exn ~min:0 ~max:1439 }) };
    { name = "session_end_min"; default = Float.of_int default_config.session_end_min;
      bounds = (0., 1440.); integer = int; tunable = true; description = "session end minute-of-day (ET)";
      set = (fun cfg v -> { cfg with session_end_min = Int.of_float v |> Int.clamp_exn ~min:0 ~max:1439 }) };
    { name = "qty"; default = default_config.qty; bounds = (0.1, 20.); integer = fl; tunable = true;
      description = "contracts per trade"; set = (fun cfg v -> { cfg with qty = v }) };
    { name = "cost.slippage_roundtrip_ticks"; default = default_config.cost.slippage_roundtrip_ticks;
      bounds = (0., 3.); integer = fl; tunable = true; description = "assumed round-trip slippage in ticks";
      set = (fun cfg v -> { cfg with cost = { cfg.cost with slippage_roundtrip_ticks = v } }) };
    { name = "cost.fee_per_contract"; default = default_config.cost.fee_per_contract;
      bounds = (0., 10.); integer = fl; tunable = true; description = "exchange+broker fee per contract";
      set = (fun cfg v -> { cfg with cost = { cfg.cost with fee_per_contract = v } }) };
    { name = "cost.equity_base"; default = Option.value default_config.cost.equity_base ~default:0.;
      bounds = (0., 5_000_000.); integer = fl; tunable = false; description = "account equity in USD for pct PnL; 0 disables";
      set = (fun cfg v -> { cfg with cost = { cfg.cost with equity_base = if Float.(v <= 0.) then None else Some v } }) };
    (* Fixed domain constants below, marked tunable=false for now. Flip to true to expose to optimizer. *)
    { name = "be_trigger_mult"; default = C.be_trigger_mult; bounds = (0.1, 1.0); integer = fl; tunable = false;
      description = "break-even trigger in R"; set = (fun cfg _ -> cfg) };
    { name = "downgrade_grace_min"; default = Float.of_int C.downgrade_cutoff_offset_min; bounds = (0., 30.); integer = int; tunable = false;
      description = "minutes after B2 to allow downgrade"; set = (fun cfg _ -> cfg) };
    { name = "twoR_range_factor"; default = C.two_r_range_factor; bounds = (0.5, 3.); integer = fl; tunable = false;
      description = "max B1 range / ABR_prev to allow 2R"; set = (fun cfg _ -> cfg) };
    { name = "climactic_range_factor"; default = C.climactic_range_factor; bounds = (1.0, 5.0); integer = fl; tunable = false;
      description = "max B1 range / ABR_prev to be valid"; set = (fun cfg _ -> cfg) };
    { name = "gap_pct_low"; default = C.gap_min_pct_adr; bounds = (0., 100.); integer = fl; tunable = false;
      description = "min |gap| %% ADR to qualify"; set = (fun cfg _ -> cfg) };
    { name = "gap_pct_high"; default = C.gap_max_pct_adr; bounds = (0., 100.); integer = fl; tunable = false;
      description = "max |gap| %% ADR to qualify"; set = (fun cfg _ -> cfg) };
    { name = "body_pct_min"; default = C.body_pct_min; bounds = (0., 1.); integer = fl; tunable = false;
      description = "min body/range for trend"; set = (fun cfg _ -> cfg) };
    { name = "ibs_bull_min"; default = C.ibs_bull_min; bounds = (0., 1.); integer = fl; tunable = false;
      description = "IBS threshold bullish"; set = (fun cfg _ -> cfg) };
    { name = "ibs_bear_max"; default = C.ibs_bear_max; bounds = (0., 1.); integer = fl; tunable = false;
      description = "IBS threshold bearish"; set = (fun cfg _ -> cfg) };
  ]

let parameter_specs =
  param_table
  |> List.filter ~f:(fun p -> p.tunable)
  |> List.map ~f:(fun p ->
      Parameters.make ~name:p.name ~default:p.default ~bounds:p.bounds
        ~integer:p.integer ~description:p.description ())

let config_of_params (m : Parameters.value_map) : config =
  List.fold param_table ~init:default_config ~f:(fun cfg p ->
      match Map.find m p.name with
      | None -> cfg
      | Some v -> p.set cfg v)

let session_window cfg = cfg.session_start_min, cfg.session_end_min
let qty cfg = cfg.qty
let cost cfg = cfg.cost
let with_cost ~cost cfg = { cfg with cost }
let with_qty ~qty cfg = { cfg with qty }
let with_session ~start ~end_ cfg = { cfg with session_start_min = start; session_end_min = end_ }

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

let legacy_strategy cfg =
  let pure = make_pure_strategy cfg in
  Engine.legacy_of_pure ~id:strategy_id ~env:pure.env ?build_setups:pure.build_setups pure.strategy

let strategy = legacy_strategy default_config

let pure_strategy = make_pure_strategy
