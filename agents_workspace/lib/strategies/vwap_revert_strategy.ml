open Core
open Types
open Time_utils

module F = Features
module PS = Position_sizing
module SC = Strategy_common
module SS = Strategy_sig

(* Config *)
type config = {
  a1 : float;
  a2 : float;
  b1 : float;
  b2 : float;
  s_entry : float;
  z_exit  : float;
  time_stop_min : int;
  stop_ticks : float;
  max_units : int;
  cost : Cost_model.config;
}

let default_config = {
  a1 = 1.0;
  a2 = 0.5;
  b1 = 0.5;
  b2 = 0.5;
  s_entry = 0.7;
  z_exit = 0.2;
  time_stop_min = 10;
  stop_ticks = 6.0;
  max_units = 3;
  cost = {
    tick_size = 0.25;
    tick_value = 12.5;
    slippage_roundtrip_ticks = 1.0;
    fee_per_contract = 4.0;
    equity_base = None;
  };
}

let strategy_id = "vwap_revert"

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
    { name = "a1"; default = default_config.a1; bounds = (0., 5.); integer = fl; tunable = true;
      description = "scale on z-score distance from VWAP";
      set = (fun cfg v -> { cfg with a1 = v }) };
    { name = "a2"; default = default_config.a2; bounds = (0., 5.); integer = fl; tunable = true;
      description = "OFI exhaustion term weight";
      set = (fun cfg v -> { cfg with a2 = v }) };
    { name = "b1"; default = default_config.b1; bounds = (0., 5.); integer = fl; tunable = true;
      description = "volatility penalty";
      set = (fun cfg v -> { cfg with b1 = v }) };
    { name = "b2"; default = default_config.b2; bounds = (0., 5.); integer = fl; tunable = true;
      description = "trend penalty";
      set = (fun cfg v -> { cfg with b2 = v }) };
    { name = "s_entry"; default = default_config.s_entry; bounds = (0.1, 3.0); integer = fl; tunable = true;
      description = "signal threshold to open a trade";
      set = (fun cfg v -> { cfg with s_entry = v }) };
    { name = "z_exit"; default = default_config.z_exit; bounds = (0., 1.0); integer = fl; tunable = true;
      description = "VWAP reversion exit band";
      set = (fun cfg v -> { cfg with z_exit = v }) };
    { name = "time_stop_min"; default = Float.of_int default_config.time_stop_min; bounds = (1., 240.); integer = int; tunable = true;
      description = "time-based exit in minutes";
      set = (fun cfg v ->
          let v' = v |> Int.of_float |> Int.clamp_exn ~min:1 ~max:240 in
          { cfg with time_stop_min = v' }) };
    { name = "stop_ticks"; default = default_config.stop_ticks; bounds = (1., 30.); integer = fl; tunable = true;
      description = "hard stop distance in ticks";
      set = (fun cfg v -> { cfg with stop_ticks = v }) };
    { name = "max_units"; default = Float.of_int default_config.max_units; bounds = (1., 10.); integer = int; tunable = true;
      description = "cap on volatility-targeted units";
      set = (fun cfg v ->
          let v' = v |> Int.of_float |> Int.clamp_exn ~min:1 ~max:100 in
          { cfg with max_units = v' }) };
    { name = "cost.slippage_roundtrip_ticks"; default = default_config.cost.slippage_roundtrip_ticks; bounds = (0., 3.); integer = fl; tunable = true;
      description = "assumed round-trip slippage in ticks";
      set = (fun cfg v -> { cfg with cost = { cfg.cost with slippage_roundtrip_ticks = v } }) };
    { name = "cost.fee_per_contract"; default = default_config.cost.fee_per_contract; bounds = (0., 10.); integer = fl; tunable = true;
      description = "exchange+broker fee per contract";
      set = (fun cfg v -> { cfg with cost = { cfg.cost with fee_per_contract = v } }) };
    { name = "cost.equity_base"; default = Option.value default_config.cost.equity_base ~default:0.; bounds = (0., 5_000_000.); integer = fl; tunable = false;
      description = "account equity in USD for pct PnL; 0 disables";
      set = (fun cfg v ->
          let eb = if Float.(v <= 0.) then None else Some v in
          { cfg with cost = { cfg.cost with equity_base = eb } }) };
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

let cost cfg = cfg.cost
let with_cost ~cost cfg = { cfg with cost }
let with_s_entry ~s_entry cfg = { cfg with s_entry }
let with_z_exit ~z_exit cfg = { cfg with z_exit }

type position =
  | Flat
  | Long_pos of { entry_ts : timestamp; entry_price : float; target_units : int }
  | Short_pos of { entry_ts : timestamp; entry_price : float; target_units : int }

type state = {
  features : F.state;
  position : position;
  cfg : config;
}

let init_state cfg = { features = F.create (); position = Flat; cfg }

let init_day cfg (_setup_opt : setup option) = init_state cfg

let compute_signal cfg (snap : F.snapshot) : float option =
  match snap.z_vwap, snap.ofi_long, snap.rv_ratio, snap.trend with
  | Some z, Some ofi_long, Some rv_ratio, Some trend ->
      let s_mr =
        let s1 = -. cfg.a1 *. z in
        let sign_ofi =
          if Float.(ofi_long > 0.) then 1.0
          else if Float.(ofi_long < 0.) then -1.0
          else 0.0
        in
        let s2 = -. cfg.a2 *. sign_ofi *. Float.abs z in
        s1 +. s2
      in
      let penalty =
        Float.exp (-. cfg.b1 *. rv_ratio) *. Float.exp (-. cfg.b2 *. trend)
      in
      Some (s_mr *. penalty)
  | _ -> None

let build_trade ~direction ~(entry_time : timestamp) ~entry_price
    ~(exit_time : timestamp) ~(exit_price : float)
    ~(reason : exit_reason) ~(target_units : int) ~(state : state)
    ~(meta : (string * string) list) : trade =
  SC.Trade.make
    ~qty:(Float.of_int target_units) ~r_pts:1.0 state.cfg.cost direction
    ~entry_ts:entry_time ~entry_px:entry_price
    ~exit_ts:exit_time ~exit_px:exit_price ~reason
    ~meta:(("strategy", strategy_id) :: ("target_units", Int.to_string target_units) :: meta)

let on_bar (state : state) (bar : bar_1m) : state * trade list =
  let features = F.update state.features bar in
  let snap = F.snapshot features in
  let cfg = state.cfg in
  let stop_distance_pts = cfg.stop_ticks *. cfg.cost.tick_size in

  let in_session =
    bar.ts.minute_of_day >= rth_start_min && bar.ts.minute_of_day <= rth_end_min
  in
  if not in_session then ({ features; position = Flat; cfg }, [])
  else
    let signal_opt = compute_signal cfg snap in
    match state.position, signal_opt with
    | Flat, Some s when Float.(abs s >= cfg.s_entry) ->
        let sigma = snap.rv60 in
        let target_units = PS.vol_target_units ~max:cfg.max_units ~signal:s ~sigma in
        if target_units <= 0 then ({ features; position = Flat; cfg }, [])
        else
          let direction = if Float.(s > 0.) then Long else Short in
          let position =
            match direction with
            | Long  -> Long_pos { entry_ts = bar.ts; entry_price = bar.close; target_units }
            | Short -> Short_pos { entry_ts = bar.ts; entry_price = bar.close; target_units }
          in
          ({ features; position; cfg }, [])
    | Flat, _ -> ({ features; position = Flat; cfg }, [])
    | Long_pos pos, signal_opt ->
        let entry_ts = pos.entry_ts in
        let entry_price = pos.entry_price in
        let stop_price = entry_price -. stop_distance_pts in
        let stopped = Float.(bar.low <= stop_price) in
        if stopped then
          let trade =
            build_trade
              ~direction:Long ~entry_time:entry_ts ~entry_price
              ~exit_time:bar.ts ~exit_price:stop_price ~reason:Stop
              ~target_units:pos.target_units ~state ~meta:[]
          in
          ({ features; position = Flat; cfg }, [ trade ])
        else
          let time_exit =
            bar.ts.minute_of_day - entry_ts.minute_of_day >= cfg.time_stop_min
          in
          let z_revert =
            match snap.z_vwap with
            | Some z -> Float.(abs z <= cfg.z_exit)
            | None -> false
          in
          let opp_signal =
            match signal_opt with
            | Some s when Float.(s <= 0.) -> true
            | _ -> false
          in
          if time_exit || z_revert || opp_signal then
            let trade =
              build_trade
                ~direction:Long ~entry_time:entry_ts ~entry_price
                ~exit_time:bar.ts ~exit_price:bar.close ~reason:Target
                ~target_units:pos.target_units ~state ~meta:[]
            in
            ({ features; position = Flat; cfg }, [ trade ])
          else
            ({ features; position = Long_pos pos; cfg }, [])
    | Short_pos pos, signal_opt ->
        let entry_ts = pos.entry_ts in
        let entry_price = pos.entry_price in
        let stop_price = entry_price +. stop_distance_pts in
        let stopped = Float.(bar.high >= stop_price) in
        if stopped then
          let trade =
            build_trade
              ~direction:Short ~entry_time:entry_ts ~entry_price
              ~exit_time:bar.ts ~exit_price:stop_price ~reason:Stop
              ~target_units:pos.target_units ~state ~meta:[]
          in
          ({ features; position = Flat; cfg }, [ trade ])
        else
          let time_exit =
            bar.ts.minute_of_day - entry_ts.minute_of_day >= cfg.time_stop_min
          in
          let z_revert =
            match snap.z_vwap with
            | Some z -> Float.(abs z <= cfg.z_exit)
            | None -> false
          in
          let opp_signal =
            match signal_opt with
            | Some s when Float.(s >= 0.) -> true
            | _ -> false
          in
          if time_exit || z_revert || opp_signal then
            let trade =
              build_trade
                ~direction:Short ~entry_time:entry_ts ~entry_price
                ~exit_time:bar.ts ~exit_price:bar.close ~reason:Target
                ~target_units:pos.target_units ~state ~meta:[]
            in
            ({ features; position = Flat; cfg }, [ trade ])
          else
            ({ features; position = Short_pos pos; cfg }, [])

let on_session_end (state : state) (last_bar : bar_1m option) : state * trade list =
  match state.position, last_bar with
  | Flat, _ -> (state, [])
  | (Long_pos _ | Short_pos _), None -> ({ state with position = Flat }, [])
  | Long_pos pos, Some lb ->
      let trade =
        build_trade
          ~direction:Long ~entry_time:pos.entry_ts ~entry_price:pos.entry_price
          ~exit_time:lb.ts ~exit_price:lb.close ~reason:Eod_flat
          ~target_units:pos.target_units ~state ~meta:[]
      in
      ({ state with position = Flat }, [ trade ])
  | Short_pos pos, Some lb ->
      let trade =
        build_trade
          ~direction:Short ~entry_time:pos.entry_ts ~entry_price:pos.entry_price
          ~exit_time:lb.ts ~exit_price:lb.close ~reason:Eod_flat
          ~target_units:pos.target_units ~state ~meta:[]
      in
      ({ state with position = Flat }, [ trade ])

module Pure (Cfg : sig val cfg : config end) : SS.S with type state = state = struct
  type nonrec state = state
  let init setup = init_day Cfg.cfg setup

  let step (env : SS.env) st bar =
    let in_session =
      bar.ts.minute_of_day >= env.session_start_min
      && bar.ts.minute_of_day <= env.session_end_min
    in
    if not in_session then ({ st with position = Flat }, [])
    else on_bar st bar

  let finalize_day (_env : SS.env) st last_bar = on_session_end st last_bar
end

let pure_strategy cfg =
  let env = {
    SS.session_start_min = rth_start_min;
    session_end_min = rth_end_min;
    qty = 1.0;
    cost = cfg.cost;
  } in
  let module S = Pure(struct let cfg = cfg end) in
  {
    Engine._id = strategy_id;
    env;
    build_setups = None;
    strategy = (module S);
  }

let strategy_pure = pure_strategy default_config

let legacy_strategy cfg =
  let pure = pure_strategy cfg in
  Engine.legacy_of_pure ~id:strategy_id ~env:pure.env ?build_setups:pure.build_setups pure.strategy

let strategy = legacy_strategy default_config
