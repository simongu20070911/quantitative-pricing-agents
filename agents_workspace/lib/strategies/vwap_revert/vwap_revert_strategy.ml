open Core
open Types
open Time_utils

module F = Features
module PS = Position_sizing
module SC = Strategy_common
module SS = Strategy_sig
module Params = Vwap_revert_params

[@@@warning "-32"]

(* Config *)
type config = {
  session_start_min : int;
  session_end_min   : int;
  qty : float;
  params : Params.t;
  cost : Cost_model.config;
}

let defaults_params = Params.defaults

let defaults_env = {
  SC.Tunables.session_start_min = rth_start_min;
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

let make_config ~env ~params ~cost = {
  session_start_min = env.SC.Tunables.session_start_min;
  session_end_min = env.session_end_min;
  qty = env.qty;
  params;
  cost;
}

let default_config = make_config ~env:defaults_env ~params:defaults_params ~cost:defaults_env.cost

let strategy_id = "vwap_revert"

let param_table : Params.t SC.Tunables.param_field list =
  List.map Params.param_table ~f:(fun f ->
      { SC.Tunables.name = f.name;
        default = f.default;
        bounds = f.bounds;
        integer = f.integer;
        tunable = f.tunable;
        description = f.description;
        set = f.set; })

let default_env = defaults_env

let parameter_specs, config_parts_of_params =
  SC.Tunables.make
    ~defaults_env
    ~defaults_params:defaults_params
    ~param_table

let config_of_params (m : Parameters.value_map) : config =
  let env, params = config_parts_of_params m in
  make_config ~env ~params ~cost:env.cost

let session_window cfg = cfg.session_start_min, cfg.session_end_min
let qty cfg = cfg.qty
let cost cfg = cfg.cost
let with_cost ~cost cfg = { cfg with cost }
let with_s_entry ~s_entry cfg = { cfg with params = { cfg.params with s_entry } }
let with_z_exit ~z_exit cfg = { cfg with params = { cfg.params with z_exit } }
let with_session ~start ~end_ cfg = { cfg with session_start_min = start; session_end_min = end_ }
let with_qty ~qty cfg = { cfg with qty }

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
        let s1 = -. cfg.params.a1 *. z in
        let sign_ofi =
          if Float.(ofi_long > 0.) then 1.0
          else if Float.(ofi_long < 0.) then -1.0
          else 0.0
        in
        let s2 = -. cfg.params.a2 *. sign_ofi *. Float.abs z in
        s1 +. s2
      in
      let penalty =
        Float.exp (-. cfg.params.b1 *. rv_ratio) *. Float.exp (-. cfg.params.b2 *. trend)
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
  let stop_distance_pts = cfg.params.stop_ticks *. cfg.cost.tick_size in

  let in_session =
    bar.ts.minute_of_day >= rth_start_min && bar.ts.minute_of_day <= rth_end_min
  in
  if not in_session then ({ features; position = Flat; cfg }, [])
  else
    let signal_opt = compute_signal cfg snap in
    match state.position, signal_opt with
    | Flat, Some s when Float.(abs s >= cfg.params.s_entry) ->
        let sigma = snap.rv60 in
        let target_units = PS.vol_target_units ~max:cfg.params.max_units ~signal:s ~sigma in
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
            bar.ts.minute_of_day - entry_ts.minute_of_day >= cfg.params.time_stop_min
          in
          let z_revert =
            match snap.z_vwap with
            | Some z -> Float.(abs z <= cfg.params.z_exit)
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
            bar.ts.minute_of_day - entry_ts.minute_of_day >= cfg.params.time_stop_min
          in
          let z_revert =
            match snap.z_vwap with
            | Some z -> Float.(abs z <= cfg.params.z_exit)
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
  let env =
    SC.Env.of_config
      ~session_start_min:cfg.session_start_min
      ~session_end_min:cfg.session_end_min
      ~qty:cfg.qty
      ~cost:cfg.cost
  in
  let module S = Pure(struct let cfg = cfg end) in
  SC.Strategy_builder.make_pure ~id:strategy_id ~env (module S)

let strategy_pure = pure_strategy default_config
