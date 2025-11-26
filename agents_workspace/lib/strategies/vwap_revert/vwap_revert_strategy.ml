open Core
open Types
open Time_utils

module F = Features
module PS = Position_sizing
module SC = Strategy_common
module SS = Strategy_sig
module Params = Vwap_revert_params

(* Config *)
type config = {
  session_start_min : int;
  session_end_min   : int;
  qty : float;
  params : Params.t;
  cost : Cost_model.config;
  exec : Execution_params.t;
}

let defaults_params = Params.defaults

let defaults_env = {
  SC.Tunables.session_start_min = rth_start_min;
  session_end_min = rth_end_min;
  qty = 1.0;
  cost = {
    tick_size = 0.25;
    tick_value = 12.5;
    fee_per_contract = 0.0;
    equity_base = None;
  };
}

let make_config ~env ~params ~cost = {
  session_start_min = env.SC.Tunables.session_start_min;
  session_end_min = env.session_end_min;
  qty = env.qty;
  params;
  cost;
  exec = Execution_params.default ~tick_size:cost.tick_size ();
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
let exec cfg = cfg.exec
let with_cost ~cost cfg = { cfg with cost }
let with_s_entry ~s_entry cfg = { cfg with params = { cfg.params with s_entry } }
let with_z_exit ~z_exit cfg = { cfg with params = { cfg.params with z_exit } }
let with_session ~start ~end_ cfg = { cfg with session_start_min = start; session_end_min = end_ }
let with_qty ~qty cfg = { cfg with qty }
let with_exec ~exec cfg = { cfg with exec }

type position =
  | Flat
  | Long_pos of {
      entry_ts : timestamp;
      entry_price : float;
      stop_price : float;
      target_units : int;
      plan : trade_plan;
    }
  | Short_pos of {
      entry_ts : timestamp;
      entry_price : float;
      stop_price : float;
      target_units : int;
      plan : trade_plan;
    }

type state = {
  features : F.state;
  position : position;
  cfg : config;
}

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

let make_plan ~(cfg : config) ~(direction : direction) ~(entry_price : float) : trade_plan * float =
  let stop_distance = cfg.params.stop_ticks *. cfg.cost.tick_size in
  let stop_init =
    match direction with
    | Long -> entry_price -. stop_distance
    | Short -> entry_price +. stop_distance
  in
  let far = 1e6 *. cfg.cost.tick_size in
  let target_price =
    match direction with
    | Long -> entry_price +. far
    | Short -> entry_price -. far
  in
  let cancel_level =
    match direction with
    | Long -> entry_price -. (2. *. stop_distance)
    | Short -> entry_price +. (2. *. stop_distance)
  in
  let plan =
    { direction;
      entry_price;
      cancel_level;
      stop_init;
      r_pts = stop_distance;
      target_mult = 0.0;
      target_price;
      be_trigger = target_price;
      b2_end_minute = cfg.session_end_min;
      downgrade_after_b2 = false;
      abr_prev = 0.0;
      b1_range = stop_distance;
      b2_follow = Follow_good;
    }
  in
  plan, stop_init

let base_meta ~target_units =
  [ ("strategy", strategy_id); ("target_units", Int.to_string target_units) ]

module Intent (Cfg : sig val cfg : config end) : SS.V2 = struct
  type nonrec state = state

  let init setup_opt =
    let features = F.create () in
    let position = Flat in
    let cfg = Cfg.cfg in
    match setup_opt with
    | None -> { features; position; cfg }
    | Some _ -> { features; position; cfg }

  let step (env : SS.env) st bar =
    let features = F.update st.features bar in
    let snap = F.snapshot features in
    let cfg = st.cfg in
    let in_session =
      bar.ts.minute_of_day >= env.session_start_min
      && bar.ts.minute_of_day <= env.session_end_min
    in
    if not in_session then ({ st with features }, [])
    else
      let signal_opt = compute_signal cfg snap in
      match st.position, signal_opt with
      | Flat, Some s when Float.(abs s >= cfg.params.s_entry) ->
          let sigma = snap.rv60 in
          let target_units = PS.vol_target_units ~max:cfg.params.max_units ~signal:s ~sigma in
          if target_units <= 0 then ({ st with features; position = Flat }, [])
          else
            let direction = if Float.(s > 0.) then Long else Short in
            let plan, stop_price = make_plan ~cfg ~direction ~entry_price:bar.close in
            let meta = base_meta ~target_units in
            let position =
              match direction with
              | Long  -> Long_pos { entry_ts = bar.ts; entry_price = bar.close; stop_price; target_units; plan }
              | Short -> Short_pos { entry_ts = bar.ts; entry_price = bar.close; stop_price; target_units; plan }
            in
            let cmds = [ SS.Submit_bracket { plan; qty = Float.of_int target_units; meta } ] in
            ({ features; position; cfg }, cmds)
      | Flat, _ -> ({ st with features; position = Flat }, [])
      | Long_pos pos, _ ->
          let stop_hit = Float.(bar.low <= pos.stop_price) in
          if stop_hit then
            ({ features; position = Flat; cfg }, [])
          else
            let time_exit =
              bar.ts.minute_of_day - pos.entry_ts.minute_of_day >= cfg.params.time_stop_min
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
              let cmds = [ SS.Flatten_all { reason = Target; meta = [] } ] in
              ({ features; position = Flat; cfg }, cmds)
            else
              ({ st with features; position = Long_pos pos }, [])
      | Short_pos pos, _ ->
          let stop_hit = Float.(bar.high >= pos.stop_price) in
          if stop_hit then
            ({ features; position = Flat; cfg }, [])
          else
            let time_exit =
              bar.ts.minute_of_day - pos.entry_ts.minute_of_day >= cfg.params.time_stop_min
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
              let cmds = [ SS.Flatten_all { reason = Target; meta = [] } ] in
              ({ features; position = Flat; cfg }, cmds)
            else
              ({ st with features; position = Short_pos pos }, [])

  let finalize_day (_env : SS.env) st _last_bar =
    match st.position with
    | Flat -> (st, [])
    | Long_pos _ | Short_pos _ ->
        ({ st with position = Flat }, [ SS.Flatten_all { reason = Eod_flat; meta = [] } ])
end

let pure_strategy cfg =
  let env =
    SC.Env.of_config
      ~session_start_min:cfg.session_start_min
      ~session_end_min:cfg.session_end_min
      ~qty:cfg.qty
      ~cost:cfg.cost
      ~exec:cfg.exec
      ()
  in
  let module S = Intent(struct let cfg = cfg end) in
  SC.Strategy_builder.make_pure ~id:strategy_id ~env (module S)

let strategy_pure = pure_strategy default_config
