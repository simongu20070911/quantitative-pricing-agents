open Core
open Types
open Time_utils

module SC = Strategy_common
module SS = Strategy_sig
module Abr = Indicators.Abr

(* Minimal intraday breakout: 30-minute open range (ORB) with range offset filter *)

type params = {
  open_range_minutes : int;
  breakout_k : float;
  abr_length : int;
}

type config = {
  session_start_min : int;
  session_end_min : int;
  qty : float;
  params : params;
  cost : Cost_model.config;
  exec : Execution_params.t;
}

let defaults_params = { open_range_minutes = 30; breakout_k = 1.5; abr_length = 8 }

let defaults_env =
  {
    SC.Tunables.session_start_min = rth_start_min;
    session_end_min = rth_end_min;
    qty = 1.0;
    cost =
      {
        tick_size = 0.25;
        tick_value = 12.5;
        fee_per_contract = 4.0;
        equity_base = None;
      };
  }

let make_config ~env ~params ~cost =
  {
    session_start_min = env.SC.Tunables.session_start_min;
    session_end_min = env.session_end_min;
    qty = env.qty;
    params;
    cost;
    exec = Execution_params.default ~tick_size:cost.tick_size ();
  }

let default_config =
  make_config ~env:defaults_env ~params:defaults_params ~cost:defaults_env.cost

let strategy_id = "orb30"

let clamp_range_minutes mins =
  let mins = Int.max 1 mins in
  let mins = Int.min 300 mins in
  mins

let clamp_abr_length n =
  let n = Int.max 1 n in
  let n = Int.min 500 n in
  n

let clamp_breakout_k k =
  if Float.is_nan k || not (Float.is_finite k) then 0.0 else k

let param_table : params SC.Tunables.param_field list =
  [
    {
      name = "orb.open_range_minutes";
      default = Float.of_int defaults_params.open_range_minutes;
      bounds = (5., 120.);
      integer = true;
      tunable = true;
      description = "Opening range length in minutes (from session start).";
      set =
        (fun _ v ->
          let mins = v |> Int.of_float |> clamp_range_minutes in
          { open_range_minutes = mins; breakout_k = defaults_params.breakout_k; abr_length = defaults_params.abr_length });
    };
    {
      name = "orb.breakout_k";
      default = defaults_params.breakout_k;
      bounds = (0.0, 5.0);
      integer = false;
      tunable = true;
      description = "Offset above/below range in units of ABR(abr_length).";
      set =
        (fun p v ->
          let k = clamp_breakout_k v in
          { p with breakout_k = k });
    };
    {
      name = "orb.abr_length";
      default = Float.of_int defaults_params.abr_length;
      bounds = (2., 60.);
      integer = true;
      tunable = true;
      description = "ABR window length in bars used for breakout offset.";
      set =
        (fun p v ->
          let n = v |> Int.of_float |> clamp_abr_length in
          { p with abr_length = n });
    };
  ]

let parameter_specs, config_parts_of_params =
  SC.Tunables.make ~defaults_env ~defaults_params ~param_table

let config_of_params (m : Parameters.value_map) : config =
  let env, params = config_parts_of_params m in
  let params =
    { open_range_minutes = clamp_range_minutes params.open_range_minutes;
      breakout_k = clamp_breakout_k params.breakout_k;
      abr_length = clamp_abr_length params.abr_length; }
  in
  make_config ~env ~params ~cost:env.cost

let session_window cfg = cfg.session_start_min, cfg.session_end_min
let qty cfg = cfg.qty
let cost cfg = cfg.cost
let exec cfg = cfg.exec
let with_cost ~cost cfg = { cfg with cost }
let with_open_range_minutes ~mins cfg =
  let mins = clamp_range_minutes mins in
  { cfg with params = { cfg.params with open_range_minutes = mins } }
let with_breakout_k ~k cfg =
  let k = clamp_breakout_k k in
  { cfg with params = { cfg.params with breakout_k = k } }
let with_abr_length ~n cfg =
  let n = clamp_abr_length n in
  { cfg with params = { cfg.params with abr_length = n } }
let with_session ~start ~end_ cfg = { cfg with session_start_min = start; session_end_min = end_ }
let with_qty ~qty cfg = { cfg with qty }
let with_exec ~exec cfg = { cfg with exec }

type position =
  | Flat
  | Longing
  | Shorting

type state = {
  day : Date.t option;
  open_high : float option;
  open_low : float option;
  window_complete : bool;
  position : position;
  abr : Abr.t;
  cfg : config;
}

let reset_state ~cfg date =
  { day = Some date;
    open_high = None; open_low = None; window_complete = false;
    position = Flat;
    abr = Abr.create ~n:cfg.params.abr_length;
    cfg }

let window_end_min cfg =
  cfg.session_start_min + cfg.params.open_range_minutes - 1

let update_open_range st (bar : bar_1m) ~window_end =
  if bar.ts.minute_of_day < st.cfg.session_start_min then st
  else if bar.ts.minute_of_day > window_end then st
  else
    let open_high =
      match st.open_high with
      | None -> bar.high
      | Some h -> Float.max h bar.high
    in
    let open_low =
      match st.open_low with
      | None -> bar.low
      | Some l -> Float.min l bar.low
    in
    let window_complete = bar.ts.minute_of_day >= window_end in
    { st with open_high = Some open_high; open_low = Some open_low; window_complete }

let update_abr st (bar : bar_1m) =
  let range = bar.high -. bar.low in
  Abr.update st.abr range;
  st

let abr_value st = Abr.value st.abr

let base_meta ~direction ~range_high ~range_low ~range_minutes ~k ~abr =
  [
    ("strategy", strategy_id);
    ("direction", (match direction with Long -> "long" | Short -> "short"));
    ("range_high", Float.to_string range_high);
    ("range_low", Float.to_string range_low);
    ("range_minutes", Int.to_string range_minutes);
    ("breakout_k", Float.to_string k);
    ("abr", Float.to_string abr);
  ]

let wide_level tick_size = 1_000_000. *. tick_size

let make_plan ~cfg ~direction ~entry_price ~range_high ~range_low =
  let far = wide_level cfg.cost.tick_size in
  let stop_init, target_price, cancel_level, be_trigger =
    match direction with
    | Long ->
        (entry_price -. far, entry_price +. far, entry_price -. (far /. 2.), entry_price +. far)
    | Short ->
        (entry_price +. far, entry_price -. far, entry_price +. (far /. 2.), entry_price -. far)
  in
  let range_width = Float.abs (range_high -. range_low) in
  {
    direction;
    entry_price;
    cancel_level;
    stop_init;
    r_pts = Float.max cfg.cost.tick_size range_width;
    target_mult = 0.0;
    target_price;
    be_trigger;
    b2_end_minute = cfg.session_end_min;
    downgrade_after_b2 = false;
    abr_prev = 0.0;
    b1_range = if Float.(range_width <= 0.) then cfg.cost.tick_size else range_width;
    b2_follow = Follow_good;
  }

let desired_direction ~hi ~lo ~k ~abr (bar : bar_1m) =
  let offset = k *. abr in
  let hi_t = hi +. offset in
  let lo_t = lo -. offset in
  let hit_high = Float.(bar.high >= hi_t) in
  let hit_low = Float.(bar.low <= lo_t) in
  match hit_high, hit_low with
  | true, false -> Some (Long, hi_t, lo_t)
  | false, true -> Some (Short, hi_t, lo_t)
  | true, true ->
      if Float.(bar.close >= bar.open_) then Some (Long, hi_t, lo_t)
      else Some (Short, hi_t, lo_t)
  | false, false -> None

let pos_dir = function
  | Flat -> None
  | Longing -> Some Long
  | Shorting -> Some Short

module Intent (Cfg : sig val cfg : config end) : SS.V2 = struct
  type nonrec state = state

  let init _setup_opt =
    { day = None; open_high = None; open_low = None; window_complete = false; position = Flat; abr = Abr.create ~n:Cfg.cfg.params.abr_length; cfg = Cfg.cfg }

  let step (env : SS.env) st bar =
    let st =
      match st.day with
      | None -> reset_state ~cfg:st.cfg bar.ts.date
      | Some d when not (Date.equal d bar.ts.date) ->
          reset_state ~cfg:st.cfg bar.ts.date
      | Some _ -> st
    in
    let window_end = window_end_min st.cfg in
    let in_session = SC.Session.within ~start:env.session_start_min ~end_:env.session_end_min bar in
    let st = if in_session then update_abr st bar else st in
    let st = if in_session then update_open_range st bar ~window_end else st in
    if not in_session then (st, [])
    else if not st.window_complete then (st, [])
    else
      match st.open_high, st.open_low with
      | None, _ | _, None -> (st, [])
      | Some hi, Some lo -> (
          match abr_value st with
          | None -> (st, [])
          | Some abr ->
              let desired = desired_direction ~hi ~lo ~k:st.cfg.params.breakout_k ~abr bar in
              begin match desired with
              | None -> (st, [])
              | Some (dir, _, _) when Option.equal Poly.equal (pos_dir st.position) (Some dir) ->
                  ({ st with position = st.position }, [])
              | Some (dir, hi_t, lo_t) ->
                let flatten_cmds =
                  match st.position with
                  | Flat -> []
                  | Longing | Shorting ->
                      [ SS.Flatten_all { reason = Target; meta = [ ("strategy", strategy_id); ("action", "flatten_on_reverse") ] } ]
                in
                let entry_price = (match dir with Long -> hi_t | Short -> lo_t) in
                let plan = make_plan ~cfg:st.cfg ~direction:dir ~entry_price ~range_high:hi ~range_low:lo in
                let meta =
                  base_meta ~direction:dir ~range_high:hi ~range_low:lo
                    ~range_minutes:st.cfg.params.open_range_minutes
                    ~k:st.cfg.params.breakout_k ~abr
                in
                let submit = SS.Submit_bracket { plan; qty = env.qty; meta } in
                let position = match dir with Long -> Longing | Short -> Shorting in
                ({ st with position }, flatten_cmds @ [ submit ])
              end)

  let finalize_day (_env : SS.env) st _last_bar =
    match st.position with
    | Flat -> (st, [])
    | Longing | Shorting ->
        let cmds =
          [ SS.Flatten_all { reason = Eod_flat; meta = [ ("strategy", strategy_id); ("action", "eod_flat") ] } ]
        in
        ({ st with position = Flat }, cmds)
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
