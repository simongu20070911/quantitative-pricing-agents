open Core
open Types

module Trade = struct
  let make ~qty ?(r_pts = 1.0) (cost : Cost_model.config)
      (direction : direction)
      ~entry_ts:(entry_ts : timestamp) ~entry_px ~exit_ts:(exit_ts : timestamp)
      ~exit_px ~reason ~(meta : (string * string) list) : trade =
    Trade_base.make_raw ~qty ~r_pts ~direction ~entry_ts ~entry_px
      ~exit_ts ~exit_px ~exit_reason:reason ~meta
    |> Trade_base.apply_costs ~qty cost
end

module Config = struct
  let cost_params (defaults : Cost_model.config) : Parameters.t list =
    [
      Parameters.make ~name:"cost.slippage_roundtrip_ticks"
        ~default:defaults.slippage_roundtrip_ticks ~bounds:(0., 3.)
        ~description:"assumed round-trip slippage in ticks" ();
      Parameters.make ~name:"cost.fee_per_contract"
        ~default:defaults.fee_per_contract ~bounds:(0., 10.)
        ~description:"exchange+broker fee per contract" ();
      Parameters.make ~name:"cost.equity_base"
        ~default:(Option.value defaults.equity_base ~default:0.)
        ~bounds:(0., 5_000_000.)
        ~description:"account equity in USD for pct PnL; 0 => disabled" ();
    ]

  let cost_of_params ~(defaults : Cost_model.config) (m : Parameters.value_map) : Cost_model.config =
    let get name default = Map.find m name |> Option.value ~default in
    let equity_base =
      let eb = get "cost.equity_base" (Option.value defaults.equity_base ~default:0.) in
      if Float.(eb <= 0.) then None else Some eb
    in
    {
      tick_size = defaults.tick_size;
      tick_value = defaults.tick_value;
      slippage_roundtrip_ticks = get "cost.slippage_roundtrip_ticks" defaults.slippage_roundtrip_ticks;
      fee_per_contract = get "cost.fee_per_contract" defaults.fee_per_contract;
      equity_base;
    }

  let session_params ~default_start ~default_end : Parameters.t list =
    [
      Parameters.make ~name:"session_start_min" ~default:(Float.of_int default_start)
        ~bounds:(0., 24. *. 60.) ~integer:true
        ~description:"session start minute-of-day (ET)" ();
      Parameters.make ~name:"session_end_min" ~default:(Float.of_int default_end)
        ~bounds:(0., 24. *. 60.) ~integer:true
        ~description:"session end minute-of-day (ET)" ();
    ]

  let session_of_params ~defaults:(d_start, d_end) (m : Parameters.value_map) : int * int =
    let get name default = Map.find m name |> Option.value ~default in
    let ss = get "session_start_min" (Float.of_int d_start) |> Int.of_float in
    let se = get "session_end_min" (Float.of_int d_end) |> Int.of_float in
    let clamp = Int.clamp_exn ~min:0 ~max:1439 in
    (clamp ss, clamp se)
end

module Session = struct
  let within ~start ~end_ (bar : bar_1m) =
    let m = bar.ts.minute_of_day in
    m >= start && m <= end_

  let eod_flat ~qty ?r_pts (cost : Cost_model.config)
      ~(direction : direction) ~entry_ts ~entry_px ~(last_bar : bar_1m)
      ~(meta : (string * string) list) : trade =
    Trade.make ~qty ?r_pts cost direction
      ~entry_ts ~entry_px ~exit_ts:last_bar.ts ~exit_px:last_bar.close
      ~reason:Eod_flat
      ~meta
end

module Setup = struct
  let noop (_filename : string) : setup Core.Date.Table.t = Core.Date.Table.create ()
end

module Setups = struct
  let with_params ~params f filename = f params filename
end

module Strategy_builder = struct
  let make_pure ~id ~env ?build_setups (module S : Strategy_sig.S) : Engine.pure_strategy =
    { Engine._id = id; env; build_setups; strategy = (module S) }
end

module Trade_common = struct
  let with_strategy id meta = ("strategy", id) :: meta
end

module Tunables = struct
  type env_defaults = {
    session_start_min : int;
    session_end_min   : int;
    qty : float;
    cost : Cost_model.config;
  }

  let env_specs ~defaults =
    Config.session_params ~default_start:defaults.session_start_min
      ~default_end:defaults.session_end_min
    @ [
        Parameters.make ~name:"qty" ~default:defaults.qty ~bounds:(0.1, 20.)
          ~description:"contracts per trade" ();
      ]
    @ Config.cost_params defaults.cost

  let env_of_params ~defaults (m : Parameters.value_map) : env_defaults =
    let session_start_min, session_end_min =
      Config.session_of_params
        ~defaults:(defaults.session_start_min, defaults.session_end_min) m
    in
    let qty = Map.find m "qty" |> Option.value ~default:defaults.qty in
    let cost = Config.cost_of_params ~defaults:defaults.cost m in
    { session_start_min; session_end_min; qty; cost }

  type 'p param_field = {
    name : string;
    default : float;
    bounds : float * float;
    integer : bool;
    tunable : bool;
    description : string;
    set : 'p -> float -> 'p;
  }

  let parameter_specs_of_table (tbl : 'p param_field list) : Parameters.t list =
    tbl
    |> List.filter ~f:(fun f -> f.tunable)
    |> List.map ~f:(fun f ->
        Parameters.make ~name:f.name ~default:f.default ~bounds:f.bounds
          ~integer:f.integer ~description:f.description ())

  let params_of_table ~defaults ~param_table (m : Parameters.value_map) =
    List.fold param_table ~init:defaults ~f:(fun acc f ->
        match Map.find m f.name with
        | None -> acc
        | Some v -> f.set acc v)

  let make ~defaults_env ~defaults_params ~param_table =
    let specs =
      env_specs ~defaults:defaults_env
      @ parameter_specs_of_table param_table
    in
    let build m =
      let env = env_of_params ~defaults:defaults_env m in
      let params = params_of_table ~defaults:defaults_params ~param_table m in
      env, params
    in
    specs, build
end

module Env = struct
  let of_config ~session_start_min ~session_end_min ~qty ~cost : Strategy_sig.env =
    { Strategy_sig.session_start_min; session_end_min; qty; cost }
end
