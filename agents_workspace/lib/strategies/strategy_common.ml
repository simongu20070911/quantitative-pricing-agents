open Core
open Types

module Trade = struct
  let make ~qty ?(r_pts = 1.0) (cost : Cost_model.config)
      (direction : direction)
      ~entry_ts:(entry_ts : timestamp) ~entry_px ~exit_ts:(exit_ts : timestamp)
      ~exit_px ~reason ~(meta : (string * string) list) : trade =
    let pnl_pts =
      match direction with
      | Long  -> exit_px -. entry_px
      | Short -> entry_px -. exit_px
    in
    let pnl_R = pnl_pts /. r_pts in
    let duration_min = Float.of_int (exit_ts.minute_of_day - entry_ts.minute_of_day) in
    let t =
      {
        date = exit_ts.date;
        direction;
        entry_ts = entry_ts;
        exit_ts = exit_ts;
        entry_price = entry_px;
        exit_price = exit_px;
        qty;
        r_pts;
        pnl_pts;
        pnl_R;
        pnl_usd = 0.0;
        pnl_pct = None;
        duration_min;
        exit_reason = reason;
        meta;
      }
    in
    Cost_model.apply ~qty cost t
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
