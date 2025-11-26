open Core
open Strategy_fast
open Types

module PS = Core.Position_sizing
module CM = Core.Cost_model
module TB = Strategy_fast__Trade_base
module Summary = Summary

let approx ?(tol = 1e-6) a b = Float.(abs (a -. b) <= tol)

let date s = Date.of_string s
let ts date minute_of_day = { date; minute_of_day }

(* Position sizing -------------------------------------------------------- *)

let%test_unit "vol_target_units handles missing sigma and nonpositive vol" =
  [%test_eq: int] (PS.vol_target_units ~max:5 ~signal:2.0 ~sigma:None) 0;
  [%test_eq: int] (PS.vol_target_units ~max:5 ~signal:2.0 ~sigma:(Some (-1.))) 0;
  [%test_eq: int] (PS.vol_target_units ~max:5 ~signal:2.0 ~sigma:(Some 0.)) 0

let%test_unit "vol_target_units floors, caps, and uses absolute signal" =
  [%test_eq: int]
    (PS.vol_target_units ~max:10 ~signal:(-3.4) ~sigma:(Some 1.1))
    3;
  [%test_eq: int]
    (PS.vol_target_units ~max:2 ~signal:10.0 ~sigma:(Some 1.0))
    2

(* Cost model and execution slippage ------------------------------------ *)

let%test_unit "cost_model applies fees and pct PnL (slippage handled in execution)" =
  let cfg = {
    CM.tick_size = 0.25;
    tick_value = 12.5;
    fee_per_contract = 2.0;
    equity_base = Some 100_000.;
  } in
  let entry_ts = ts (date "2020-01-01") 0 in
  let exit_ts = ts (date "2020-01-01") 10 in
  let raw =
    TB.make_raw ~qty:1.0 ~r_pts:1.0 ~direction:Long
      ~entry_ts ~entry_px:100.0 ~exit_ts ~exit_px:102.0
      ~exit_reason:Target ~meta:[]
  in
  let qty = 3.0 in
  let t = CM.apply ~qty cfg raw in
  let dollars_per_point = cfg.tick_value /. cfg.tick_size in
  let expected_net_usd =
    (raw.pnl_pts *. dollars_per_point *. qty)
    -. (cfg.fee_per_contract *. qty)
  in
  assert (approx t.pnl_usd expected_net_usd);
  let expected_pts = expected_net_usd /. dollars_per_point /. qty in
  assert (approx t.pnl_pts expected_pts);
  assert (approx t.pnl_R (expected_pts /. raw.r_pts));
  (match t.pnl_pct with
   | None -> assert false
   | Some pct -> assert (approx pct (expected_net_usd /. Option.value_exn cfg.equity_base)))

let%test_unit "execution_model applies constant tick slippage per side" =
  let module EM = Strategy_fast.Engine.Execution_model in
  let module EP = Strategy_fast.Engine.Execution_params in
  let params =
    let base = EP.default ~tick_size:0.25 () in
    { base with
      slip_model = EP.Constant_ticks 1.0;
      spread_ticks = 0.0;
    }
  in
  let rng = EM.make_rng ~seed:42 () in
  let buy_px = EM.adjust_price ~params ~side:EM.Buy ~rng 100.0 in
  let sell_px = EM.adjust_price ~params ~side:EM.Sell ~rng 100.0 in
  assert (approx buy_px 100.25);
  assert (approx sell_px 99.75)

let%test_unit "pct PnL is dropped when equity base is zero" =
  let cfg = {
    CM.tick_size = 0.25;
    tick_value = 12.5;
    fee_per_contract = 0.0;
    equity_base = Some 0.0;
  } in
  let t =
    TB.make_raw ~qty:1.0 ~r_pts:1.0 ~direction:Long
      ~entry_ts:(ts (date "2020-01-02") 0) ~entry_px:100.0
      ~exit_ts:(ts (date "2020-01-02") 5) ~exit_px:101.0
      ~exit_reason:Target ~meta:[]
  in
  let applied = CM.apply ~qty:1.0 cfg t in
  assert (Option.is_none applied.pnl_pct)

let%test_unit "cost_model keeps original R when r_pts is nonpositive" =
  let cfg = {
    CM.tick_size = 0.25;
    tick_value = 12.5;
    fee_per_contract = 0.0;
    equity_base = None;
  } in
  let base = {
    date = date "2020-01-03";
    direction = Long;
    entry_ts = ts (date "2020-01-03") 0;
    exit_ts = ts (date "2020-01-03") 1;
    entry_price = 100.0;
    exit_price = 99.0;
    qty = 1.0;
    r_pts = 0.0;
    pnl_pts = -1.0;
    pnl_R = -1.2;
    pnl_usd = 0.0;
    pnl_pct = None;
    duration_min = 1.0;
    exit_reason = Stop;
    meta = [];
  } in
  let applied = CM.apply ~qty:1.0 cfg base in
  assert (approx applied.pnl_R base.pnl_R);
  assert (approx applied.pnl_pts base.pnl_pts)

(* Trade_base ------------------------------------------------------------- *)

let%test_unit "make_raw computes long pnl and duration" =
  let entry_ts = ts (date "2020-02-01") 600 in
  let exit_ts = ts (date "2020-02-01") 640 in
  let t =
    TB.make_raw ~qty:2.0 ~r_pts:1.5 ~direction:Long
      ~entry_ts ~entry_px:100.0 ~exit_ts ~exit_px:102.5
      ~exit_reason:Target ~meta:[]
  in
  assert (approx t.pnl_pts 2.5);
  assert (approx t.pnl_R (2.5 /. 1.5));
  assert (approx t.duration_min 40.0);
  assert (Float.(t.qty = 2.0))

let%test_unit "make_raw computes short pnl" =
  let entry_ts = ts (date "2020-02-02") 100 in
  let exit_ts = ts (date "2020-02-02") 130 in
  let t =
    TB.make_raw ~qty:1.0 ~r_pts:1.0 ~direction:Short
      ~entry_ts ~entry_px:100.0 ~exit_ts ~exit_px:95.0
      ~exit_reason:Target ~meta:[]
  in
  assert (approx t.pnl_pts 5.0);
  assert (approx t.pnl_R 5.0)

let%test_unit "make_raw rejects exit timestamp before entry" =
  let entry_ts = ts (date "2020-02-03") 120 in
  let exit_ts = ts (date "2020-02-03") 110 in
  [%test_result: bool]
    (Exn.does_raise (fun () ->
         TB.make_raw ~qty:1.0 ~r_pts:1.0 ~direction:Long
           ~entry_ts ~entry_px:100.0 ~exit_ts ~exit_px:101.0
           ~exit_reason:Stop ~meta:[]))
    ~expect:true

let%test_unit "make_raw handles cross-day durations" =
  let entry_ts = ts (date "2020-02-04") 1430 in
  let exit_ts = ts (date "2020-02-05") 10 in
  let t =
    TB.make_raw ~qty:1.0 ~r_pts:1.0 ~direction:Long
      ~entry_ts ~entry_px:100.0 ~exit_ts ~exit_px:100.5
      ~exit_reason:Eod_flat ~meta:[]
  in
  assert (approx t.duration_min 20.0)

(* Summary ----------------------------------------------------------------- *)

let make_trade ~date ~entry_px ~exit_px ~r_pts =
  let entry_ts = ts date 0 in
  let exit_ts = ts date 10 in
  TB.make_raw ~qty:1.0 ~r_pts ~direction:Long
    ~entry_ts ~entry_px ~exit_ts ~exit_px
    ~exit_reason:Target ~meta:[]

let%test_unit "compute_stats aggregates win rate, sharpe, drawdown, and skew" =
  let d1 = date "2020-03-01" in
  let d2 = date "2020-03-02" in
  let d3 = date "2020-03-03" in
  let t1 = make_trade ~date:d1 ~entry_px:100.0 ~exit_px:101.0 ~r_pts:1.0 |> fun t -> { t with pnl_usd = 50.0 } in
  let t2 = make_trade ~date:d2 ~entry_px:100.0 ~exit_px:99.5 ~r_pts:1.0 |> fun t -> { t with pnl_usd = -25.0 } in
  let t3 = make_trade ~date:d3 ~entry_px:100.0 ~exit_px:102.0 ~r_pts:1.0 |> fun t -> { t with pnl_usd = 100.0 } in
  let daily_R = [ d1, 1.0; d2, -0.5; d3, 2.0 ] in
  let daily_usd = [ d1, 50.0; d2, -25.0; d3, 100.0 ] in
  let stats = Summary.compute_stats ~daily_usd [ t1; t2; t3 ] daily_R in
  assert (stats.n_trades = 3);
  assert (stats.n_days = 3);
  assert (Option.value_exn stats.win_rate |> approx (2.0 /. 3.0));
  assert (Option.value_exn stats.expectancy_R |> approx (2.5 /. 3.0));
  assert (Option.value_exn stats.avg_R_per_day |> approx (2.5 /. 3.0));
  assert (Option.value_exn stats.expectancy_usd |> approx (125.0 /. 3.0));
  assert (Option.value_exn stats.avg_usd_per_day |> approx (125.0 /. 3.0));
  Option.iter stats.ann_sharpe ~f:(fun s -> assert (approx ~tol:1e-4 s 10.51315));
  Option.iter stats.ann_sharpe_usd ~f:(fun s -> assert (approx ~tol:1e-4 s 10.51315));
  Option.iter stats.max_drawdown_R ~f:(fun dd -> assert (approx dd 0.5));
  Option.iter stats.max_drawdown_usd ~f:(fun dd -> assert (approx dd 25.0));
  Option.iter stats.skew_R ~f:(fun skew -> assert (approx ~tol:1e-4 skew (-0.130129)))

let%test_unit "compute_stats returns None metrics when no data" =
  let stats = Summary.compute_stats [] [] in
  assert (stats.n_trades = 0);
  assert (stats.n_days = 0);
  assert (Option.is_none stats.win_rate);
  assert (Option.is_none stats.avg_R_per_day);
  assert (Option.is_none stats.ann_sharpe);
  assert (Option.is_none stats.max_drawdown_R)
