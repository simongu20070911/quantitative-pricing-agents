open Core
open Strategy_fast
open Types

let find_fixture () =
  let target = Filename.concat "agents_workspace" "sample_es.csv" in
  let rec ascend n dir =
    if n < 0 then None
    else
      let candidate = Filename.concat dir target in
      if Stdlib.Sys.file_exists candidate then Some candidate
      else ascend (n - 1) (Filename.dirname dir)
  in
  match ascend 6 (Stdlib.Sys.getcwd ()) with
  | Some path -> path
  | None -> failwithf "Fixture missing: %s" target ()

let sum_pnl trades = List.fold trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_R)

let%test_unit "backtest wrapper matches engine run for b1b2" =
  let fixture = find_fixture () in
  let strat = Strategies.Strategy_b1b2.strategy in
  let engine_res = Engine.Engine.run strat ~filename:fixture in
  let setups = engine_res.setups in
  let cfg = Strategies.Strategy_b1b2.default_config in
  let module TB = Engine.Backtest in
  let open Time_utils in
  let bt_cfg = {
    TB.session_start_min = cfg.session_start_min;
    session_end_min = cfg.session_end_min;
    trade_start_min = b2_min;
    trade_end_min = abr_eod_min;
    build_trade_plan = Strategy_fast.Trade_logic.build_trade_plan;
    on_plan_bar = (fun _ _ -> ());
  } in
  let trades_bt, _ = TB.run ~config:bt_cfg fixture setups in
  assert (List.length trades_bt = List.length engine_res.trades);
  List.iter2_exn trades_bt engine_res.trades ~f:(fun a b ->
      assert (Poly.(a.direction = b.direction));
      assert (Poly.(a.exit_reason = b.exit_reason));
      assert (Poly.(a.entry_ts.date = b.entry_ts.date)))
