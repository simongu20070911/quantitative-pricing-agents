open Core
open Strategy_fast
open Types
module E = Strategy_fast.Engine.Engine

let find_fixture () =
  let target = Filename.concat "agents_workspace" "sample_es.csv" in
  let rec ascend n dir =
    if n < 0 then None else
      let candidate = Filename.concat dir target in
      if Stdlib.Sys.file_exists candidate then Some candidate
      else ascend (n-1) (Filename.dirname dir)
  in
  match ascend 6 (Stdlib.Sys.getcwd ()) with
  | Some p -> p
  | None -> failwith "fixture not found"

let sum_pnl trades = List.fold trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_R)

let assert_parity pure legacy =
  let pure_trades = (pure : E.run_result).trades in
  let legacy_trades = (legacy : E.run_result).trades in
  assert (List.length pure_trades = List.length legacy_trades);
  let tol = 1e-6 in
  let diff = Float.abs (sum_pnl pure_trades -. sum_pnl legacy_trades) in
  assert (Float.(diff < tol));
  List.iter2_exn pure_trades legacy_trades ~f:(fun a b ->
      assert (Poly.(a.direction = b.direction));
      assert (Poly.(a.exit_reason = b.exit_reason));
      assert (Date.equal a.date b.date))

let%test_unit "b1b2 pure matches legacy" =
  let file = find_fixture () in
  let pure = E.run_pure Strategies.Strategy_b1b2.strategy_pure ~filename:file in
  let legacy = E.run Strategies.Strategy_b1b2.strategy ~filename:file in
  assert_parity pure legacy

let%test_unit "vwap pure matches legacy" =
  let file = find_fixture () in
  let pure = E.run_pure Strategies.Vwap_revert_strategy.strategy_pure ~filename:file in
  let legacy = E.run Strategies.Vwap_revert_strategy.strategy ~filename:file in
  assert_parity pure legacy
