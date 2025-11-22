open Core
open Strategy_fast
module T = Test_utils

let%test_unit "b1b2 matches golden baseline on sample_es fixture" =
  let sample_file = T.find_fixture () in
  let strat = Strategies.Strategy_b1b2.strategy_pure in
  let r = Engine.Engine.run_pure strat ~filename:sample_file in
  let trades = r.trades in
  let n = List.length trades in
  let total_R = List.fold trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_R) in
  let total_usd = List.fold trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_usd) in
  (* Golden values captured from the legacy monolithic runner on the same fixture. *)
  let expected_n = 2 in
  let expected_R = 1.4814 in
  let expected_usd = 67.0 in
  let tol = 1e-4 in
  assert (n = expected_n);
  assert (Float.(abs (total_R -. expected_R) < tol));
  assert (Float.(abs (total_usd -. expected_usd) < tol));
  ()
