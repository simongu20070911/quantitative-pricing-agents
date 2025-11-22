open Core
open Strategy_fast
open Types
module E = Strategy_fast.Engine.Engine
module T = Test_utils

let sum_pnl trades = List.fold trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_R)

let assert_same_runs a b =
  let trades_a = (a : E.run_result).trades in
  let trades_b = (b : E.run_result).trades in
  assert (List.length trades_a = List.length trades_b);
  let tol = 1e-6 in
  let diff = Float.abs (sum_pnl trades_a -. sum_pnl trades_b) in
  assert (Float.(diff < tol));
  List.iter2_exn trades_a trades_b ~f:(fun x y ->
      assert (Poly.(x.direction = y.direction));
      assert (Poly.(x.exit_reason = y.exit_reason));
      assert (Date.equal x.date y.date))

type 'c strat_surface = {
  strategy_pure : E.pure_strategy;
  pure_strategy : 'c -> E.pure_strategy;
  parameter_specs : Parameters.t list;
  config_of_params : Parameters.value_map -> 'c;
}

let run_with_default_params (s : 'c strat_surface) ~filename =
  let map = Parameters.default_map s.parameter_specs in
  let cfg = s.config_of_params map in
  E.run_pure (s.pure_strategy cfg) ~filename

let%test_unit "b1b2 default params map matches default config" =
  let file = T.find_fixture () in
  let baseline = E.run_pure Strategies.Strategy_b1b2.strategy_pure ~filename:file in
  let via_params =
    run_with_default_params
      {
        strategy_pure = Strategies.Strategy_b1b2.strategy_pure;
        pure_strategy = Strategies.Strategy_b1b2.pure_strategy;
        parameter_specs = Strategies.Strategy_b1b2.parameter_specs;
        config_of_params = Strategies.Strategy_b1b2.config_of_params;
      }
      ~filename:file
  in
  assert_same_runs baseline via_params

let%test_unit "vwap default params map matches default config" =
  let file = T.find_fixture () in
  let baseline = E.run_pure Strategies.Vwap_revert_strategy.strategy_pure ~filename:file in
  let via_params =
    run_with_default_params
      {
        strategy_pure = Strategies.Vwap_revert_strategy.strategy_pure;
        pure_strategy = Strategies.Vwap_revert_strategy.pure_strategy;
        parameter_specs = Strategies.Vwap_revert_strategy.parameter_specs;
        config_of_params = Strategies.Vwap_revert_strategy.config_of_params;
      }
      ~filename:file
  in
  assert_same_runs baseline via_params
