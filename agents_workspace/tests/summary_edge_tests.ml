open Core
open Strategy_fast
open Types

module Summary = Summary
module TB = Strategy_fast__Trade_base

let make_trade date pnl_R =
  let ts = { date; minute_of_day = 0 } in
  TB.make_raw ~qty:1.0 ~r_pts:1.0 ~direction:Long
    ~entry_ts:ts ~entry_px:100.0 ~exit_ts:{ ts with minute_of_day = 10 } ~exit_px:(100.0 +. pnl_R)
    ~exit_reason:Target ~meta:[]

let%test_unit "max_drawdown is order-agnostic" =
  let d1 = Date.of_string "2020-01-01" in
  let d2 = Date.of_string "2020-01-02" in
  let d3 = Date.of_string "2020-01-03" in
  let daily = [ d3, -1.0; d1, 2.0; d2, -0.5 ] in
  let stats = Summary.compute_stats [] daily in
  Option.iter stats.max_drawdown_R ~f:(fun dd -> assert (Float.(dd = 1.5)))

let%test_unit "sharpe is None when variance is zero" =
  let d = Date.of_string "2020-01-01" in
  let stats = Summary.compute_stats [] [ d, 1.0; d, 1.0 ] in
  assert (Option.is_none stats.ann_sharpe)
