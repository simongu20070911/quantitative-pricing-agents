open Core
open Strategy_fast
open Types
module SB = Strategy_fast.Strategies.Setup_builder_b1b2
module P = Strategy_fast.Strategies.B1b2_params.Setup
module T = Test_utils

let compare_setup (a : Types.setup) (b : Types.setup) =
  let float_eq (x:float) (y:float) = Float.(abs (x -. y) < 1e-8) in
  let cmp_bar (b1:Types.bar_5m) (b2:Types.bar_5m) =
    assert (Date.equal b1.date b2.date);
    assert (b1.minute_of_day = b2.minute_of_day);
    assert (float_eq b1.open_ b2.open_);
    assert (float_eq b1.high b2.high);
    assert (float_eq b1.low b2.low);
    assert (float_eq b1.close b2.close)
  in
  assert (Poly.(a.direction = b.direction));
  cmp_bar a.b1 b.b1;
  cmp_bar a.b2 b.b2;
  assert (float_eq a.abr_prev b.abr_prev);
  assert (float_eq a.prev_close b.prev_close);
  assert (float_eq a.adr21 b.adr21)

let%test_unit "aggregate_bars + build_setups matches build" =
  let file = T.find_fixture () in
  let params = P.defaults in
  let (setups_direct : Types.setup Date.Table.t) = SB.build params file in
  let tables = SB.aggregate_bars params file in
  let (setups_folded : Types.setup Date.Table.t) = SB.build_setups params tables in
  let keys_direct = Hashtbl.keys setups_direct |> List.sort ~compare:Date.compare in
  let keys_folded = Hashtbl.keys setups_folded |> List.sort ~compare:Date.compare in
  assert (List.equal Date.equal keys_direct keys_folded);
  List.iter keys_direct ~f:(fun d ->
      let (a : Types.setup) = Hashtbl.find_exn setups_direct d in
      let (b : Types.setup) = Hashtbl.find_exn setups_folded d in
      compare_setup a b)
