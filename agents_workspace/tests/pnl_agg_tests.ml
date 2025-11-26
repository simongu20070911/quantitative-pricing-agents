open Core
open Strategy_fast
open Types

module P = Strategy_fast__Pnl_agg

let date s = Date.of_string s
let ts d m = { date = d; minute_of_day = m }

let make_trade ~date ~pnl_R ~pnl_usd ~pnl_pct =
  {
    date;
    direction = Long;
    entry_ts = ts date 0;
    exit_ts = ts date 10;
    entry_price = 100.0;
    exit_price = 100.0 +. pnl_R;
    qty = 1.0;
    r_pts = 1.0;
    pnl_pts = pnl_R;
    pnl_R;
    pnl_usd;
    pnl_pct;
    duration_min = 10.0;
    exit_reason = Target;
    meta = [];
  }

let find_daily alist d =
  List.Assoc.find ~equal:Date.equal alist d |> Option.value ~default:0.0

let%test_unit "single trade populates all daily aggregates" =
  let d = date "2020-01-01" in
  let t = make_trade ~date:d ~pnl_R:1.5 ~pnl_usd:75.0 ~pnl_pct:(Some 0.01) in
  let acc = P.add_trade P.empty t in
  let r, usd, pct = P.to_alists_unsorted acc in
  assert (Int.equal (List.length r) 1);
  assert (Int.equal (List.length usd) 1);
  assert (Int.equal (List.length pct) 1);
  assert (Float.(find_daily r d = 1.5));
  assert (Float.(find_daily usd d = 75.0));
  assert (Float.(find_daily pct d = 0.01))

let%test_unit "multiple trades same day accumulate" =
  let d = date "2020-01-02" in
  let t1 = make_trade ~date:d ~pnl_R:1.0 ~pnl_usd:50.0 ~pnl_pct:(Some 0.005) in
  let t2 = make_trade ~date:d ~pnl_R:(-0.4) ~pnl_usd:(-20.0) ~pnl_pct:(Some (-0.002)) in
  let acc = P.add_trade (P.add_trade P.empty t1) t2 in
  let r, usd, pct = P.to_alists_unsorted acc in
  assert (Float.(find_daily r d = 0.6));
  assert (Float.(find_daily usd d = 30.0));
  assert (Float.(find_daily pct d = 0.003))

let%test_unit "trades without pct do not affect pct map" =
  let d1 = date "2020-01-03" in
  let d2 = date "2020-01-04" in
  let t1 = make_trade ~date:d1 ~pnl_R:0.5 ~pnl_usd:25.0 ~pnl_pct:None in
  let t2 = make_trade ~date:d2 ~pnl_R:0.7 ~pnl_usd:35.0 ~pnl_pct:(Some 0.004) in
  let acc = P.add_trade (P.add_trade P.empty t1) t2 in
  let r, usd, pct = P.to_alists_unsorted acc in
  assert (Float.(find_daily r d1 = 0.5));
  assert (Float.(find_daily r d2 = 0.7));
  assert (Float.(find_daily usd d1 = 25.0));
  assert (Float.(find_daily usd d2 = 35.0));
  (* no entry for d1 in pct map; d2 present *)
  assert (Float.(find_daily pct d1 = 0.0));
  assert (Float.(find_daily pct d2 = 0.004))
