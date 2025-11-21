open Core
open Strategy_fast
open Types

module E = Engine.Engine

let sample_bar minute price =
  let ts = { date = Date.of_string "2020-01-01"; minute_of_day = minute } in
  { ts; open_ = price; high = price; low = price; close = price; volume = 1.0 }

let bars =
  List.init 10 ~f:(fun i -> sample_bar (400 + i) (100. +. Float.of_int i *. 0.1))

let run_with_perm perm =
  let file = Filename_unix.temp_file "bars" ".csv" in
  Out_channel.with_file file ~f:(fun oc ->
      Out_channel.output_string oc "date,time,open,high,low,close,volume\n";
      List.iter perm ~f:(fun b ->
          let d = Date.to_string b.ts.date in
          let h = b.ts.minute_of_day / 60 and m = b.ts.minute_of_day mod 60 in
          let t = Printf.sprintf "%02d:%02d" h m in
          Out_channel.printf oc "%s,%s,%.4f,%.4f,%.4f,%.4f,%.0f\n"
            d t b.open_ b.high b.low b.close b.volume));
  let res = E.run_pure Strategies.Strategy_b1b2.strategy_pure ~filename:file in
  Stdlib.Sys.remove file;
  res

let%test_unit "permuting bars within same day is deterministic" =
  let base = bars in
  let perm = List.permute (List.last_exn (List.permute base)) base in
  let res1 = run_with_perm base in
  let res2 = run_with_perm perm in
  assert (List.length res1.trades = List.length res2.trades);
  let pnl1 = List.fold res1.trades ~init:0.0 ~f:(fun a t -> a +. t.pnl_R) in
  let pnl2 = List.fold res2.trades ~init:0.0 ~f:(fun a t -> a +. t.pnl_R) in
  assert (Float.(abs (pnl1 -. pnl2) < 1e-9))
