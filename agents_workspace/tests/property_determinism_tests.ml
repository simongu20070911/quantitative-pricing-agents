open Core
open Strategy_fast
open Types

module E = Engine.Engine

let base_start_min = Time_utils.rth_start_min

let make_line ~minute ~price =
  let date_str = "2020-01-01" in
  let hh = minute / 60 and mm = minute mod 60 in
  let ts = Printf.sprintf "%s %02d:%02d" date_str hh mm in
  String.concat ~sep:","
    [ ts; "RT"; "pub"; "ES";
      Printf.sprintf "%.4f" price;
      Printf.sprintf "%.4f" price;
      Printf.sprintf "%.4f" price;
      Printf.sprintf "%.4f" price;
      "1";
      "ES";
      ts ]

let write_csv path bars =
  Out_channel.with_file path ~f:(fun oc ->
      Out_channel.output_string oc "ts_event,rtype,publisher_id,instr_id,open,high,low,close,volume,symbol,ET_datetime\n";
      List.iter bars ~f:(fun (min, price) ->
          Out_channel.output_string oc (make_line ~minute:min ~price);
          Out_channel.output_char oc '\n'))

let bars n = List.init n ~f:(fun i -> base_start_min + i, 100. +. Float.of_int i *. 0.1)

let run_with_bars bars =
  let file = Stdlib.Filename.temp_file "bars" ".csv" in
  write_csv file bars;
  let res = E.run_pure Strategies.Strategy_b1b2.strategy_pure ~filename:file in
  Stdlib.Sys.remove file;
  res

let%test_unit "permuting bars within same day is deterministic" =
  let b = bars 10 in
  let perm = List.permute b in
  let res1 = run_with_bars b in
  let res2 = run_with_bars perm in
  assert (List.length res1.trades = List.length res2.trades);
  let pnl1 = List.fold res1.trades ~init:0.0 ~f:(fun a t -> a +. t.pnl_R) in
  let pnl2 = List.fold res2.trades ~init:0.0 ~f:(fun a t -> a +. t.pnl_R) in
  assert (Float.(abs (pnl1 -. pnl2) < 1e-9))
