open Core
open Strategy_fast
module ME = Strategy_fast.Engine.Multi_engine

let filename =
  match Array.to_list (Sys.get_argv ()) with
  | [_; f] -> f
  | _ -> "sample_es.csv"

let total_days () =
  let dates = Date.Hash_set.create () in
  Csv_parser.iter_bars filename ~f:(fun b -> Hash_set.add dates b.ts.date);
  Hash_set.length dates

let run_streaming () =
  let strat = Strategies.Strategy_b1b2.strategy_pure in
  Engine.Multi_engine.run_shared [ strat ] ~filename |> List.hd_exn

let run_prebuilt () =
  let base = Strategies.Strategy_b1b2.strategy_pure in
  let strat = { base with build_setups_stream = None } in
  Engine.Multi_engine.run_shared [ strat ] ~filename |> List.hd_exn

let () =
  let days = total_days () in
  let res_stream : ME.run_result = run_streaming () in
  let res_pre : ME.run_result = run_prebuilt () in
  let setups_stream = Hashtbl.length res_stream.ME.setups in
  let setups_pre = Hashtbl.length res_pre.ME.setups in
  let trades_stream = List.length res_stream.ME.trades in
  let trades_pre = List.length res_pre.ME.trades in
  let filtered_stream = days - setups_stream in
  let filtered_pre = days - setups_pre in
  let dates_stream = Hashtbl.keys res_stream.ME.setups |> List.map ~f:Date.to_string |> String.concat ~sep:"," in
  let dates_pre = Hashtbl.keys res_pre.ME.setups |> List.map ~f:Date.to_string |> String.concat ~sep:"," in
  printf "file=%s days=%d\n" filename days;
  printf "streaming: setups=%d filtered=%d trades=%d\n" setups_stream filtered_stream trades_stream;
  printf "prebuild : setups=%d filtered=%d trades=%d\n" setups_pre filtered_pre trades_pre;
  printf "stream setup dates: %s\n" dates_stream;
  printf "prebuild setup dates: %s\n" dates_pre;

  (* Set diffs on setup dates *)
  let set_of_dates hs =
    let s = Date.Hash_set.create () in
    Hashtbl.iter_keys hs ~f:(fun d -> Hash_set.add s d); s
  in
  let s_stream = set_of_dates res_stream.ME.setups in
  let s_pre = set_of_dates res_pre.ME.setups in
  let only_stream = Hash_set.diff s_stream s_pre |> Hash_set.to_list |> List.sort ~compare:Date.compare in
  let only_pre = Hash_set.diff s_pre s_stream |> Hash_set.to_list |> List.sort ~compare:Date.compare in
  printf "only_stream setups (%d): %s\n" (List.length only_stream)
    (String.concat ~sep:"," (List.map only_stream ~f:Date.to_string));
  printf "only_pre setups (%d): %s\n" (List.length only_pre)
    (String.concat ~sep:"," (List.map only_pre ~f:Date.to_string));

  (* Trades per entry date *)
  let trades_per_date (trades : Types.trade list) =
    let tbl = Date.Table.create () in
    List.iter trades ~f:(fun t ->
        let d = t.Types.entry_ts.date in
        Hashtbl.update tbl d ~f:(function None -> 1 | Some n -> n + 1));
    tbl
  in
  let tp_stream = trades_per_date res_stream.ME.trades in
  let tp_pre = trades_per_date res_pre.ME.trades in

  let all_trade_dates =
    let s = Date.Hash_set.create () in
    Hashtbl.iter_keys tp_stream ~f:(fun d -> Hash_set.add s d);
    Hashtbl.iter_keys tp_pre ~f:(fun d -> Hash_set.add s d);
    Hash_set.to_list s |> List.sort ~compare:Date.compare
  in

  let differing =
    List.filter all_trade_dates ~f:(fun d ->
        let a = Option.value (Hashtbl.find tp_stream d) ~default:0 in
        let b = Option.value (Hashtbl.find tp_pre d) ~default:0 in
        a <> b)
  in
  printf "trade count diff dates (%d):\n" (List.length differing);
  List.iter (List.take differing 20) ~f:(fun d ->
      let a = Option.value (Hashtbl.find tp_stream d) ~default:0 in
      let b = Option.value (Hashtbl.find tp_pre d) ~default:0 in
      printf "  %s stream=%d pre=%d\n" (Date.to_string d) a b);
  if List.length differing > 20 then printf "  ... (+%d more)\n" (List.length differing - 20);

  let pp_setup label (s : Types.setup) =
    printf "%s date=%s dir=%s b1o=%.2f b1h=%.2f b1l=%.2f b1c=%.2f b2o=%.2f b2c=%.2f abr_prev=%.4f prev_close=%.4f adr21=%.4f\n"
      label (Date.to_string s.date)
      (match s.direction with Types.Long -> "Long" | Types.Short -> "Short")
      s.b1.open_ s.b1.high s.b1.low s.b1.close s.b2.open_ s.b2.close s.abr_prev s.prev_close s.adr21
  in

  let show_details res label =
    Hashtbl.iteri res.ME.setups ~f:(fun ~key:_ ~data -> pp_setup label data)
  in

  printf "-- streaming setup details --\n";
  show_details res_stream "stream";
  printf "-- prebuild setup details --\n";
  show_details res_pre "pre";
  ()
