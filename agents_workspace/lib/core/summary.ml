open Core
open Types

[@@@warning "-27-32-69"]

let sharpe_of_series daily =
  match daily with
  | [] -> None
  | _ ->
      let n = Float.of_int (List.length daily) in
      let mean = List.fold daily ~init:0.0 ~f:( +. ) /. n in
      let var = List.fold daily ~init:0.0 ~f:(fun acc x -> let d = x -. mean in acc +. d *. d) /. n in
      let std = Float.sqrt var in
      if Float.(std = 0.) then None else Some (Float.sqrt 252.0 *. (mean /. std))

let compute_stats ?daily_usd ?daily_pct trades daily_pnl : perf_stats =
  let n_trades = List.length trades in
  let n_days   = List.length daily_pnl in
  let win_rate, expectancy =
    if n_trades = 0 then None, None
    else
      let total_R = List.fold trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_R) in
      let wins = List.count trades ~f:(fun t -> Float.(t.pnl_R > 0.0)) in
      Some (Float.of_int wins /. Float.of_int n_trades),
      Some (total_R /. Float.of_int n_trades)
  in
  let avg_R_per_day, ann_sharpe =
    if n_days = 0 then None, None
    else
      let daily_R = List.map daily_pnl ~f:snd in
      let mean = List.fold daily_R ~init:0.0 ~f:( +. ) /. Float.of_int n_days in
      let sharpe = sharpe_of_series daily_R in
      Some mean, sharpe
  in
  let avg_usd_per_day, ann_sharpe_usd =
    match daily_usd with
    | None -> None, None
    | Some ds when List.is_empty ds -> None, None
    | Some ds ->
        let daily = List.map ds ~f:snd in
        let mean = List.fold daily ~init:0.0 ~f:( +. ) /. Float.of_int (List.length daily) in
        let sharpe = sharpe_of_series daily in
        Some mean, sharpe
  in
  let avg_pct_per_day, ann_sharpe_pct =
    match daily_pct with
    | None -> None, None
    | Some ds when List.is_empty ds -> None, None
    | Some ds ->
        let vals = List.map ds ~f:snd in
        let mean = List.fold vals ~init:0.0 ~f:( +. ) /. Float.of_int (List.length vals) in
        let sharpe = sharpe_of_series vals in
        Some mean, sharpe
  in
  let expectancy_usd =
    if n_trades = 0 then None
    else
      let total_usd = List.fold trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_usd) in
      Some (total_usd /. Float.of_int n_trades)
  in
  {
    n_trades;
    n_days;
    win_rate;
    expectancy_R = expectancy;
    avg_R_per_day;
    ann_sharpe;
    expectancy_usd;
    avg_usd_per_day;
    ann_sharpe_usd;
    avg_pct_per_day;
    ann_sharpe_pct;
  }

let print_stats stats =
  printf "Number of trades: %d\n" stats.n_trades;
  printf "Number of trading days (with this strategy): %d\n" stats.n_days;
  Option.iter stats.win_rate ~f:(fun w -> printf "Win rate          : %.2f%%\n" (w *. 100.));
  Option.iter stats.expectancy_R ~f:(fun e -> printf "Expectancy / trade: %.3f R\n" e);
  Option.iter stats.avg_R_per_day ~f:(fun x -> printf "Average R / day   : %.3f\n" x);
  (match stats.ann_sharpe with
   | Some s -> printf "Ann. Sharpe (R/day): %.2f\n" s
   | None -> printf "Ann. Sharpe (R/day): n/a (zero volatility or no days)\n");
  Option.iter stats.expectancy_usd ~f:(fun e -> printf "Expectancy / trade: $%.2f\n" e);
  Option.iter stats.avg_usd_per_day ~f:(fun x -> printf "Average USD / day : $%.2f\n" x);
  Option.iter stats.ann_sharpe_usd ~f:(fun s -> printf "Ann. Sharpe (USD/day): %.2f\n" s);
  Option.iter stats.avg_pct_per_day ~f:(fun x -> printf "Average pct / day : %.4f%%\n" (x *. 100.));
  Option.iter stats.ann_sharpe_pct ~f:(fun s -> printf "Ann. Sharpe (pct/day): %.2f\n" s)

let lookup_meta key meta =
  List.find_map meta ~f:(fun (k,v) -> if String.(k = key) then Some v else None)

let print_sample_trades trades n =
  List.iter (List.take trades n) ~f:(fun t ->
      let b2 = Option.value (lookup_meta "b2_follow" t.meta) ~default:"" in
      printf "%s %s entry=%.2f exit=%.2f R=%.2f reason=%s %s\n"
        (Date.to_string t.date)
        (string_of_direction t.direction)
        t.entry_price t.exit_price t.pnl_R
        (string_of_exit_reason t.exit_reason)
        b2)

let minute_to_hhmm m =
  let h = m / 60 and mn = m mod 60 in
  Printf.sprintf "%02d:%02d" h mn

let export_trades_csv ~outfile ~trades =
  Out_channel.with_file outfile ~f:(fun oc ->
      Out_channel.output_string oc
        "date,entry_time,exit_time,direction,entry_price,exit_price,qty,r_pts,pnl_pts,pnl_R,pnl_usd,pnl_pct,duration_min,exit_reason,meta\n";
      List.iter trades ~f:(fun t ->
          let meta_str =
            t.meta
            |> List.map ~f:(fun (k,v) ->
                   let safe_v = String.substr_replace_all v ~pattern:"," ~with_:";" in
                   k ^ "=" ^ safe_v)
            |> String.concat ~sep:";"
          in
          let row = Printf.sprintf "%s,%s,%s,%s,%.4f,%.4f,%.2f,%.4f,%.4f,%.4f,%.2f,%.6f,%.2f,%s,%s\n"
              (Date.to_string t.date)
              (minute_to_hhmm t.entry_ts.minute_of_day)
              (minute_to_hhmm t.exit_ts.minute_of_day)
              (string_of_direction t.direction)
              t.entry_price t.exit_price t.qty t.r_pts t.pnl_pts t.pnl_R t.pnl_usd
              (Option.value t.pnl_pct ~default:0.0)
              t.duration_min
              (string_of_exit_reason t.exit_reason)
              meta_str
          in
          Out_channel.output_string oc row))

let export_daily_csv ~outfile ~daily ?daily_usd ?daily_pct () =
  Out_channel.with_file outfile ~f:(fun oc ->
      Out_channel.output_string oc "date,pnl_R,pnl_usd,pnl_pct\n";
      let usd_tbl = daily_usd |> Option.value ~default:[] |> Date.Map.of_alist_reduce ~f:(+.) in
      let pct_tbl =
        daily_pct
        |> Option.value ~default:[]
        |> Date.Map.of_alist_reduce ~f:(+.)
      in
      List.iter daily ~f:(fun (d, r) ->
          let usd = Map.find usd_tbl d |> Option.value ~default:0.0 in
          let pct = Map.find pct_tbl d |> Option.value ~default:0.0 in
          Out_channel.output_string oc
            (Printf.sprintf "%s,%.6f,%.2f,%.6f\n" (Date.to_string d) r usd pct)))
