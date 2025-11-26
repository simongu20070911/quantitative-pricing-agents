open Core

module ORB = Strategy_fast.Strategies.Open_range_breakout_strategy
module Cli = Strategy_fast.Cli_helpers

let main ?plot_dir ?export_trades ?export_daily ?plot_python ~cfg filename =
  let strat = ORB.pure_strategy cfg in
  let (result : Strategy_fast.Engine.Engine.run_result) =
    Strategy_fast.Engine.Engine.run_pure strat ~filename
  in
  let n_trades = List.length result.trades in
  let n_days = List.length result.daily_pnl in
  printf "ORB30 strategy: %d trades over %d days\n%!" n_trades n_days;
  let stats =
    Strategy_fast.Summary.compute_stats
      ~daily_usd:result.daily_pnl_usd
      ~daily_pct:result.daily_pnl_pct
      result.trades result.daily_pnl
  in
  Strategy_fast.Summary.print_stats stats;
  Strategy_fast.Summary.print_sample_trades result.trades 10;
  Cli.apply_outputs
    ~opts:(Cli.outputs ?plot_dir ?export_trades ?export_daily ?plot_python ())
    ~result

let () =
  let argv = Sys.get_argv () in
  let prog = Filename.basename argv.(0) in
  let usage () =
    eprintf
      "Usage: %s <csvfile> [--qty x] [--fees usd] [--range-minutes n] [--session-start m] [--session-end m] [--plot dir] [--export-trades file] [--export-daily file] [--plot-python dir]\n\
       session minutes are ET minute-of-day (9:30am=570)\n%!"
      prog;
    exit 1
  in
  let filename = ref None in
  let qty = ref None in
  let fees = ref None in
  let range_minutes = ref None in
  let session_start = ref None in
  let session_end = ref None in
  let plot_dir = ref None in
  let export_trades = ref None in
  let export_daily  = ref None in
  let plot_python   = ref None in
  let rec parse i =
    if i >= Array.length argv then ()
    else
      match argv.(i) with
      | "-h" | "--help" -> usage ()
      | "--qty" when i + 1 < Array.length argv ->
          qty := Float.of_string_opt argv.(i+1); parse (i+2)
      | "--fees" when i + 1 < Array.length argv ->
          fees := Float.of_string_opt argv.(i+1); parse (i+2)
      | "--range-minutes" when i + 1 < Array.length argv ->
          range_minutes := Int.of_string_opt argv.(i+1); parse (i+2)
      | "--session-start" when i + 1 < Array.length argv ->
          session_start := Int.of_string_opt argv.(i+1); parse (i+2)
      | "--session-end" when i + 1 < Array.length argv ->
          session_end := Int.of_string_opt argv.(i+1); parse (i+2)
      | "--plot" when i + 1 < Array.length argv ->
          plot_dir := Some argv.(i+1); parse (i+2)
      | "--export-trades" when i + 1 < Array.length argv ->
          export_trades := Some argv.(i+1); parse (i+2)
      | "--export-daily" when i + 1 < Array.length argv ->
          export_daily := Some argv.(i+1); parse (i+2)
      | "--plot-python" when i + 1 < Array.length argv ->
          plot_python := Some argv.(i+1); parse (i+2)
      | arg when Option.is_none !filename ->
          filename := Some arg; parse (i+1)
      | _ -> usage ()
  in
  parse 1;
  match !filename with
  | None -> usage ()
  | Some file ->
      let base = ORB.default_config in
      let base_cost = ORB.cost base in
      let cost =
        match !fees with
        | None -> base_cost
        | Some f -> { base_cost with fee_per_contract = f }
      in
      let cfg =
        base
        |> ORB.with_qty ~qty:(Option.value !qty ~default:(ORB.qty base))
        |> ORB.with_cost ~cost
        |> (fun c ->
               match !range_minutes with
               | None -> c
               | Some m -> ORB.with_open_range_minutes ~mins:m c)
        |> (fun c ->
               match !session_start, !session_end with
               | Some s, Some e -> ORB.with_session ~start:s ~end_:e c
               | _ -> c)
      in
      main ~cfg ?plot_dir:!plot_dir ?export_trades:!export_trades
        ?export_daily:!export_daily ?plot_python:!plot_python file
