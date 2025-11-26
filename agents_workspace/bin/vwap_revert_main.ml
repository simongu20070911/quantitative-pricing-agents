open Core
module EP = Strategy_fast.Engine.Execution_params
module Cli = Strategy_fast.Cli_helpers

let main ~slippage ~fees ~s_entry ~z_exit ?plot_dir ?export_trades ?export_daily ?plot_python filename =
  let open Strategy_fast.Strategies.Vwap_revert_strategy in
  let tuned_cost = { (cost default_config) with
                     fee_per_contract = fees; } in
  let exec =
    let base = exec default_config in
    { base with
      slip_model = EP.Constant_ticks (slippage /. 2.);
      spread_ticks = 0.0;
    }
  in
  let cfg =
    default_config
    |> with_cost ~cost:tuned_cost
    |> with_exec ~exec
    |> with_s_entry ~s_entry
    |> with_z_exit ~z_exit
  in
  let strat = pure_strategy cfg in
  let (result : Strategy_fast.Engine.Engine.run_result) =
    Strategy_fast.Engine.Engine.run_pure strat ~filename
  in
  let n_trades = List.length result.trades in
  let n_days = List.length result.daily_pnl in
  printf "VWAP-revert strategy: %d trades over %d days\n%!" n_trades n_days;
  let stats = Strategy_fast.Summary.compute_stats ~daily_usd:result.daily_pnl_usd ~daily_pct:result.daily_pnl_pct result.trades result.daily_pnl in
  Strategy_fast.Summary.print_stats stats;
  Strategy_fast.Summary.print_sample_trades result.trades 10;
  Cli.apply_outputs
    ~opts:(Cli.outputs ?plot_dir ?export_trades ?export_daily ?plot_python ())
    ~result

let () =
  let argv = Sys.get_argv () in
  let prog = Filename.basename argv.(0) in
  let usage () =
    eprintf "Usage: %s <csvfile> [--slippage ticks] [--fees usd] [--s-entry x] [--z-exit x] [--plot dir] [--export-trades file] [--export-daily file] [--plot-python dir]\n%!"
      prog;
    exit 1
  in
  let filename = ref None in
  let slippage = ref 1.5 in
  let fees = ref 2.5 in
  let s_entry = ref 0.7 in
  let z_exit = ref 0.2 in
  let plot_dir = ref None in
  let export_trades = ref None in
  let export_daily  = ref None in
  let plot_python   = ref None in
  let rec parse i =
    if i >= Array.length argv then ()
    else match argv.(i) with
      | ("-h" | "--help") -> usage ()
      | "--slippage" when i+1 < Array.length argv ->
          slippage := Float.of_string argv.(i+1); parse (i+2)
      | "--fees" when i+1 < Array.length argv ->
          fees := Float.of_string argv.(i+1); parse (i+2)
      | "--s-entry" when i+1 < Array.length argv ->
          s_entry := Float.of_string argv.(i+1); parse (i+2)
      | "--z-exit" when i+1 < Array.length argv ->
          z_exit := Float.of_string argv.(i+1); parse (i+2)
      | "--plot" when i+1 < Array.length argv ->
          plot_dir := Some argv.(i+1); parse (i+2)
      | "--export-trades" when i+1 < Array.length argv ->
          export_trades := Some argv.(i+1); parse (i+2)
      | "--export-daily" when i+1 < Array.length argv ->
          export_daily := Some argv.(i+1); parse (i+2)
      | "--plot-python" when i+1 < Array.length argv ->
          plot_python := Some argv.(i+1); parse (i+2)
      | arg when Option.is_none !filename ->
          filename := Some arg; parse (i+1)
      | _ -> usage ()
  in
  parse 1;
  match !filename with
  | None -> usage ()
  | Some file -> main ~slippage:!slippage ~fees:!fees ~s_entry:!s_entry ~z_exit:!z_exit
                    ?plot_dir:!plot_dir ?export_trades:!export_trades
                    ?export_daily:!export_daily ?plot_python:!plot_python file
