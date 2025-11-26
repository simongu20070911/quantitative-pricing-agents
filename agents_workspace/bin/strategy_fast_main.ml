open Core
module EP = Strategy_fast.Engine.Execution_params
module Cli = Strategy_fast.Cli_helpers

let main ?plot_dir ?export_trades ?export_daily ?plot_python ~cfg filename =
  let strat = Strategy_fast.Strategies.Strategy_b1b2.pure_strategy cfg in
  let (result : Strategy_fast.Engine.Engine.run_result) =
    Strategy_fast.Engine.Engine.run_pure strat ~filename
  in
  let n_setups = Hashtbl.length result.setups in
  printf "Number of B1 setups (filtered days): %d\n%!" n_setups;
  let stats = Strategy_fast.Summary.compute_stats ~daily_usd:result.daily_pnl_usd ~daily_pct:result.daily_pnl_pct result.trades result.daily_pnl in
  Strategy_fast.Summary.print_stats stats;
  Strategy_fast.Summary.print_sample_trades result.trades 5;
  Cli.apply_outputs
    ~opts:(Cli.outputs ?plot_dir ?export_trades ?export_daily ?plot_python ())
    ~result

let () =
  let argv = Sys.get_argv () in
  let prog = Filename.basename argv.(0) in
  let usage () =
    eprintf "Usage: %s <es_1min_csv> [--plot dir] [--export-trades file] [--export-daily file] [--plot-python dir]\n%!" prog;
    exit 1
  in
  let filename = ref None in
  let plot_dir = ref None in
  let export_trades = ref None in
  let export_daily  = ref None in
  let plot_python   = ref None in
  let fee = ref None in
  let qty = ref None in
  let equity = ref None in
  let rec parse i =
    if i >= Array.length argv then () else
      match argv.(i) with
      | "-h" | "--help" -> usage ()
      | "--plot" when i + 1 < Array.length argv ->
          plot_dir := Some argv.(i+1); parse (i+2)
      | "--export-trades" when i + 1 < Array.length argv ->
          export_trades := Some argv.(i+1); parse (i+2)
      | "--export-daily" when i + 1 < Array.length argv ->
          export_daily := Some argv.(i+1); parse (i+2)
      | "--plot-python" when i + 1 < Array.length argv ->
          plot_python := Some argv.(i+1); parse (i+2)
      | "--cost-fee" when i + 1 < Array.length argv ->
          fee := Float.of_string_opt argv.(i+1); parse (i+2)
      | "--qty" when i + 1 < Array.length argv ->
          qty := Float.of_string_opt argv.(i+1); parse (i+2)
      | "--equity-base" when i + 1 < Array.length argv ->
          equity := Float.of_string_opt argv.(i+1); parse (i+2)
      | arg when Option.is_none !filename -> filename := Some arg; parse (i+1)
      | _ -> usage ()
  in
  parse 1;
  match !filename with
  | None -> usage ()
  | Some file ->
      let module B = Strategy_fast.Strategies.Strategy_b1b2 in
      let base = B.default_config in
      let base_cost = B.cost base in
      let cost =
        { base_cost with
          fee_per_contract = Option.value !fee ~default:0.;
          equity_base = (match !equity with None -> base_cost.equity_base | Some x -> if Float.(x <= 0.) then None else Some x);
        }
      in
      let base_exec = B.exec base in
      let exec =
        base_exec
      in
      let cfg =
        base
        |> B.with_qty ~qty:(Option.value !qty ~default:(B.qty base))
        |> B.with_cost ~cost
        |> B.with_exec ~exec
      in
      main ~cfg ?plot_dir:!plot_dir ?export_trades:!export_trades
        ?export_daily:!export_daily ?plot_python:!plot_python file
