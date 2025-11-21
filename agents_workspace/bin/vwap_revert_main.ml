open Core

let ensure_dir dir =
  try Core_unix.mkdir dir ~perm:0o755 with _ -> ()

let run_python_plot ~trades_csv ~daily_csv ~outdir =
  let script = Filename.concat "tools" "plot_trades.py" in
  if not (Stdlib.Sys.file_exists script) then
    eprintf "Python plotter not found at %s; skipping\n%!" script
  else
    let cmd =
      Printf.sprintf "python3 %s --trades %s --daily %s --outdir %s"
        script trades_csv daily_csv outdir
    in
    match Stdlib.Sys.command cmd with
    | 0 -> ()
    | c -> eprintf "Python plot command exited with code %d\n%!" c

let plot dir (result : Strategy_fast.Engine.Engine.run_result) =
  ensure_dir dir;
  let open Strategy_fast.Plot.Plotter in
  render_equity ~outfile:(Filename.concat dir "equity_trades.png") ~trades:result.trades;
  render_daily_curve ~outfile:(Filename.concat dir "equity_daily.png") ~daily:result.daily_pnl;
  let pnl = List.map result.trades ~f:(fun t -> t.pnl_R) |> Array.of_list in
  render_histogram ~outfile:(Filename.concat dir "pnl_hist.png") ~values:pnl ~bins:40

let main ~slippage ~fees ~s_entry ~z_exit ?plot_dir ?export_trades ?export_daily ?plot_python filename =
  let open Strategy_fast.Strategies.Vwap_revert_strategy in
  let cfg =
    { default_config with
      s_entry;
      z_exit;
      cost = { default_config.cost with
               slippage_roundtrip_ticks = slippage;
               fee_per_contract = fees; } }
  in
  let strat = make_strategy cfg in
  let (result : Strategy_fast.Engine.Engine.run_result) =
    Strategy_fast.Engine.Engine.run strat ~filename
  in
  let n_trades = List.length result.trades in
  let n_days = List.length result.daily_pnl in
  printf "VWAP-revert strategy: %d trades over %d days\n%!" n_trades n_days;
  let stats = Strategy_fast.Summary.compute_stats ~daily_usd:result.daily_pnl_usd ~daily_pct:result.daily_pnl_pct result.trades result.daily_pnl in
  Strategy_fast.Summary.print_stats stats;
  Strategy_fast.Summary.print_sample_trades result.trades 10;
  Option.iter export_trades ~f:(fun outfile ->
      Strategy_fast.Summary.export_trades_csv ~outfile ~trades:result.trades);
  Option.iter export_daily ~f:(fun outfile ->
      Strategy_fast.Summary.export_daily_csv ~outfile ~daily:result.daily_pnl ~daily_usd:result.daily_pnl_usd ~daily_pct:result.daily_pnl_pct ());
  Option.iter plot_dir ~f:(fun d -> plot d result);
  (match plot_python with
   | Some dir ->
       ensure_dir dir;
       let trades_csv = Filename.concat dir "trades.csv" in
       let daily_csv  = Filename.concat dir "daily.csv" in
       Strategy_fast.Summary.export_trades_csv ~outfile:trades_csv ~trades:result.trades;
       Strategy_fast.Summary.export_daily_csv  ~outfile:daily_csv  ~daily:result.daily_pnl ~daily_usd:result.daily_pnl_usd ~daily_pct:result.daily_pnl_pct ();
       run_python_plot ~trades_csv ~daily_csv ~outdir:dir
   | None -> ())

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
