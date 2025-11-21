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

let main ?plot_dir ?export_trades ?export_daily ?plot_python ~cfg filename =
  let strat = Strategy_fast.Strategies.Strategy_b1b2.make_pure_strategy cfg in
  let (result : Strategy_fast.Engine.Engine.run_result) =
    Strategy_fast.Engine.Engine.run_pure strat ~filename
  in
  let n_setups = Hashtbl.length result.setups in
  printf "Number of B1 setups (filtered days): %d\n%!" n_setups;
  let stats = Strategy_fast.Summary.compute_stats ~daily_usd:result.daily_pnl_usd ~daily_pct:result.daily_pnl_pct result.trades result.daily_pnl in
  Strategy_fast.Summary.print_stats stats;
  Strategy_fast.Summary.print_sample_trades result.trades 5;
  Option.iter export_trades ~f:(fun outfile ->
      Strategy_fast.Summary.export_trades_csv ~outfile ~trades:result.trades);
  Option.iter export_daily ~f:(fun outfile ->
      Strategy_fast.Summary.export_daily_csv ~outfile ~daily:result.daily_pnl ~daily_usd:result.daily_pnl_usd ~daily_pct:result.daily_pnl_pct ());
  Option.iter plot_dir ~f:(fun d -> plot d result)
  |> fun () ->
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
    eprintf "Usage: %s <es_1min_csv> [--plot dir] [--export-trades file] [--export-daily file] [--plot-python dir]\n%!" prog;
    exit 1
  in
  let filename = ref None in
  let plot_dir = ref None in
  let export_trades = ref None in
  let export_daily  = ref None in
  let plot_python   = ref None in
  let fee = ref None in
  let slip = ref None in
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
      | "--cost-slippage" when i + 1 < Array.length argv ->
          slip := Float.of_string_opt argv.(i+1); parse (i+2)
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
      let base = Strategy_fast.Strategies.Strategy_b1b2.default_config in
      let cost = {
        base.cost with
        fee_per_contract = Option.value !fee ~default:base.cost.fee_per_contract;
        slippage_roundtrip_ticks = Option.value !slip ~default:base.cost.slippage_roundtrip_ticks;
        equity_base = (match !equity with None -> base.cost.equity_base | Some x -> if Float.(x <= 0.) then None else Some x);
      } in
      let cfg = {
        base with
        qty = Option.value !qty ~default:base.qty;
        cost;
      } in
      main ~cfg ?plot_dir:!plot_dir ?export_trades:!export_trades
        ?export_daily:!export_daily ?plot_python:!plot_python file
