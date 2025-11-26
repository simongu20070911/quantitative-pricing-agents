open Core
open Types

type outputs = {
  plot_dir : string option;
  export_trades : string option;
  export_daily : string option;
  plot_python : string option;
}

let outputs ?plot_dir ?export_trades ?export_daily ?plot_python () =
  { plot_dir; export_trades; export_daily; plot_python }

let ensure_dir dir =
  if not (String.is_empty dir) then
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

let export_csvs ~trades_file ~daily_file ~trades ~daily ~daily_usd ~daily_pct =
  Option.iter trades_file ~f:(fun outfile ->
      Summary.export_trades_csv ~outfile ~trades);
  Option.iter daily_file ~f:(fun outfile ->
      Summary.export_daily_csv ~outfile ~daily ~daily_usd ~daily_pct ())

let plot_ocaml ~dir ~trades ~daily =
  ensure_dir dir;
  let open Plotter in
  render_equity ~outfile:(Filename.concat dir "equity_trades.png") ~trades;
  render_daily_curve ~outfile:(Filename.concat dir "equity_daily.png") ~daily;
  let pnl = List.map trades ~f:(fun t -> t.pnl_R) |> Array.of_list in
  render_histogram ~outfile:(Filename.concat dir "pnl_hist.png") ~values:pnl ~bins:40

let apply_outputs ~opts ~(result : Engine_types.run_result) =
  export_csvs
    ~trades_file:opts.export_trades
    ~daily_file:opts.export_daily
    ~trades:result.trades
    ~daily:result.daily_pnl
    ~daily_usd:result.daily_pnl_usd
    ~daily_pct:result.daily_pnl_pct;
  Option.iter opts.plot_dir ~f:(fun d ->
      plot_ocaml ~dir:d ~trades:result.trades ~daily:result.daily_pnl);
  Option.iter opts.plot_python ~f:(fun dir ->
      ensure_dir dir;
      let trades_csv = Filename.concat dir "trades.csv" in
      let daily_csv  = Filename.concat dir "daily.csv" in
      Summary.export_trades_csv ~outfile:trades_csv ~trades:result.trades;
      Summary.export_daily_csv  ~outfile:daily_csv  ~daily:result.daily_pnl
        ~daily_usd:result.daily_pnl_usd ~daily_pct:result.daily_pnl_pct ();
      run_python_plot ~trades_csv ~daily_csv ~outdir:dir)
