open Types

type outputs = {
  plot_dir : string option;
  export_trades : string option;
  export_daily : string option;
  plot_python : string option;
}

val outputs :
  ?plot_dir:string ->
  ?export_trades:string ->
  ?export_daily:string ->
  ?plot_python:string ->
  unit -> outputs

val ensure_dir : string -> unit

val export_csvs :
  trades_file:string option ->
  daily_file:string option ->
  trades:trade list ->
  daily:(Core.Date.t * float) list ->
  daily_usd:(Core.Date.t * float) list ->
  daily_pct:(Core.Date.t * float) list ->
  unit

val plot_ocaml :
  dir:string ->
  trades:trade list ->
  daily:(Core.Date.t * float) list ->
  unit

val run_python_plot :
  trades_csv:string ->
  daily_csv:string ->
  outdir:string ->
  unit

val apply_outputs :
  opts:outputs ->
  result:Engine_types.run_result ->
  unit
