open Core
open Types

val render_equity : outfile:string -> trades:trade list -> unit
val render_daily_curve : outfile:string -> daily:(Date.t * float) list -> unit
val render_histogram : outfile:string -> values:float array -> bins:int -> unit

