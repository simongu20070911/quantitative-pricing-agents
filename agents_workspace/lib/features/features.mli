open Types

type state

(** Coarse session phase for intraday context. *)
type session_phase =
  | Pre_rth      (** before regular trading hours open *)
  | Rth_open     (** first part of RTH session *)
  | Rth_mid      (** middle of RTH session *)
  | Rth_late     (** final part of RTH session *)
  | Post_rth     (** after RTH close *)

type snapshot = {
  vwap          : float option;
  z_vwap        : float option;
  ofi_short     : float option;
  ofi_long      : float option;
  rv10          : float option;
  rv60          : float option;
  rv_ratio      : float option;
  trend         : float option;
  gap           : float option;
  dist_onh      : float option;
  dist_onl      : float option;
  minute_of_day : int option;         (** last seen bar minute-of-day (0-1439) *)
  session_phase : session_phase option;
  ema20         : float option;       (** EMA20 of close, intraday, causal *)
  dist_ema20    : float option;       (** close - ema20 in points *)
  ema20_slope   : float option;       (** ema20_t - ema20_{t-1} *)
}

val create : unit -> state

val update : state -> bar_1m -> state

val snapshot : state -> snapshot
