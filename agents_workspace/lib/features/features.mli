open Types

type state

type snapshot = {
  vwap       : float option;
  z_vwap     : float option;
  ofi_short  : float option;
  ofi_long   : float option;
  rv10       : float option;
  rv60       : float option;
  rv_ratio   : float option;
  trend      : float option;
  gap        : float option;
  dist_onh   : float option;
  dist_onl   : float option;
}

val create : unit -> state

val update : state -> bar_1m -> state

val snapshot : state -> snapshot
