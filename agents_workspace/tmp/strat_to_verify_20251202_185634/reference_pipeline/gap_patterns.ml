open Core

(** Gap and intraday breakout/measurement helpers on daily OHLC data.

    This module is intentionally stateless and purely functional so it can be
    used both from OCaml strategies and from off-line research plumbing. *)

type gap_kind =
  | Gap_up_breakout
  | Gap_down_breakout
  | Gap_up_within
  | Gap_down_within
  | Gap_none

type runaway_kind =
  | Runaway_up
  | Runaway_down
  | Runaway_none

type hit_first =
  | Hit_up
  | Hit_down
  | Hit_both
  | Hit_none

type gap_snapshot = {
  gap_pts   : float option;
  gap_atr   : float option;
  kind      : gap_kind;
  runaway   : runaway_kind;
  gap_fill  : float option;  (** prior close, classic gap-fill magnet *)
  gap_mm    : float option;  (** simple measured-move magnet: open + gap_pts *)
}

(** Classify the open of a new day relative to the prior day's range and ATR.

    [prev_high], [prev_low], [prev_close] are the prior day's RTH high/low/close.
    [open_] is the current day's RTH open.
    [atr20] is an optional 20-day ATR in points.
    [trend20] is an optional directional 20-day move (e.g. close_t - close_{t-20}). *)
let classify_gap
    ~(prev_high : float)
    ~(prev_low : float)
    ~(prev_close : float)
    ~(open_ : float)
    ?atr20
    ?trend20
    ()
  : gap_snapshot =
  let gap_pts = open_ -. prev_close in
  let gap_atr =
    match atr20 with
    | None -> None
    | Some atr when Float.(atr > 0.) -> Some (gap_pts /. atr)
    | Some _ -> None
  in
  (* breakout thresholds mirror the Python prototypes: open beyond prior high/low by 0.25 *)
  let breakout_up =
    Float.(gap_pts > 0.) && Float.(open_ > prev_high +. 0.25)
  in
  let breakout_dn =
    Float.(gap_pts < 0.) && Float.(open_ < prev_low -. 0.25)
  in
  let within_up =
    Float.(gap_pts > 0.)
    && Float.(open_ >= prev_low)
    && Float.(open_ <= prev_high)
  in
  let within_dn =
    Float.(gap_pts < 0.)
    && Float.(open_ >= prev_low)
    && Float.(open_ <= prev_high)
  in
  let kind =
    if breakout_up then Gap_up_breakout
    else if breakout_dn then Gap_down_breakout
    else if within_up then Gap_up_within
    else if within_dn then Gap_down_within
    else Gap_none
  in
  let runaway =
    match gap_atr, trend20 with
    | Some ga, Some tr when Float.(ga >= 1.0) && Float.(tr > 0.) ->
      Runaway_up
    | Some ga, Some tr when Float.(ga <= -1.0) && Float.(tr < 0.) ->
      Runaway_down
    | _ -> Runaway_none
  in
  let gap_fill = Some prev_close in
  let gap_mm = Some (open_ +. gap_pts) in
  { gap_pts = Some gap_pts; gap_atr; kind; runaway; gap_fill; gap_mm }

(** Hit-first excursion classification relative to the day's open and thresholds [h_up], [h_dn].

    [open_], [high], [low] are the day's RTH open/high/low in points.
    If [high - open_] >= [h_up], we mark an up hit; if [open_ - low] >= [h_dn],
    we mark a down hit. If both are reached, result is [Hit_both]. *)
let hit_first_excursion
    ~(open_ : float)
    ~(high : float)
    ~(low : float)
    ~(h_up : float)
    ~(h_dn : float)
  : hit_first =
  let hit_up = Float.(high -. open_ >= h_up) in
  let hit_dn = Float.(open_ -. low >= h_dn) in
  match hit_up, hit_dn with
  | false, false -> Hit_none
  | true, false -> Hit_up
  | false, true -> Hit_down
  | true, true -> Hit_both
