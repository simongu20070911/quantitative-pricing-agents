open Core
open Types
open Pattern_types

(** Helpers to classify individual bars and simple multi-bar patterns.

    The goal is to provide reusable, purely-functional building blocks that
    strategies and research tools can compose, without baking in any engine
    assumptions. *)

let bar_shape_of_ohlc ~open_ ~high ~low ~close : bar_shape =
  let range = Float.max 0. (high -. low) in
  let body = close -. open_ in
  let upper_wick =
    if Float.(range = 0.) then 0.
    else Float.max 0. (high -. Float.max open_ close)
  in
  let lower_wick =
    if Float.(range = 0.) then 0.
    else Float.max 0. (Float.min open_ close -. low)
  in
  let trend =
    if Float.(body > 0.) then Bull
    else if Float.(body < 0.) then Bear
    else Doji
  in
  { trend; body; range; upper_wick; lower_wick }

let bar_shape_of_bar_1m (b : bar_1m) : bar_shape =
  bar_shape_of_ohlc ~open_:b.open_ ~high:b.high ~low:b.low ~close:b.close

let bar_shape_of_bar_5m (b : bar_5m) : bar_shape =
  bar_shape_of_ohlc ~open_:b.open_ ~high:b.high ~low:b.low ~close:b.close

let is_pinned_low ?(max_frac = 0.1) (shape : bar_shape) : bool =
  if Float.(shape.range <= 0.) then false
  else
    let trend_ok =
      match shape.trend with
      | Bear | Doji -> true
      | Bull -> false
    in
    Float.(shape.lower_wick <= max_frac *. shape.range) && trend_ok

let is_pinned_high ?(max_frac = 0.1) (shape : bar_shape) : bool =
  if Float.(shape.range <= 0.) then false
  else
    let trend_ok =
      match shape.trend with
      | Bull | Doji -> true
      | Bear -> false
    in
    Float.(shape.upper_wick <= max_frac *. shape.range) && trend_ok

(** --- Swing-based wedge detection (Brooks-style legs) --------------------- *)

(** Causal swing lows: mark bar j as a swing low once bar (j+1) has printed.
    We require [low_j <= low_{j-1}] and [low_j <= low_{j+1}] in a rolling window. *)
let swing_lows_5m
    ?(lookback = 60)
    (bars : bar_5m array)
    ~(last_index : int)
  : int list =
  let n = Array.length bars in
  if last_index <= 1 || n < 3 then
    []
  else
    let first_i = Int.max 2 (last_index - lookback + 1) in
    let lows = ref [] in
    (* i is the right neighbour; j = i-1 is the swing candidate *)
    for i = first_i to last_index do
      let j = i - 1 in
      if j > 0 then begin
        let low_prev = bars.(j - 1).low in
        let low_cur  = bars.(j    ).low in
        let low_next = bars.(i    ).low in
        if Float.(low_cur <= low_prev && low_cur <= low_next) then
          lows := j :: !lows
      end
    done;
    List.rev !lows

(** Causal swing highs: mirror of [swing_lows_5m]. *)
let swing_highs_5m
    ?(lookback = 60)
    (bars : bar_5m array)
    ~(last_index : int)
  : int list =
  let n = Array.length bars in
  if last_index <= 1 || n < 3 then
    []
  else
    let first_i = Int.max 2 (last_index - lookback + 1) in
    let highs = ref [] in
    for i = first_i to last_index do
      let j = i - 1 in
      if j > 0 then begin
        let high_prev = bars.(j - 1).high in
        let high_cur  = bars.(j    ).high in
        let high_next = bars.(i    ).high in
        if Float.(high_cur >= high_prev && high_cur >= high_next) then
          highs := j :: !highs
      end
    done;
    List.rev !highs

(** Swing-based 3-push wedge down:
    - use swing lows as leg endpoints
    - require three descending lows, each separated by at least [min_leg_bars]
    - total span from first leg to [last_index] must be <= [max_span] bars. *)
let swing_wedge_down_5m_at
    ?(lookback = 60)
    ?(min_leg_bars = 2)
    ?(max_span = 40)
    (bars : bar_5m array)
    ~(last_index : int)
  : wedge option =
  if last_index < 3 || last_index >= Array.length bars then
    None
  else
    let lows = swing_lows_5m ~lookback bars ~last_index in
    match List.rev lows with
    | i3 :: i2 :: i1 :: _ ->
        let b1 = bars.(i1) and b2 = bars.(i2) and b3 = bars.(i3) in
        let legs_ok =
          i2 - i1 >= min_leg_bars
          && i3 - i2 >= min_leg_bars
          && last_index - i1 <= max_span
        in
        let lows_descend =
          Float.(b1.low > b2.low && b2.low > b3.low)
        in
        if legs_ok && lows_descend then
          Some { side = Down; last_index; length = last_index - i1 + 1 }
        else
          None
    | _ -> None

(** Swing-based 3-push wedge up: mirror on swing highs. *)
let swing_wedge_up_5m_at
    ?(lookback = 60)
    ?(min_leg_bars = 2)
    ?(max_span = 40)
    (bars : bar_5m array)
    ~(last_index : int)
  : wedge option =
  if last_index < 3 || last_index >= Array.length bars then
    None
  else
    let highs = swing_highs_5m ~lookback bars ~last_index in
    match List.rev highs with
    | i3 :: i2 :: i1 :: _ ->
        let b1 = bars.(i1) and b2 = bars.(i2) and b3 = bars.(i3) in
        let legs_ok =
          i2 - i1 >= min_leg_bars
          && i3 - i2 >= min_leg_bars
          && last_index - i1 <= max_span
        in
        let highs_ascend =
          Float.(b1.high < b2.high && b2.high < b3.high)
        in
        if legs_ok && highs_ascend then
          Some { side = Up; last_index; length = last_index - i1 + 1 }
        else
          None
    | _ -> None

(** Simple 3-push wedge on lows: four-bar span, with three consecutive lower lows.

    This is the narrow “three pushes down” proxy we have been using in Python.
    It is intentionally minimal and purely geometric; higher-level context
    (trend, channel, location in day) should be layered on elsewhere. *)
let simple_wedge_down_5m_at
    (bars : bar_5m array)
    ~(last_index : int)
  : wedge option =
  if last_index < 3 || last_index >= Array.length bars then
    None
  else
    let b0 = bars.(last_index)
    and b1 = bars.(last_index - 1)
    and b2 = bars.(last_index - 2)
    and b3 = bars.(last_index - 3) in
    if Float.(b0.low < b1.low && b1.low < b2.low && b2.low < b3.low) then
      Some { side = Down; last_index; length = 4 }
    else
      None

(** Simple 3-push wedge on highs: four-bar span, with three consecutive higher highs. *)
let simple_wedge_up_5m_at
    (bars : bar_5m array)
    ~(last_index : int)
  : wedge option =
  if last_index < 3 || last_index >= Array.length bars then
    None
  else
    let b0 = bars.(last_index)
    and b1 = bars.(last_index - 1)
    and b2 = bars.(last_index - 2)
    and b3 = bars.(last_index - 3) in
    if Float.(b0.high > b1.high && b1.high > b2.high && b2.high > b3.high) then
      Some { side = Up; last_index; length = 4 }
    else
      None

(** Convenience: detect a simple wedge on the tail of a 5m bar list. *)
let simple_wedge_5m_on_tail (bars : bar_5m list) : wedge option =
  match List.rev bars with
  | b0 :: b1 :: b2 :: b3 :: _ ->
      let arr = [| b3; b2; b1; b0 |] in
      (* index 3 is the last bar in this 4-element window *)
      (match simple_wedge_down_5m_at arr ~last_index:3 with
       | Some w -> Some w
       | None ->
         simple_wedge_up_5m_at arr ~last_index:3)
  | _ -> None
