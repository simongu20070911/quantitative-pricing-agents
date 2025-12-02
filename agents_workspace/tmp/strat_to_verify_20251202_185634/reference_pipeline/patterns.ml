open Core
open Types
open Pattern_types
module Time_utils = Time_utils

(** Streaming, strictly causal Brooks-style geometric context on 1-minute bars.

    This module is intended to be used by strategies and research tooling to
    derive features such as legs, wedges, range regimes, and simple H1/H2,
    L1/L2-style pullback markers without any lookahead. *)

type swing_kind = Swing_high | Swing_low

type swing = {
  index : int;
  price : float;
  kind  : swing_kind;
}

type range_state = {
  highs       : float Deque.t;
  lows        : float Deque.t;
  ranges      : float Deque.t;
  tests_high  : int Deque.t;
  tests_low   : int Deque.t;
  fails_high  : int Deque.t;
  fails_low   : int Deque.t;
  capacity    : int;
}

type htf_state_5m = {
  mutable current_bar : bar_5m option;
  mutable current_bucket : int option;
  bars      : bar_5m Deque.t;
  mutable leg_side : leg_side;
  mutable leg_len  : int;
  mutable wedge_up : bool;
  mutable wedge_down : bool;
}

type state = {
  mutable index          : int;
  mutable prev2          : bar_1m option;
  mutable prev1          : bar_1m option;
  mutable last_swing_low  : swing option;
  mutable last_swing_high : swing option;
  mutable prev_swing_low  : swing option;
  mutable prev_swing_high : swing option;
  mutable leg_side       : leg_side;
  mutable leg_start_idx  : int option;
  mutable leg_range      : float;
  mutable micro_up_len   : int;
  mutable micro_down_len : int;
  mutable soft_micro_up_len : int;
  mutable soft_micro_down_len : int;
  mutable soft_micro_up_len_max_leg : int;
  mutable soft_micro_down_len_max_leg : int;
  mutable soft_break_up_severity   : float option;
  mutable soft_break_up_trend      : bool;
  mutable soft_break_down_severity : float option;
  mutable soft_break_down_trend    : bool;
  range_state            : range_state;
  mutable wedge_up       : bool;
  mutable wedge_down     : bool;
  mutable h1_flag        : bool;
  mutable h2_flag        : bool;
  mutable l1_flag        : bool;
  mutable l2_flag        : bool;
  mutable h_pullback_count : int;
  mutable l_pullback_count : int;
   (* bar cluster patterns *)
  mutable inside_bar     : bool;
  mutable outside_bar    : bool;
  mutable ii_flag        : bool;
  mutable ioi_flag       : bool;
  mutable outside_seq_len : int;
  (* always-in bias *)
  mutable ai_state       : ai_state;
  (* simple regression channel over current leg *)
  mutable reg_sum_x      : float;
  mutable reg_sum_y      : float;
  mutable reg_sum_x2     : float;
  mutable reg_sum_xy     : float;
  mutable reg_n          : int;
  (* recent bar-strength window *)
  recent_trend_bodies    : (bar_trend * float) Deque.t;
  mutable recent_bull_count : int;
  mutable recent_bear_count : int;
  mutable recent_doji_count : int;
  mutable recent_body_sum   : float;
  (* higher timeframe 5m structure *)
  htf_5m                 : htf_state_5m;
  (* measured-move leg magnets *)
  mutable leg_mm_up      : float option;
  mutable leg_mm_down    : float option;
  (* day-level RTH structure *)
  mutable current_day    : Date.t option;
  mutable day_open       : float option;
  mutable day_high       : float;
  mutable day_low        : float;
  mutable day_close      : float option;
  mutable day_first_60_high : float option;
  mutable day_first_60_low  : float option;
  mutable prev_day_high  : float option;
  mutable prev_day_low   : float option;
  mutable prev_day_close : float option;
  mutable prev_day_regime : day_regime;
  mutable consec_trend_days : int;
}

let create_range_state ~capacity =
  { highs = Deque.create ();
    lows = Deque.create ();
    ranges = Deque.create ();
    tests_high = Deque.create ();
    tests_low  = Deque.create ();
    fails_high = Deque.create ();
    fails_low  = Deque.create ();
    capacity;
  }

let push_bounded dq x ~capacity =
  Deque.enqueue_back dq x;
  if Deque.length dq > capacity then ignore (Deque.dequeue_front dq)

let update_range (r : range_state) ~high ~low =
  let range = high -. low in
  push_bounded r.highs high ~capacity:r.capacity;
  push_bounded r.lows  low  ~capacity:r.capacity;
  push_bounded r.ranges range ~capacity:r.capacity

let record_range_events (r : range_state) ~(bar : bar_1m) =
  if Deque.is_empty r.highs || Deque.is_empty r.lows then begin
    push_bounded r.tests_high 0 ~capacity:r.capacity;
    push_bounded r.tests_low  0 ~capacity:r.capacity;
    push_bounded r.fails_high 0 ~capacity:r.capacity;
    push_bounded r.fails_low  0 ~capacity:r.capacity;
  end else begin
    let hi = Deque.fold r.highs ~init:Float.neg_infinity ~f:Float.max in
    let lo = Deque.fold r.lows  ~init:Float.infinity     ~f:Float.min in
    let width = hi -. lo in
    let close = bar.close in
    let test_thresh = 0.1 in
    let is_test_high =
      Float.(width > 0.)
      && Float.(hi -. close >= 0.)
      && Float.(hi -. close <= test_thresh *. width)
    in
    let is_test_low =
      Float.(width > 0.)
      && Float.(close -. lo >= 0.)
      && Float.(close -. lo <= test_thresh *. width)
    in
    let is_fail_high =
      Float.(bar.high > hi) && Float.(close <= hi)
    in
    let is_fail_low =
      Float.(bar.low < lo) && Float.(close >= lo)
    in
    push_bounded r.tests_high (if is_test_high then 1 else 0) ~capacity:r.capacity;
    push_bounded r.tests_low  (if is_test_low  then 1 else 0) ~capacity:r.capacity;
    push_bounded r.fails_high (if is_fail_high then 1 else 0) ~capacity:r.capacity;
    push_bounded r.fails_low  (if is_fail_low  then 1 else 0) ~capacity:r.capacity;
  end

let range_snapshot (r : range_state) (last_close : float option) :
    (float * float * float option * range_regime * int * int * int * int * bool) =
  if Deque.is_empty r.highs || Deque.is_empty r.lows then
    (0., 0., None, Regime_unknown, 0, 0, 0, 0, false)
  else
    let hi = Deque.fold r.highs ~init:Float.neg_infinity ~f:Float.max in
    let lo = Deque.fold r.lows  ~init:Float.infinity     ~f:Float.min in
    let width = hi -. lo in
    let avg_range =
      if Deque.is_empty r.ranges then 0.
      else
        let sum = Deque.fold r.ranges ~init:0.0 ~f:( +. ) in
        sum /. Float.of_int (Deque.length r.ranges)
    in
    let pos =
      match last_close with
      | None -> None
      | Some c ->
        if Float.(width <= 0.) then None
        else Some ((c -. lo) /. width)
    in
    let sum_int dq = Deque.fold dq ~init:0 ~f:( + ) in
    let tests_high = sum_int r.tests_high in
    let tests_low  = sum_int r.tests_low in
    let fails_high = sum_int r.fails_high in
    let fails_low  = sum_int r.fails_low in
    (* Calibrated regime heuristic:
       - Compute a width_ratio = window range / avg bar range.
       - If both highs and lows have been tested several times and the window
         is not extremely wide, treat as Range.
       - Otherwise, if avg_range>0, fall back to Trend. *)
    let width_ratio =
      if Float.(avg_range > 0.) then width /. avg_range else 0.
    in
    let both_sides_tested =
      tests_high >= 3 && tests_low >= 3
    in
    let is_range =
      Float.(avg_range > 0.)
      && (Float.(width_ratio <= 3.)
          || (both_sides_tested && Float.(width_ratio <= 12.)))
    in
    let regime =
      if Float.(avg_range <= 0.) then Regime_unknown
      else if is_range then Range
      else Trend
    in
    let tight =
      Float.(avg_range > 0.)
      && Float.(width <= 1.5 *. avg_range)
    in
    (lo, hi, pos, regime, tests_high, tests_low, fails_high, fails_low, tight)

let create_htf_state_5m () =
  { current_bar = None;
    current_bucket = None;
    bars = Deque.create ();
    leg_side = Leg_none;
    leg_len = 0;
    wedge_up = false;
    wedge_down = false;
  }

let update_htf_5m (htf : htf_state_5m) (bar : bar_1m) =
  let bucket = bar.ts.minute_of_day / 5 in
  let rollover_current () =
    match htf.current_bar with
    | None -> ()
    | Some b ->
        Deque.enqueue_back htf.bars b;
        (* keep last ~200 bars *)
        while Deque.length htf.bars > 200 do
          ignore (Deque.dequeue_front htf.bars)
        done
  in
  (match htf.current_bucket with
   | None ->
       (* start first 5m bar *)
       htf.current_bucket <- Some bucket;
       htf.current_bar <- Some {
         date = bar.ts.date;
         minute_of_day = bucket * 5;
         open_ = bar.open_;
         high = bar.high;
         low  = bar.low;
         close = bar.close;
       }
   | Some b when b = bucket ->
       (* update existing 5m bar *)
       (match htf.current_bar with
        | None -> ()
        | Some b5 ->
            b5.high <- Float.max b5.high bar.high;
            b5.low  <- Float.min b5.low  bar.low;
            b5.close <- bar.close)
   | Some _ ->
       (* bucket changed: roll current, start new *)
       rollover_current ();
       htf.current_bucket <- Some bucket;
       htf.current_bar <- Some {
         date = bar.ts.date;
         minute_of_day = bucket * 5;
         open_ = bar.open_;
         high = bar.high;
         low  = bar.low;
         close = bar.close;
       });
  (* update leg and wedge state only when we have at least one completed 5m bar *)
  (match Deque.peek_back htf.bars with
   | None -> ()
   | Some last_bar ->
       (* simple leg: compare last 5m close vs previous 5m close *)
       let prev_bar_opt =
         match List.rev (Deque.to_list htf.bars) with
         | _last :: prev :: _ -> Some prev
         | _ -> None
       in
       (match prev_bar_opt with
        | None ->
            htf.leg_side <- Leg_none;
            htf.leg_len <- 0
        | Some prev ->
            let side =
              if Float.(last_bar.close > prev.close) then Leg_up
              else if Float.(last_bar.close < prev.close) then Leg_down
              else Leg_none
            in
            if phys_equal side htf.leg_side then
              htf.leg_len <- (max 1 (htf.leg_len + 1))
            else begin
              htf.leg_side <- side;
              htf.leg_len <- 1
            end);
       (* simple wedge detection on tail using Bar_patterns.simple_wedge_5m_on_tail *)
       let bars_list = Deque.to_list htf.bars in
       (match Bar_patterns.simple_wedge_5m_on_tail bars_list with
        | None ->
            htf.wedge_up <- false;
            htf.wedge_down <- false
        | Some w ->
            (match w.side with
             | Up ->
                 htf.wedge_up <- true;
                 htf.wedge_down <- false
             | Down ->
                 htf.wedge_up <- false;
                 htf.wedge_down <- true)))

let create () =
  { index = 0;
    prev2 = None;
    prev1 = None;
    last_swing_low = None;
    last_swing_high = None;
    prev_swing_low = None;
    prev_swing_high = None;
    leg_side = Leg_none;
    leg_start_idx = None;
    leg_range = 0.;
    micro_up_len = 0;
    micro_down_len = 0;
    soft_micro_up_len = 0;
    soft_micro_down_len = 0;
    soft_micro_up_len_max_leg = 0;
    soft_micro_down_len_max_leg = 0;
    soft_break_up_severity = None;
    soft_break_up_trend = false;
    soft_break_down_severity = None;
    soft_break_down_trend = false;
    range_state = create_range_state ~capacity:60;
    wedge_up = false;
    wedge_down = false;
    h1_flag = false;
    h2_flag = false;
    l1_flag = false;
    l2_flag = false;
    h_pullback_count = 0;
    l_pullback_count = 0;
    inside_bar = false;
    outside_bar = false;
    ii_flag = false;
    ioi_flag = false;
    outside_seq_len = 0;
    ai_state = Ai_flat;
    reg_sum_x = 0.;
    reg_sum_y = 0.;
    reg_sum_x2 = 0.;
    reg_sum_xy = 0.;
    reg_n = 0;
    recent_trend_bodies = Deque.create ();
    recent_bull_count = 0;
    recent_bear_count = 0;
    recent_doji_count = 0;
    recent_body_sum = 0.;
    htf_5m = create_htf_state_5m ();
    leg_mm_up = None;
    leg_mm_down = None;
    current_day = None;
    day_open = None;
    day_high = Float.neg_infinity;
    day_low = Float.infinity;
    day_close = None;
    day_first_60_high = None;
    day_first_60_low = None;
    prev_day_high = None;
    prev_day_low = None;
    prev_day_close = None;
    prev_day_regime = Day_unknown;
    consec_trend_days = 0;
  }

let reset_flags (s : state) =
  s.wedge_up <- false;
  s.wedge_down <- false;
  s.h1_flag <- false;
  s.h2_flag <- false;
  s.l1_flag <- false;
  s.l2_flag <- false;
  s.inside_bar <- false;
  s.outside_bar <- false;
  s.ii_flag <- false;
  s.ioi_flag <- false

let update_leg_from_swings (s : state) =
  match s.last_swing_low, s.last_swing_high with
  | Some low_sw, Some high_sw ->
    if low_sw.index > high_sw.index then begin
      (* Up leg from last swing low *)
      s.leg_side <- Leg_up;
      s.leg_start_idx <- Some low_sw.index
    end else begin
      s.leg_side <- Leg_down;
      s.leg_start_idx <- Some high_sw.index
    end
  | Some low_sw, None ->
    s.leg_side <- Leg_up;
    s.leg_start_idx <- Some low_sw.index
  | None, Some high_sw ->
    s.leg_side <- Leg_down;
    s.leg_start_idx <- Some high_sw.index
  | None, None ->
    s.leg_side <- Leg_none;
    s.leg_start_idx <- None

let record_swing_low (s : state) ~(index:int) ~(price:float) =
  (* shift previous low *)
  s.prev_swing_low <- s.last_swing_low;
  s.last_swing_low <- Some { index; price; kind = Swing_low };
  (* update down-leg measured move (from last swing high to this low) *)
  (match s.last_swing_high with
   | Some high_sw ->
       let leg_size = high_sw.price -. price in
       if Float.(leg_size > 0.) then
         s.leg_mm_down <- Some (price -. leg_size)
   | None -> ());
  (* update leg direction based on swings before we consider H1/H2 *)
  update_leg_from_swings s;
  (* H1/H2 approximation: in an up leg, count higher lows *)
  (match s.leg_side, s.prev_swing_low, s.last_swing_low with
   | Leg_up, Some prev, Some last_ when Float.(last_.price > prev.price) ->
     s.h_pullback_count <- s.h_pullback_count + 1;
     (match s.h_pullback_count with
      | 1 -> s.h1_flag <- true
      | 2 -> s.h2_flag <- true
      | _ -> ())
   | _ -> ())

let record_swing_high (s : state) ~(index:int) ~(price:float) =
  s.prev_swing_high <- s.last_swing_high;
  s.last_swing_high <- Some { index; price; kind = Swing_high };
  (* update up-leg measured move (from last swing low to this high) *)
  (match s.last_swing_low with
   | Some low_sw ->
       let leg_size = price -. low_sw.price in
       if Float.(leg_size > 0.) then
         s.leg_mm_up <- Some (price +. leg_size)
   | None -> ());
  update_leg_from_swings s;
  (* L1/L2 approximation: in a down leg, count lower highs *)
  (match s.leg_side, s.prev_swing_high, s.last_swing_high with
   | Leg_down, Some prev, Some last_ when Float.(last_.price < prev.price) ->
     s.l_pullback_count <- s.l_pullback_count + 1;
     (match s.l_pullback_count with
      | 1 -> s.l1_flag <- true
      | 2 -> s.l2_flag <- true
      | _ -> ())
   | _ -> ())

let detect_swings (s : state) (bar_t : bar_1m) =
  match s.prev2, s.prev1 with
  | Some b_prev2, Some b_prev1 ->
    let idx_swing = s.index - 1 in
    (* approximate ATR from recent range window *)
    let avg_range =
      if Deque.is_empty s.range_state.ranges then 0.
      else
        let sum = Deque.fold s.range_state.ranges ~init:0.0 ~f:( +. ) in
        sum /. Float.of_int (Deque.length s.range_state.ranges)
    in
    let swing_min_factor = 0.5 in
    let min_range =
      if Float.(avg_range <= 0.) then 0. else swing_min_factor *. avg_range
    in
    let range_prev1 = b_prev1.high -. b_prev1.low in
    (* swing low at prev1 *)
    if Float.(range_prev1 >= min_range)
       && Float.(b_prev1.low <= b_prev2.low && b_prev1.low <= bar_t.low)
    then
      record_swing_low s ~index:idx_swing ~price:b_prev1.low;
    (* swing high at prev1 *)
    if Float.(range_prev1 >= min_range)
       && Float.(b_prev1.high >= b_prev2.high && b_prev1.high >= bar_t.high)
    then
      record_swing_high s ~index:idx_swing ~price:b_prev1.high
  | _ -> ()

let update_micro (s : state) (bar_t : bar_1m) =
  match s.prev1 with
  | None ->
    s.micro_up_len <- 0;
    s.micro_down_len <- 0
  | Some prev ->
    if Float.(bar_t.close > prev.close) then begin
      s.micro_up_len <- s.micro_up_len + 1;
      s.micro_down_len <- 0
    end else if Float.(bar_t.close < prev.close) then begin
      s.micro_down_len <- s.micro_down_len + 1;
      s.micro_up_len <- 0
    end else begin
      s.micro_up_len <- 0;
      s.micro_down_len <- 0
    end

let recent_capacity = 20

let soft_body_frac_trend = 0.6
let soft_body_frac_doji  = 0.2
let soft_close_pos_thresh = 0.6

let update_recent_strength (s : state) (shape : bar_shape) =
  let trend = shape.trend in
  let body = shape.body in
  (* Enqueue new observation *)
  Deque.enqueue_back s.recent_trend_bodies (trend, body);
  (match trend with
   | Bull -> s.recent_bull_count <- s.recent_bull_count + 1
   | Bear -> s.recent_bear_count <- s.recent_bear_count + 1
   | Doji -> s.recent_doji_count <- s.recent_doji_count + 1);
  s.recent_body_sum <- s.recent_body_sum +. body;
  (* Enforce capacity and remove oldest if needed *)
  if Deque.length s.recent_trend_bodies > recent_capacity then
    match Deque.dequeue_front s.recent_trend_bodies with
    | None -> ()
    | Some (old_trend, old_body) ->
        (match old_trend with
         | Bull -> s.recent_bull_count <- max 0 (s.recent_bull_count - 1)
         | Bear -> s.recent_bear_count <- max 0 (s.recent_bear_count - 1)
         | Doji -> s.recent_doji_count <- max 0 (s.recent_doji_count - 1));
        s.recent_body_sum <- s.recent_body_sum -. old_body

let update_soft_micro (s : state) (bar : bar_1m) (shape : bar_shape) =
  match s.prev1 with
  | None ->
      s.soft_micro_up_len <- 0;
      s.soft_micro_down_len <- 0
  | Some prev ->
      let range = shape.range in
      let body  = shape.body in
      let body_frac =
        if Float.(range <= 0.) then 0. else Float.abs body /. range
      in
      let close_pos =
        if Float.(range <= 0.) then 0.5
        else (bar.close -. bar.low) /. range
      in
      let is_doji = Float.(body_frac <= soft_body_frac_doji) in
      let is_bear_trend =
        Float.(body < 0.) && Float.(body_frac >= soft_body_frac_trend)
      in
      let is_bull_trend =
        Float.(body > 0.) && Float.(body_frac >= soft_body_frac_trend)
      in
      (* approximate ATR using recent average range *)
      let avg_range =
        if Deque.is_empty s.range_state.ranges then 0.
        else
          let sum = Deque.fold s.range_state.ranges ~init:0.0 ~f:( +. ) in
          sum /. Float.of_int (Deque.length s.range_state.ranges)
      in
      (* Bull soft microchannel *)
      let bull_break =
        Float.(bar.low < prev.low) || is_bear_trend
      in
      if bull_break then begin
        let sev_pts =
          if Float.(bar.low < prev.low) then prev.low -. bar.low else 0.
        in
        let sev =
          if Float.(avg_range > 0.) then sev_pts /. avg_range else sev_pts
        in
        s.soft_break_up_severity <- Some sev;
        s.soft_break_up_trend <- is_bear_trend;
        s.soft_micro_up_len <- 0
      end else begin
        let bull_eligible =
          Float.(bar.low >= prev.low) && Float.(close_pos >= soft_close_pos_thresh)
        in
        if bull_eligible then begin
          s.soft_micro_up_len <- s.soft_micro_up_len + 1;
          if s.soft_micro_up_len > s.soft_micro_up_len_max_leg then
            s.soft_micro_up_len_max_leg <- s.soft_micro_up_len
        end else if is_doji && s.inside_bar then
          () (* neutral: keep length *)
        else
          () (* neither break nor extension: keep length as is *)
      end;
      (* Bear soft microchannel *)
      let bear_break =
        Float.(bar.high > prev.high) || is_bull_trend
      in
      if bear_break then begin
        let sev_pts =
          if Float.(bar.high > prev.high) then bar.high -. prev.high else 0.
        in
        let sev =
          if Float.(avg_range > 0.) then sev_pts /. avg_range else sev_pts
        in
        s.soft_break_down_severity <- Some sev;
        s.soft_break_down_trend <- is_bull_trend;
        s.soft_micro_down_len <- 0
      end else begin
        let bear_eligible =
          Float.(bar.high <= prev.high)
          && Float.(close_pos <= (1. -. soft_close_pos_thresh))
        in
        if bear_eligible then begin
          s.soft_micro_down_len <- s.soft_micro_down_len + 1;
          if s.soft_micro_down_len > s.soft_micro_down_len_max_leg then
            s.soft_micro_down_len_max_leg <- s.soft_micro_down_len
        end else if is_doji && s.inside_bar then
          ()
        else
          ()
      end

let update_bar_clusters (s : state) (bar_t : bar_1m) =
  match s.prev1 with
  | None ->
      s.inside_bar <- false;
      s.outside_bar <- false;
      s.ii_flag <- false;
      s.ioi_flag <- false;
      s.outside_seq_len <- 0
  | Some prev ->
      let inside =
        Float.(bar_t.high <= prev.high && bar_t.low >= prev.low)
      in
      let outside =
        Float.(bar_t.high >= prev.high && bar_t.low <= prev.low)
      in
      s.inside_bar <- inside;
      s.outside_bar <- outside;
      (* update outside sequence length *)
      if outside then
        s.outside_seq_len <- s.outside_seq_len + 1
      else
        s.outside_seq_len <- 0;
      (* ii / ioi patterns: we treat them as "just completed at this bar" *)
      (match s.prev2 with
       | Some b2 ->
           let prev_inside =
             Float.(prev.high <= b2.high && prev.low >= b2.low)
           in
           let prev_outside =
             Float.(prev.high >= b2.high && prev.low <= b2.low)
           in
           s.ii_flag <- prev_inside && inside;
           s.ioi_flag <- prev_inside && prev_outside && inside
       | None ->
           s.ii_flag <- false;
           s.ioi_flag <- false)

let reset_regression (s : state) =
  s.reg_sum_x <- 0.;
  s.reg_sum_y <- 0.;
  s.reg_sum_x2 <- 0.;
  s.reg_sum_xy <- 0.;
  s.reg_n <- 0

let update_leg_from_swings (s : state) =
  let prev_side = s.leg_side in
  match s.last_swing_low, s.last_swing_high with
  | Some low_sw, Some high_sw ->
    if low_sw.index > high_sw.index then begin
      (* Up leg from last swing low *)
      s.leg_side <- Leg_up;
      s.leg_start_idx <- Some low_sw.index;
      reset_regression s
    end else begin
      s.leg_side <- Leg_down;
      s.leg_start_idx <- Some high_sw.index;
      reset_regression s
    end
  | Some low_sw, None ->
    s.leg_side <- Leg_up;
    s.leg_start_idx <- Some low_sw.index;
    reset_regression s;
    s.soft_micro_up_len <- 0;
    s.soft_micro_down_len <- 0;
    s.soft_micro_up_len_max_leg <- 0;
    s.soft_micro_down_len_max_leg <- 0
  | None, Some high_sw ->
    s.leg_side <- Leg_down;
    s.leg_start_idx <- Some high_sw.index;
    reset_regression s;
    s.soft_micro_up_len <- 0;
    s.soft_micro_down_len <- 0;
    s.soft_micro_up_len_max_leg <- 0;
    s.soft_micro_down_len_max_leg <- 0
  | None, None ->
    s.leg_side <- Leg_none;
    s.leg_start_idx <- None;
    reset_regression s;
    s.soft_micro_up_len <- 0;
    s.soft_micro_down_len <- 0;
    s.soft_micro_up_len_max_leg <- 0;
    s.soft_micro_down_len_max_leg <- 0
  ;
  if not (phys_equal prev_side s.leg_side) then begin
    s.h_pullback_count <- 0;
    s.l_pullback_count <- 0
  end

let detect_wedges (s : state) =
  (* Simple swing-based 3-push wedge on highs/lows. *)
  match s.last_swing_low, s.prev_swing_low, s.last_swing_high, s.prev_swing_high with
  | Some low3, Some _, _, _ ->
    (* We need three lows: prev_swing_low, and an older one; approximate via indices *)
    (match s.prev_swing_low with
     | Some low2' ->
       (* find a yet older low via last_swing_low in prev_swing_low *)
       (match s.prev_swing_low, s.prev_swing_low with
        | Some _, Some _ ->
          (* Already captured by low3/low2; minimal check: descending lows and short span *)
          let span = s.index - low3.index in
          if Float.(low2'.price > low3.price) && span <= 60 then
            s.wedge_down <- true
        | _ -> ())
     | None -> ())
  | _ -> ();
  (* Up wedge via swing highs: use similar heuristic *)
  match s.last_swing_high, s.prev_swing_high with
  | Some high3, Some high2 ->
    let span = s.index - high3.index in
    if Float.(high2.price < high3.price) && span <= 60 then
      s.wedge_up <- true
  | _ -> ()

let update (s : state) (bar : bar_1m) : state =
  s.index <- s.index + 1;
  reset_flags s;
  (* day-level rollover and RTH state update *)
  let minute = bar.ts.minute_of_day in
  let rth_start = Time_utils.rth_start_min in
  let rth_end = Time_utils.rth_end_min in
  let in_rth = minute >= rth_start && minute <= rth_end in
  (* detect new calendar day and finalize previous day regime/run length *)
  (match s.current_day with
   | None ->
       s.current_day <- Some bar.ts.date;
       s.day_open <- None;
       s.day_high <- Float.neg_infinity;
       s.day_low <- Float.infinity;
       s.day_close <- None;
       s.day_first_60_high <- None;
       s.day_first_60_low <- None
   | Some d when not (Date.equal d bar.ts.date) ->
       (* finalize previous day *)
       (match s.day_open, s.day_close with
        | Some o, Some c ->
            let day_range = s.day_high -. s.day_low in
            let day_net = c -. o in
            let ratio_net =
              if Float.(day_range > 0.) then Float.abs day_net /. day_range else 0.
            in
            let early_range =
              match s.day_first_60_high, s.day_first_60_low with
              | Some h, Some l -> h -. l
              | _ -> 0.
            in
            let ratio_early =
              if Float.(day_range > 0.) then early_range /. day_range else 0.
            in
            let prev_regime =
              if Float.(day_range <= 0.) then Day_unknown
              else if Float.(ratio_early >= 0.7 && ratio_net >= 0.5) then Day_spike
              else if Float.(ratio_net >= 0.6) then Day_trend
              else Day_range
            in
            s.prev_day_high <-
              (if Float.is_finite s.day_high then Some s.day_high else None);
            s.prev_day_low <-
              (if Float.is_finite s.day_low then Some s.day_low else None);
            s.prev_day_close <- Some c;
            s.prev_day_regime <- prev_regime;
            (match prev_regime with
             | Day_trend | Day_spike ->
                 s.consec_trend_days <- s.consec_trend_days + 1
             | _ ->
                 s.consec_trend_days <- 0)
        | _ ->
            s.consec_trend_days <- 0);
       (* reset for new day *)
       s.current_day <- Some bar.ts.date;
       s.day_open <- None;
       s.day_high <- Float.neg_infinity;
       s.day_low <- Float.infinity;
       s.day_close <- None;
       s.day_first_60_high <- None;
       s.day_first_60_low <- None
   | Some _ -> ());
  (* update current-day RTH stats *)
  if in_rth then begin
    (match s.day_open with
     | None ->
         s.day_open <- Some bar.open_;
         s.day_high <- bar.high;
         s.day_low <- bar.low;
         s.day_close <- Some bar.close;
         s.day_first_60_high <- Some bar.high;
         s.day_first_60_low <- Some bar.low
     | Some _ ->
         if Float.(bar.high > s.day_high) then s.day_high <- bar.high;
         if Float.(bar.low < s.day_low) then s.day_low <- bar.low;
         s.day_close <- Some bar.close;
         let rel_min = minute - rth_start in
         if rel_min < 60 then
           (match s.day_first_60_high, s.day_first_60_low with
            | None, _ ->
                s.day_first_60_high <- Some bar.high;
                s.day_first_60_low <- Some bar.low
            | Some h, Some l ->
                if Float.(bar.high > h) then s.day_first_60_high <- Some bar.high;
                if Float.(bar.low < l) then s.day_first_60_low <- Some bar.low
            | Some h, None ->
                if Float.(bar.high > h) then s.day_first_60_high <- Some bar.high;
                s.day_first_60_low <- Some bar.low))
  end;
  (* detect swings using prev2, prev1, and current bar *)
  detect_swings s bar;
  (* update microchannel lengths *)
  update_micro s bar;
  (* update bar clusters *)
  update_bar_clusters s bar;
  (* trading-range events are recorded against the *previous* window, then we
     update the window with the current bar. This lets failed breakouts be
     detected as "beyond prior hi/lo, close back inside". *)
  record_range_events s.range_state ~bar;
  update_range s.range_state ~high:bar.high ~low:bar.low;
  (* update higher timeframe 5m structure *)
  update_htf_5m s.htf_5m bar;
  (* update leg_length and leg_range approximately from swings *)
  (match s.leg_start_idx, s.leg_side with
   | Some _, Leg_up ->
     s.leg_range <- bar.close -. (match s.last_swing_low with Some sw -> sw.price | None -> bar.close);
     ()
   | Some _, Leg_down ->
     s.leg_range <- (match s.last_swing_high with Some sw -> sw.price | None -> bar.close) -. bar.close;
     ()
   | _ ->
     s.leg_range <- 0.);
  (* regression channel accumulators over current leg, using close vs index *)
  (match s.leg_side with
   | Leg_up | Leg_down ->
       let x = Float.of_int s.index in
       let y = bar.close in
       s.reg_sum_x <- s.reg_sum_x +. x;
       s.reg_sum_y <- s.reg_sum_y +. y;
       s.reg_sum_x2 <- s.reg_sum_x2 +. (x *. x);
       s.reg_sum_xy <- s.reg_sum_xy +. (x *. y);
       s.reg_n <- s.reg_n + 1
   | Leg_none -> ());
  (* detect wedges from swing state at this bar *)
  detect_wedges s;
  (* bar-strength snapshot and always-in update use current bar shape *)
  let shape =
    Bar_patterns.bar_shape_of_ohlc
      ~open_:bar.open_ ~high:bar.high ~low:bar.low ~close:bar.close
  in
  update_soft_micro s bar shape;
  update_recent_strength s shape;
  (* update always-in state using simple strong-bar + leg heuristics *)
  (match s.prev1 with
   | None -> ()
   | Some _prev ->
       let body_frac =
         if Float.(shape.range <= 0.) then 0.
         else Float.abs shape.body /. shape.range
       in
       let (_lo, _hi, _pos, regime, _th, _tl, _fh, _fl, _tight) =
         range_snapshot s.range_state (Some bar.close)
       in
       let strong_trend_bar =
         Float.(body_frac >= 0.5)
       in
       let in_trend = match regime with Trend -> true | _ -> false in
       let flip_to_long =
         in_trend
         && strong_trend_bar
         && (match shape.trend with Bull -> true | _ -> false)
         && phys_equal s.leg_side Leg_up
       in
       let flip_to_short =
         in_trend
         && strong_trend_bar
         && (match shape.trend with Bear -> true | _ -> false)
         && phys_equal s.leg_side Leg_down
       in
       s.ai_state <-
         (match s.ai_state with
          | Ai_flat ->
              if flip_to_long then Ai_long
              else if flip_to_short then Ai_short
              else Ai_flat
          | Ai_long ->
              let strong_bear =
                strong_trend_bar && (match shape.trend with Bear -> true | _ -> false)
              in
              if strong_bear && (s.micro_down_len >= 2 || s.l1_flag || s.l2_flag)
              then Ai_short
              else Ai_long
          | Ai_short ->
              let strong_bull =
                strong_trend_bar && (match shape.trend with Bull -> true | _ -> false)
              in
              if strong_bull && (s.micro_up_len >= 2 || s.h1_flag || s.h2_flag)
              then Ai_long
              else Ai_short));
  (* shift prev bars *)
  s.prev2 <- s.prev1;
  s.prev1 <- Some bar;
  s

let snapshot (s : state) : snapshot =
  let (_lo, _hi, pos_in_range, regime, tests_high, tests_low, fails_high, fails_low, tight) =
    range_snapshot s.range_state (Option.map s.prev1 ~f:(fun b -> b.close))
  in
  let width = _hi -. _lo in
  let avg_range =
    if Deque.is_empty s.range_state.ranges then 0.
    else
      let sum = Deque.fold s.range_state.ranges ~init:0.0 ~f:( +. ) in
      sum /. Float.of_int (Deque.length s.range_state.ranges)
  in
  let range_width_ratio =
    if Float.(avg_range > 0.) then width /. avg_range else 0.
  in
  let range_mid, range_mm_up, range_mm_down =
    if Float.(width > 0.) then
      let mid = (_hi +. _lo) /. 2. in
      let mm_up = _hi +. width in
      let mm_down = _lo -. width in
      (Some mid, Some mm_up, Some mm_down)
    else (None, None, None)
  in
  let window_size = Float.of_int s.range_state.capacity in
  let range_test_rate_high =
    if Float.(window_size > 0.) then Float.of_int tests_high /. window_size else 0.
  in
  let range_test_rate_low =
    if Float.(window_size > 0.) then Float.of_int tests_low /. window_size else 0.
  in
  let range_fail_rate_high =
    if Float.(window_size > 0.) then Float.of_int fails_high /. window_size else 0.
  in
  let range_fail_rate_low =
    if Float.(window_size > 0.) then Float.of_int fails_low /. window_size else 0.
  in
  let leg_len =
    match s.leg_start_idx with
    | None -> 0
    | Some start_idx -> Int.max 0 (s.index - start_idx + 1)
  in
  let channel_midline, channel_slope, channel_overshoot =
    if s.reg_n >= 2 then
      let n = Float.of_int s.reg_n in
      let denom = (n *. s.reg_sum_x2) -. (s.reg_sum_x *. s.reg_sum_x) in
      if Float.(denom = 0.) then (None, None, false)
      else
        let slope = ((n *. s.reg_sum_xy) -. (s.reg_sum_x *. s.reg_sum_y)) /. denom in
        let intercept = (s.reg_sum_y -. (slope *. s.reg_sum_x)) /. n in
        let x = Float.of_int s.index in
        let mid = (slope *. x) +. intercept in
        let close_opt = Option.map s.prev1 ~f:(fun b -> b.close) in
        match close_opt with
        | None -> (Some mid, Some slope, false)
        | Some c ->
            let z =
              if Float.(avg_range > 0.) then Float.abs (c -. mid) /. avg_range else 0.
            in
            let overshoot = Float.(z >= 3.) in
            (Some mid, Some slope, overshoot)
    else (None, None, false)
  in
  let major_channel_midline, major_channel_slope, major_channel_overshoot =
    let close_opt = Option.map s.prev1 ~f:(fun b -> b.close) in
    match s.leg_side, close_opt with
    | Leg_up, Some close_price ->
        (match s.prev_swing_low, s.last_swing_low with
         | Some low1, Some low2 when low2.index > low1.index ->
             let x1 = Float.of_int low1.index
             and y1 = low1.price in
             let x2 = Float.of_int low2.index
             and y2 = low2.price in
             let dx = x2 -. x1 in
             if Float.(dx = 0.) then (None, None, false)
             else
             let slope = (y2 -. y1) /. dx in
             let intercept = y1 -. (slope *. x1) in
             let x = Float.of_int s.index in
             let mid = (slope *. x) +. intercept in
             let overshoot =
               let z =
                 if Float.(avg_range > 0.) then Float.abs (close_price -. mid) /. avg_range else 0.
               in
               Float.(z >= 3.)
             in
             (Some mid, Some slope, overshoot)
         | _ -> (None, None, false))
    | Leg_down, Some close_price ->
        (match s.prev_swing_high, s.last_swing_high with
         | Some high1, Some high2 when high2.index > high1.index ->
             let x1 = Float.of_int high1.index
             and y1 = high1.price in
             let x2 = Float.of_int high2.index
             and y2 = high2.price in
             let dx = x2 -. x1 in
             if Float.(dx = 0.) then (None, None, false)
             else
             let slope = (y2 -. y1) /. dx in
             let intercept = y1 -. (slope *. x1) in
             let x = Float.of_int s.index in
             let mid = (slope *. x) +. intercept in
             let overshoot =
               let z =
                 if Float.(avg_range > 0.) then Float.abs (close_price -. mid) /. avg_range else 0.
               in
               Float.(z >= 3.)
             in
             (Some mid, Some slope, overshoot)
         | _ -> (None, None, false))
    | _ -> (None, None, false)
  in
  let micro_channel_z =
    match channel_midline, Option.map s.prev1 ~f:(fun b -> b.close) with
    | Some mid, Some c when Float.(avg_range > 0.) ->
        Some (Float.abs (c -. mid) /. avg_range)
    | _ -> None
  in
  let major_channel_z =
    match major_channel_midline, Option.map s.prev1 ~f:(fun b -> b.close) with
    | Some mid, Some c when Float.(avg_range > 0.) ->
        Some (Float.abs (c -. mid) /. avg_range)
    | _ -> None
  in
  let bar_shape_opt =
    Option.map s.prev1 ~f:Bar_patterns.bar_shape_of_bar_1m
  in
  let bar_body, bar_range, bar_body_frac, bar_close_pos, bar_is_trend, bar_is_doji =
    match bar_shape_opt, s.prev1 with
    | Some shape, Some b ->
        let body = shape.body in
        let range = shape.range in
        if Float.(range <= 0.) then
          (body, range, None, None, false, true)
        else
          let body_frac = Float.abs body /. range in
          let close_pos = (b.close -. b.low) /. range in
          let is_trend = Float.(body_frac >= 0.6) in
          let is_doji = Float.(body_frac <= 0.2) in
          (body, range, Some body_frac, Some close_pos, is_trend, is_doji)
    | _ ->
        (0., 0., None, None, false, true)
  in
  (* day-level features based on RTH state *)
  let day_open_opt = s.day_open in
  let day_close_opt = s.day_close in
  let day_range_opt =
    if Float.is_finite s.day_high && Float.is_finite s.day_low then
      let rg = s.day_high -. s.day_low in
      if Float.(rg > 0.) then Some rg else None
    else None
  in
  let day_net_change =
    match day_open_opt, day_close_opt with
    | Some o, Some c -> Some (c -. o)
    | _ -> None
  in
  let day_pos_in_range =
    match day_range_opt, day_close_opt with
    | Some rg, Some c when Float.(rg > 0.) ->
        Some ((c -. s.day_low) /. rg)
    | _ -> None
  in
  let day_regime =
    match day_open_opt, day_close_opt, day_range_opt with
    | Some o, Some c, Some rg ->
        let day_net = c -. o in
        let ratio_net =
          if Float.(rg > 0.) then Float.abs day_net /. rg else 0.
        in
        let early_range =
          match s.day_first_60_high, s.day_first_60_low with
          | Some h, Some l -> h -. l
          | _ -> 0.
        in
        let ratio_early =
          if Float.(rg > 0.) then early_range /. rg else 0.
        in
        if Float.(ratio_early >= 0.7 && ratio_net >= 0.5) then Day_spike
        else if Float.(ratio_net >= 0.6) then Day_trend
        else Day_range
    | _ -> Day_unknown
  in
  let day_inside_prev_range, dist_prev_day_high, dist_prev_day_low =
    match s.prev_day_high, s.prev_day_low, day_close_opt with
    | Some ph, Some pl, Some c ->
        let inside = Float.(c <= ph && c >= pl) in
        let d_hi = c -. ph in
        let d_lo = c -. pl in
        (Some inside, Some d_hi, Some d_lo)
    | _ -> (None, None, None)
  in
  let intraday_phase =
    match s.prev1 with
    | None -> Pre_rth
    | Some b ->
        let m = b.ts.minute_of_day in
        let rs = Time_utils.rth_start_min in
        let re = Time_utils.rth_end_min in
        if m < rs then Pre_rth
        else if m <= rs + 60 then Rth_open
        else if m <= re - 60 then Rth_mid
        else if m <= re then Rth_late
        else Post_rth
  in
  { leg_side = s.leg_side;
    leg_len_bars = leg_len;
    leg_range = s.leg_range;
    range_regime = regime;
    pos_in_range;
    range_tests_high = tests_high;
    range_tests_low = tests_low;
    range_failed_high = fails_high;
    range_failed_low = fails_low;
    range_tight = tight;
    range_test_rate_high;
    range_test_rate_low;
    range_fail_rate_high;
    range_fail_rate_low;
    range_width_ratio;
    day_regime;
    day_net_change;
    day_range = day_range_opt;
    day_pos_in_range;
    day_inside_prev_range;
    day_trend_run_len = s.consec_trend_days;
    dist_prev_day_high;
    dist_prev_day_low;
    intraday_phase;
    wedge_up = s.wedge_up;
    wedge_down = s.wedge_down;
    h1 = s.h1_flag;
    h2 = s.h2_flag;
    l1 = s.l1_flag;
    l2 = s.l2_flag;
    h_pullback_count = s.h_pullback_count;
    l_pullback_count = s.l_pullback_count;
    micro_up_len = s.micro_up_len;
    micro_down_len = s.micro_down_len;
    soft_micro_up_len = s.soft_micro_up_len;
    soft_micro_down_len = s.soft_micro_down_len;
    soft_micro_up_len_max_leg = s.soft_micro_up_len_max_leg;
    soft_micro_down_len_max_leg = s.soft_micro_down_len_max_leg;
    soft_micro_bias = s.soft_micro_up_len - s.soft_micro_down_len;
    inside_bar = s.inside_bar;
    outside_bar = s.outside_bar;
    ii_pattern = s.ii_flag;
    ioi_pattern = s.ioi_flag;
    consecutive_outside = (s.outside_seq_len >= 2);
    bar_body;
    bar_range;
    bar_body_frac;
    bar_close_pos;
    bar_is_trend;
    bar_is_doji;
    micro_channel_midline = channel_midline;
    micro_channel_slope = channel_slope;
    micro_channel_z;
    micro_channel_overshoot = channel_overshoot;
    major_channel_midline;
    major_channel_slope;
    major_channel_z;
    major_channel_overshoot;
    leg_mm_up = s.leg_mm_up;
    leg_mm_down = s.leg_mm_down;
    range_mid;
    range_mm_up;
    range_mm_down;
    soft_break_up_severity = s.soft_break_up_severity;
    soft_break_up_trend = s.soft_break_up_trend;
    soft_break_down_severity = s.soft_break_down_severity;
    soft_break_down_trend = s.soft_break_down_trend;
    recent_bull_count = s.recent_bull_count;
    recent_bear_count = s.recent_bear_count;
    recent_doji_count = s.recent_doji_count;
    recent_body_sum = s.recent_body_sum;
    recent_strength_score =
      (let denom = s.recent_bull_count + s.recent_bear_count in
       if denom <= 0 then None
       else Some (s.recent_body_sum /. Float.of_int denom));
    always_in = s.ai_state;
    htf_leg_side_5m = s.htf_5m.leg_side;
    htf_leg_len_bars_5m = s.htf_5m.leg_len;
    htf_wedge_up_5m = s.htf_5m.wedge_up;
    htf_wedge_down_5m = s.htf_5m.wedge_down;
  }
