open Core
open Types
open Time_utils

let tick_size = 0.25

let build_trade_plan (s : setup) : trade_plan option =
  let b1 = s.b1 in
  let b2 = s.b2 in
  let b1_range = b1.high -. b1.low in
  let direction = s.direction in

  let entry_price, cancel_level, stop_init, b2_good =
    match direction with
    | Long ->
        let entry_price  = b1.high +. tick_size in
        let cancel_level = b1.low in
        let stop_init    = b1.low -. tick_size in
        let b2_good      = Float.(b2.close > b2.open_) in
        entry_price, cancel_level, stop_init, b2_good
    | Short ->
        let entry_price  = b1.low -. tick_size in
        let cancel_level = b1.high in
        let stop_init    = b1.high +. tick_size in
        let b2_good      = Float.(b2.close < b2.open_) in
        entry_price, cancel_level, stop_init, b2_good
  in
  let r_pts = Float.abs (entry_price -. stop_init) in
  if Float.(r_pts <= 0.) || Float.(s.abr_prev <= 0.) then
    None
  else begin
    let can_use_twoR = Float.(b1_range <= 1.5 *. s.abr_prev) in
    let initial_target_mult = if can_use_twoR then 2.0 else 1.0 in
    let target_price =
      match direction with
      | Long  -> entry_price +. initial_target_mult *. r_pts
      | Short -> entry_price -. initial_target_mult *. r_pts
    in
    let be_trigger =
      match direction with
      | Long  -> entry_price +. 0.8 *. r_pts
      | Short -> entry_price -. 0.8 *. r_pts
    in
    let b2_follow = if b2_good then Follow_good else Follow_poor in
    let downgrade_after_b2 = can_use_twoR && (not b2_good) in
    let b2_end_minute = b2_min + 4 in
    Some {
      direction;
      entry_price;
      cancel_level;
      stop_init;
      r_pts;
      target_mult  = initial_target_mult;
      target_price;
      be_trigger;
      b2_end_minute;
      downgrade_after_b2;
      abr_prev = s.abr_prev;
      b1_range;
      b2_follow;
    }
  end
