open Core
open Strategy_fast
open Types

module EP = Engine.Execution_params
module TT = Engine.Trade_transition
module EM = Engine.Execution_model

let date = Date.of_string "2020-06-01"

let ts minute_of_day = { date; minute_of_day }

let base_plan ~direction =
  { direction;
    entry_price = 100.;
    cancel_level = 0.;
    stop_init = 99.;
    r_pts = 1.0;
    target_mult = 1.0;
    target_price = 101.;
    be_trigger = 100.5;
    b2_end_minute = 600;
    downgrade_after_b2 = false;
    abr_prev = 0.;
    b1_range = 1.;
    b2_follow = Types.Follow_good; }

let active_state ~stop_price ~entry_price ~qty =
  { stop_price;
    moved_to_be = false;
    entry_ts = ts 1;
    entry_price;
    entry_value = entry_price *. qty;
    qty;
    pending_entry_qty = 0.;
    pending_entry_cancel_after = 0; }

let record_exit_reason ~active:_ ~exit_ts:_ ~exit_price:_ ~exit_qty:_ ~exit_reason = exit_reason
let record_exit_price ~active:_ ~exit_ts:_ ~exit_price ~exit_qty:_ ~exit_reason:_ = exit_price

let exec_path_order =
  { (EP.default ~tick_size:0.25) with
    spread_ticks = 0.;
    slip_model = EP.No_slip;
    exit_priority = EP.Path_order;
    break_even_intrabar = true;
    rng_state = EM.make_rng ~seed:7 (); }

let exec_stop_first = { exec_path_order with exit_priority = EP.Stop_first }

let bar_up_hits_target_then_stop =
  { ts = ts 2;
    open_ = 100.;
    high = 101.2;  (* target touched first *)
    low = 98.8;    (* stop touched later in path *)
    close = 100.1;
    volume = 4.0; }

let%test_unit "path-order exits target when target touched before stop" =
  let plan = base_plan ~direction:Long in
  let active = active_state ~stop_price:plan.stop_init ~entry_price:plan.entry_price ~qty:1.0 in
  let state, trades =
    TT.step_with_exec ~exec:exec_path_order ~plan ~state:(Active active)
      ~bar:bar_up_hits_target_then_stop ~record_trade:record_exit_reason
  in
  assert (match state with Done -> true | _ -> false);
  let exit_reason = List.hd_exn trades in
  assert (Poly.(exit_reason = Types.Target))

let%test_unit "stop-first exits stop even if target touched earlier" =
  let plan = base_plan ~direction:Long in
  let active = active_state ~stop_price:plan.stop_init ~entry_price:plan.entry_price ~qty:1.0 in
  let _, trades =
    TT.step_with_exec ~exec:exec_stop_first ~plan ~state:(Active active)
      ~bar:bar_up_hits_target_then_stop ~record_trade:record_exit_reason
  in
  let exit_reason = List.hd_exn trades in
  assert (Poly.(exit_reason = Types.Stop))

let%test_unit "slippage model adjusts exit price (constant ticks on sell)" =
  let plan = base_plan ~direction:Long in
  let active = active_state ~stop_price:plan.stop_init ~entry_price:plan.entry_price ~qty:1.0 in
  let touch_price = plan.target_price in
  let exec =
    { exec_path_order with
      slip_model = EP.Constant_ticks 2.0;  (* 2 ticks * 0.25 = 0.5 *)
      spread_ticks = 0.;
      rng_state = EM.make_rng ~seed:11 (); }
  in
  let _, trades =
    TT.step_with_exec ~exec ~plan ~state:(Active active)
      ~bar:{ bar_up_hits_target_then_stop with high = touch_price; low = touch_price; close = touch_price }
      ~record_trade:record_exit_price
  in
  let exit_px = List.hd_exn trades in
  (* Exit side is Sell for a long, so slip subtracts from touch price *)
  assert (Float.(abs (exit_px -. (touch_price -. 0.5)) < 1e-6))

let%test_unit "probabilistic slippage is reproducible with seed" =
  let exec =
    { exec_path_order with
      slip_model = EP.Prob_one_tick { prob = 0.5 };
      spread_ticks = 0.;
      rng_state = EM.make_rng ~seed:123 (); }
  in
  let px1 = EM.adjust_price ~params:exec ~side:EM.Buy ~rng:exec.rng_state 100. in
  let px2 = EM.adjust_price ~params:exec ~side:EM.Buy ~rng:exec.rng_state 100. in
  let exec2 = { exec with rng_state = EM.make_rng ~seed:123 () } in
  let q1 = EM.adjust_price ~params:exec2 ~side:EM.Buy ~rng:exec2.rng_state 100. in
  let q2 = EM.adjust_price ~params:exec2 ~side:EM.Buy ~rng:exec2.rng_state 100. in
  assert (Float.(abs (px1 -. q1) < 1e-9));
  assert (Float.(abs (px2 -. q2) < 1e-9));
  let tick = exec.tick_size in
  assert (List.mem [0.; tick] (Float.abs (px2 -. px1)) ~equal:Float.equal)

let%test_unit "down-bar path uses O-L-H-C order for exit resolution" =
  let plan = base_plan ~direction:Short in
  let active =
    active_state ~stop_price:101. ~entry_price:plan.entry_price ~qty:1.0
  in
  (* Down bar: open above close; path = O, L, H, C.
     Low hits target first; high hits stop later. *)
  let bar =
    { ts = ts 2; open_ = 100.; high = 102.; low = 98.5; close = 99.0; volume = 4. }
  in
  let exec =
    { exec_path_order with
      exit_priority = EP.Path_order;
      break_even_intrabar = false; }
  in
  let _, trades =
    TT.step_with_exec ~exec ~plan ~state:(Active active)
      ~bar ~record_trade:record_exit_reason
  in
  let exit_reason = List.hd_exn trades in
  assert (Poly.(exit_reason = Types.Target));
  (* If stop-first, would stop even though target touched earlier. *)
  let exec_stop = { exec with exit_priority = EP.Stop_first } in
  let _, trades_stop =
    TT.step_with_exec ~exec:exec_stop
      ~plan ~state:(Active { active with stop_price = 101.; moved_to_be = false })
      ~bar ~record_trade:record_exit_reason
  in
  let exit_reason_stop = List.hd_exn trades_stop in
  assert (Poly.(exit_reason_stop = Types.Stop))

let%test_unit "entry slippage and spread apply on buys" =
  let plan = { (base_plan ~direction:Long) with target_price = 105.; be_trigger = 200. } in
  let exec =
    { exec_path_order with
      spread_ticks = 1.0;
      slip_model = EP.Constant_ticks 2.0; (* 2 ticks *)
      rng_state = EM.make_rng ~seed:5 (); }
  in
  let bar =
    { ts = ts 1; open_ = 100.; high = 101.; low = 99.5; close = 100.4; volume = 4. }
  in
  let state, trades =
    TT.step_with_exec ~exec ~plan ~state:(TT.init_pending ~qty:1.0 ~latency_bars:0 ~cancel_after:(-1)) ~bar
      ~record_trade:record_exit_reason
  in
  assert (List.is_empty trades);
  match state with
  | Active a ->
      let expected = 100. +. (exec.tick_size *. (exec.spread_ticks /. 2.)) +. (exec.tick_size *. 2.) in
      assert (Float.(abs (a.entry_price -. expected) < 1e-6))
  | _ -> assert false

let%test_unit "exit spread worsens buy-to-cover on short target" =
  let plan = { (base_plan ~direction:Short) with target_price = 98.; stop_init = 102.; cancel_level = 150. } in
  let active = active_state ~stop_price:101. ~entry_price:plan.entry_price ~qty:1.0 in
  let touch = 98.0 in
  let exec =
    { exec_path_order with
      spread_ticks = 2.0; (* half-spread = 1 tick = 0.25 *)
      slip_model = EP.No_slip;
      rng_state = EM.make_rng ~seed:9 (); }
  in
  let bar =
    { ts = ts 2; open_ = 100.; high = 100.; low = touch; close = touch; volume = 4. }
  in
  let _, trades =
    TT.step_with_exec ~exec ~plan ~state:(Active active)
      ~bar ~record_trade:record_exit_price
  in
  let exit_px = List.hd_exn trades in
  assert (Float.(exit_px > touch))

let%test_unit "break-even intrabar can flip to stop within same bar" =
  let plan = { (base_plan ~direction:Long) with be_trigger = 100.5; stop_init = 99.0; target_price = 105.; cancel_level = 80. } in
  let bar =
    { ts = ts 3; open_ = 100.; high = 101.0; low = 99.4; close = 100.2; volume = 4. }
  in
  let active_be = active_state ~stop_price:plan.stop_init ~entry_price:plan.entry_price ~qty:1.0 in
  let active_close = active_state ~stop_price:plan.stop_init ~entry_price:plan.entry_price ~qty:1.0 in
  let exec_be = { exec_path_order with break_even_intrabar = true } in
  let exec_close = { exec_path_order with break_even_intrabar = false } in
  let _, trades_be =
    TT.step_with_exec ~exec:exec_be ~plan ~state:(Active active_be)
      ~bar ~record_trade:record_exit_reason
  in
  assert (not (List.is_empty trades_be));
  assert (Poly.(List.hd_exn trades_be = Types.Stop));
  let _, trades_close =
    TT.step_with_exec ~exec:exec_close ~plan ~state:(Active active_close)
      ~bar ~record_trade:record_exit_reason
  in
  assert (List.is_empty trades_close)
