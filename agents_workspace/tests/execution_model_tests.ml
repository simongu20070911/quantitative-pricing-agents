open Core
open Strategy_fast
open Types

module TT = Engine.Trade_transition

let date = Date.of_string "2020-02-01"

let mk_bar ~minute ~open_ ~high ~low ~close ~volume =
  let ts = { date; minute_of_day = minute } in
  { ts; open_; high; low; close; volume }

let base_plan ~direction =
  { direction;
    entry_price = 100.;
    cancel_level = 99.;
    stop_init = 99.;
    r_pts = 1.0;
    target_mult = 2.0;
    target_price = (match direction with Long -> 102. | Short -> 98.);
    be_trigger = 101.;
    b2_end_minute = 600;
    downgrade_after_b2 = false;
    abr_prev = 0.;
    b1_range = 1.;
    b2_follow = Types.Follow_good; }

let make_trade ~(plan : trade_plan) ~(active : active_state)
    ~(exit_ts : timestamp) ~(exit_price : float) ~(exit_qty : float)
    ~(exit_reason : exit_reason) : trade =
  let pnl_pts =
    match plan.direction with
    | Long -> (exit_price -. active.entry_price)
    | Short -> (active.entry_price -. exit_price)
  in
  { date = exit_ts.date;
    direction = plan.direction;
    entry_ts = active.entry_ts;
    exit_ts;
    entry_price = active.entry_price;
    exit_price;
    qty = exit_qty;
    r_pts = plan.r_pts;
    pnl_pts;
    pnl_R = pnl_pts /. plan.r_pts;
    pnl_usd = 0.;
    pnl_pct = None;
    duration_min = Float.of_int (exit_ts.minute_of_day - active.entry_ts.minute_of_day);
    exit_reason;
    meta = []; }

let%test_unit "gap through stop exits at gap open" =
  let exec =
    let base = Execution_params.default ~tick_size:0.25 in
    { base with spread_ticks = 0.; slip_model = Execution_params.No_slip;
                latency_bars = 0; allow_same_bar_entry = true }
  in
  let plan = base_plan ~direction:Long in
  let active =
    { stop_price = 99.; moved_to_be = false;
      entry_ts = { date; minute_of_day = 0 };
      entry_price = 100.; entry_value = 100.; qty = 1.;
      pending_entry_qty = 0.; pending_entry_cancel_after = 0; }
  in
  let bar = mk_bar ~minute:1 ~open_:95.0 ~high:96.0 ~low:94.0 ~close:95.0 ~volume:10. in
  let _, trades =
    TT.step ~exec ~plan ~state:(Active active) ~bar
      ~record_trade:(make_trade ~plan)
  in
  assert (List.length trades = 1);
  let t = List.hd_exn trades in
  assert (Float.(t.exit_price = 95.));
  assert (Poly.(t.exit_reason = Stop))

let%test_unit "latency one bar delays entry" =
  let exec = { (Execution_params.default ~tick_size:0.25) with latency_bars = 1 } in
  let plan = base_plan ~direction:Long in
  let pending = TT.init_pending ~qty:1.0 ~latency_bars:exec.latency_bars ~cancel_after:exec.cancel_latency_bars in
  let bar1 = mk_bar ~minute:1 ~open_:100.1 ~high:100.2 ~low:99.9 ~close:100.15 ~volume:5. in
  let state1, trades1 =
    TT.step ~exec ~plan ~state:pending ~bar:bar1 ~record_trade:(make_trade ~plan)
  in
  assert (List.is_empty trades1);
  (* latency=1 => still pending after first bar *)
  assert (match state1 with Pending _ -> true | _ -> false);
  let bar2 = mk_bar ~minute:2 ~open_:100.3 ~high:100.6 ~low:100.2 ~close:100.5 ~volume:5. in
  let state2, trades2 =
    TT.step ~exec ~plan ~state:state1 ~bar:bar2 ~record_trade:(make_trade ~plan)
  in
  assert (List.is_empty trades2);
  assert (match state2 with Active _ -> true | _ -> false)

let%test_unit "spread widens entry fill" =
  let exec =
    let base = Execution_params.default ~tick_size:0.25 in
    { base with spread_ticks = 2.0; slip_model = Execution_params.No_slip;
                latency_bars = 0; allow_same_bar_entry = true }
  in
  let plan = base_plan ~direction:Long in
  let pending = TT.init_pending ~qty:1.0 ~latency_bars:exec.latency_bars ~cancel_after:exec.cancel_latency_bars in
  let bar = mk_bar ~minute:1 ~open_:100.0 ~high:100.5 ~low:99.8 ~close:100.4 ~volume:4. in
  let state, _ =
    TT.step ~exec ~plan ~state:pending ~bar ~record_trade:(make_trade ~plan)
  in
  match state with
  | Active a ->
      let expected = 100.0 +. Execution_params.apply_tick exec (exec.spread_ticks /. 2.) in
      assert (Float.(abs (a.entry_price -. expected) < 1e-6))
  | _ -> assert false

let%test_unit "partial stop fills across bars" =
  let exec =
    let base = Execution_params.default ~tick_size:0.25 in
    { base with spread_ticks = 0.; slip_model = Execution_params.No_slip;
                latency_bars = 0; allow_same_bar_entry = true; allow_partial_fills = true }
  in
  let plan = base_plan ~direction:Long in
  let active =
    { stop_price = 99.; moved_to_be = false;
      entry_ts = { date; minute_of_day = 0 };
      entry_price = 100.; entry_value = 100.; qty = 1.;
      pending_entry_qty = 0.; pending_entry_cancel_after = 0; }
  in
  let bar1 = mk_bar ~minute:1 ~open_:100.0 ~high:100.0 ~low:98.0 ~close:98.5 ~volume:1.0 in
  let state1, trades1 =
    TT.step ~exec ~plan ~state:(Active active) ~bar:bar1 ~record_trade:(make_trade ~plan)
  in
  assert (not (List.is_empty trades1));
  let t1 = List.hd_exn trades1 in
  assert (Float.(t1.qty < 1.0 +. 1e-6));
  let bar2 = mk_bar ~minute:2 ~open_:97.5 ~high:97.5 ~low:97.0 ~close:97.2 ~volume:4.0 in
  let state2, trades2 =
    TT.step ~exec ~plan ~state:state1 ~bar:bar2 ~record_trade:(make_trade ~plan)
  in
  assert (not (List.is_empty trades2));
  assert (match state2 with Done -> true | _ -> false)
