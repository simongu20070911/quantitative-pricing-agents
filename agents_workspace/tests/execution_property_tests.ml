open Core
open Strategy_fast
open Types

module EP = Execution_params
module EM = Execution_model
module TT = Engine.Trade_transition

let date = Date.of_string "2020-03-01"

let mk_ts minute = { date; minute_of_day = minute }

let mk_bar ~open_ ~high ~low ~close ~volume ~minute =
  { ts = mk_ts minute; open_; high; low; close; volume }

let base_plan ~direction =
  {
    direction;
    entry_price = 100.;
    cancel_level =
      (match direction with Long -> 99. | Short -> 101.);
    stop_init =
      (match direction with Long -> 99.5 | Short -> 100.5);
    r_pts = 1.0;
    target_mult = 2.0;
    target_price =
      (match direction with Long -> 102. | Short -> 98.);
    be_trigger =
      (match direction with Long -> 101. | Short -> 99.);
    b2_end_minute = 600;
    downgrade_after_b2 = false;
    abr_prev = 0.;
    b1_range = 1.;
    b2_follow = Types.Follow_good;
  }

let make_trade ~(plan : trade_plan) ~(active : active_state)
    ~(exit_ts : timestamp) ~(exit_price : float) ~(exit_qty : float)
    ~(exit_reason : exit_reason) : trade =
  let pnl_pts =
    match plan.direction with
    | Long -> (exit_price -. active.entry_price)
    | Short -> (active.entry_price -. exit_price)
  in
  {
    date = exit_ts.date;
    direction = plan.direction;
    entry_ts = active.entry_ts;
    exit_ts;
    entry_price = active.entry_price;
    exit_price;
    qty = exit_qty;
    r_pts = plan.r_pts;
    pnl_pts;
    pnl_R = pnl_pts /. plan.r_pts;
    pnl_usd = pnl_pts *. exit_qty *. 50.; (* ES multiplier approximation *)
    pnl_pct = None;
    duration_min = Float.of_int (exit_ts.minute_of_day - active.entry_ts.minute_of_day);
    exit_reason;
    meta = [];
  }

let exec_deterministic ?(be_intrabar=true) ?(volume_aware=true) () =
  { (EP.default ~tick_size:0.25) with
    rng_state = Random.State.make [| 7 |];
    break_even_intrabar = be_intrabar;
    volume_aware;
    allow_same_bar_entry = true;
  }

let%test_unit "BE off defers arm until close" =
  let plan = base_plan ~direction:Long in
  let exec = exec_deterministic ~be_intrabar:false () in
  let pending = TT.init_pending ~qty:1.0 ~latency_bars:exec.latency_bars ~cancel_after:(-1) in
  let bar_entry = mk_bar ~open_:100.1 ~high:100.6 ~low:99.9 ~close:100.5 ~volume:4. ~minute:1 in
  let state1, _ = TT.step ~exec ~plan ~state:pending ~bar:bar_entry ~record_trade:(make_trade ~plan) in
  let bar = mk_bar ~open_:100.6 ~high:101.2 ~low:100.0 ~close:101.1 ~volume:4. ~minute:2 in
  let state2, trades = TT.step ~exec ~plan ~state:state1 ~bar ~record_trade:(make_trade ~plan) in
  assert (List.is_empty trades);
  match state2 with
  | Active a -> assert a.moved_to_be; assert Float.(a.stop_price = a.entry_price)
  | _ -> assert false

let%test_unit "range_weighted volume slices sum to volume and weight moves" =
  let exec = exec_deterministic () in
  let bar = mk_bar ~open_:100. ~high:103. ~low:99. ~close:102. ~volume:40. ~minute:1 in
  let slices =
    EM.volume_slices ~params:{ exec with volume_model = EP.Range_weighted } bar
  in
  let sum = Array.fold slices ~init:0.0 ~f:( +. ) in
  assert Float.(abs (sum -. 40.) < 1e-6);
  (* largest range is high->low leg on up bar, slice1 should be largest *)
  assert (Float.(slices.(1) > slices.(0)));
  assert (Float.(slices.(1) > slices.(2)))

let%test_unit "probabilistic slip deterministic with seed" =
  let exec =
    { (exec_deterministic ()) with
      slip_model = EP.Prob_one_tick { prob = 0.5 };
      rng_state = Random.State.make [| 123 |];
    }
  in
  let p1 = EM.adjust_price ~params:exec ~side:EM.Buy ~rng:exec.rng_state 100. in
  let p2 = EM.adjust_price ~params:exec ~side:EM.Buy ~rng:exec.rng_state 100. in
  let diff = Float.abs (p2 -. p1) in
  let tick = exec.tick_size in
  assert (Float.(diff = 0.) || Float.(abs (diff -. tick) < 1e-6));
  (* Restart seed to confirm determinism *)
  let exec2 = { exec with rng_state = Random.State.make [| 123 |] } in
  let q1 = EM.adjust_price ~params:exec2 ~side:EM.Buy ~rng:exec2.rng_state 100. in
  let q2 = EM.adjust_price ~params:exec2 ~side:EM.Buy ~rng:exec2.rng_state 100. in
  assert Float.(abs (p1 -. q1) < 1e-9);
  assert Float.(abs (p2 -. q2) < 1e-9)

let%test_unit "cancel-before-fill is ignored (no latency model)" =
  let plan = base_plan ~direction:Long in
  let exec = { (exec_deterministic ()) with allow_same_bar_entry = false } in
  let pending = TT.init_pending ~qty:1.0 ~latency_bars:exec.latency_bars ~cancel_after:(-1) in
  let bar1 = mk_bar ~open_:100.0 ~high:100.1 ~low:98.5 ~close:98.8 ~volume:4. ~minute:1 in
  let state1, trades1 = TT.step ~exec ~plan ~state:pending ~bar:bar1 ~record_trade:(make_trade ~plan) in
  assert (List.length trades1 = 1);
  assert (match state1 with Types.Done -> true | _ -> false);
  let t = List.hd_exn trades1 in
  assert (Poly.(t.exit_reason = Types.Stop))

let%test_unit "residual entry cancels after live window once active" =
  let plan = base_plan ~direction:Long in
  let exec = exec_deterministic () in
  let active =
    { stop_price = plan.stop_init; moved_to_be = false;
      entry_ts = mk_ts 0; entry_price = 100.; entry_value = 100.;
      qty = 0.5; pending_entry_qty = 0.5; pending_entry_cancel_after = 0; }
  in
  let bar = mk_bar ~open_:100.0 ~high:100.4 ~low:99.8 ~close:100.1 ~volume:4. ~minute:1 in
  let state, _ = TT.step ~exec ~plan ~state:(Active active) ~bar ~record_trade:(make_trade ~plan) in
  match state with
  | Active a -> assert Float.(a.pending_entry_qty = 0.0)
  | _ -> assert false

let%test_unit "entry/exit competition shares slice with partials off" =
  let plan = base_plan ~direction:Long in
  let exec = { (exec_deterministic ()) with allow_partial_fills = false; volume_aware = true } in
  let active =
    { stop_price = plan.stop_init; moved_to_be = false;
      entry_ts = mk_ts 0; entry_price = 100.; entry_value = 100.;
      qty = 1.0; pending_entry_qty = 1.0; pending_entry_cancel_after = 1; }
  in
  let bar =
    (* High hits target, low hits stop, volume small so neither fully fills with partials off *)
    mk_bar ~open_:100.0 ~high:102.1 ~low:99.4 ~close:100.5 ~volume:0.5 ~minute:1
  in
  let state, trades = TT.step ~exec ~plan ~state:(Active active) ~bar ~record_trade:(make_trade ~plan) in
  assert (List.is_empty trades);
  (* pending entry still pending, position still 1.0 *)
  match state with
  | Active a ->
      assert Float.(a.qty = 1.0);
      assert Float.(a.pending_entry_qty = 1.0)
  | _ -> assert false

let%test_unit "PnL uses weighted avg when entry fills at two prices and partial stop" =
  let plan = base_plan ~direction:Long in
  let exec = { (exec_deterministic ()) with allow_partial_fills = true; volume_aware = true } in
  let pending = TT.init_pending ~qty:2.0 ~latency_bars:exec.latency_bars ~cancel_after:0 in
  let bar1 = mk_bar ~open_:100.0 ~high:100.2 ~low:99.8 ~close:100.2 ~volume:2.0 ~minute:1 in
  let state1, _ = TT.step ~exec ~plan ~state:pending ~bar:bar1 ~record_trade:(make_trade ~plan) in
  let bar2 = mk_bar ~open_:100.4 ~high:100.6 ~low:100.0 ~close:100.5 ~volume:2.0 ~minute:2 in
  let state2, _ = TT.step ~exec ~plan ~state:state1 ~bar:bar2 ~record_trade:(make_trade ~plan) in
  let bar_stop = mk_bar ~open_:100.4 ~high:100.4 ~low:99.2 ~close:99.4 ~volume:1.0 ~minute:3 in
  let state3, trades = TT.step ~exec ~plan ~state:state2 ~bar:bar_stop ~record_trade:(make_trade ~plan) in
  assert (List.length trades = 1);
  let t = List.hd_exn trades in
  (* exit is partial due to volume 1.0 slice *)
  assert Float.(t.qty <= 1.0 +. 1e-6);
  let avg_entry =
    match state2 with
    | Active a -> a.entry_price
    | _ -> failwith "expected active"
  in
  (* stop below entry => negative pnl per contract *)
  assert Float.(t.pnl_pts /. t.qty < 0.);
  assert Float.(avg_entry > 100.0 && avg_entry < 100.5);
  match state3 with
  | Active _ -> () (* remaining qty still live *)
  | _ -> assert false
