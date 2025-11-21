open Core
open Strategy_fast
open Types

module TT = Engine.Trade_transition

let date = Date.of_string "2020-01-01"

let mk_bar ~minute ~high ~low ~close =
  let ts = Types.{ date; minute_of_day = minute } in
  Types.{ ts; open_ = close; high; low; close; volume = 1.0 }

let base_plan ~direction =
  { Types.direction;
    entry_price = 100.;
    cancel_level = 99.;
    stop_init = 99.5;
    r_pts = 1.0;
    target_mult = 2.0;
    target_price = (match direction with Long -> 102. | Short -> 98.);
    be_trigger = 101.;
    b2_end_minute = 600;
    downgrade_after_b2 = false;
    abr_prev = 0.;
    b1_range = 1.;
    b2_follow = Types.Follow_good;
  }

let make_trade
    ~(plan : Types.trade_plan)
    ~(active : Types.active_state)
    ~(exit_ts : Types.timestamp)
    ~(exit_price : float)
    ~(exit_reason : Types.exit_reason)
  : Types.trade =
  Types.{
    date = exit_ts.date;
    direction = plan.direction;
    entry_ts = active.entry_ts;
    exit_ts = exit_ts;
    entry_price = plan.entry_price;
    exit_price = exit_price;
    qty = 1.0;
    r_pts = plan.r_pts;
    pnl_pts = (match plan.direction with Long -> exit_price -. plan.entry_price | Short -> plan.entry_price -. exit_price);
    pnl_R = (match plan.direction with Long -> exit_price -. plan.entry_price | Short -> plan.entry_price -. exit_price) /. plan.r_pts;
    pnl_usd = 0.;
    pnl_pct = None;
    duration_min = Float.of_int (exit_ts.minute_of_day - active.entry_ts.minute_of_day);
    exit_reason = exit_reason;
    meta = [];
  }

let%test_unit "hits target after entry" =
  let plan = base_plan ~direction:Long in
  let pending = Types.Pending in
  let bar_entry = mk_bar ~minute:1 ~high:100.2 ~low:99.9 ~close:100.1 in
  let state1, trades1 =
    TT.step ~plan ~state:pending ~bar:bar_entry ~record_trade:(make_trade ~plan)
  in
  assert (Stdlib.(trades1 = []));
  let is_active =
    match state1 with
    | Types.Active _ -> true
    | _ -> false
  in
  assert is_active;
  let bar_target = mk_bar ~minute:2 ~high:102.5 ~low:100.0 ~close:102.0 in
  let _, trades2 =
    TT.step ~plan ~state:state1 ~bar:bar_target ~record_trade:(make_trade ~plan)
  in
  assert (List.length trades2 = 1);
  let t = List.hd_exn trades2 in
  assert (Poly.(t.exit_reason = Types.Target))

let%test_unit "moves to break_even then stops at entry" =
  let plan = base_plan ~direction:Long in
  let bar_entry = mk_bar ~minute:1 ~high:100.5 ~low:99.9 ~close:100.4 in
  let state1, _ = TT.step ~plan ~state:Types.Pending ~bar:bar_entry ~record_trade:(make_trade ~plan) in
  let bar_be = mk_bar ~minute:2 ~high:101.2 ~low:100.2 ~close:101.0 in
  let state2, _ = TT.step ~plan ~state:state1 ~bar:bar_be ~record_trade:(make_trade ~plan) in
  let bar_stop = mk_bar ~minute:3 ~high:100.8 ~low:99.8 ~close:100.0 in
  let _, trades = TT.step ~plan ~state:state2 ~bar:bar_stop ~record_trade:(make_trade ~plan) in
  assert (List.length trades = 1);
  let t = List.hd_exn trades in
  assert (Poly.(t.exit_reason = Types.Stop));
  assert (Float.(abs t.pnl_R < 1e-6))

let%test_unit "short hits target after entry" =
  let plan = base_plan ~direction:Short in
  let pending = Types.Pending in
  let bar_entry = mk_bar ~minute:1 ~high:100.0 ~low:97.8 ~close:98.5 in
  let state1, trades1 =
    TT.step ~plan ~state:pending ~bar:bar_entry ~record_trade:(make_trade ~plan)
  in
  assert (List.is_empty trades1);
  assert (match state1 with Types.Active _ -> true | _ -> false);
  let bar_target = mk_bar ~minute:2 ~high:98.0 ~low:96.0 ~close:96.5 in
  let _, trades2 =
    TT.step ~plan ~state:state1 ~bar:bar_target ~record_trade:(make_trade ~plan)
  in
  assert (List.length trades2 = 1);
  let t = List.hd_exn trades2 in
  assert (Poly.(t.exit_reason = Types.Target))

let%test_unit "cancelled when price breaches cancel before entry" =
  let plan = base_plan ~direction:Long in
  let bar_cancel = mk_bar ~minute:1 ~high:99.2 ~low:98.5 ~close:98.8 in
  let state, trades =
    TT.step ~plan ~state:Types.Pending ~bar:bar_cancel ~record_trade:(make_trade ~plan)
  in
  assert (Stdlib.(trades = []));
  assert (match state with Types.Done -> true | _ -> false)

let%test_unit "stop takes precedence over target in same bar" =
  let plan = base_plan ~direction:Long in
  let state1, _ =
    TT.step ~plan ~state:Types.Pending
      ~bar:(mk_bar ~minute:1 ~high:100.5 ~low:99.9 ~close:100.2)
      ~record_trade:(make_trade ~plan)
  in
  (* bar hits both stop and target; stop should win *)
  let bar = mk_bar ~minute:2 ~high:102.2 ~low:99.0 ~close:101.0 in
  let _, trades = TT.step ~plan ~state:state1 ~bar ~record_trade:(make_trade ~plan) in
  assert (List.length trades = 1);
  let t = List.hd_exn trades in
  assert (Poly.(t.exit_reason = Types.Stop))

let%test_unit "short stop precedes target when both touched" =
  let plan = base_plan ~direction:Short in
  let state1, _ =
    TT.step ~plan ~state:Types.Pending
      ~bar:(mk_bar ~minute:1 ~high:99.5 ~low:97.5 ~close:98.0)
      ~record_trade:(make_trade ~plan)
  in
  let bar = mk_bar ~minute:2 ~high:103.0 ~low:95.0 ~close:99.0 in
  let _, trades = TT.step ~plan ~state:state1 ~bar ~record_trade:(make_trade ~plan) in
  assert (List.length trades = 1);
  let t = List.hd_exn trades in
  assert (Poly.(t.exit_reason = Types.Stop))

let%test_unit "stop outranks target when both in same bar (property-ish)" =
  let directions = [ Types.Long; Types.Short ] in
  List.iter directions ~f:(fun dir ->
      let base = base_plan ~direction:dir in
      let plan = { base with target_mult = 2.0; target_price = (match dir with Long -> base.entry_price +. 2.0 | Short -> base.entry_price -. 2.0) } in
      let state1, _ =
        TT.step ~plan ~state:Types.Pending
          ~bar:(mk_bar ~minute:1 ~high:(plan.entry_price +. 0.1) ~low:(plan.entry_price -. 0.1) ~close:plan.entry_price)
          ~record_trade:(make_trade ~plan)
      in
      let bar =
        match dir with
        | Types.Long  -> mk_bar ~minute:2 ~high:(plan.target_price +. 0.5) ~low:(plan.stop_init -. 0.5) ~close:plan.entry_price
        | Types.Short -> mk_bar ~minute:2 ~high:(plan.stop_init +. 0.5) ~low:(plan.target_price -. 0.5) ~close:plan.entry_price
      in
      let _, trades = TT.step ~plan ~state:state1 ~bar ~record_trade:(make_trade ~plan) in
      assert (List.length trades = 1);
      let t = List.hd_exn trades in
      assert (Poly.(t.exit_reason = Types.Stop)))

let%test_unit "downgrade copies plan (helper does not mutate input)" =
  let downgrade_if_needed plan ~minute_of_day =
    if plan.downgrade_after_b2 && Float.(plan.target_mult = 2.0) && minute_of_day > plan.b2_end_minute then
      let target_price =
        match plan.direction with
        | Types.Long  -> plan.entry_price +. plan.r_pts
        | Types.Short -> plan.entry_price -. plan.r_pts
      in
      { plan with target_mult = 1.0; target_price }
    else plan
  in
  let plan =
    { direction = Types.Long;
      entry_price = 100.;
      cancel_level = 99.;
      stop_init = 99.5;
      r_pts = 1.0;
      target_mult = 2.0;
      target_price = 102.0;
      be_trigger = 101.0;
      b2_end_minute = 401;
      downgrade_after_b2 = true;
      abr_prev = 0.;
      b1_range = 1.;
      b2_follow = Types.Follow_good; }
  in
  let original = plan in
  let downgraded = downgrade_if_needed plan ~minute_of_day:(plan.b2_end_minute + 2) in
  assert (Poly.(plan = original)); (* immutable input preserved *)
  assert (Float.(downgraded.target_mult = 1.0));
  assert (Float.(downgraded.target_price = 101.0))

let%test_unit "downgrade helper is idempotent" =
  let downgrade_if_needed plan ~minute_of_day =
    if plan.downgrade_after_b2 && Float.(plan.target_mult = 2.0) && minute_of_day > plan.b2_end_minute then
      let target_price =
        match plan.direction with
        | Types.Long  -> plan.entry_price +. plan.r_pts
        | Types.Short -> plan.entry_price -. plan.r_pts
      in
      { plan with target_mult = 1.0; target_price }
    else plan
  in
  let plan =
    { direction = Types.Long;
      entry_price = 100.;
      cancel_level = 99.;
      stop_init = 99.5;
      r_pts = 1.0;
      target_mult = 2.0;
      target_price = 102.0;
      be_trigger = 101.0;
      b2_end_minute = 401;
      downgrade_after_b2 = true;
      abr_prev = 0.;
      b1_range = 1.;
      b2_follow = Types.Follow_good; }
  in
  let first = downgrade_if_needed plan ~minute_of_day:(plan.b2_end_minute + 5) in
  let second = downgrade_if_needed first ~minute_of_day:(plan.b2_end_minute + 10) in
  assert (Poly.(first = second))
