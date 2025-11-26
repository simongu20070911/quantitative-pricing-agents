open Core
open Strategy_fast
open Types

module OB = Strategy_fast.Order_book

let date s = Date.of_string s
let ts d m = { date = d; minute_of_day = m }

let base_plan =
  { direction = Long;
    entry_price = 100.;
    cancel_level = 99.;
    stop_init = 99.5;
    r_pts = 1.0;
    target_mult = 2.0;
    target_price = 102.0;
    be_trigger = 101.0;
    b2_end_minute = 600;
    downgrade_after_b2 = false;
    abr_prev = 1.0;
    b1_range = 1.0;
    b2_follow = Follow_good;
  }

let make_bracket_cmd ~qty =
  let meta = [ "test", "order_book" ] in
  Strategy_fast.Engine.Strategy_sig.Submit_bracket { plan = base_plan; qty; meta }

let%test_unit "empty book starts with no orders or positions" =
  let b = OB.empty () in
  assert (Int.equal b.next_order_id 1);
  assert (List.is_empty b.orders);
  assert (List.is_empty b.positions)

let%test_unit "submit_bracket allocates id and stores working order" =
  let ts0 = ts (date "2020-01-01") 0 in
  let cmd = make_bracket_cmd ~qty:2.0 in
  let b1 = OB.apply_cmds (OB.empty ()) ~ts:ts0 [ cmd ] in
  assert (Int.equal b1.next_order_id 2);
  assert (List.length b1.orders = 1);
  let o = List.hd_exn b1.orders in
  assert (Poly.(o.status = Working));
  assert (Float.(o.qty = 2.0));
  (match o.kind with
   | Bracket p ->
       assert (Float.(p.entry_price = base_plan.entry_price));
       assert (Float.(p.stop_init = base_plan.stop_init));
       assert (Float.(p.target_price = base_plan.target_price)));
  (match o.side, base_plan.direction with
   | Buy, Long | Sell, Short -> ()
   | _ -> assert false)

let%test_unit "update_all mutates embedded plan but preserves meta" =
  let ts0 = ts (date "2020-01-01") 0 in
  let cmd = make_bracket_cmd ~qty:1.0 in
  let b1 = OB.apply_cmds (OB.empty ()) ~ts:ts0 [ cmd ] in
  let bump_target p = { p with target_price = p.target_price +. 1.0 } in
  let ts1 = ts (date "2020-01-01") 1 in
  let b2 =
    OB.apply_cmds b1 ~ts:ts1
      [ Strategy_fast.Engine.Strategy_sig.Update_all bump_target ]
  in
  let o = List.hd_exn b2.orders in
  (match o.kind with
   | Bracket p ->
       assert (Float.(p.target_price = base_plan.target_price +. 1.0)));
  (* meta preserved *)
  assert (List.Assoc.mem ~equal:String.equal o.meta "test")

let%test_unit "update_stop updates both plan and active state" =
  let ts0 = ts (date "2020-01-01") 0 in
  let cmd = make_bracket_cmd ~qty:1.0 in
  let b1 = OB.apply_cmds (OB.empty ()) ~ts:ts0 [ cmd ] in
  (* simulate activation by attaching an Active state to the order *)
  let orders_active =
    List.map b1.orders ~f:(fun o ->
        match o.kind with
        | Bracket p ->
            let active =
              { stop_price = p.stop_init;
                moved_to_be = false;
                entry_ts = ts0;
                entry_price = p.entry_price;
                entry_value = p.entry_price;
                qty = 1.0;
                pending_entry_qty = 0.0;
                pending_entry_cancel_after = -1;
              }
            in
            { o with trade_state = Active active })
  in
  let b_active = OB.with_orders_positions b1 ~orders:orders_active ~positions:[] in
  let new_stop p = p.stop_init -. 0.5 in
  let ts1 = ts (date "2020-01-01") 1 in
  let b2 =
    OB.apply_cmds b_active ~ts:ts1
      [ Strategy_fast.Engine.Strategy_sig.Update_stop new_stop ]
  in
  let o = List.hd_exn b2.orders in
  (match o.kind, o.trade_state with
   | Bracket p, Active a ->
       assert (Float.(p.stop_init = base_plan.stop_init -. 0.5));
       assert (Float.(a.stop_price = base_plan.stop_init -. 0.5))
   | _ -> assert false)

let%test_unit "cancel_all marks working orders as cancelled" =
  let ts0 = ts (date "2020-01-01") 0 in
  let cmd = make_bracket_cmd ~qty:1.0 in
  let b1 = OB.apply_cmds (OB.empty ()) ~ts:ts0 [ cmd ] in
  let ts1 = ts (date "2020-01-01") 1 in
  let b2 =
    OB.apply_cmds b1 ~ts:ts1
      [ Strategy_fast.Engine.Strategy_sig.Cancel_all ]
  in
  let o = List.hd_exn b2.orders in
  assert (Poly.(o.status = Cancelled));
  assert (Poly.(o.updated_ts = ts1))
