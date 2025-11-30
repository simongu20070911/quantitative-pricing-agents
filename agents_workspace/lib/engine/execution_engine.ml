open Core
open Types

module TT = Trade_transition
module EM = Execution_model

type config = {
  cost : Cost_model.config;
  exec : Execution_params.t;
  build_trade :
    plan:trade_plan ->
    active:active_state ->
    exit_ts:timestamp ->
    exit_price:float ->
    exit_qty:float ->
    exit_reason:exit_reason ->
    trade;
}

let step_order ~(cfg : config) ~(bar : bar_1m) (o : order)
  : order * trade list =
  match o.status, o.kind with
  | (Filled | Cancelled), _ -> o, []
  | Working, Bracket plan ->
      let trade_state, trades =
        TT.step
          ~exec:cfg.exec
          ~plan
          ~state:o.trade_state
          ~bar
          ~record_trade:(fun ~active ~exit_ts ~exit_price ~exit_qty ~exit_reason ->
            cfg.build_trade ~plan ~active ~exit_ts ~exit_price ~exit_qty ~exit_reason)
      in
      let status =
        match trade_state with
        | Done -> Filled
        | No_trade | Pending _ | Active _ -> Working
      in
      { o with trade_state; status; updated_ts = bar.ts }, trades

let exit_side = function
  | Long -> EM.Sell
  | Short -> EM.Buy

let copy_active (a : active_state) : active_state =
  {
    stop_price = a.stop_price;
    moved_to_be = a.moved_to_be;
    entry_ts = a.entry_ts;
    entry_price = a.entry_price;
    entry_value = a.entry_value;
    qty = a.qty;
    pending_entry_qty = a.pending_entry_qty;
    pending_entry_cancel_after = a.pending_entry_cancel_after;
  }

let rebuild_positions (orders : order list) : book_position list =
  orders
  |> List.filter_map ~f:(fun o ->
         match o.status, o.trade_state with
         | Working, Active a ->
             Some {
               id = o.id;
               direction = (match o.side with Buy -> Long | Sell -> Short);
               qty = a.qty;
               entry_ts = a.entry_ts;
               entry_price = a.entry_price;
               meta = o.meta;
             }
         | _ -> None)

let step ~config ~(book : Order_book.t) ~(bar : bar_1m) =
  let trades_acc, orders =
    List.fold_map book.orders ~init:[] ~f:(fun acc o ->
        let o', ts = step_order ~cfg:config ~bar o in
        (ts @ acc, o'))
  in
  let positions = rebuild_positions orders in
  let book' = Order_book.with_orders_positions book ~orders ~positions in
  book', List.rev trades_acc

let apply_flatten_cmds ~config ~(book : Order_book.t) ~(bar : bar_1m)
    (cmds : Strategy_sig.order_cmd list) : Order_book.t * trade list =
  let apply_one ((book_acc : Order_book.t), trades_acc) cmd =
    match cmd with
    | Strategy_sig.Flatten_all { reason; meta = _ } ->
        let trades_rev, orders =
          List.fold_map book_acc.orders ~init:trades_acc ~f:(fun acc o ->
              match o.status, o.trade_state, o.kind with
              | Working, Active a, Bracket plan ->
                  let exit_px =
                    EM.adjust_price ~params:config.exec
                      ~side:(exit_side plan.direction)
                      ~rng:config.exec.rng_state bar.close
                  in
                  let trade =
                    config.build_trade ~plan ~active:(copy_active a)
                      ~exit_ts:bar.ts ~exit_price:exit_px
                      ~exit_qty:a.qty ~exit_reason:reason
                  in
                  let o' =
                    { o with
                      trade_state = Done;
                      status = Filled;
                      updated_ts = bar.ts; }
                  in
                  (trade :: acc, o')
              | Working, Pending _, _ ->
                  let o' = { o with status = Cancelled; trade_state = Done; updated_ts = bar.ts } in
                  (acc, o')
              | _ -> acc, o)
        in
        let positions = rebuild_positions orders in
        let book' = Order_book.with_orders_positions book_acc ~orders ~positions in
        (book', trades_rev)
    | _ -> (book_acc, trades_acc)
  in
  List.fold cmds ~init:(book, []) ~f:apply_one

let on_session_end ~config ~(book : Order_book.t) ~(last_bar : bar_1m option) =
  match last_bar with
  | None -> book, []
  | Some lb ->
      let trades_acc, orders =
        List.fold_map book.orders ~init:[] ~f:(fun acc o ->
            match o.status, o.trade_state, o.kind with
            | Working, Active a, Bracket plan ->
                (* Allow EOD flatten even if the last bar precedes entry; clamp
                   exit_ts forward to the entry timestamp to avoid invalid trades
                   while preserving a fill. *)
                let exit_ts =
                  if Date.compare lb.ts.date a.entry_ts.date < 0
                     || (Date.equal lb.ts.date a.entry_ts.date
                         && lb.ts.minute_of_day < a.entry_ts.minute_of_day)
                  then a.entry_ts else lb.ts
                in
                let exit_px =
                  EM.adjust_price ~params:config.exec
                    ~side:(exit_side plan.direction)
                    ~rng:config.exec.rng_state lb.close
                in
                let trade =
                  config.build_trade ~plan ~active:a
                    ~exit_ts ~exit_price:exit_px ~exit_qty:a.qty ~exit_reason:Eod_flat
                in
                let o' =
                  { o with
                    trade_state = Done;
                    status = Filled;
                    updated_ts = lb.ts; }
                in
                (trade :: acc, o')
            | _ -> acc, o)
      in
      let positions = rebuild_positions orders in
      let book' = Order_book.with_orders_positions book ~orders ~positions in
      book', List.rev trades_acc
