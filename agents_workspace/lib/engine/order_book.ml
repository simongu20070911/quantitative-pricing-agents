open Core
open Types

type t = {
  next_order_id : int;
  orders : order list;
  positions : book_position list;
}

let empty () = { next_order_id = 1; orders = []; positions = [] }

let with_orders_positions t ~orders ~positions =
  { t with orders; positions }

let alloc_id t = t.next_order_id, { t with next_order_id = t.next_order_id + 1 }

let make_bracket_order ~id ~ts ~(cmd : Strategy_sig.order_cmd) =
  match cmd with
  | Strategy_sig.Submit_bracket { plan; qty; meta } ->
      let side =
        match plan.direction with
        | Long -> Buy
        | Short -> Sell
      in
      Some {
        id;
        parent_id = None;
        side;
        qty;
        kind = Bracket plan;
        trade_state = Pending { qty_remaining = qty; cancel_after = -1; latency_remaining = 0 };
        created_ts = ts;
        updated_ts = ts;
        status = Working;
        meta;
      }
  | _ -> None

let update_stop ~(ts : timestamp) ~(f : trade_plan -> float) (o : order) : order =
  match o.kind with
  | Bracket plan ->
      let stop_price = f plan in
      let kind = Bracket { plan with stop_init = stop_price } in
      let trade_state =
        match o.trade_state with
        | Active a ->
            a.stop_price <- stop_price;
            Active a
        | _ -> o.trade_state
      in
      { o with kind; trade_state; updated_ts = ts }

let update_target ~(ts : timestamp) ~(f : trade_plan -> float) (o : order) : order =
  match o.kind with
  | Bracket plan ->
      let target_price = f plan in
      let kind = Bracket { plan with target_price } in
      { o with kind; updated_ts = ts }

let apply_cmd t ~ts (cmd : Strategy_sig.order_cmd) : t =
  match cmd with
  | Strategy_sig.Submit_bracket _ ->
      let id, t' = alloc_id t in
      let order_opt = make_bracket_order ~id ~ts ~cmd in
      (match order_opt with
       | None -> t'
       | Some o -> { t' with orders = o :: t'.orders })
  | Strategy_sig.Update_all f ->
      let orders =
        List.map t.orders ~f:(fun o ->
            match o.kind with
            | Bracket plan ->
                { o with
                  kind = Bracket (f plan);
                  updated_ts = ts; }
            )
      in
      { t with orders }
  | Strategy_sig.Update_stop f ->
      let orders = List.map t.orders ~f:(update_stop ~ts ~f) in
      { t with orders }
  | Strategy_sig.Update_target f ->
      let orders = List.map t.orders ~f:(update_target ~ts ~f) in
      { t with orders }
  | Strategy_sig.Cancel_all ->
      let orders =
        List.map t.orders ~f:(fun o ->
            match o.status with
            | Working -> { o with status = Cancelled; updated_ts = ts }
            | Filled | Cancelled -> o)
      in
      { t with orders }
  | Strategy_sig.Flatten_all _ -> t

let apply_cmds t ~ts cmds =
  List.fold_left cmds ~init:t ~f:(fun acc cmd -> apply_cmd acc ~ts cmd)
