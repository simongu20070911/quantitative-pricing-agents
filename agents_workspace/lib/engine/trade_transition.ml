open Core
open Types

module EM = Execution_model
module EP = Execution_params

let side_of_direction = function
  | Long -> EM.Buy
  | Short -> EM.Sell

let exit_side = function
  | Long -> EM.Sell
  | Short -> EM.Buy

let stop_triggered ~plan ~price (active : active_state) =
  match plan.direction with
  | Long -> Float.(price <= active.stop_price)
  | Short -> Float.(price >= active.stop_price)

let target_triggered ~plan ~price =
  match plan.direction with
  | Long -> Float.(price >= plan.target_price)
  | Short -> Float.(price <= plan.target_price)

let entry_condition ~plan price =
  match plan.direction with
  | Long -> Float.(price >= plan.entry_price)
  | Short -> Float.(price <= plan.entry_price)

let cancel_condition ~plan price =
  match plan.direction with
  | Long -> Float.(price <= plan.cancel_level)
  | Short -> Float.(price >= plan.cancel_level)

let apply_break_even_intrabar ~exec ~(plan : trade_plan) ~price (active : active_state) =
  if active.moved_to_be || not exec.EP.break_even_intrabar then ()
  else
    match plan.direction with
    | Long when Float.(price >= plan.be_trigger) ->
        active.stop_price <- active.entry_price;
        active.moved_to_be <- true
    | Short when Float.(price <= plan.be_trigger) ->
        active.stop_price <- active.entry_price;
        active.moved_to_be <- true
    | _ -> ()

let apply_break_even_close ~(plan : trade_plan) ~(bar : bar_1m) (active : active_state) =
  if active.moved_to_be then ()
  else
    match plan.direction with
    | Long when Float.(bar.close >= plan.be_trigger) ->
        active.stop_price <- active.entry_price;
        active.moved_to_be <- true
    | Short when Float.(bar.close <= plan.be_trigger) ->
        active.stop_price <- active.entry_price;
        active.moved_to_be <- true
    | _ -> ()

let resolve_exit ~(exec : EP.t) ~stop_idx ~target_idx =
  match stop_idx, target_idx with
  | None, None -> None
  | Some (i, px), None -> Some (i, px, Stop)
  | None, Some (i, px) -> Some (i, px, Target)
  | Some (is, pxs), Some (it, pxt) ->
      (match exec.exit_priority with
       | EP.Stop_first -> Some (is, pxs, Stop)
       | EP.Path_order ->
           if is <= it then Some (is, pxs, Stop) else Some (it, pxt, Target))

let process_active ~exec ~(plan : trade_plan) ~(bar : bar_1m)
    ~(path : float array) ~(start_idx : int)
    ~(record_trade :
       active:active_state ->
       exit_ts:timestamp ->
       exit_price:float ->
       exit_reason:exit_reason ->
       'a)
    (active : active_state)
  : trade_state * 'a list =
  let stop_idx = ref None in
  let target_idx = ref None in

  Array.iteri path ~f:(fun idx price_touch ->
      if idx >= start_idx then begin
        apply_break_even_intrabar ~exec ~plan ~price:price_touch active;
        if Option.is_none !stop_idx && stop_triggered ~plan ~price:price_touch active then
          stop_idx := Some (idx, price_touch);
        if Option.is_none !target_idx && target_triggered ~plan ~price:price_touch then
          target_idx := Some (idx, price_touch)
      end);

  (* Arm BE at close when intrabar disabled *)
  if not exec.break_even_intrabar then
    apply_break_even_close ~plan ~bar active;

  match resolve_exit ~exec ~stop_idx:!stop_idx ~target_idx:!target_idx with
  | None -> Active active, []
  | Some (_idx, price_touch, exit_reason) ->
      let exit_px =
        EM.adjust_price ~params:exec ~side:(exit_side plan.direction)
          ~rng:exec.rng_state price_touch
      in
      let trade =
        record_trade ~active ~exit_ts:bar.ts ~exit_price:exit_px ~exit_reason
      in
      Done, [ trade ]

let step_with_exec ~exec ~qty ~(plan : trade_plan) ~state ~(bar : bar_1m)
    ~record_trade : trade_state * 'a list =
  let path = EM.path_prices bar in
  match state with
  | No_trade | Done -> state, []
  | Pending ->
      (* Walk path to see which happens first: cancel or entry. *)
      let entry_idx = ref None in
      let entry_px = ref plan.entry_price in
      let cancel_first = ref None in
      Array.iteri path ~f:(fun idx price_touch ->
          if Option.is_none !entry_idx then begin
            if cancel_condition ~plan price_touch then
              cancel_first := Option.first_some !cancel_first (Some idx);
            if entry_condition ~plan price_touch then begin
              entry_idx := Some idx;
              entry_px := price_touch
            end
          end);
      (match !entry_idx, !cancel_first with
       | None, Some _ -> Done, []
       | None, None -> state, []
       | Some ei, Some ci when ci < ei -> Done, []
       | Some idx, _ ->
           let filled_price =
             EM.adjust_price ~params:exec ~side:(side_of_direction plan.direction)
               ~rng:exec.rng_state !entry_px
           in
           let active =
             { stop_price = plan.stop_init;
               moved_to_be = false;
               entry_ts = bar.ts;
               entry_price = filled_price;
               qty; }
           in
           process_active ~exec ~plan ~bar ~path ~start_idx:idx
             ~record_trade active)
  | Active active ->
      process_active ~exec ~plan ~bar ~path ~start_idx:0
        ~record_trade active

(* Legacy bar-extrema semantics used by existing strategies/tests. *)
let step ~plan ~state ~(bar : bar_1m) ~record_trade : trade_state * 'a list =
  match state with
  | No_trade | Done -> state, []
  | Pending ->
      (match plan.direction with
       | Long ->
           if Float.(bar.high >= plan.entry_price) then
             Active { stop_price = plan.stop_init; moved_to_be = false; entry_ts = bar.ts; entry_price = plan.entry_price; qty = 1.0 }, []
           else if Float.(bar.low <= plan.cancel_level) then Done, []
           else state, []
       | Short ->
           if Float.(bar.low <= plan.entry_price) then
             Active { stop_price = plan.stop_init; moved_to_be = false; entry_ts = bar.ts; entry_price = plan.entry_price; qty = 1.0 }, []
           else if Float.(bar.high >= plan.cancel_level) then Done, []
           else state, [])
  | Active active ->
      let stop_hit =
        match plan.direction with
        | Long  -> Float.(bar.low <= active.stop_price)
        | Short -> Float.(bar.high >= active.stop_price)
      in
      if stop_hit then
        let trade = record_trade ~active ~exit_ts:bar.ts ~exit_price:active.stop_price ~exit_reason:Stop in
        Done, [ trade ]
      else
        let active' =
          if active.moved_to_be then active
          else
            match plan.direction with
            | Long when Float.(bar.high >= plan.be_trigger) ->
                { active with stop_price = plan.entry_price; moved_to_be = true }
            | Short when Float.(bar.low <= plan.be_trigger) ->
                { active with stop_price = plan.entry_price; moved_to_be = true }
            | _ -> active
        in
        let target_hit =
          match plan.direction with
          | Long  -> Float.(bar.high >= plan.target_price)
          | Short -> Float.(bar.low <= plan.target_price)
        in
        if target_hit then
          let trade = record_trade ~active:active' ~exit_ts:bar.ts ~exit_price:plan.target_price ~exit_reason:Target in
          Done, [ trade ]
        else
          Active active', []
