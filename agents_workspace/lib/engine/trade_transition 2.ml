open Core
open Types

module EM = Execution_model
module EP = Execution_params

let init_pending ~qty ~cancel_after =
  Pending { qty_remaining = qty; cancel_after }

let consume_volume available_qty want =
  let take = Float.min !available_qty want in
  available_qty := !available_qty -. take;
  take

let update_entry ~(active : active_state) ~price ~qty =
  active.entry_value <- active.entry_value +. (price *. qty);
  active.qty <- active.qty +. qty;
  active.entry_price <- active.entry_value /. active.qty

let copy_active (a : active_state) : active_state =
  {
    stop_price = a.stop_price;
    moved_to_be = a.moved_to_be;
    entry_ts = a.entry_ts;
    entry_price = a.entry_price;
    qty = a.qty;
    entry_value = a.entry_value;
    pending_entry_qty = a.pending_entry_qty;
    pending_entry_cancel_after = a.pending_entry_cancel_after;
  }

let create_active ~plan ~ts ~price ~filled_qty ~pending_qty ~cancel_after =
  {
    stop_price = plan.stop_init;
    moved_to_be = false;
    entry_ts = ts;
    entry_price = price;
    entry_value = price *. filled_qty;
    qty = filled_qty;
    pending_entry_qty = pending_qty;
    pending_entry_cancel_after = cancel_after;
  }

let apply_break_even ~(exec : EP.t) ~plan ~price (active : active_state) =
  if active.moved_to_be then ()
  else
    match plan.direction, exec.break_even_intrabar with
    | _, false -> ()
    | Long, true when Float.(price >= plan.be_trigger) ->
        active.stop_price <- active.entry_price;
        active.moved_to_be <- true
    | Short, true when Float.(price <= plan.be_trigger) ->
        active.stop_price <- active.entry_price;
        active.moved_to_be <- true
    | _ -> ()

let stop_triggered ~plan ~price active =
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

let side_of_direction = function
  | Long -> EM.Buy
  | Short -> EM.Sell

let exit_side = function
  | Long -> EM.Sell
  | Short -> EM.Buy

let step
    ~(exec : EP.t)
    ~plan
    ~state
    ~(bar : bar_1m)
    ~record_trade
  : trade_state * 'a list =
  let rng : EM.rng = exec.rng_state in
  let path = EM.path_prices bar in
  let volumes =
    if exec.volume_aware then EM.volume_slices ~params:exec bar
    else Array.create ~len:(Array.length path) Float.infinity
  in

  let trades = ref [] in
  let current_state = ref state in

  let exit_emitted = ref false in
  Array.iteri path ~f:(fun idx price_touch ->
      let available_vol =
        if exec.volume_aware then ref volumes.(idx) else ref Float.infinity
      in
      let entry_live =
        match !current_state with
        | Pending _ -> exec.allow_same_bar_entry || true
        | Active _ -> true
        | _ -> false
      in
      (* Pending entry fill or cancellation *)
      (match !current_state with
       | Pending p when entry_live ->
           if cancel_condition ~plan price_touch then
             current_state := Done
           else if entry_condition ~plan price_touch && Float.(p.qty_remaining > 0.) then
             let fill_qty =
               if exec.allow_partial_fills then consume_volume available_vol p.qty_remaining
               else if Float.(!available_vol >= p.qty_remaining) then
                 consume_volume available_vol p.qty_remaining
               else
                 0.0
             in
             if Float.(fill_qty > 0.) then
               let fill_px =
                 EM.adjust_price ~params:exec ~side:(side_of_direction plan.direction)
                   ~rng price_touch
               in
               let remaining = p.qty_remaining -. fill_qty in
               current_state :=
                Active
                  (create_active ~plan ~ts:bar.ts ~price:fill_px
                      ~filled_qty:fill_qty ~pending_qty:remaining
                      ~cancel_after:p.cancel_after)
             else
               ()
       | _ -> ());

      (* Additional entry fills while active if any pending qty remains *)
      (match !current_state with
       | Active a when entry_live && Float.(a.pending_entry_qty > 0.) &&
                        entry_condition ~plan price_touch ->
           let fill_qty =
             if exec.allow_partial_fills then consume_volume available_vol a.pending_entry_qty
             else if Float.(!available_vol >= a.pending_entry_qty) then
               consume_volume available_vol a.pending_entry_qty
             else
               0.0
           in
           if Float.(fill_qty > 0.) then begin
             let fill_px =
               EM.adjust_price ~params:exec ~side:(side_of_direction plan.direction)
                 ~rng price_touch
             in
             update_entry ~active:a ~price:fill_px ~qty:fill_qty;
             a.pending_entry_qty <- a.pending_entry_qty -. fill_qty
           end
       | _ -> ());

      (* Exit logic for active positions *)
      (match !current_state with
       | Active a ->
           if !exit_emitted then ()
           else begin
            (* Stop first *)
            if stop_triggered ~plan ~price:price_touch a then begin
              let exit_qty =
                if exec.allow_partial_fills then consume_volume available_vol a.qty
                else if Float.(!available_vol >= a.qty) then consume_volume available_vol a.qty
                else 0.0
             in
             if Float.(exit_qty > 0.) then begin
               let exit_px =
                 EM.adjust_price ~params:exec ~side:(exit_side plan.direction)
                   ~rng price_touch
               in
               let trade =
                 record_trade ~active:(copy_active a) ~exit_ts:bar.ts ~exit_price:exit_px
                   ~exit_qty ~exit_reason:Stop
               in
                trades := trade :: !trades;
                a.qty <- a.qty -. exit_qty;
                exit_emitted := true;
                if Float.(a.qty <= 1e-6) then current_state := Done
              end
            end;
            if not (phys_equal !current_state Done) then begin
             apply_break_even ~exec ~plan ~price:price_touch a;
             if target_triggered ~plan ~price:price_touch then begin
               let exit_qty =
                 if exec.allow_partial_fills then consume_volume available_vol a.qty
                 else if Float.(!available_vol >= a.qty) then consume_volume available_vol a.qty
                 else 0.0
                in
                if Float.(exit_qty > 0.) then begin
                let exit_px =
                  EM.adjust_price ~params:exec ~side:(exit_side plan.direction)
                    ~rng price_touch
                in
                let trade =
                  record_trade ~active:(copy_active a) ~exit_ts:bar.ts ~exit_price:exit_px
                    ~exit_qty ~exit_reason:Target
                in
                 trades := trade :: !trades;
                 a.qty <- a.qty -. exit_qty;
                 exit_emitted := true;
                 if Float.(a.qty <= 1e-6) then current_state := Done
               end
             end
           end end
       | _ -> ()));
  (* If break-even is deferred to bar close, apply once here. *)
  (match !current_state with
   | Active a when (not exec.break_even_intrabar) && not a.moved_to_be ->
       let exec_intrabar = { exec with EP.break_even_intrabar = true } in
       apply_break_even ~exec:exec_intrabar ~plan ~price:bar.close a
   | _ -> ());
 let final_state =
    match !current_state with
    | Pending p when p.cancel_after >= 0 ->
        let cancel_after = p.cancel_after - 1 in
        if cancel_after < 0 && Float.(p.qty_remaining > 0.) then Done
        else Pending { p with cancel_after }
    | Pending p -> Pending p
    | Active a when Float.(a.pending_entry_qty > 0.) && a.pending_entry_cancel_after >= 0 ->
        let cancel_after = a.pending_entry_cancel_after - 1 in
        if cancel_after < 0 then a.pending_entry_qty <- 0.0
        else a.pending_entry_cancel_after <- cancel_after;
        Active a
    | s -> s
  in
  (final_state, List.rev !trades)
