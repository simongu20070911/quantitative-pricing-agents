open Core
open Types

module EM = Execution_model
module EP = Execution_params

let init_pending ~qty ~latency_bars ~cancel_after =
  Pending { qty_remaining = qty; cancel_after; latency_remaining = latency_bars }

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
    entry_value = a.entry_value;
    qty = a.qty;
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

let side_of_direction = function
  | Long -> EM.Buy
  | Short -> EM.Sell

let exit_side = function
  | Long -> EM.Sell
  | Short -> EM.Buy

let step
    ~(exec : EP.t)
    ~(plan : trade_plan)
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
  let be_trigger_hit = ref false in
  let exit_emitted = ref false in

  (* ---------- helpers shared by both modes ---------- *)
  let direction_between a b x =
    if Float.(b >= a) then Float.(x >= a && x <= b)
    else Float.(x <= a && x >= b)
  in

  let ordered_levels ~p0 ~p1 levels =
    let sign = if Float.(p1 >= p0) then 1. else -1. in
    levels
    |> List.filter_map ~f:(fun (lvl, kind) ->
           if direction_between p0 p1 lvl then
             Some ((lvl -. p0) *. sign, lvl, kind)
           else None)
    |> List.sort ~compare:(fun (d1,_,_) (d2,_,_) -> Float.compare d1 d2)
    |> List.map ~f:(fun (_,lvl,kind) -> (lvl, kind))
  in

  let process_exit ~(a:active_state) ~reason ~price_touch ~available_vol =
    if Poly.(reason = Stop) && a.moved_to_be then begin
      let exit_qty = a.qty in
      if Float.(exit_qty > 0.) then begin
        let exit_px = plan.entry_price in
        let trade =
          record_trade ~active:(copy_active a) ~exit_ts:bar.ts ~exit_price:exit_px
            ~exit_qty ~exit_reason:reason
        in
        trades := trade :: !trades;
        a.qty <- 0.0;
        exit_emitted := true;
        current_state := Done
      end
    end else begin
      let exit_qty =
        if exec.allow_partial_fills then
          Float.min a.qty !available_vol
        else if Float.(!available_vol >= a.qty) then a.qty
        else 0.0
      in
      if exec.allow_partial_fills then available_vol := !available_vol -. exit_qty;
      if Float.(exit_qty > 0.) then begin
        let exit_px_raw =
          EM.adjust_price ~params:exec ~side:(exit_side plan.direction)
            ~rng price_touch
        in
        let exit_px =
          match reason with
          | Stop when a.moved_to_be -> a.stop_price
          | _ -> exit_px_raw
        in
        let trade =
          record_trade ~active:(copy_active a) ~exit_ts:bar.ts ~exit_price:exit_px
            ~exit_qty ~exit_reason:reason
        in
        trades := trade :: !trades;
        a.qty <- a.qty -. exit_qty;
        exit_emitted := true;
        if Float.(a.qty <= 1e-6) then current_state := Done
      end
    end
  in

  if exec.continuous_path then begin
    (* ---------- continuous first-cross along each segment ---------- *)
    for i = 0 to Array.length path - 2 do
      let p_start = ref path.(i) in
      let p_end = path.(i+1) in
      let available_vol =
        if exec.volume_aware then ref volumes.(i) else ref Float.infinity
      in

      let rec step_segment () =
        if !exit_emitted then ()
        else
          let levels =
            match !current_state with
            | Pending p ->
                let entry_live = p.latency_remaining <= 0 in
                let candidates =
                  if entry_live then [ (plan.cancel_level, `Cancel); (plan.entry_price, `Entry) ]
                  else [ (plan.cancel_level, `Cancel) ]
                in
                ordered_levels ~p0:!p_start ~p1:p_end candidates
            | Active a ->
                let candidates = ref [] in
                if Float.(a.pending_entry_qty > 0.) && exec.allow_same_bar_entry then
                  candidates := (plan.entry_price, `Add_entry) :: !candidates;
                if not a.moved_to_be then
                  candidates := (plan.be_trigger, `BE) :: !candidates;
                candidates := (a.stop_price, `Stop) :: (plan.target_price, `Target) :: !candidates;
                let ordered = ordered_levels ~p0:!p_start ~p1:p_end !candidates in
                (match exec.exit_priority, List.find ordered ~f:(fun (_,k) -> Poly.(k = `Stop)) with
                 | EP.Stop_first, Some (lvl_stop, _) ->
                     let others =
                       List.filter ordered ~f:(fun (_,k) -> not Poly.(k = `Target))
                     in
                     (lvl_stop, `Stop) ::
                     List.filter others ~f:(fun (_,k) -> not Poly.(k = `Stop))
                 | _ -> ordered)
            | _ -> []
          in
          match levels with
          | [] ->
              p_start := p_end
          | (lvl, kind) :: _ ->
              p_start := lvl;
              begin match !current_state, kind with
              | Pending _, `Cancel ->
                  current_state := Done
              | Pending p, `Entry ->
                  if p.latency_remaining <= 0 && Float.(p.qty_remaining > 0.) then
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
                          ~rng lvl
                      in
                      let remaining = p.qty_remaining -. fill_qty in
                      current_state :=
                        Active
                          (create_active ~plan ~ts:bar.ts ~price:fill_px
                             ~filled_qty:fill_qty ~pending_qty:remaining
                             ~cancel_after:p.cancel_after)
              | Active a, `Add_entry ->
                  if exec.allow_same_bar_entry && Float.(a.pending_entry_qty > 0.) then
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
                          ~rng lvl
                      in
                      update_entry ~active:a ~price:fill_px ~qty:fill_qty;
                      a.pending_entry_qty <- a.pending_entry_qty -. fill_qty
                    end
              | Active a, `BE ->
                  if not a.moved_to_be then begin
                    be_trigger_hit := true;
                    a.stop_price <- plan.entry_price;
                    a.moved_to_be <- true
                  end
              | Active a, `Stop ->
                  process_exit ~a ~reason:Stop ~price_touch:lvl ~available_vol
              | Active a, `Target ->
                  process_exit ~a ~reason:Target ~price_touch:lvl ~available_vol
              | _ -> ()
              end;
              if not Float.( !p_start = p_end ) && not !exit_emitted then step_segment ()
      in
      step_segment ()
    done
  end else begin
    (* ---------- discrete touch-only mode (backward compatible) ---------- *)
    Array.iteri path ~f:(fun idx price_touch ->
        let available_vol =
          if exec.volume_aware then ref volumes.(idx) else ref Float.infinity
        in
        let entry_live =
          match !current_state with
          | Pending p -> p.latency_remaining <= 0
          | Active _ -> true
          | _ -> false
        in
        (match !current_state with
         | Pending p when entry_live ->
             if (match plan.direction with Long -> Float.(price_touch <= plan.cancel_level) | Short -> Float.(price_touch >= plan.cancel_level)) then
               current_state := Done
             else if (match plan.direction with Long -> Float.(price_touch >= plan.entry_price) | Short -> Float.(price_touch <= plan.entry_price))
                  && Float.(p.qty_remaining > 0.) then
               let fill_qty =
                 if exec.allow_partial_fills then consume_volume available_vol p.qty_remaining
                 else if Float.(!available_vol >= p.qty_remaining) then consume_volume available_vol p.qty_remaining
                 else 0.0
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
         | _ -> ());

        (match !current_state with
         | Active a when entry_live && Float.(a.pending_entry_qty > 0.) &&
                          (match plan.direction with Long -> Float.(price_touch >= plan.entry_price) | Short -> Float.(price_touch <= plan.entry_price)) ->
             let fill_qty =
               if exec.allow_partial_fills then consume_volume available_vol a.pending_entry_qty
               else if Float.(!available_vol >= a.pending_entry_qty) then consume_volume available_vol a.pending_entry_qty
               else 0.0
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

        (match !current_state with
         | Active a ->
             if exec.break_even_intrabar && (not a.moved_to_be) then begin
               let triggered =
                 match plan.direction with
                 | Long  -> Float.(price_touch >= plan.be_trigger)
                 | Short -> Float.(price_touch <= plan.be_trigger)
               in
               if triggered then begin
                 be_trigger_hit := true;
                 a.stop_price <- plan.entry_price;
                 a.moved_to_be <- true
               end
             end;
             if not !exit_emitted then begin
               let stop_hit = match plan.direction with Long -> Float.(price_touch <= a.stop_price) | Short -> Float.(price_touch >= a.stop_price) in
               let target_hit = match plan.direction with Long -> Float.(price_touch >= plan.target_price) | Short -> Float.(price_touch <= plan.target_price) in
               let process_exit reason =
                 process_exit ~a ~reason ~price_touch ~available_vol
               in
               match exec.exit_priority with
               | EP.Path_order ->
                   if stop_hit then process_exit Stop
                   else if target_hit then process_exit Target
               | EP.Stop_first ->
                   if stop_hit then process_exit Stop
                   else if target_hit then process_exit Target
             end
         | _ -> ());
      )
  end;

  (* Apply break-even once per bar (after exits) if triggered and still active. *)
  (match !current_state with
   | Active a when (not !exit_emitted) && not a.moved_to_be ->
       let triggered =
         if exec.break_even_intrabar then !be_trigger_hit
         else
           match plan.direction with
           | Long  -> Float.(bar.close >= plan.be_trigger)
           | Short -> Float.(bar.close <= plan.be_trigger)
       in
       if triggered then begin
         a.stop_price <- a.entry_price;
         a.moved_to_be <- true
       end
   | _ -> ());

  let final_state =
    match !current_state with
    | Pending p ->
        let latency_remaining = max 0 (p.latency_remaining - 1) in
        let cancel_after =
          if p.cancel_after < 0 then -1 else p.cancel_after - 1
        in
        if cancel_after = -1 && p.cancel_after < 0 then
          Pending { p with latency_remaining; cancel_after }
        else if cancel_after < 0 && Float.(p.qty_remaining > 0.) then Done
        else Pending { p with latency_remaining; cancel_after }
    | Active a when Float.(a.pending_entry_qty > 0.) && a.pending_entry_cancel_after >= 0 ->
        let cancel_after =
          if a.pending_entry_cancel_after < 0 then -1
          else a.pending_entry_cancel_after - 1
        in
        if cancel_after < 0 then a.pending_entry_qty <- 0.0
        else a.pending_entry_cancel_after <- cancel_after;
        Active a
    | s -> s
  in
  (final_state, List.rev !trades)
