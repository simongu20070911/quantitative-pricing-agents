open Core
open Types
open Time_utils
open Csv_parser
open Indicators
module P = B1b2_params.Setup

type agg_tables = {
  day_macro_tbl : day_macro Date.Table.t;
  b1_tbl : bar_5m Date.Table.t;
  b2_tbl : bar_5m Date.Table.t;
  eod_abr_tbl : float Date.Table.t;
}

let create_day_macro () =
  { rth_high  = Float.neg_infinity;
    rth_low   = Float.infinity;
    rth_close = None;
    has_rth   = false; }

let update_day_macro tbl ~date ~high ~low ~close =
  let macro = Hashtbl.find_or_add tbl date ~default:create_day_macro in
  macro.rth_high  <- Float.max macro.rth_high high;
  macro.rth_low   <- Float.min macro.rth_low low;
  macro.rth_close <- Some close;
  macro.has_rth   <- true

let update_5m current_5m ~date ~bucket_minute ~open_ ~high ~low ~close =
  match !current_5m with
  | None ->
      current_5m := Some { date; minute_of_day = bucket_minute; open_; high; low; close }
      ; `No_flush
  | Some b5 when Date.equal date b5.date && bucket_minute = b5.minute_of_day ->
      b5.high <- Float.max b5.high high;
      b5.low  <- Float.min b5.low low;
      b5.close <- close
      ; `No_flush
  | Some b5 ->
      `Flush_and_replace b5

let flush_5m b abr_window b1_tbl b2_tbl eod_abr_tbl =
  let range = b.high -. b.low in
  Abr.update abr_window range;
  (match Abr.value abr_window with
   | Some abr when b.minute_of_day = abr_eod_min ->
       Hashtbl.set eod_abr_tbl ~key:b.date ~data:abr
   | _ -> ());
  if b.minute_of_day = b1_min then Hashtbl.set b1_tbl ~key:b.date ~data:b;
  if b.minute_of_day = b2_min then Hashtbl.set b2_tbl ~key:b.date ~data:b

let init_aggregators (params : P.t) =
  let day_macro_tbl = Date.Table.create () in
  let b1_tbl = Date.Table.create () in
  let b2_tbl = Date.Table.create () in
  let eod_abr_tbl = Date.Table.create () in
  let abr_window = Abr.create ~n:params.abr_window_n in
  let current_5m : bar_5m option ref = ref None in

  let process_bar (bar : bar_1m) =
    let { ts = { date; minute_of_day }; open_; high; low; close; _ } = bar in

    if minute_of_day >= rth_start_min && minute_of_day <= rth_end_min then
      update_day_macro day_macro_tbl ~date ~high ~low ~close;

    let bucket_minute = (minute_of_day / 5) * 5 in
    match update_5m current_5m ~date ~bucket_minute ~open_ ~high ~low ~close with
    | `Flush_and_replace b5 ->
        flush_5m b5 abr_window b1_tbl b2_tbl eod_abr_tbl;
        current_5m := Some { date; minute_of_day = bucket_minute; open_; high; low; close }
    | `No_flush -> ()
  in

  let finalize () =
    Option.iter !current_5m ~f:(fun b ->
        flush_5m b abr_window b1_tbl b2_tbl eod_abr_tbl);
    { day_macro_tbl; b1_tbl; b2_tbl; eod_abr_tbl }
  in
  process_bar, finalize

let aggregate_bars (params : P.t) filename : agg_tables =
  let process_bar, finalize = init_aggregators params in
  iter_bars filename ~f:process_bar;
  finalize ()

let build_setups (params : P.t) (tables : agg_tables) : setup Date.Table.t =
  let setups_tbl : setup Date.Table.t = Date.Table.create () in
  let adr_window = Adr.create ~n:params.adr_window_n in
  let prev_close_opt : float option ref = ref None in
  let prev_eod_abr_opt : float option ref = ref None in

  let dates =
    Hashtbl.keys tables.day_macro_tbl
    |> List.sort ~compare:Date.compare
  in

  List.iter dates ~f:(fun date ->
      let macro = Hashtbl.find_exn tables.day_macro_tbl date in
      if macro.has_rth then begin
        let daily_range = macro.rth_high -. macro.rth_low in
        let adr21_opt = Adr.value adr_window in

        let prev_close = !prev_close_opt in
        let abr_prev   = !prev_eod_abr_opt in

        (match Hashtbl.find tables.b1_tbl date,
               Hashtbl.find tables.b2_tbl date,
               prev_close, adr21_opt, abr_prev with
         | Some b1, Some b2, Some prev_close, Some adr21, Some abr_prev ->
             let gap_pts      = b1.open_ -. prev_close in
             let gap_pct_adr  = Float.(abs gap_pts /. adr21 *. 100.) in
             let range        = b1.high -. b1.low in
             let body         = Float.abs (b1.close -. b1.open_) in
             if Float.(range > 0.) then begin
               let body_pct = body /. range in
               let ibs = (b1.close -. b1.low) /. range in

               let f_gap       = Float.(gap_pct_adr >= params.gap_min_pct_adr && gap_pct_adr <= params.gap_max_pct_adr) in
               let f_trend     = Float.(body_pct >= params.body_pct_min) in
               let f_climactic = Float.(range <= params.climactic_range_factor *. abr_prev) in
               let f_bull      = Float.(b1.close > b1.open_ && ibs > params.ibs_bull_min) in
               let f_bear      = Float.(b1.close < b1.open_ && ibs < params.ibs_bear_max) in

               let dir_opt =
                 if f_gap && f_trend && f_climactic && f_bull then Some Long
                 else if f_gap && f_trend && f_climactic && f_bear then Some Short
                 else None
               in
               Option.iter dir_opt ~f:(fun direction ->
                   Hashtbl.set setups_tbl ~key:date
                     ~data:{ date; direction; b1; b2; abr_prev; prev_close; adr21 })
             end
         | _ -> ());

        Adr.update adr_window daily_range;

        prev_close_opt := macro.rth_close;
        prev_eod_abr_opt := Hashtbl.find tables.eod_abr_tbl date
      end);

  setups_tbl

(* Top-level: stream bars, aggregate intermediates, then build setups. *)
let build (params : P.t) filename : setup Date.Table.t =
  let process_bar, finalize = init_aggregators params in
  iter_bars filename ~f:process_bar;
  let tables = finalize () in
  build_setups params tables

(* --- Streaming, single-pass builder for shared-stream runs --- *)

module Streaming = struct
  type day_state = {
    date : Date.t;
    rth_high : float;
    rth_low  : float;
    rth_close : float option;
    has_rth : bool;
    current_5m : bar_5m option;
    b1 : bar_5m option;
    b2 : bar_5m option;
    eod_abr : float option;
  }

  type state = {
    prev_close : float option;
    prev_eod_abr : float option;
    day : day_state option;
  }

  type t = {
    params : P.t;
    adr_window : Adr.t;
    abr_window : Abr.t;
    mutable state : state;
  }

  let fresh_day date =
    { date;
      rth_high = Float.neg_infinity;
      rth_low  = Float.infinity;
      rth_close = None;
      has_rth = false;
      current_5m = None;
      b1 = None;
      b2 = None;
      eod_abr = None; }

  let create (params : P.t) : t =
    {
      params;
      adr_window = Adr.create ~n:params.adr_window_n;
      abr_window = Abr.create ~n:params.abr_window_n;
      state = { prev_close = None; prev_eod_abr = None; day = None };
    }

  let flush_5m (t : t) (day : day_state) (b : bar_5m) =
    let range = b.high -. b.low in
    Abr.update t.abr_window range;
    let b1 = if b.minute_of_day = b1_min then Some b else day.b1 in
    let b2 = if b.minute_of_day = b2_min then Some b else day.b2 in
    let eod_abr =
      if b.minute_of_day = abr_eod_min then Abr.value t.abr_window else day.eod_abr
    in
    { day with b1; b2; eod_abr }

  let finalize_day t day =
    let day =
      match day.current_5m with
      | None -> day
      | Some b -> flush_5m t day b
    in
    if day.has_rth && Float.(day.rth_high > day.rth_low) then (
      (* Keep ADR window in sync even on days that fail the setup filter. *)
      Adr.update t.adr_window (day.rth_high -. day.rth_low);
      let prev_close = day.rth_close in
      (* Align with prebuild: require an abr_eod_min bar; no fallback to last ABR. *)
      let prev_eod_abr = day.eod_abr in
      { prev_close; prev_eod_abr; day = None })
    else
      { t.state with day = None }

  let ensure_day t date state =
    match state.day with
    | None -> { state with day = Some (fresh_day date) }
    | Some d when Date.equal d.date date -> state
    | Some d ->
        let state' = finalize_day t d in
        { state' with day = Some (fresh_day date) }

  let update_5m t day ~date ~minute_of_day ~bucket_minute ~open_ ~high ~low ~close =
    (* Update or roll the current 5m bar. *)
    let day, current_5m =
      match day.current_5m with
      | None ->
          let b = { date; minute_of_day = bucket_minute; open_; high; low; close } in
          { day with current_5m = Some b }, b
      | Some b when Date.equal b.date date && bucket_minute = b.minute_of_day ->
          let b' = { b with
                     high = Float.max b.high high;
                     low  = Float.min b.low low;
                     close } in
          { day with current_5m = Some b' }, b'
      | Some b ->
          let day' = flush_5m t day b in
          let b' = { date; minute_of_day = bucket_minute; open_; high; low; close } in
          { day' with current_5m = Some b' }, b'
    in
    (* Mirror prebuild availability: as soon as the 09:35 bucket opens, we treat the
       in-progress b2 bar as available for setup construction. *)
    let day =
      if Int.(bucket_minute = b2_min) then { day with b2 = Some current_5m } else day
    in
    (* Flush at the end of the bucket (minute 4 of the 5-min window) so B2 becomes
       available immediately at bucket close, not on the next bucket's first bar. *)
    if Int.(minute_of_day = bucket_minute + 4) then
      match day.current_5m with
      | None -> day
      | Some b -> flush_5m t { day with current_5m = None } b
    else
      day

  let compute_setup t day state_before =
    let adr21 = Adr.value t.adr_window in
    match day.b1, day.b2, state_before.prev_close, adr21, state_before.prev_eod_abr with
    | Some b1, Some b2, Some prev_close, Some adr21, Some abr_prev ->
        let range = b1.high -. b1.low in
        let body = Float.abs (b1.close -. b1.open_) in
        if Float.(range > 0.) then
          let body_pct = body /. range in
          let ibs = (b1.close -. b1.low) /. range in
          let gap_pts = b1.open_ -. prev_close in
          let gap_pct_adr = Float.(abs gap_pts /. adr21 *. 100.) in
          let open_in_gap =
            Float.(gap_pct_adr >= t.params.gap_min_pct_adr
                   && gap_pct_adr <= t.params.gap_max_pct_adr)
          in
          let trend = Float.(body_pct >= t.params.body_pct_min) in
          let climactic = Float.(range <= t.params.climactic_range_factor *. abr_prev) in
          let bull = Float.(b1.close > b1.open_ && ibs > t.params.ibs_bull_min) in
          let bear = Float.(b1.close < b1.open_ && ibs < t.params.ibs_bear_max) in
          let dir_opt =
            if open_in_gap && trend && climactic && bull then Some Long
            else if open_in_gap && trend && climactic && bear then Some Short
            else None
          in
          Option.map dir_opt ~f:(fun direction ->
              { date = day.date; direction; b1; b2; abr_prev; prev_close; adr21 })
        else None
    | _ -> None

  let step t state bar : state * setup option =
    let { ts = { date; minute_of_day }; open_; high; low; close; _ } = bar in
    let state = ensure_day t date state in
    let day = Option.value_exn state.day in

    let day =
      if minute_of_day >= rth_start_min && minute_of_day <= rth_end_min then
        { day with
          rth_high = Float.max day.rth_high high;
          rth_low  = Float.min day.rth_low low;
          rth_close = Some close;
          has_rth = true }
      else day
    in

    let bucket_minute = (minute_of_day / 5) * 5 in
    let day =
      update_5m t day ~date ~minute_of_day ~bucket_minute ~open_ ~high ~low ~close
    in

    let setup_opt, day =
      match day.b1, day.b2 with
      | Some _, Some _ ->
          let setup_opt = compute_setup t day state in
          let day' = { day with b1 = None; b2 = None } in
          setup_opt, day'
      | _ -> None, day
    in

    { state with day = Some day }, setup_opt

  let on_bar (t : t) (bar : bar_1m) : setup option =
    let new_state, setup_opt = step t t.state bar in
    t.state <- new_state;
    setup_opt

  let finalize t =
    match t.state.day with
    | None -> ()
    | Some d -> t.state <- finalize_day t d
end

let build_with_defaults filename = build P.defaults filename
