open Core
open Types
open Time_utils
open Csv_parser
open Indicators
module P = B1b2_params.Setup

[@@@warning "-27-32-69"]

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

let build (params : P.t) filename : setup Date.Table.t =
  let process_bar, finalize = init_aggregators params in
  iter_bars filename ~f:process_bar;
  let tables = finalize () in
  build_setups params tables

let build_with_defaults filename = build P.defaults filename
