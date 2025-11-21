open Core
open Types
open Time_utils
open Csv_parser
open Indicators

[@@@warning "-27-32-69"]

let compute_daily_context_and_setups filename : setup Date.Table.t =
  let day_macro_tbl : day_macro Date.Table.t = Date.Table.create () in
  let b1_tbl : bar_5m Date.Table.t = Date.Table.create () in
  let b2_tbl : bar_5m Date.Table.t = Date.Table.create () in
  let eod_abr_tbl : float Date.Table.t = Date.Table.create () in
  let abr_window = Abr.create ~n:8 in
  let current_5m : bar_5m option ref = ref None in

  let flush_5m (b : bar_5m) =
    let range = b.high -. b.low in
    Abr.update abr_window range;
    let abr8_opt = Abr.value abr_window in
    (match abr8_opt with
     | Some abr when b.minute_of_day = abr_eod_min ->
         Hashtbl.set eod_abr_tbl ~key:b.date ~data:abr
     | _ -> ());
    if b.minute_of_day = b1_min then
      Hashtbl.set b1_tbl ~key:b.date ~data:b;
    if b.minute_of_day = b2_min then
      Hashtbl.set b2_tbl ~key:b.date ~data:b;
  in

  let process_bar (bar : bar_1m) =
    let { ts = { date; minute_of_day }; open_; high; low; close; _ } = bar in

    if minute_of_day >= rth_start_min && minute_of_day <= rth_end_min then begin
      let macro =
        Hashtbl.find_or_add day_macro_tbl date ~default:(fun () ->
            { rth_high  = Float.neg_infinity;
              rth_low   = Float.infinity;
              rth_close = None;
              has_rth   = false; })
      in
      macro.rth_high  <- Float.max macro.rth_high high;
      macro.rth_low   <- Float.min macro.rth_low low;
      macro.rth_close <- Some close;
      macro.has_rth   <- true;
    end;

    let bucket_minute = (minute_of_day / 5) * 5 in
    match !current_5m with
    | None ->
        current_5m :=
          Some { date; minute_of_day = bucket_minute; open_; high; low; close }
    | Some b5 ->
        if Date.equal date b5.date && bucket_minute = b5.minute_of_day then begin
          b5.high <- Float.max b5.high high;
          b5.low  <- Float.min b5.low low;
          b5.close <- close
        end else begin
          flush_5m b5;
          current_5m :=
            Some { date; minute_of_day = bucket_minute; open_; high; low; close }
        end
  in

  iter_bars filename ~f:process_bar;
  Option.iter !current_5m ~f:flush_5m;

  let setups_tbl : setup Date.Table.t = Date.Table.create () in
  let adr_window = Adr.create ~n:21 in
  let prev_close_opt : float option ref = ref None in
  let prev_eod_abr_opt : float option ref = ref None in

  let dates =
    Hashtbl.keys day_macro_tbl
    |> List.sort ~compare:Date.compare
  in

  List.iter dates ~f:(fun date ->
      let macro = Hashtbl.find_exn day_macro_tbl date in
      if macro.has_rth then begin
        let daily_range = macro.rth_high -. macro.rth_low in
        let adr21_opt = Adr.value adr_window in

        let prev_close = !prev_close_opt in
        let abr_prev   = !prev_eod_abr_opt in

        (match Hashtbl.find b1_tbl date,
               Hashtbl.find b2_tbl date,
               prev_close, adr21_opt, abr_prev with
         | Some b1, Some b2, Some prev_close, Some adr21, Some abr_prev ->
             let gap_pts      = b1.open_ -. prev_close in
             let gap_pct_adr  = Float.(abs gap_pts /. adr21 *. 100.) in
             let range        = b1.high -. b1.low in
             let body         = Float.abs (b1.close -. b1.open_) in
             if Float.(range > 0.) then begin
               let body_pct = body /. range in
               let ibs = (b1.close -. b1.low) /. range in

               let f_gap       = Float.(gap_pct_adr >= 11. && gap_pct_adr <= 60.) in
               let f_trend     = Float.(body_pct >= 0.5) in
               let f_climactic = Float.(range <= 2.5 *. abr_prev) in
               let f_bull      = Float.(b1.close > b1.open_ && ibs > 0.69) in
               let f_bear      = Float.(b1.close < b1.open_ && ibs < 0.31) in

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

        prev_close_opt :=
          Option.map macro.rth_close ~f:(fun c -> c);
        prev_eod_abr_opt :=
          Hashtbl.find eod_abr_tbl date
      end);

  setups_tbl
