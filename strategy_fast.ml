open Core

[@@@warning "-27-32-69"]

(* ========= Basic types ========= *)

type direction = Long | Short

let string_of_direction = function
  | Long -> "LONG"
  | Short -> "SHORT"

(* Local timestamp: only date + minute-of-day in ET.
   We do NOT rely on timezone libraries; we parse from the ET_datetime string. *)
type timestamp = {
  date : Date.t;
  minute_of_day : int;  (* 0 .. 1439 *)
}

type bar_1m = {
  ts     : timestamp;
  open_  : float;
  high   : float;
  low    : float;
  close  : float;
  volume : float;
}

type bar_5m = {
  date          : Date.t;
  minute_of_day : int;   (* start minute of the 5-min bar *)
  open_         : float;
  mutable high  : float;
  mutable low   : float;
  mutable close : float;
}

type day_macro = {
  mutable rth_high  : float;
  mutable rth_low   : float;
  mutable rth_close : float option;
  mutable has_rth   : bool;
}

type setup = {
  date       : Date.t;
  direction  : direction;
  b1         : bar_5m;
  b2         : bar_5m;
  abr_prev   : float;   (* ABR_8_From_Prev_RTH *)
  prev_close : float;
  adr21      : float;
}

type exit_reason =
  | Stop
  | Target
  | Eod_flat

let string_of_exit_reason = function
  | Stop     -> "stop"
  | Target   -> "target"
  | Eod_flat -> "eod_flat"

type b2_follow =
  | Follow_good
  | Follow_poor

let string_of_b2_follow = function
  | Follow_good -> "good"
  | Follow_poor -> "poor"

type trade = {
  date         : Date.t;
  direction    : direction;
  entry_ts     : timestamp;
  exit_ts      : timestamp;
  entry_price  : float;
  exit_price   : float;
  r_pts        : float;
  pnl_pts      : float;
  pnl_R        : float;
  target_mult  : float;
  abr_prev     : float;
  b1_range     : float;
  b2_follow    : b2_follow;
  duration_min : float;
  exit_reason  : exit_reason;
}

(* ========= Time helpers ========= *)

let minute_ofday_of_string (s : string) : int =
  (* "HH:MM" *)
  let hour = Int.of_string (String.sub s ~pos:0 ~len:2) in
  let min  = Int.of_string (String.sub s ~pos:3 ~len:2) in
  (hour * 60) + min

let rth_start_min    = minute_ofday_of_string "09:30"   (* 570 *)
let rth_end_min      = minute_ofday_of_string "16:15"   (* 975 *)
let b1_min           = minute_ofday_of_string "09:30"   (* 570 *)
let b2_min           = minute_ofday_of_string "09:35"   (* 575 *)
let abr_eod_min      = minute_ofday_of_string "16:10"   (* 970 *)
let minutes_per_day  = 24 * 60

let parse_timestamp (s : string) : timestamp =
  (* Example: "2010-06-06 20:00:00-04:00" *)
  let date_str = String.sub s ~pos:0 ~len:10 in
  let date = Date.of_string date_str in
  let hour = Int.of_string (String.sub s ~pos:11 ~len:2) in
  let min  = Int.of_string (String.sub s ~pos:14 ~len:2) in
  let minute_of_day = (hour * 60) + min in
  { date; minute_of_day }

(* ========= CSV parsing ========= *)

let parse_bar_1m (line : string) : bar_1m =
  (* Accept either column order:
     - ts_event first, ET_datetime absent (10 columns)
     - ts_event first, ET_datetime last (11 columns, *.et.ohlcv files)
     If ET_datetime is present we prefer it; otherwise we fall back to ts_event.
  *)
  let fields = String.split ~on:',' line in
  let parse ts_str open_s high_s low_s close_s volume_s =
    let ts     = parse_timestamp ts_str in
    let open_  = Float.of_string open_s in
    let high   = Float.of_string high_s in
    let low    = Float.of_string low_s in
    let close  = Float.of_string close_s in
    let volume = Float.of_string volume_s in
    { ts; open_; high; low; close; volume }
  in
  match fields with
  | [ ts_event; _rtype; _publisher_id; _instr_id;
      open_s; high_s; low_s; close_s; volume_s; _symbol; et_ts ] ->
      parse et_ts open_s high_s low_s close_s volume_s
  | [ ts_event; _rtype; _publisher_id; _instr_id;
      open_s; high_s; low_s; close_s; volume_s; _symbol ] ->
      parse ts_event open_s high_s low_s close_s volume_s
  | _ ->
      failwith ("Malformed CSV line: " ^ line)

(* Stream over a CSV file, applying [f] to each parsed 1‑minute bar. *)
let iter_bars (filename : string) ~(f : bar_1m -> unit) : unit =
  In_channel.with_file filename ~f:(fun ic ->
      (* skip header *)
      let _ = In_channel.input_line ic in
      let rec loop () =
        match In_channel.input_line ic with
        | None -> ()
        | Some line ->
            let bar = parse_bar_1m line in
            f bar;
            loop ()
      in
      loop ())

(* ========= 5‑minute resampling + daily context (Pass 1) ========= *)

let compute_daily_context_and_setups (filename : string)
  : setup Date.Table.t =
  (* Per‑day RTH macro stats *)
  let day_macro_tbl : day_macro Date.Table.t = Date.Table.create () in

  (* B1/B2 5‑min bars per date *)
  let b1_tbl : bar_5m Date.Table.t = Date.Table.create () in
  let b2_tbl : bar_5m Date.Table.t = Date.Table.create () in

  (* EOD ABR(8) at 16:10 for each date *)
  let eod_abr_tbl : float Date.Table.t = Date.Table.create () in

  (* Global 5‑min rolling ABR(8) window *)
  let abr_window : float Deque.t = Deque.create () in

  (* Current 5‑min bar being aggregated *)
  let current_5m : bar_5m option ref = ref None in

  let flush_5m (b : bar_5m) =
    let range = b.high -. b.low in
    (* update rolling ABR(8) window *)
    Deque.enqueue_back abr_window range;
    if Deque.length abr_window > 8 then ignore (Deque.dequeue_front abr_window);
    let abr8_opt =
      if Deque.length abr_window >= 8 then
        let sum = Deque.fold abr_window ~init:0.0 ~f:( +. ) in
        Some (sum /. Float.of_int (Deque.length abr_window))
      else
        None
    in
    (* record EOD ABR at 16:10 if available *)
    (match abr8_opt with
     | Some abr when b.minute_of_day = abr_eod_min ->
         Hashtbl.set eod_abr_tbl ~key:b.date ~data:abr
     | _ -> ());
    (* record B1 / B2 5‑minute bars *)
    if b.minute_of_day = b1_min then
      Hashtbl.set b1_tbl ~key:b.date ~data:b;
    if b.minute_of_day = b2_min then
      Hashtbl.set b2_tbl ~key:b.date ~data:b;
  in

  let process_bar (bar : bar_1m) =
    let { ts = { date; minute_of_day }; open_; high; low; close; _ } = bar in

    (* RTH daily macro stats *)
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

    (* 5‑minute aggregation *)
    let bucket_minute = (minute_of_day / 5) * 5 in
    match !current_5m with
    | None ->
        current_5m :=
          Some { date; minute_of_day = bucket_minute; open_; high; low; close }
    | Some b5 ->
        if Date.equal date b5.date && bucket_minute = b5.minute_of_day then begin
          (* Compare using float-specific operators to avoid int specialization. *)
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

  (* Second phase: walk days in order and build setups with ADR_21 and ABR_prev *)

  let setups_tbl : setup Date.Table.t = Date.Table.create () in

  (* rolling window of last 21 daily ranges *)
  let adr_window : float Deque.t = Deque.create () in
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

        (* ADR_21 for THIS date uses *previous* 21 daily ranges *)
        let adr21_opt =
          if Deque.length adr_window >= 21 then
            let sum = Deque.fold adr_window ~init:0.0 ~f:( +. ) in
            Some (sum /. Float.of_int (Deque.length adr_window))
          else
            None
        in

        let prev_close = !prev_close_opt in
        let abr_prev   = !prev_eod_abr_opt in

        (match Hashtbl.find b1_tbl date,
               Hashtbl.find b2_tbl date,
               prev_close, adr21_opt, abr_prev with
         | Some b1, Some b2, Some prev_close, Some adr21, Some abr_prev ->
             (* B1 filters (get_valid_b1_days equivalent) *)
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

        (* update rolling windows for the NEXT day *)
        Deque.enqueue_back adr_window daily_range;
        if Deque.length adr_window > 21 then ignore (Deque.dequeue_front adr_window);

        prev_close_opt :=
          Option.map macro.rth_close ~f:(fun c -> c);
        prev_eod_abr_opt :=
          Hashtbl.find eod_abr_tbl date
      end);

  setups_tbl

(* ========= B1/B2 trading logic (Pass 2) ========= *)

type trade_plan = {
  direction         : direction;
  entry_price       : float;
  cancel_level      : float;
  stop_init         : float;
  r_pts             : float;
  mutable target_mult : float;
  mutable target_price : float;
  be_trigger        : float;
  b2_end_minute     : int;
  downgrade_after_b2 : bool;
  abr_prev          : float;
  b1_range          : float;
  b2_follow         : b2_follow;
}

type active_state = {
  mutable stop_price : float;
  mutable moved_to_be : bool;
  entry_ts           : timestamp;
}

type trade_state =
  | No_trade
  | Pending
  | Active of active_state
  | Done

let tick_size = 0.25

let build_trade_plan (s : setup) : trade_plan option =
  let b1 = s.b1 in
  let b2 = s.b2 in
  let b1_range = b1.high -. b1.low in
  let direction = s.direction in

  let entry_price, cancel_level, stop_init, b2_good =
    match direction with
    | Long ->
        let entry_price  = b1.high +. tick_size in
        let cancel_level = b1.low in         (* cancel if returns to B1 low pre-entry *)
        let stop_init    = b1.low -. tick_size in
        let b2_good      = Float.(b2.close > b2.open_) in
        entry_price, cancel_level, stop_init, b2_good
    | Short ->
        let entry_price  = b1.low -. tick_size in
        let cancel_level = b1.high in
        let stop_init    = b1.high +. tick_size in
        let b2_good      = Float.(b2.close < b2.open_) in
        entry_price, cancel_level, stop_init, b2_good
  in
  let r_pts = Float.abs (entry_price -. stop_init) in
  if Float.(r_pts <= 0.) || Float.(s.abr_prev <= 0.) then
    None
  else begin
    let can_use_twoR = Float.(b1_range <= 1.5 *. s.abr_prev) in
    let initial_target_mult = if can_use_twoR then 2.0 else 1.0 in
    let target_price =
      match direction with
      | Long  -> entry_price +. initial_target_mult *. r_pts
      | Short -> entry_price -. initial_target_mult *. r_pts
    in
    let be_trigger =
      match direction with
      | Long  -> entry_price +. 0.8 *. r_pts
      | Short -> entry_price -. 0.8 *. r_pts
    in
    let b2_follow = if b2_good then Follow_good else Follow_poor in
    let downgrade_after_b2 = can_use_twoR && (not b2_good) in
    let b2_end_minute = b2_min + 4 in  (* 09:35–09:39, so end after 09:39 *)
    Some {
      direction;
      entry_price;
      cancel_level;
      stop_init;
      r_pts;
      target_mult  = initial_target_mult;
      target_price;
      be_trigger;
      b2_end_minute;
      downgrade_after_b2;
      abr_prev = s.abr_prev;
      b1_range;
      b2_follow;
    }
  end

let backtest_b1_b2 (filename : string) (setups_tbl : setup Date.Table.t)
  : trade list * (Date.t * float) list =
  let trades_acc : trade list ref = ref [] in
  let daily_pnl_tbl : float Date.Table.t = Date.Table.create () in

  let current_date  : Date.t option ref = ref None in
  let trade_plan    : trade_plan option ref = ref None in
  let trade_state   : trade_state ref = ref No_trade in
  let last_rth_bar  : bar_1m option ref = ref None in

  let record_trade ~(plan : trade_plan) ~(active : active_state)
      ~(exit_ts : timestamp) ~(exit_price : float)
      ~(reason : exit_reason) =
    let pnl_pts =
      match plan.direction with
      | Long  -> exit_price -. plan.entry_price
      | Short -> plan.entry_price -. exit_price
    in
    let pnl_R = pnl_pts /. plan.r_pts in
    let duration_min =
      Float.of_int (exit_ts.minute_of_day - active.entry_ts.minute_of_day)
    in
    let t = {
      date         = exit_ts.date;
      direction    = plan.direction;
      entry_ts     = active.entry_ts;
      exit_ts;
      entry_price  = plan.entry_price;
      exit_price;
      r_pts        = plan.r_pts;
      pnl_pts;
      pnl_R;
      target_mult  = plan.target_mult;
      abr_prev     = plan.abr_prev;
      b1_range     = plan.b1_range;
      b2_follow    = plan.b2_follow;
      duration_min;
      exit_reason  = reason;
    } in
    trades_acc := t :: !trades_acc;
    Hashtbl.update daily_pnl_tbl exit_ts.date ~f:(function
        | None   -> pnl_R
        | Some x -> x +. pnl_R)
  in

  let finalize_day () =
    match !current_date, !trade_plan, !trade_state, !last_rth_bar with
    | Some date, Some plan, Active active, Some last_bar
      when Date.equal last_bar.ts.date date ->
        (* force flat at RTH close *)
        record_trade ~plan ~active
          ~exit_ts:last_bar.ts ~exit_price:last_bar.close
          ~reason:Eod_flat;
        trade_state := Done;
        trade_plan  := None;
        last_rth_bar := None
    | _ ->
        trade_state := No_trade;
        trade_plan  := None;
        last_rth_bar := None
  in

  let process_bar (bar : bar_1m) =
    let { ts = { date; minute_of_day } as ts; high; low; close; _ } = bar in

    (* Detect calendar day change *)
    (match !current_date with
     | None ->
         current_date := Some date;
         (* initialize trade plan for this date if any setup *)
         (match Hashtbl.find setups_tbl date with
          | None -> trade_plan := None; trade_state := No_trade
          | Some s ->
              (match build_trade_plan s with
               | None ->
                   trade_plan := None; trade_state := No_trade
               | Some plan ->
                   trade_plan := Some plan;
                   trade_state := Pending))
     | Some d when Date.equal d date -> ()
     | Some _d ->
         finalize_day ();
         current_date := Some date;
         (match Hashtbl.find setups_tbl date with
          | None -> trade_plan := None; trade_state := No_trade
          | Some s ->
              (match build_trade_plan s with
               | None ->
                   trade_plan := None; trade_state := No_trade
               | Some plan ->
                   trade_plan := Some plan;
                   trade_state := Pending)));

    (* Track last RTH bar for potential EOD flatten *)
    if minute_of_day >= rth_start_min && minute_of_day <= rth_end_min then
      last_rth_bar := Some bar;

    match !trade_plan with
    | None -> ()
    | Some plan ->
        if minute_of_day < b2_min || minute_of_day > rth_end_min then
          ()  (* trade not active outside [B2, RTH close] *)
        else begin
          (* After B2 has fully closed and if follow-through was poor, downgrade 2R → 1R.
             This applies to both pending and active trades, but only from that moment on. *)
          if plan.downgrade_after_b2
             && Float.(plan.target_mult = 2.0)
             && minute_of_day > plan.b2_end_minute
          then begin
            plan.target_mult <- 1.0;
            plan.target_price <-
              (match plan.direction with
               | Long  -> plan.entry_price +. plan.r_pts
               | Short -> plan.entry_price -. plan.r_pts)
          end;

          match !trade_state with
          | No_trade | Done -> ()
          | Pending ->
              (match plan.direction with
               | Long ->
                   if Float.(high >= plan.entry_price) then
                     (* enter at entry_price *)
                     trade_state := Active {
                         stop_price   = plan.stop_init;
                         moved_to_be  = false;
                         entry_ts     = ts;
                       }
                   else if Float.(low <= plan.cancel_level) then
                     (* cancel before entry *)
                     trade_state := Done
               | Short ->
                   if Float.(low <= plan.entry_price) then
                     trade_state := Active {
                         stop_price   = plan.stop_init;
                         moved_to_be  = false;
                         entry_ts     = ts;
                       }
                   else if Float.(high >= plan.cancel_level) then
                     trade_state := Done)
          | Active active ->
              (* 1) stop check first *)
              let stopped =
                match plan.direction with
                | Long  -> Float.(low <= active.stop_price)
                | Short -> Float.(high >= active.stop_price)
              in
              if stopped then begin
                record_trade ~plan ~active
                  ~exit_ts:ts ~exit_price:active.stop_price
                  ~reason:Stop;
                trade_state := Done
              end else begin
                (* 2) move to break-even at +0.8R *)
                if not active.moved_to_be then begin
                  match plan.direction with
                  | Long when Float.(high >= plan.be_trigger) ->
                      active.stop_price <- plan.entry_price;
                      active.moved_to_be <- true
                  | Short when Float.(low <= plan.be_trigger) ->
                      active.stop_price <- plan.entry_price;
                      active.moved_to_be <- true
                  | _ -> ()
                end;
                (* 3) target check at current target_price *)
                let hit_target =
                  match plan.direction with
                  | Long  -> Float.(high >= plan.target_price)
                  | Short -> Float.(low <= plan.target_price)
                in
                if hit_target then begin
                  record_trade ~plan ~active
                    ~exit_ts:ts ~exit_price:plan.target_price
                    ~reason:Target;
                  trade_state := Done
                end
              end
        end
  in

  iter_bars filename ~f:process_bar;
  finalize_day ();

  let trades = List.rev !trades_acc in
  let daily_pnl =
    Hashtbl.to_alist daily_pnl_tbl
    |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
  in
  trades, daily_pnl

(* ========= Simple performance summary ========= *)

let summarize_performance (trades : trade list) (daily_pnl : (Date.t * float) list) =
  let n_trades = List.length trades in
  let n_days   = List.length daily_pnl in
  printf "Number of trades: %d\n" n_trades;
  printf "Number of trading days (with this strategy): %d\n" n_days;
  if n_trades > 0 then begin
    let total_R =
      List.fold trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_R)
    in
    let wins =
      List.count trades ~f:(fun t -> Float.(t.pnl_R > 0.0))
    in
    let win_rate = Float.of_int wins /. Float.of_int n_trades in
    let expectancy = total_R /. Float.of_int n_trades in
    printf "Win rate          : %.2f%%\n" (win_rate *. 100.0);
    printf "Expectancy / trade: %.3f R\n" expectancy;
  end;
  if n_days > 0 then begin
    let daily_R = List.map daily_pnl ~f:snd in
    let mean =
      List.fold daily_R ~init:0.0 ~f:( +. )
      /. Float.of_int n_days
    in
    let var =
      List.fold daily_R ~init:0.0
        ~f:(fun acc x -> let d = x -. mean in acc +. (d *. d))
      /. Float.of_int n_days
    in
    let std = Float.sqrt var in
    printf "Average R / day   : %.3f\n" mean;
    (match Float.(std > 0.0) with
     | true ->
         let ann_sharpe = Float.sqrt 252.0 *. (mean /. std) in
         printf "Ann. Sharpe (R/day): %.2f\n" ann_sharpe
     | false ->
         printf "Ann. Sharpe (R/day): n/a (zero volatility)\n")
  end

(* ========= Entry point ========= *)

let () =
  match Sys.get_argv () with
  | [| _; filename |] ->
      let setups_tbl = compute_daily_context_and_setups filename in
      let n_setups = Hashtbl.length setups_tbl in
      printf "Number of B1 setups (filtered days): %d\n%!" n_setups;
      let trades, daily_pnl = backtest_b1_b2 filename setups_tbl in
      summarize_performance trades daily_pnl;
      (* Example: print first few trades *)
      List.iter (List.take trades 5) ~f:(fun t ->
          printf "%s %s entry=%.2f exit=%.2f R=%.2f reason=%s B2=%s\n"
            (Date.to_string t.date)
            (string_of_direction t.direction)
            t.entry_price t.exit_price t.pnl_R
            (string_of_exit_reason t.exit_reason)
            (string_of_b2_follow t.b2_follow))
  | _ ->
      eprintf "Usage: %s <es_1min_csv>\n%!" (Sys.get_argv ()).(0)
