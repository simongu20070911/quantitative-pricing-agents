open Core
open Types
open Time_utils

[@@@warning "-27-32-69"]

module Rolling = struct
  type t = {
    window   : float Deque.t;
    capacity : int;
    mutable sum  : float;
    mutable sumsq: float;
  }

  let create ~capacity =
    { window = Deque.create (); capacity; sum = 0.; sumsq = 0. }

  let clear t =
    Deque.clear t.window;
    t.sum <- 0.;
    t.sumsq <- 0.

  let push t x =
    Deque.enqueue_back t.window x;
    t.sum   <- t.sum   +. x;
    t.sumsq <- t.sumsq +. (x *. x);
    if Deque.length t.window > t.capacity then
      match Deque.dequeue_front t.window with
      | None -> ()
      | Some old ->
          t.sum   <- t.sum   -. old;
          t.sumsq <- t.sumsq -. (old *. old)

  let length t = Deque.length t.window

  let mean t =
    let n = length t in
    if n = 0 then None else Some (t.sum /. Float.of_int n)

  let variance t =
    match mean t with
    | None -> None
    | Some mu ->
        let n = Float.of_int (length t) in
        let var = Float.max 0. (t.sumsq /. n -. (mu *. mu)) in
        Some var

  let std t = Option.map (variance t) ~f:Float.sqrt

  let rms t =
    let n = length t in
    if n = 0 then None
    else Some (Float.sqrt (t.sumsq /. Float.of_int n))
end

module Rolling_sum = struct
  type t = {
    window   : float Deque.t;
    capacity : int;
    mutable sum : float;
  }

  let create ~capacity = { window = Deque.create (); capacity; sum = 0. }

  let clear t =
    Deque.clear t.window;
    t.sum <- 0.

  let push t x =
    Deque.enqueue_back t.window x;
    t.sum <- t.sum +. x;
    if Deque.length t.window > t.capacity then
      match Deque.dequeue_front t.window with
      | None -> ()
      | Some old -> t.sum <- t.sum -. old

  let total t = if Deque.is_empty t.window then None else Some t.sum
  let length t = Deque.length t.window
end

type state = {
  mutable current_date   : Date.t option;
  mutable cum_pv         : float;
  mutable cum_v          : float;
  mutable vwap           : float option;
  mutable prev_close_rth : float option;
  mutable overnight_high : float option;
  mutable overnight_low  : float option;
  mutable gap            : float option;
  mutable last_close     : float option;

  dist_roll    : Rolling.t;       (* deviations from VWAP for z-score *)
  rv10_roll    : Rolling.t;       (* 1m returns over 10m window *)
  rv60_roll    : Rolling.t;       (* 1m returns over 60m window *)
  close_roll   : float Deque.t;   (* track last 61 closes for trend calc *)
  ofi_s_short  : Rolling_sum.t;   (* signed volume short window *)
  ofi_v_short  : Rolling_sum.t;   (* total volume short window *)
  ofi_s_long   : Rolling_sum.t;   (* signed volume long window *)
  ofi_v_long   : Rolling_sum.t;   (* total volume long window *)
}

type snapshot = {
  vwap       : float option;
  z_vwap     : float option;
  ofi_short  : float option;
  ofi_long   : float option;
  rv10       : float option;
  rv60       : float option;
  rv_ratio   : float option;
  trend      : float option;
  gap        : float option;
  dist_onh   : float option;
  dist_onl   : float option;
}

let create () =
  { current_date = None;
    cum_pv = 0.;
    cum_v = 0.;
    vwap = None;
    prev_close_rth = None;
    overnight_high = None;
    overnight_low  = None;
    gap = None;
    last_close = None;
    dist_roll = Rolling.create ~capacity:60;
    rv10_roll = Rolling.create ~capacity:10;
    rv60_roll = Rolling.create ~capacity:60;
    close_roll = Deque.create ();
    ofi_s_short = Rolling_sum.create ~capacity:2;
    ofi_v_short = Rolling_sum.create ~capacity:2;
    ofi_s_long  = Rolling_sum.create ~capacity:10;
    ofi_v_long  = Rolling_sum.create ~capacity:10;
  }

let clear_intraday_state s =
  s.cum_pv <- 0.;
  s.cum_v  <- 0.;
  s.vwap   <- None;
  s.gap    <- None;
  s.last_close <- None;
  Rolling.clear s.dist_roll;
  Rolling.clear s.rv10_roll;
  Rolling.clear s.rv60_roll;
  Deque.clear s.close_roll;
  Rolling_sum.clear s.ofi_s_short;
  Rolling_sum.clear s.ofi_v_short;
  Rolling_sum.clear s.ofi_s_long;
  Rolling_sum.clear s.ofi_v_long

let update_date_if_needed s date =
  match s.current_date with
  | None -> s.current_date <- Some date
  | Some d when not (Date.equal d date) ->
      s.current_date <- Some date;
      s.overnight_high <- None;
      s.overnight_low  <- None;
      clear_intraday_state s
  | Some _ -> ()

let signed_volume ~prev_close ~close ~volume =
  if Float.(close > prev_close) then volume
  else if Float.(close < prev_close) then -. volume
  else 0.

let update s (bar : bar_1m) : state =
  let { ts = { date; minute_of_day }; open_; high; low; close; volume } = bar in

  update_date_if_needed s date;

  (* Overnight tracking before RTH *)
  if minute_of_day < rth_start_min then begin
    s.overnight_high <- Some (match s.overnight_high with None -> high | Some h -> Float.max h high);
    s.overnight_low  <- Some (match s.overnight_low  with None -> low  | Some l -> Float.min l low)
  end;

  (* Reset intraday accumulators at RTH open and compute gap to prior close. *)
  if minute_of_day = rth_start_min then begin
    clear_intraday_state s;
    (match s.prev_close_rth with
     | Some c when Float.(c > 0.) -> s.gap <- Some (Float.log (close /. c))
     | _ -> s.gap <- None)
  end;

  (* VWAP accumulation *)
  s.cum_pv <- s.cum_pv +. (close *. volume);
  s.cum_v  <- s.cum_v  +. volume;
  if Float.(s.cum_v > 0.) then s.vwap <- Some (s.cum_pv /. s.cum_v);

  (* 1m return using previous close *)
  (match s.last_close with
   | Some prev ->
       let r1 = Float.log (close /. prev) in
       Rolling.push s.rv10_roll r1;
       Rolling.push s.rv60_roll r1
   | None -> ());

  (* VWAP deviation rolling window *)
  (match s.vwap with
   | Some vw ->
       let dist = close -. vw in
       Rolling.push s.dist_roll dist
   | None -> ());

  (* OFI approximation using signed volume *)
  (match s.last_close with
   | Some prev ->
       let sv = signed_volume ~prev_close:prev ~close ~volume in
       Rolling_sum.push s.ofi_s_short sv;
       Rolling_sum.push s.ofi_v_short volume;
       Rolling_sum.push s.ofi_s_long  sv;
       Rolling_sum.push s.ofi_v_long  volume
   | None -> ());

  (* Maintain close history for trend calc *)
  Deque.enqueue_back s.close_roll close;
  if Deque.length s.close_roll > 61 then ignore (Deque.dequeue_front s.close_roll);

  if minute_of_day = rth_end_min then s.prev_close_rth <- Some close;
  s.last_close <- Some close;
  s

let ofi_ratio signed total =
  match signed, total with
  | Some s, Some v when Float.(v > 0.) -> Some (s /. v)
  | _ -> None

let snapshot (s : state) : snapshot =
  let base_price =
    match s.last_close with
    | Some c -> c
    | None -> Option.value s.vwap ~default:0.0
  in
  let dist_onh = Option.map s.overnight_high ~f:(fun h -> base_price -. h) in
  let dist_onl = Option.map s.overnight_low  ~f:(fun l -> base_price -. l) in
  let rv10 = Rolling.rms s.rv10_roll in
  let rv60 = Rolling.rms s.rv60_roll in
  let rv_ratio =
    match rv10, rv60 with
    | Some a, Some b when Float.(b > 0.) -> Some (a /. b)
    | _ -> None
  in
  let z_vwap =
    match s.vwap, s.last_close, Rolling.std s.dist_roll with
    | Some vw, Some close, Some std when Float.(std > 0.) ->
        Some ((close -. vw) /. std)
    | _ -> None
  in
  let ofi_short = ofi_ratio (Rolling_sum.total s.ofi_s_short) (Rolling_sum.total s.ofi_v_short) in
  let ofi_long  = ofi_ratio (Rolling_sum.total s.ofi_s_long)  (Rolling_sum.total s.ofi_v_long) in
  let trend =
    if Deque.length s.close_roll < 61 then None
    else
      match Deque.peek_front s.close_roll, s.last_close, rv60 with
      | Some old_close, Some close, Some rv when Float.(rv > 0.) ->
          Some (Float.abs (close -. old_close) /. rv)
      | _ -> None
  in
  { vwap = s.vwap;
    z_vwap;
    ofi_short;
    ofi_long;
    rv10;
    rv60;
    rv_ratio;
    trend;
    gap = s.gap;
    dist_onh;
    dist_onl; }
