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

module Vwap = struct
  type t = {
    mutable cum_pv : float;
    mutable cum_v  : float;
    mutable vwap   : float option;
    dist_roll      : Rolling.t;
  }

  let create () =
    { cum_pv = 0.; cum_v = 0.; vwap = None; dist_roll = Rolling.create ~capacity:60 }

  let reset t =
    t.cum_pv <- 0.;
    t.cum_v  <- 0.;
    t.vwap   <- None;
    Rolling.clear t.dist_roll

  let update t ~close ~volume =
    t.cum_pv <- t.cum_pv +. (close *. volume);
    t.cum_v  <- t.cum_v  +. volume;
    if Float.(t.cum_v > 0.) then t.vwap <- Some (t.cum_pv /. t.cum_v);
    Option.iter t.vwap ~f:(fun vw -> Rolling.push t.dist_roll (close -. vw))

  let z t ~close =
    match t.vwap, Rolling.std t.dist_roll with
    | Some vw, Some std when Float.(std > 0.) -> Some ((close -. vw) /. std)
    | _ -> None
end

module Rv = struct
  type t = {
    rv10_roll : Rolling.t;
    rv60_roll : Rolling.t;
  }

  let create () = { rv10_roll = Rolling.create ~capacity:10; rv60_roll = Rolling.create ~capacity:60 }

  let reset t =
    Rolling.clear t.rv10_roll;
    Rolling.clear t.rv60_roll

  let update t ~r1 =
    Rolling.push t.rv10_roll r1;
    Rolling.push t.rv60_roll r1

  let rv10 t = Rolling.rms t.rv10_roll
  let rv60 t = Rolling.rms t.rv60_roll
end

module Ofi = struct
  type t = {
    ofi_s_short : Rolling_sum.t;
    ofi_v_short : Rolling_sum.t;
    ofi_s_long  : Rolling_sum.t;
    ofi_v_long  : Rolling_sum.t;
  }

  let create () = {
    ofi_s_short = Rolling_sum.create ~capacity:2;
    ofi_v_short = Rolling_sum.create ~capacity:2;
    ofi_s_long  = Rolling_sum.create ~capacity:10;
    ofi_v_long  = Rolling_sum.create ~capacity:10;
  }

  let reset t =
    Rolling_sum.clear t.ofi_s_short;
    Rolling_sum.clear t.ofi_v_short;
    Rolling_sum.clear t.ofi_s_long;
    Rolling_sum.clear t.ofi_v_long

  let update t ~signed_volume ~volume =
    Rolling_sum.push t.ofi_s_short signed_volume;
    Rolling_sum.push t.ofi_v_short volume;
    Rolling_sum.push t.ofi_s_long  signed_volume;
    Rolling_sum.push t.ofi_v_long  volume

  let ratio signed total =
    match signed, total with
    | Some s, Some v when Float.(v > 0.) -> Some (s /. v)
    | _ -> None

  let ofi_short t =
    ratio (Rolling_sum.total t.ofi_s_short) (Rolling_sum.total t.ofi_v_short)

  let ofi_long t =
    ratio (Rolling_sum.total t.ofi_s_long) (Rolling_sum.total t.ofi_v_long)
end

module Trend = struct
  type t = {
    close_roll : float Deque.t; (* track last 61 closes *)
  }

  let create () = { close_roll = Deque.create () }

  let reset t = Deque.clear t.close_roll

  let update t ~close =
    Deque.enqueue_back t.close_roll close;
    if Deque.length t.close_roll > 61 then ignore (Deque.dequeue_front t.close_roll)

  let value t ~rv60 ~last_close =
    if Deque.length t.close_roll < 61 then None
    else
      match Deque.peek_front t.close_roll with
      | None -> None
      | Some old_close when Float.(rv60 > 0.) ->
          Some (Float.abs (last_close -. old_close) /. rv60)
      | _ -> None
end

module Overnight = struct
  type t = {
    mutable prev_close_rth : float option;
    mutable overnight_high : float option;
    mutable overnight_low  : float option;
    mutable gap            : float option;
  }

  let create () =
    { prev_close_rth = None; overnight_high = None; overnight_low = None; gap = None }

  let reset_for_new_day t =
    t.overnight_high <- None;
    t.overnight_low  <- None;
    t.gap <- None

  let track_pre_rth t ~high ~low =
    t.overnight_high <- Some (match t.overnight_high with None -> high | Some h -> Float.max h high);
    t.overnight_low  <- Some (match t.overnight_low  with None -> low  | Some l -> Float.min l low)

  let set_gap_at_open t ~open_price =
    match t.prev_close_rth with
    | Some c when Float.(c > 0.) -> t.gap <- Some (Float.log (open_price /. c))
    | _ -> t.gap <- None

  let set_prev_close t ~close = t.prev_close_rth <- Some close
end

type state = {
  mutable current_date : Date.t option;
  mutable last_close   : float option;
  vwap     : Vwap.t;
  rv       : Rv.t;
  ofi      : Ofi.t;
  trend    : Trend.t;
  overnight: Overnight.t;
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
    last_close   = None;
    vwap     = Vwap.create ();
    rv       = Rv.create ();
    ofi      = Ofi.create ();
    trend    = Trend.create ();
    overnight= Overnight.create ();
  }

let clear_intraday_state (s : state) =
  Vwap.reset s.vwap;
  Rv.reset s.rv;
  Ofi.reset s.ofi;
  Trend.reset s.trend;
  s.last_close <- None;
  s.overnight.gap <- None

let update_date_if_needed (s : state) date =
  match s.current_date with
  | None ->
      s.current_date <- Some date;
      Overnight.reset_for_new_day s.overnight;
      clear_intraday_state s
  | Some d when not (Date.equal d date) ->
      s.current_date <- Some date;
      Overnight.reset_for_new_day s.overnight;
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
    Overnight.track_pre_rth s.overnight ~high ~low
  end;

  (* Reset intraday accumulators at RTH open and compute gap to prior close. *)
  if minute_of_day = rth_start_min then begin
    clear_intraday_state s;
    Overnight.set_gap_at_open s.overnight ~open_price:close
  end;

  (* VWAP accumulation *)
  Vwap.update s.vwap ~close ~volume;

  (* 1m return using previous close *)
  (match s.last_close with
   | Some prev ->
       let r1 = Float.log (close /. prev) in
       Rv.update s.rv ~r1
   | None -> ());

  (* OFI approximation using signed volume *)
  (match s.last_close with
   | Some prev ->
       let sv = signed_volume ~prev_close:prev ~close ~volume in
       Ofi.update s.ofi ~signed_volume:sv ~volume
   | None -> ());

  (* Maintain close history for trend calc *)
  Trend.update s.trend ~close;

  if minute_of_day = rth_end_min then Overnight.set_prev_close s.overnight ~close;
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
    | None -> Option.value s.vwap.vwap ~default:0.0
  in
  let dist_onh = Option.map s.overnight.overnight_high ~f:(fun h -> base_price -. h) in
  let dist_onl = Option.map s.overnight.overnight_low  ~f:(fun l -> base_price -. l) in
  let rv10 = Rv.rv10 s.rv in
  let rv60 = Rv.rv60 s.rv in
  let rv_ratio =
    match rv10, rv60 with
    | Some a, Some b when Float.(b > 0.) -> Some (a /. b)
    | _ -> None
  in
  let z_vwap =
    match s.last_close with
    | None -> None
    | Some close -> Vwap.z s.vwap ~close
  in
  let ofi_short = Ofi.ofi_short s.ofi in
  let ofi_long  = Ofi.ofi_long s.ofi in
  let trend =
    match s.last_close, rv60 with
    | Some close, Some rv -> Trend.value s.trend ~rv60:rv ~last_close:close
    | _ -> None
  in
  { vwap = s.vwap.vwap;
    z_vwap;
    ofi_short;
    ofi_long;
    rv10;
    rv60;
    rv_ratio;
    trend;
    gap = s.overnight.gap;
    dist_onh;
    dist_onl; }
