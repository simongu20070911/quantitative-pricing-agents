open Core
open Types

[@@@warning "-27-32-69"]

let minute_ofday_of_string s =
  let hour = Int.of_string (String.sub s ~pos:0 ~len:2) in
  let min  = Int.of_string (String.sub s ~pos:3 ~len:2) in
  (hour * 60) + min

let rth_start_min    = minute_ofday_of_string "09:30"
let rth_end_min      = minute_ofday_of_string "16:15"
let b1_min           = minute_ofday_of_string "09:30"
let b2_min           = minute_ofday_of_string "09:35"
let abr_eod_min      = minute_ofday_of_string "16:10"
let minutes_per_day  = 24 * 60

let parse_timestamp s =
  (* Accepts ISO-like "YYYY-MM-DD HH:MM:SS±HH:MM" (RFC3339). *)
  let t, offset_s =
    match Ptime.of_rfc3339 s with
    | Ok (t, tz, _frac) -> t, tz
    | Error _ ->
        invalid_argf "parse_timestamp: invalid timestamp '%s'" s ()
  in
  let t_local =
    match offset_s with
    | None -> t
    | Some offs ->
        Option.value_exn (Ptime.add_span t (Ptime.Span.of_int_s offs))
  in
  let ((y, m, d), ((hh, mm, ss), _tz)) = Ptime.to_date_time t_local in
  if ss <> 0 then
    invalid_argf "parse_timestamp: expected minute-level data, got seconds '%s'" s ();
  let date =
    Date.create_exn ~y ~m:(Month.of_int_exn m) ~d
  in
  let minute_of_day = (hh * 60) + mm in
  if minute_of_day < 0 || minute_of_day >= minutes_per_day then
    invalid_argf "parse_timestamp: minute_of_day out of range for '%s'" s ();
  { date; minute_of_day }
