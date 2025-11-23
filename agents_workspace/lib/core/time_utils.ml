open Core
open Types

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
  (* Accepts RFC3339 "YYYY-MM-DDTHH:MM:SS±HH:MM" (or space instead of 'T'),
     and also a legacy intraday form "YYYY-MM-DD HH:MM" (no seconds/offset)
     used by some generated fixtures. Fractional seconds are rejected. *)
  let has_fractional =
    String.is_substring s ~substring:"."
  in
  if has_fractional then
    invalid_argf "parse_timestamp: fractional seconds not supported '%s'" s ();
  let fallback_naive () =
    match String.split ~on:' ' s with
    | [date_str; hhmm] when String.length hhmm = 5 && Char.(hhmm.[2] = ':') ->
        let hour = Int.of_string (String.sub hhmm ~pos:0 ~len:2) in
        let min  = Int.of_string (String.sub hhmm ~pos:3 ~len:2) in
        if hour < 0 || hour > 23 || min < 0 || min > 59 then
          invalid_argf "parse_timestamp: invalid timestamp '%s'" s ();
        let date = Date.of_string date_str in
        { date; minute_of_day = (hour * 60) + min }
    | _ ->
        invalid_argf "parse_timestamp: invalid timestamp '%s'" s ()
  in
  match Ptime.of_rfc3339 s with
  | Ok (t, offset_s, _count) ->
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
  | Error _ ->
      (* Try the naive fallback without timezone/seconds, only when no 'T' separator is present. *)
      (match String.lfindi s ~f:(fun _ c -> Char.equal c 'T') with
       | Some _ -> invalid_argf "parse_timestamp: invalid timestamp '%s'" s ()
       | None -> fallback_naive ())
