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
  (* Example: "2010-06-06 20:00:00-04:00" *)
  let date_str = String.sub s ~pos:0 ~len:10 in
  let date = Date.of_string date_str in
  let hour = Int.of_string (String.sub s ~pos:11 ~len:2) in
  let min  = Int.of_string (String.sub s ~pos:14 ~len:2) in
  let minute_of_day = (hour * 60) + min in
  { date; minute_of_day }
