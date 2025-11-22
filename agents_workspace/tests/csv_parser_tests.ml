open Core
open Strategy_fast

module CP = Csv_parser

let%test_unit "parses 10-column CSV row using ts_event" =
  let line = "2020-01-01T09:30:00+00:00,RT,pub,instr,100.0,101.0,99.0,100.5,10,ES" in
  let bar = CP.parse_bar_1m line in
  assert (Date.equal bar.ts.date (Date.of_string "2020-01-01"));
  assert (bar.ts.minute_of_day = (9 * 60) + 30);
  assert (Float.(bar.open_ = 100.0));
  assert (Float.(bar.volume = 10.0))

let%test_unit "prefers ET_datetime when 11 columns present" =
  let et_line = "2020-01-01T05:30:00+00:00,RT,pub,instr,100.0,100.0,100.0,100.0,1,ES,2020-01-01T09:30:00+00:00" in
  let bar = CP.parse_bar_1m et_line in
  (* Should parse using the final ET datetime, not the first timestamp. *)
  assert (bar.ts.minute_of_day = (9 * 60) + 30)

let%test_unit "malformed CSV row raises" =
  [%test_result: bool]
    (Exn.does_raise (fun () -> CP.parse_bar_1m "too,few,cols"))
    ~expect:true
