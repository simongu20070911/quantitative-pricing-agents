open Core
module TU = Strategy_fast.Time_utils

let parse_exn s = TU.parse_timestamp s

let%test_unit "parses negative offset and preserves local clock" =
  let ts = parse_exn "2020-01-01T10:00:00-04:00" in
  assert (Date.equal ts.date (Date.of_string "2020-01-01"));
  assert (ts.minute_of_day = 10 * 60)

let%test_unit "parses positive offset late evening" =
  let ts = parse_exn "2020-01-01T23:30:00+02:00" in
  assert (Date.equal ts.date (Date.of_string "2020-01-01"));
  assert (ts.minute_of_day = (23 * 60) + 30)

let%test_unit "rejects non-zero seconds" =
  [%test_result: bool]
    (Exn.does_raise (fun () -> parse_exn "2020-01-01T10:00:59+00:00"))
    ~expect:true

let%test_unit "rejects malformed timestamp" =
  [%test_result: bool]
    (Exn.does_raise (fun () -> parse_exn "bad-timestamp"))
    ~expect:true

let%test_unit "offset does not double-apply: compare against UTC baseline" =
  let local = parse_exn "2020-01-01T10:00:00-04:00" in
  (* If the offset were double-applied, date/minute would shift; we expect to
     remain the local wall clock. Cross-check by parsing an equivalent UTC time
     and ensuring local minute differs by 4h. *)
  let utc = parse_exn "2020-01-01T14:00:00+00:00" in
  assert (Date.equal local.date (Date.of_string "2020-01-01"));
  assert (local.minute_of_day = 10 * 60);
  assert (utc.minute_of_day = 14 * 60);
  assert (Int.equal (utc.minute_of_day - local.minute_of_day) (4 * 60))

let%test_unit "cross-midnight negative offset stays in same local date" =
  let ts = parse_exn "2020-01-02T00:30:00-05:00" in
  assert (Date.equal ts.date (Date.of_string "2020-01-02"));
  assert (ts.minute_of_day = 30)

let%test_unit "cross-midnight positive offset stays in same local date" =
  let ts = parse_exn "2020-01-01T23:45:00+03:00" in
  assert (Date.equal ts.date (Date.of_string "2020-01-01"));
  assert (ts.minute_of_day = (23 * 60) + 45)

let%test_unit "accepts Z suffix as zero offset" =
  let ts = parse_exn "2020-01-01T12:00:00Z" in
  assert (Date.equal ts.date (Date.of_string "2020-01-01"));
  assert (ts.minute_of_day = 12 * 60)

let%test_unit "rejects fractional seconds" =
  [%test_result: bool]
    (Exn.does_raise (fun () -> parse_exn "2020-01-01T12:00:00.123+00:00"))
    ~expect:true

let%test_unit "extreme negative offset retains local date" =
  let ts = parse_exn "2020-01-01T00:10:00-12:00" in
  assert (Date.equal ts.date (Date.of_string "2020-01-01"));
  assert (ts.minute_of_day = 10)

let%test_unit "plain timestamp without offset is rejected" =
  [%test_result: bool]
    (Exn.does_raise (fun () -> parse_exn "2020-01-01T10:00:00"))
    ~expect:true
