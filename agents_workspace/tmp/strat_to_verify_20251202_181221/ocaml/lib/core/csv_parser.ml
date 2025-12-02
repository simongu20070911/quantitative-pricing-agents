open Core
open Types
open Time_utils

let parse_bar_1m line : bar_1m =
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
  | [ _ts_event; _rtype; _publisher_id; _instr_id;
      open_s; high_s; low_s; close_s; volume_s; _symbol; et_ts ] ->
      parse et_ts open_s high_s low_s close_s volume_s
  | [ ts_event; _rtype; _publisher_id; _instr_id;
      open_s; high_s; low_s; close_s; volume_s; _symbol ] ->
      (* ts_event may be RFC3339 with optional fractional seconds; strip the fractional
         part to keep [parse_timestamp] happy (it rejects fractions). *)
      let ts_clean =
        match String.lsplit2 ts_event ~on:'.' with
        | Some (prefix, _rest) ->
            if String.is_suffix ts_event ~suffix:"Z"
            then prefix ^ "Z"
            else prefix
        | None -> ts_event
      in
      parse ts_clean open_s high_s low_s close_s volume_s
  | [ ts_event; _rtype; _publisher_id; _instr_id;
      open_s; high_s; low_s; close_s; volume_s ] ->
      (* Raw MDP3 OHLCV without symbol/ET_datetime (e.g. GC), ts_event has nanosecond
         precision and a 'Z' suffix. Strip fractional seconds before parsing. *)
      let ts_clean =
        match String.lsplit2 ts_event ~on:'.' with
        | Some (prefix, _rest) ->
            if String.is_suffix ts_event ~suffix:"Z"
            then prefix ^ "Z"
            else prefix
        | None -> ts_event
      in
      parse ts_clean open_s high_s low_s close_s volume_s
  | _ ->
      failwith ("Malformed CSV line: " ^ line)

(* Stream over a CSV file, applying [f] to each parsed 1‑minute bar. *)
let iter_bars filename ~(f : bar_1m -> unit) : unit =
  In_channel.with_file filename ~f:(fun ic ->
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
