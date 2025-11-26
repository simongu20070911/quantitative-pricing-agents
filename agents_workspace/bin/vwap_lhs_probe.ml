open Core
open Strategy_fast

(* Probe the 20 Latin-hypercube samples (seed=1) used for Bayes warm-up on
   sample_es.csv, timing each single-strategy run to spot pathological
   parameter combinations. *)

let main () =
  let datafile = "sample_es.csv" in
  if not (Stdlib.Sys.file_exists datafile) then
    failwith (Printf.sprintf "data file %s not found" datafile);

  let strat_pack = Strategy_registry.find_exn "vwap_revert" in
  let specs = strat_pack.specs in
  let params_list = Engine.Opt_algos.latin_hypercube specs ~samples:20 ~seed:1 in

  List.iteri params_list ~f:(fun idx params ->
      let t0 = Time_float.now () in
      let strategy = strat_pack.build params in
      let res = Engine.Engine.run_pure strategy ~filename:datafile in
      let dur = Time_float.diff (Time_float.now ()) t0 |> Time_float.Span.to_sec in
      let trades = List.length res.trades in
      let params_json =
        `Assoc
          (params
           |> Map.to_alist
           |> List.map ~f:(fun (k, v) -> k, `Float v))
        |> Yojson.Safe.to_string
      in
      printf "sample=%02d dur_s=%.3f trades=%d params=%s\n%!" (idx + 1) dur trades
        params_json)

let () = main ()
