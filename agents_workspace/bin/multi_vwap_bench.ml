open Core
open Strategy_fast

(* Run 20 identical vwap_revert strategies in a single shared-stream pass
   over sample_es.csv to measure Multi_engine overhead. *)

let () =
  let datafile = "sample_es.csv" in
  if not (Stdlib.Sys.file_exists datafile) then
    failwith (Printf.sprintf "data file %s not found" datafile);
  let strat_pack = Strategy_registry.find_exn "vwap_revert" in
  (* Use default parameter map for vwap_revert. *)
  let params = Parameters.default_map strat_pack.specs in
  let strategies =
    List.init 20 ~f:(fun _ -> strat_pack.build params)
  in
  let results =
    Engine.Multi_engine.run_shared_pure strategies ~filename:datafile
  in
  (* Just print total trades for sanity so the run does something observable. *)
  let total_trades =
    List.sum (module Int) results ~f:(fun r -> List.length r.trades)
  in
  Stdio.printf "Ran 20 vwap_revert strategies in shared stream; total trades = %d\n%!"
    total_trades

