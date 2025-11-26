open Core
type pure_strategy = Engine_types.pure_strategy
type run_result = Engine_types.run_result

let run_pure (strategy : pure_strategy) ~(filename : string) : run_result =
  let setups_tbl =
    match strategy.build_setups with
    | None -> Date.Table.create ()
    | Some f -> f filename
  in
  let runner = Engine_runner.create strategy ~setups:setups_tbl in
  Csv_parser.iter_bars filename ~f:(fun bar -> ignore (Engine_runner.step runner bar));
  Engine_runner.finalize_stream runner;
  Engine_runner.result runner
