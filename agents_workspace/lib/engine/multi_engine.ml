open Core
open Types
module EV2 = Engine_v2

type run_result = {
  strategy_id : string;
  setups      : setup Date.Table.t;
  trades      : trade list;
  daily_pnl   : (Date.t * float) list;
  daily_pnl_usd : (Date.t * float) list;
  daily_pnl_pct : (Date.t * float) list;
}

module type BAR_STREAM = sig
  val iter : f:(bar_1m -> unit) -> unit
end

let run_all (strategies : EV2.pure_strategy list) ~(filename : string)
  : run_result list =
  List.map strategies ~f:(fun strat ->
      let { Engine_types.setups; trades; daily_pnl; daily_pnl_usd; daily_pnl_pct } =
        EV2.run_pure strat ~filename
      in
      { strategy_id = strat._id; setups; trades; daily_pnl; daily_pnl_usd; daily_pnl_pct })

let run_all_pure = run_all

let run_shared_with_stream ~(stream : (module BAR_STREAM))
    ~(make_setups : EV2.pure_strategy -> setup Date.Table.t)
    (strategies : EV2.pure_strategy list)
  : run_result list =
  let module Stream = (val stream : BAR_STREAM) in
  let runners =
    List.map strategies ~f:(fun strat ->
        let setups = make_setups strat in
        Engine_runner.create strat ~setups)
  in
  Stream.iter ~f:(fun bar ->
      List.iter runners ~f:(fun r -> ignore (Engine_runner.step r bar)));
  List.iter runners ~f:Engine_runner.finalize_stream;
  List.map runners ~f:(fun r ->
      let res = Engine_runner.result r in
      {
        strategy_id = Engine_runner.strategy_id r;
        setups = res.setups;
        trades = res.trades;
        daily_pnl = res.daily_pnl;
        daily_pnl_usd = res.daily_pnl_usd;
        daily_pnl_pct = res.daily_pnl_pct;
      })

let run_shared (strategies : EV2.pure_strategy list) ~(filename : string)
  : run_result list =
  let module Csv_stream = struct
    let iter ~f = Csv_parser.iter_bars filename ~f
  end in
  let make_setups (strat : EV2.pure_strategy) =
    match strat.build_setups with
    | None -> Date.Table.create ()
    | Some f -> f filename
  in
  run_shared_with_stream ~stream:(module Csv_stream) ~make_setups strategies

let run_shared_pure = run_shared
