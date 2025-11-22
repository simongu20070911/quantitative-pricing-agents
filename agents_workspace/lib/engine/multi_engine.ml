open Core
open Types
open Engine
module Pnl = Pnl_agg

(* Results per strategy *)
type run_result = {
  strategy_id : string;
  setups      : setup Date.Table.t;
  trades      : trade list;
  daily_pnl   : (Date.t * float) list;
  daily_pnl_usd : (Date.t * float) list;
  daily_pnl_pct : (Date.t * float) list;
}

(* Lightweight bar source abstraction *)
module type BAR_STREAM = sig
  val iter : f:(bar_1m -> unit) -> unit
end

(* Multi-pass helper *)
let run_all (strategies : Engine.strategy list) ~(filename : string) : run_result list =
  List.map strategies ~f:(fun strat ->
      let { Engine.setups; trades; daily_pnl; daily_pnl_usd; daily_pnl_pct } = Engine.run strat ~filename in
      { strategy_id = strat.id; setups; trades; daily_pnl; daily_pnl_usd; daily_pnl_pct })

let run_all_pure (strategies : Engine.pure_strategy list) ~(filename : string) : run_result list =
  List.map strategies ~f:(fun strat ->
      let { Engine.setups; trades; daily_pnl; daily_pnl_usd; daily_pnl_pct } = Engine.run_pure strat ~filename in
      { strategy_id = strat._id; setups; trades; daily_pnl; daily_pnl_usd; daily_pnl_pct })

(* Helpers shared by contexts *)
type acc = Pnl.t

let empty_acc = Pnl.empty
let flush_trades = Pnl.add_trades

(* Shared-bar runner using a GADT to keep policy state typed, but contexts are immutable records. *)
type strat_ctx =
  | Strat : {
      strat      : Engine.strategy;
      setups     : setup Date.Table.t;
      policy     : (module Policy_sig.S with type t = 's);
      state      : 's option;
      current_date : Date.t option;
      last_session_bar : bar_1m option;
      acc        : acc;
    } -> strat_ctx

let init_ctx (strat : Engine.strategy) ~(setups : setup Date.Table.t) =
  let module P = (val strat.policy : Policy_sig.S) in
  Strat {
    strat;
    setups;
    policy = (module P);
    state = None;
    current_date = None;
    last_session_bar = None;
    acc = empty_acc;
  }

let set_date (Strat ctx) date =
  let module P = (val ctx.policy) in
  let setup_opt = Hashtbl.find ctx.setups date in
  Strat { ctx with
          current_date = Some date;
          state = Some (P.init_day setup_opt);
          last_session_bar = None; }

let finalize_day (Strat ctx) =
  match ctx.state with
  | None -> Strat { ctx with current_date = None; last_session_bar = None }
  | Some st ->
      let module P = (val ctx.policy) in
      let st', trades = P.on_session_end st ctx.last_session_bar in
      let acc = flush_trades ctx.acc trades in
      Strat {
        ctx with
        state = Some st';
        current_date = None;
        last_session_bar = None;
        acc;
      }

let step_ctx (Strat ctx0) (bar : bar_1m) : strat_ctx =
  let { ts = { date; minute_of_day }; _ } = bar in
  let ctx =
    match ctx0.current_date with
    | None -> set_date (Strat ctx0) date
    | Some d when Date.equal d date -> Strat ctx0
    | Some _ ->
        let Strat ctx' = finalize_day (Strat ctx0) in
        set_date (Strat ctx') date
  in
  let Strat ctx = ctx in
  let module P = (val ctx.policy) in
  let last_session_bar =
    if minute_of_day >= ctx.strat.session_start_min
       && minute_of_day <= ctx.strat.session_end_min
    then Some bar
    else ctx.last_session_bar
  in
  match ctx.state with
  | None ->
      Strat { ctx with last_session_bar }
  | Some st ->
      let st', trades = P.on_bar st bar in
      let acc = flush_trades ctx.acc trades in
      Strat { ctx with state = Some st'; last_session_bar; acc }

let extract_result (Strat ctx) : run_result =
  let trades = List.rev ctx.acc.trades in
  let daily_pnl, daily_pnl_usd, daily_pnl_pct = Pnl.to_alists_unsorted ctx.acc in
  let sort = List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2) in
  let daily_pnl = sort daily_pnl in
  let daily_pnl_usd = sort daily_pnl_usd in
  let daily_pnl_pct = sort daily_pnl_pct in
  {
    strategy_id = ctx.strat.id;
    setups = ctx.setups;
    trades;
    daily_pnl;
    daily_pnl_usd;
    daily_pnl_pct;
  }

(* Generic driver to eliminate duplicate shared-stream logic. *)
module type CTX = sig
  type t
  type strat

  val init : strat -> setups:setup Date.Table.t -> t
  val step : t -> bar_1m -> t
  val finalize_day : t -> t
  val extract : t -> run_result
end

let run_shared_generic
    (type s)
    (module C : CTX with type strat = s)
    ~(iter : (bar_1m -> unit) -> unit)
    ~(make_setups : s -> setup Date.Table.t)
    (strategies : s list) : run_result list =
  let ctxs =
    List.map strategies ~f:(fun strat ->
        C.init strat ~setups:(make_setups strat))
  in
  let ctxs_ref = ref ctxs in
  iter (fun bar ->
      ctxs_ref := List.map !ctxs_ref ~f:(fun ctx -> C.step ctx bar));
  ctxs_ref := List.map !ctxs_ref ~f:C.finalize_day;
  List.map !ctxs_ref ~f:C.extract

let run_shared_with_stream ~(stream : (module BAR_STREAM))
    ~(make_setups : Engine.strategy -> setup Date.Table.t)
    (strategies : Engine.strategy list)
  : run_result list =
  let module Stream = (val stream : BAR_STREAM) in
  let module C = struct
    type strat = Engine.strategy
    type t = strat_ctx
    let init = init_ctx
    let step = step_ctx
    let finalize_day = finalize_day
    let extract = extract_result
  end in
  run_shared_generic (module C)
    ~iter:(fun g -> Stream.iter ~f:g)
    ~make_setups strategies

let run_shared (strategies : Engine.strategy list) ~(filename : string)
  : run_result list =
  let module Csv_stream = struct
    let iter ~f = Csv_parser.iter_bars filename ~f
  end in
  let make_setups (strat : Engine.strategy) =
    match strat.build_setups with
    | None -> Date.Table.create ()
    | Some f -> f filename
  in
  run_shared_with_stream ~stream:(module Csv_stream) ~make_setups strategies

(* Pure strategy shared runner *)
type pure_ctx =
  | Pure_strat : {
      strat      : Engine.pure_strategy;
      setups     : setup Date.Table.t;
      strategy   : (module Strategy_sig.S with type state = 's);
      state      : 's option;
      current_date : Date.t option;
      last_session_bar : bar_1m option;
      acc        : acc;
    } -> pure_ctx

let init_pure_ctx (strat : Engine.pure_strategy) ~(setups:setup Date.Table.t) =
  let module S = (val strat.strategy : Strategy_sig.S) in
  Pure_strat {
    strat;
    setups;
    strategy = (module S);
    state = None;
    current_date = None;
    last_session_bar = None;
    acc = empty_acc;
  }

let set_date_pure (Pure_strat ctx) date =
  let module S = (val ctx.strategy) in
  let setup_opt = Hashtbl.find ctx.setups date in
  Pure_strat { ctx with
               current_date = Some date;
               state = Some (S.init setup_opt);
               last_session_bar = None; }

let finalize_day_pure (Pure_strat ctx) =
  match ctx.state with
  | None -> Pure_strat { ctx with current_date = None; last_session_bar = None }
  | Some st ->
      let module S = (val ctx.strategy) in
      let st', trades = S.finalize_day ctx.strat.env st ctx.last_session_bar in
      let acc = flush_trades ctx.acc trades in
      Pure_strat {
        ctx with
        state = Some st';
        current_date = None;
        last_session_bar = None;
        acc;
      }

let step_pure_ctx (Pure_strat ctx0) (bar : bar_1m) : pure_ctx =
  let { ts = { date; minute_of_day }; _ } = bar in
  let ctx =
    match ctx0.current_date with
    | None -> set_date_pure (Pure_strat ctx0) date
    | Some d when Date.equal d date -> Pure_strat ctx0
    | Some _ ->
        let Pure_strat ctx' = finalize_day_pure (Pure_strat ctx0) in
        set_date_pure (Pure_strat ctx') date
  in
  let Pure_strat ctx = ctx in
  let module S = (val ctx.strategy) in
  let last_session_bar =
    if minute_of_day >= ctx.strat.env.session_start_min
       && minute_of_day <= ctx.strat.env.session_end_min
    then Some bar
    else ctx.last_session_bar
  in
  match ctx.state with
  | None -> Pure_strat { ctx with last_session_bar }
  | Some st ->
      let st', trades = S.step ctx.strat.env st bar in
      let acc = flush_trades ctx.acc trades in
      Pure_strat { ctx with state = Some st'; last_session_bar; acc }

let extract_result_pure (Pure_strat ctx) : run_result =
  let trades = List.rev ctx.acc.trades in
  let daily_pnl, daily_pnl_usd, daily_pnl_pct = Pnl.to_alists_unsorted ctx.acc in
  let sort = List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2) in
  let daily_pnl = sort daily_pnl in
  let daily_pnl_usd = sort daily_pnl_usd in
  let daily_pnl_pct = sort daily_pnl_pct in
  {
    strategy_id = ctx.strat._id;
    setups = ctx.setups;
    trades;
    daily_pnl;
    daily_pnl_usd;
    daily_pnl_pct;
  }

let run_shared_pure (strategies : Engine.pure_strategy list) ~(filename : string)
  : run_result list =
  let make_setups strat =
    match strat.build_setups with
    | None -> Date.Table.create ()
    | Some f -> f filename
  in
  let module Csv_stream = struct
    let iter ~f = Csv_parser.iter_bars filename ~f
  end in
  let module C = struct
    type strat = Engine.pure_strategy
    type t = pure_ctx
    let init = init_pure_ctx
    let step = step_pure_ctx
    let finalize_day = finalize_day_pure
    let extract = extract_result_pure
  end in
  run_shared_generic (module C)
    ~iter:(fun g -> Csv_stream.iter ~f:g)
    ~make_setups strategies
