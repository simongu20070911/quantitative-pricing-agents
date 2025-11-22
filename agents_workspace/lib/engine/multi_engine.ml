open Core
open Types
open Engine

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
type acc = {
  trades : trade list;
  daily_pnl : float Date.Map.t;
  daily_pnl_usd : float Date.Map.t;
  daily_pnl_pct : float Date.Map.t;
}

let empty_acc = {
  trades = [];
  daily_pnl = Date.Map.empty;
  daily_pnl_usd = Date.Map.empty;
  daily_pnl_pct = Date.Map.empty;
}

let add_trade acc t =
  let daily_pnl =
    Map.update acc.daily_pnl t.date ~f:(function
        | None -> t.pnl_R
        | Some x -> x +. t.pnl_R)
  in
  let daily_pnl_usd =
    Map.update acc.daily_pnl_usd t.date ~f:(function
        | None -> t.pnl_usd
        | Some x -> x +. t.pnl_usd)
  in
  let daily_pnl_pct =
    match t.pnl_pct with
    | None -> acc.daily_pnl_pct
    | Some pct ->
        Map.update acc.daily_pnl_pct t.date ~f:(function
            | None -> pct
            | Some x -> x +. pct)
  in
  {
    trades = t :: acc.trades;
    daily_pnl;
    daily_pnl_usd;
    daily_pnl_pct;
  }

let flush_trades acc trades = List.fold trades ~init:acc ~f:add_trade

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

let finalize_all ctxs =
  List.map ctxs ~f:(fun ctx ->
      let Strat c = finalize_day ctx in
      Strat c)

let extract_result (Strat ctx) : run_result =
  let trades = List.rev ctx.acc.trades in
  let daily_pnl =
    Map.to_alist ctx.acc.daily_pnl
    |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
  in
  let daily_pnl_usd =
    Map.to_alist ctx.acc.daily_pnl_usd
    |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
  in
  let daily_pnl_pct =
    Map.to_alist ctx.acc.daily_pnl_pct
    |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
  in
  {
    strategy_id = ctx.strat.id;
    setups = ctx.setups;
    trades;
    daily_pnl;
    daily_pnl_usd;
    daily_pnl_pct;
  }

let run_shared_with_stream ~(stream : (module BAR_STREAM))
    ~(make_setups : Engine.strategy -> setup Date.Table.t)
    (strategies : Engine.strategy list)
  : run_result list =
  let ctxs =
    List.map strategies ~f:(fun s ->
        let setups = make_setups s in
        init_ctx s ~setups)
  in
  let ctxs_ref = ref ctxs in
  let module Stream = (val stream : BAR_STREAM) in
  Stream.iter ~f:(fun bar ->
      ctxs_ref := List.map !ctxs_ref ~f:(fun ctx -> step_ctx ctx bar));
  ctxs_ref := finalize_all !ctxs_ref;
  List.map !ctxs_ref ~f:extract_result

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

let finalize_all_pure ctxs =
  List.map ctxs ~f:(fun ctx ->
      let Pure_strat c = finalize_day_pure ctx in
      Pure_strat c)

let extract_result_pure (Pure_strat ctx) : run_result =
  let trades = List.rev ctx.acc.trades in
  let daily_pnl =
    Map.to_alist ctx.acc.daily_pnl
    |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
  in
  let daily_pnl_usd =
    Map.to_alist ctx.acc.daily_pnl_usd
    |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
  in
  let daily_pnl_pct =
    Map.to_alist ctx.acc.daily_pnl_pct
    |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
  in
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
  let ctxs =
    List.map strategies ~f:(fun s ->
        let setups = make_setups s in
        init_pure_ctx s ~setups)
  in
  let ctxs_ref = ref ctxs in
  Csv_parser.iter_bars filename ~f:(fun bar ->
      ctxs_ref := List.map !ctxs_ref ~f:(fun ctx -> step_pure_ctx ctx bar));
  ctxs_ref := finalize_all_pure !ctxs_ref;
  List.map !ctxs_ref ~f:extract_result_pure
