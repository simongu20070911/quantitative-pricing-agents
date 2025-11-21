open Core
open Types

module type POLICY = Policy_sig.S

module type PURE_STRATEGY = Strategy_sig.S

type strategy = {
  id : string;
  session_start_min : int;
  session_end_min   : int;
  build_setups : (string -> setup Date.Table.t) option;
  policy : (module POLICY);
}

type pure_strategy = {
  _id : string;
  env : Strategy_sig.env;
  build_setups : (string -> setup Date.Table.t) option;
  strategy : (module PURE_STRATEGY);
}

type run_result = {
  setups     : setup Date.Table.t;
  trades     : trade list;
  daily_pnl  : (Date.t * float) list;
  daily_pnl_usd : (Date.t * float) list;
  daily_pnl_pct : (Date.t * float) list;
}

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

let run (strategy : strategy) ~(filename : string) : run_result =
  let setups_tbl =
    match strategy.build_setups with
    | None -> Date.Table.create ()
    | Some f -> f filename
  in
  let module P = (val strategy.policy : POLICY) in
  let module S = struct
    type t = {
      current_date : Date.t option;
      policy_state : P.t option;
      last_session_bar : bar_1m option;
      acc : acc;
    }
  end in

  let flush_trades acc trades =
    List.fold trades ~init:acc ~f:add_trade
  in

  let finalize_day (s : S.t) =
    match s.policy_state with
    | None ->
        { s with current_date = None; policy_state = None; last_session_bar = None }
    | Some st ->
        let _, trades = P.on_session_end st s.last_session_bar in
        let acc = flush_trades s.acc trades in
        { current_date = None; policy_state = None; last_session_bar = None; acc }
  in

  let set_date (s : S.t) date =
    let setup_opt = Hashtbl.find setups_tbl date in
    let policy_state = Some (P.init_day setup_opt) in
    { s with current_date = Some date; policy_state; last_session_bar = None }
  in

  let step (s : S.t) (bar : bar_1m) : S.t =
    let { ts = { date; minute_of_day }; _ } = bar in
    let s =
      match s.current_date with
      | None -> set_date s date
      | Some d when Date.equal d date -> s
      | Some _ -> set_date (finalize_day s) date
    in
    let last_session_bar =
      if minute_of_day >= strategy.session_start_min
         && minute_of_day <= strategy.session_end_min
      then Some bar else s.last_session_bar
    in
    match s.policy_state with
    | None -> { s with last_session_bar }
    | Some st ->
        let st', trades = P.on_bar st bar in
        let acc = flush_trades s.acc trades in
        { s with policy_state = Some st'; last_session_bar; acc }
  in

  let init_state : S.t = { current_date = None; policy_state = None; last_session_bar = None; acc = empty_acc } in
  let state_ref = ref init_state in

  Csv_parser.iter_bars filename ~f:(fun bar ->
      state_ref := step !state_ref bar);

  let final_state = finalize_day !state_ref in
  let acc = final_state.acc in
  let trades = List.rev acc.trades in
  let daily_pnl = Map.to_alist acc.daily_pnl in
  let daily_pnl_usd = Map.to_alist acc.daily_pnl_usd in
  let daily_pnl_pct = Map.to_alist acc.daily_pnl_pct in
  { setups = setups_tbl; trades; daily_pnl; daily_pnl_usd; daily_pnl_pct }

let run_pure (strategy : pure_strategy) ~(filename : string) : run_result =
  let setups_tbl =
    match strategy.build_setups with
    | None -> Date.Table.create ()
    | Some f -> f filename
  in
  let module S = (val strategy.strategy : PURE_STRATEGY) in
  let env = strategy.env in

  let flush_trades acc trades = List.fold trades ~init:acc ~f:add_trade in

  let module L = struct
    type t = {
      current_date : Date.t option;
      strat_state  : S.state option;
      last_session_bar : bar_1m option;
      acc : acc;
    }
  end in

  let finalize_day (s : L.t) =
    match s.strat_state with
    | None ->
        { s with current_date = None; strat_state = None; last_session_bar = None }
    | Some st ->
        let _, trades = S.finalize_day env st s.last_session_bar in
        let acc = flush_trades s.acc trades in
        { current_date = None; strat_state = None; last_session_bar = None; acc }
  in

  let set_date (s : L.t) date =
    let setup_opt = Hashtbl.find setups_tbl date in
    let strat_state = Some (S.init setup_opt) in
    { s with current_date = Some date; strat_state; last_session_bar = None }
  in

  let step (s : L.t) (bar : bar_1m) : L.t =
    let { ts = { date; minute_of_day }; _ } = bar in
    let s =
      match s.current_date with
      | None -> set_date s date
      | Some d when Date.equal d date -> s
      | Some _ -> set_date (finalize_day s) date
    in
    let last_session_bar =
      if minute_of_day >= env.session_start_min && minute_of_day <= env.session_end_min
      then Some bar else s.last_session_bar
    in
    match s.strat_state with
    | None -> { s with last_session_bar }
    | Some st ->
        let st', trades = S.step env st bar in
        let acc = flush_trades s.acc trades in
        { s with strat_state = Some st'; last_session_bar; acc }
  in

  let init_state : L.t = {
    current_date = None;
    strat_state = None;
    last_session_bar = None;
    acc = empty_acc;
  } in

  let state_ref = ref init_state in
  Csv_parser.iter_bars filename ~f:(fun bar ->
      state_ref := step !state_ref bar);

  let final_state = finalize_day !state_ref in
  let acc = final_state.acc in
  let trades = List.rev acc.trades in
  let daily_pnl = Map.to_alist acc.daily_pnl in
  let daily_pnl_usd = Map.to_alist acc.daily_pnl_usd in
  let daily_pnl_pct = Map.to_alist acc.daily_pnl_pct in
  { setups = setups_tbl; trades; daily_pnl; daily_pnl_usd; daily_pnl_pct }

let _ = run_pure
