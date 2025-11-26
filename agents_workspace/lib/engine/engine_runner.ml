open Core
open Types

module Pnl = Pnl_agg

(* Shared single-strategy runner used by both Engine_v2 and Multi_engine.
   Keeps semantics identical to the original per-strategy loop while avoiding duplication. *)

let make_exec_config (env : Strategy_sig.env) : Execution_engine.config =
  let build_trade ~(plan : trade_plan) ~(active : active_state)
      ~(exit_ts : timestamp) ~(exit_price : float)
      ~(exit_qty : float) ~(exit_reason : exit_reason) : trade =
    Trade_base.make_raw
      ~qty:exit_qty ~r_pts:plan.r_pts ~direction:plan.direction
      ~entry_ts:active.entry_ts ~entry_px:active.entry_price
      ~exit_ts ~exit_px:exit_price ~exit_reason
      ~meta:[]
    |> Trade_base.apply_costs ~qty:exit_qty env.cost
  in
  { Execution_engine.cost = env.cost;
    exec = env.exec;
    build_trade; }

type t =
  | Runner : {
      strat : Engine_types.pure_strategy;
      strategy : (module Strategy_sig.V2 with type state = 's);
      mutable state : 's option;
      setups : setup Date.Table.t;
      mutable book : Order_book.t;
      mutable current_date : Date.t option;
      mutable last_session_bar : bar_1m option;
      mutable acc : Pnl.t;
    } -> t

let create (strat : Engine_types.pure_strategy) ~(setups : setup Date.Table.t) : t =
  let module S = (val strat.strategy : Strategy_sig.V2) in
  Runner {
    strat;
    strategy = (module S);
    state = None;
    setups;
    book = Order_book.empty ();
    current_date = None;
    last_session_bar = None;
    acc = Pnl.empty;
  }

let strategy_id (Runner r) = r.strat._id

let set_date (Runner r as t) date =
  let module S = (val r.strategy) in
  let setup_opt = Hashtbl.find r.setups date in
  r.current_date <- Some date;
  r.state <- Some (S.init setup_opt);
  r.book <- Order_book.empty ();
  r.last_session_bar <- None;
  t

let finalize_day (Runner r as t) : t =
  match r.state with
  | None ->
      r.current_date <- None;
      r.last_session_bar <- None;
      t
  | Some st ->
      let module S = (val r.strategy) in
      let env = r.strat.env in
      let st', cmds = S.finalize_day env st r.last_session_bar in
      let order_cmds, flatten_cmds =
        List.partition_tf cmds ~f:(function
            | Strategy_sig.Submit_bracket _
            | Strategy_sig.Update_all _
            | Strategy_sig.Update_stop _
            | Strategy_sig.Update_target _
            | Strategy_sig.Cancel_all -> true
            | Strategy_sig.Flatten_all _ -> false)
      in
      let cfg = make_exec_config env in
      let book_after_cmds =
        match r.last_session_bar with
        | None -> r.book
        | Some lb -> Order_book.apply_cmds r.book ~ts:lb.ts order_cmds
      in
      let book_after_flat, flatten_trades =
        match r.last_session_bar with
        | None -> book_after_cmds, []
        | Some lb ->
            Execution_engine.apply_flatten_cmds ~config:cfg ~book:book_after_cmds
              ~bar:lb flatten_cmds
      in
      let book', trades =
        Execution_engine.on_session_end ~config:cfg
          ~book:book_after_flat ~last_bar:r.last_session_bar
      in
      let acc = flatten_trades @ trades |> Pnl.add_trades r.acc in
      r.state <- Some st';
      r.current_date <- None;
      r.last_session_bar <- None;
      r.book <- book';
      r.acc <- acc;
      t

let step (Runner r as t) (bar : bar_1m) : t =
  let { ts = { date; minute_of_day }; _ } = bar in
  let t =
    match r.current_date with
    | None -> set_date t date
    | Some d when Date.equal d date -> t
    | Some _ -> set_date (finalize_day t) date
  in
  let Runner r = t in
  let module S = (val r.strategy) in
  let env = r.strat.env in
  let last_session_bar =
    if minute_of_day >= env.session_start_min
       && minute_of_day <= env.session_end_min
    then Some bar
    else r.last_session_bar
  in
  match r.state with
  | None ->
      r.last_session_bar <- last_session_bar;
      t
  | Some st ->
      let st', cmds = S.step env st bar in
      let order_cmds, flatten_cmds =
        List.partition_tf cmds ~f:(function
            | Strategy_sig.Submit_bracket _
            | Strategy_sig.Update_all _
            | Strategy_sig.Update_stop _
            | Strategy_sig.Update_target _
            | Strategy_sig.Cancel_all -> true
            | Strategy_sig.Flatten_all _ -> false)
      in
      let book_after_cmds = Order_book.apply_cmds r.book ~ts:bar.ts order_cmds in
      let cfg = make_exec_config env in
      let book_after_flat, flatten_trades =
        Execution_engine.apply_flatten_cmds ~config:cfg ~book:book_after_cmds
          ~bar flatten_cmds
      in
      let book', trades_step =
        Execution_engine.step ~config:cfg ~book:book_after_flat ~bar
      in
      let trades = flatten_trades @ trades_step in
      let acc = Pnl.add_trades r.acc trades in
      r.state <- Some st';
      r.last_session_bar <- last_session_bar;
      r.book <- book';
      r.acc <- acc;
      t

let finalize_stream t = ignore (finalize_day t)

let result (Runner r) : Engine_types.run_result =
  let trades = List.rev r.acc.trades in
  let daily_pnl, daily_pnl_usd, daily_pnl_pct = Pnl.to_alists_unsorted r.acc in
  {
    setups = r.setups;
    trades;
    daily_pnl;
    daily_pnl_usd;
    daily_pnl_pct;
  }
