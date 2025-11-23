open Core
open Types

module Pnl = Pnl_agg

type pure_strategy = {
  _id : string;
  env : Strategy_sig.env;
  build_setups : (string -> setup Date.Table.t) option;
  strategy : (module Strategy_sig.V2);
}

type run_result = {
  setups     : setup Date.Table.t;
  trades     : trade list;
  daily_pnl  : (Date.t * float) list;
  daily_pnl_usd : (Date.t * float) list;
  daily_pnl_pct : (Date.t * float) list;
}

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

let run_pure (strategy : pure_strategy) ~(filename : string) : run_result =
  let setups_tbl =
    match strategy.build_setups with
    | None -> Date.Table.create ()
    | Some f -> f filename
  in
  let module S = (val strategy.strategy : Strategy_sig.V2) in
  let env = strategy.env in

  let flush_trades acc trades = Pnl.add_trades acc trades in

  let module L = struct
    type t = {
      current_date : Date.t option;
      strat_state  : S.state option;
      book         : Order_book.t;
      last_session_bar : bar_1m option;
      acc          : Pnl.t;
    }
  end in

  let finalize_day (s : L.t) =
    match s.strat_state with
    | None ->
        { s with current_date = None; strat_state = None;
                 last_session_bar = None; }
    | Some st ->
        let st', cmds = S.finalize_day env st s.last_session_bar in
        let cfg = make_exec_config env in
        let order_cmds, flatten_cmds =
          List.partition_tf cmds ~f:(function
              | Strategy_sig.Submit_bracket _
              | Strategy_sig.Update_all _
              | Strategy_sig.Cancel_all -> true
              | Strategy_sig.Flatten_all _ -> false)
        in
        let book_after_cmds =
          match s.last_session_bar with
          | None -> s.book
          | Some lb -> Order_book.apply_cmds s.book ~ts:lb.ts order_cmds
        in
        let book_after_flat, flatten_trades =
          match s.last_session_bar with
          | None -> book_after_cmds, []
          | Some lb ->
              Execution_engine.apply_flatten_cmds ~config:cfg ~book:book_after_cmds
                ~bar:lb flatten_cmds
        in
        let book', trades =
          Execution_engine.on_session_end ~config:cfg
            ~book:book_after_flat ~last_bar:s.last_session_bar
        in
        let acc = flatten_trades @ trades |> flush_trades s.acc in
        { current_date = None;
          strat_state = Some st';
          book = book';
          last_session_bar = None;
          acc; }
  in

  let set_date (s : L.t) date =
    let setup_opt = Hashtbl.find setups_tbl date in
    let strat_state = Some (S.init setup_opt) in
    { s with current_date = Some date;
             strat_state;
             book = Order_book.empty ();
             last_session_bar = None; }
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
      if minute_of_day >= env.session_start_min
         && minute_of_day <= env.session_end_min
      then Some bar else s.last_session_bar
    in
    match s.strat_state with
    | None -> { s with last_session_bar }
    | Some st ->
        let st', cmds = S.step env st bar in
        let order_cmds, flatten_cmds =
          List.partition_tf cmds ~f:(function
              | Strategy_sig.Submit_bracket _
              | Strategy_sig.Update_all _
              | Strategy_sig.Cancel_all -> true
              | Strategy_sig.Flatten_all _ -> false)
        in
        let book_after_cmds = Order_book.apply_cmds s.book ~ts:bar.ts order_cmds in
        let cfg = make_exec_config env in
        let book_after_flat, flatten_trades =
          Execution_engine.apply_flatten_cmds ~config:cfg ~book:book_after_cmds
            ~bar flatten_cmds
        in
        let book', trades_step =
          Execution_engine.step ~config:cfg ~book:book_after_flat ~bar
        in
        let trades = flatten_trades @ trades_step in
        let acc = flush_trades s.acc trades in
        { s with
          strat_state = Some st';
          book = book';
          last_session_bar;
          acc; }
  in

  let init_state : L.t = {
    current_date = None;
    strat_state = None;
    book = Order_book.empty ();
    last_session_bar = None;
    acc = Pnl.empty;
  } in

  let state_ref = ref init_state in
  Csv_parser.iter_bars filename ~f:(fun bar ->
      state_ref := step !state_ref bar);

  let final_state = finalize_day !state_ref in
  let acc = final_state.acc in
  let trades = List.rev acc.trades in
  let daily_pnl, daily_pnl_usd, daily_pnl_pct =
    Pnl.to_alists_unsorted acc
  in
  { setups = setups_tbl;
    trades;
    daily_pnl;
    daily_pnl_usd;
    daily_pnl_pct; }
