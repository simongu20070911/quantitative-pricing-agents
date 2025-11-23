open Core
open Types
module Pnl = Pnl_agg
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
      let { EV2.setups; trades; daily_pnl; daily_pnl_usd; daily_pnl_pct } =
        EV2.run_pure strat ~filename
      in
      { strategy_id = strat._id; setups; trades; daily_pnl; daily_pnl_usd; daily_pnl_pct })

let run_all_pure = run_all

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

type ctx =
  | Strat : {
      strat      : EV2.pure_strategy;
      strategy   : (module Strategy_sig.V2 with type state = 's);
      state      : 's option;
      setups     : setup Date.Table.t;
      book       : Order_book.t;
      current_date : Date.t option;
      last_session_bar : bar_1m option;
      acc        : Pnl.t;
    } -> ctx

let empty_acc = Pnl.empty
let flush_trades = Pnl.add_trades

let init_ctx (strat : EV2.pure_strategy) ~(setups : setup Date.Table.t) : ctx =
  let module S = (val strat.strategy : Strategy_sig.V2) in
  Strat {
    strat;
    strategy = (module S);
    state = None;
    setups;
    book = Order_book.empty ();
    current_date = None;
    last_session_bar = None;
    acc = empty_acc;
  }

let set_date (Strat ctx) date =
  let module S = (val ctx.strategy) in
  let setup_opt = Hashtbl.find ctx.setups date in
  Strat {
    ctx with
    current_date = Some date;
    state = Some (S.init setup_opt);
    book = Order_book.empty ();
    last_session_bar = None;
  }

let finalize_day (Strat ctx) : ctx =
  match ctx.state with
  | None -> Strat { ctx with current_date = None; last_session_bar = None }
  | Some st ->
      let module S = (val ctx.strategy) in
      let env = ctx.strat.env in
      let st', cmds = S.finalize_day env st ctx.last_session_bar in
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
        match ctx.last_session_bar with
        | None -> ctx.book
        | Some lb -> Order_book.apply_cmds ctx.book ~ts:lb.ts order_cmds
      in
      let book_after_flat, flatten_trades =
        match ctx.last_session_bar with
        | None -> book_after_cmds, []
        | Some lb ->
            Execution_engine.apply_flatten_cmds ~config:cfg ~book:book_after_cmds
              ~bar:lb flatten_cmds
      in
      let book', trades =
        Execution_engine.on_session_end ~config:cfg
          ~book:book_after_flat ~last_bar:ctx.last_session_bar
      in
      let acc = flatten_trades @ trades |> flush_trades ctx.acc in
      Strat {
        ctx with
        state = Some st';
        current_date = None;
        last_session_bar = None;
        book = book';
        acc;
      }

let step_ctx (Strat ctx0) (bar : bar_1m) : ctx =
  let { ts = { date; minute_of_day }; _ } = bar in
  let ctx =
    match ctx0.current_date with
    | None -> set_date (Strat ctx0) date
    | Some d when Date.equal d date -> Strat ctx0
    | Some _ -> set_date (finalize_day (Strat ctx0)) date
  in
  let Strat ctx = ctx in
  let module S = (val ctx.strategy) in
  let env = ctx.strat.env in
  let last_session_bar =
    if minute_of_day >= env.session_start_min
       && minute_of_day <= env.session_end_min
    then Some bar
    else ctx.last_session_bar
  in
  match ctx.state with
  | None -> Strat { ctx with last_session_bar }
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
      let cfg = make_exec_config env in
      let book_after_cmds = Order_book.apply_cmds ctx.book ~ts:bar.ts order_cmds in
      let book_after_flat, flatten_trades =
        Execution_engine.apply_flatten_cmds ~config:cfg ~book:book_after_cmds
          ~bar flatten_cmds
      in
      let book', trades_step =
        Execution_engine.step ~config:cfg ~book:book_after_flat ~bar
      in
      let trades = flatten_trades @ trades_step in
      let acc = flush_trades ctx.acc trades in
      Strat {
        ctx with
        state = Some st';
        last_session_bar;
        book = book';
        acc;
      }

let extract_result (Strat ctx) : run_result =
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

let run_shared_with_stream ~(stream : (module BAR_STREAM))
    ~(make_setups : EV2.pure_strategy -> setup Date.Table.t)
    (strategies : EV2.pure_strategy list)
  : run_result list =
  let module Stream = (val stream : BAR_STREAM) in
  let ctxs = List.map strategies ~f:(fun strat -> init_ctx strat ~setups:(make_setups strat)) in
  let ctxs_ref = ref ctxs in
  Stream.iter ~f:(fun bar ->
      ctxs_ref := List.map !ctxs_ref ~f:(fun ctx -> step_ctx ctx bar));
  ctxs_ref := List.map !ctxs_ref ~f:finalize_day;
  List.map !ctxs_ref ~f:extract_result

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
