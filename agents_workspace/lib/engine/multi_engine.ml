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

(* Multi-pass helper *)
let run_all (strategies : Engine.strategy list) ~(filename : string) : run_result list =
  List.map strategies ~f:(fun strat ->
      let { Engine.setups; trades; daily_pnl; daily_pnl_usd; daily_pnl_pct } = Engine.run strat ~filename in
      { strategy_id = strat.id; setups; trades; daily_pnl; daily_pnl_usd; daily_pnl_pct })

(* Shared-bar runner using a GADT to keep policy state typed. *)
type strat_ctx =
  | StratCtx : {
      strat      : Engine.strategy;
      setups     : setup Date.Table.t;
      policy     : (module Policy_sig.S with type t = 's);
      state      : 's option ref;
      current_date : Date.t option ref;
      trades_acc : trade list ref;
      daily_pnl_tbl : float Date.Table.t;
      daily_pnl_usd_tbl : float Date.Table.t;
      daily_pnl_pct_tbl : float Date.Table.t;
    } -> strat_ctx

let init_ctx (strat : Engine.strategy) filename =
  let setups =
    match strat.build_setups with
    | None -> Date.Table.create ()
    | Some f -> f filename
  in
  let module P = (val strat.policy : Policy_sig.S) in
  StratCtx {
    strat;
    setups;
    policy = (module P);
    state = ref None;
    current_date = ref None;
    trades_acc = ref [];
    daily_pnl_tbl = Date.Table.create ();
    daily_pnl_usd_tbl = Date.Table.create ();
    daily_pnl_pct_tbl = Date.Table.create ();
  }

let flush_trades (StratCtx ctx) trades =
  List.iter trades ~f:(fun t ->
      ctx.trades_acc := t :: !(ctx.trades_acc);
      Hashtbl.update ctx.daily_pnl_tbl t.date ~f:(function
          | None -> t.pnl_R
          | Some x -> x +. t.pnl_R);
       Hashtbl.update ctx.daily_pnl_usd_tbl t.date ~f:(function
           | None -> t.pnl_usd
           | Some x -> x +. t.pnl_usd);
       (match t.pnl_pct with
        | None -> ()
        | Some pct ->
            Hashtbl.update ctx.daily_pnl_pct_tbl t.date ~f:(function
                | None -> pct
                | Some x -> x +. pct)))

let finalize_day (StratCtx ctx) last_bar_opt =
  let module P = (val ctx.policy) in
  match !(ctx.state) with
  | None -> ()
  | Some st ->
      let st', trades = P.on_session_end st last_bar_opt in
      ctx.state := Some st';
      flush_trades (StratCtx ctx) trades;
      ctx.state := None

let run_shared (strategies : Engine.strategy list) ~(filename : string)
  : run_result list =
  let ctxs = List.map strategies ~f:(fun s -> init_ctx s filename) in

  Csv_parser.iter_bars filename ~f:(fun bar ->
      let date = bar.ts.date in
      let minute_of_day = bar.ts.minute_of_day in

      (* Init or roll day per strategy *)
      List.iter ctxs ~f:(fun (StratCtx ctx) ->
          let module P = (val ctx.policy) in
          match !(ctx.current_date) with
          | None ->
              ctx.current_date := Some date;
              let setup_opt = Hashtbl.find ctx.setups date in
              ctx.state := Some (P.init_day setup_opt)
          | Some d when Date.equal d date -> ()
          | Some _ ->
              finalize_day (StratCtx ctx) None;
              ctx.current_date := Some date;
              let setup_opt = Hashtbl.find ctx.setups date in
              ctx.state := Some (P.init_day setup_opt));

      (* Feed bar during session window for each strategy *)
      List.iter ctxs ~f:(fun (StratCtx ctx) ->
          match !(ctx.state) with
          | None -> ()
          | Some st ->
              if minute_of_day < ctx.strat.session_start_min
                 || minute_of_day > ctx.strat.session_end_min
              then ()
              else
                let module P = (val ctx.policy) in
                let st', trades = P.on_bar st bar in
                ctx.state := Some st';
                flush_trades (StratCtx ctx) trades
        )
  );

  (* Finalize day for all strategies (no last bar tracking in shared runner). *)
  List.iter ctxs ~f:(fun ctx -> finalize_day ctx None);

  List.map ctxs ~f:(fun (StratCtx ctx) ->
      let trades = List.rev !(ctx.trades_acc) in
      let daily_pnl =
        Hashtbl.to_alist ctx.daily_pnl_tbl
        |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
      in
      let daily_pnl_usd =
        Hashtbl.to_alist ctx.daily_pnl_usd_tbl
        |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
      in
      let daily_pnl_pct =
        Hashtbl.to_alist ctx.daily_pnl_pct_tbl
        |> List.sort ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2)
      in
      { strategy_id = ctx.strat.id;
        setups = ctx.setups;
        trades;
        daily_pnl;
        daily_pnl_usd;
        daily_pnl_pct })
