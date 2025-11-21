open Core
open Types
module TT = Trade_transition

type engine_config = {
  session_start_min : int;
  session_end_min   : int;
  trade_start_min   : int;
  trade_end_min     : int;
  build_trade_plan  : setup -> trade_plan option;
  on_plan_bar       : trade_plan -> bar_1m -> unit;
}

module Policy_from_config (Cfg : sig
    val cfg : engine_config
  end) = struct
  type t = {
    plan : trade_plan option;
    trade_state : trade_state;
  }

  let make_trade ~(plan : trade_plan) ~(active : active_state)
      ~(exit_ts : timestamp) ~(exit_price : float) ~(reason : exit_reason) =
    let pnl_pts =
      match plan.direction with
      | Long  -> exit_price -. plan.entry_price
      | Short -> plan.entry_price -. exit_price
    in
    let pnl_R = pnl_pts /. plan.r_pts in
    let duration_min =
      Float.of_int (exit_ts.minute_of_day - active.entry_ts.minute_of_day)
    in
    {
      date         = exit_ts.date;
      direction    = plan.direction;
      entry_ts     = active.entry_ts;
      exit_ts;
      entry_price  = plan.entry_price;
      exit_price;
      qty          = 1.0;
      r_pts        = plan.r_pts;
      pnl_pts;
      pnl_R;
      pnl_usd      = 0.0;
      pnl_pct      = None;
      duration_min;
      exit_reason  = reason;
      meta         = [
        ("target_mult", Float.to_string plan.target_mult);
        ("abr_prev", Float.to_string plan.abr_prev);
        ("b1_range", Float.to_string plan.b1_range);
        ("b2_follow",
         (match plan.b2_follow with Follow_good -> "good" | Follow_poor -> "poor"));
      ];
    }

  let init_day setup_opt =
    match setup_opt with
    | None -> { plan = None; trade_state = No_trade }
    | Some s ->
        (match Cfg.cfg.build_trade_plan s with
         | None -> { plan = None; trade_state = No_trade }
         | Some plan -> { plan = Some plan; trade_state = Pending })

  let on_bar state (bar : bar_1m) : t * trade list =
    let minute_of_day = bar.ts.minute_of_day in
    match state.plan with
    | None -> (state, [])
    | Some plan ->
        if minute_of_day < Cfg.cfg.trade_start_min || minute_of_day > Cfg.cfg.trade_end_min then
          (state, [])
        else begin
          Cfg.cfg.on_plan_bar plan bar;
          let trade_state, trades =
            TT.step ~plan ~state:state.trade_state ~bar
              ~record_trade:(fun ~active ~exit_ts ~exit_price ~exit_reason ->
                  make_trade ~plan ~active ~exit_ts ~exit_price ~reason:exit_reason)
          in
          ({ plan = state.plan; trade_state }, trades)
        end

  let on_session_end state last_bar =
    match state.plan, state.trade_state, last_bar with
    | Some plan, Active active, Some lb when lb.ts.minute_of_day <= Cfg.cfg.trade_end_min ->
        let trade =
          make_trade ~plan ~active ~exit_ts:lb.ts ~exit_price:lb.close ~reason:Eod_flat
        in
        ({ plan = state.plan; trade_state = Done }, [ trade ])
    | _ -> (state, [])
end

let run ~(config : engine_config) filename setups_tbl
  : trade list * (Date.t * float) list =
  let module Pol = Policy_from_config(struct let cfg = config end) in
  let strategy = {
    Engine.id = "backtest";
    session_start_min = config.session_start_min;
    session_end_min   = config.session_end_min;
    build_setups = Some (fun _ -> setups_tbl);
    policy = (module Pol : Policy_sig.S);
  } in
  let r = Engine.run strategy ~filename in
  r.trades, r.daily_pnl
