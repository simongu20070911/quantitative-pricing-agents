open Core
open Types
open Time_utils
module SC = Strategy_common
module OB = Order_book
module EE = Execution_engine
module P = B1b2_params

module Setup_builder = Setup_builder_b1b2
module SS = Strategy_sig

type config = {
  session_start_min : int;
  session_end_min   : int;
  qty : float;
  cost : Cost_model.config;
  exec : Execution_params.t;
  params : P.t;
}

let default_params = P.defaults

let default_env =
  { SC.Tunables.session_start_min = rth_start_min;
    session_end_min = rth_end_min;
    qty = 1.0;
  cost = {
    tick_size = default_params.exec.tick_size;
    tick_value = default_params.exec.tick_value;
    fee_per_contract = 4.0;
    equity_base = None;
  }; }

let default_config = {
  session_start_min = default_env.session_start_min;
  session_end_min = default_env.session_end_min;
  qty = default_env.qty;
  cost = default_env.cost;
  exec = Execution_params.default ~tick_size:default_env.cost.tick_size ();
  params = default_params;
}

let strategy_id = "b1b2"

let param_table =
  List.map P.param_table ~f:(fun (f : P.field) ->
      { SC.Tunables.name = f.name;
        default = f.default;
        bounds = f.bounds;
        integer = f.integer;
        tunable = f.tunable;
        description = f.description;
        set = f.set; })

let parameter_specs, config_parts_of_params =
  SC.Tunables.make
    ~defaults_env:default_env
    ~defaults_params:default_params
    ~param_table

let config_of_params (m : Parameters.value_map) : config =
  let env, params = config_parts_of_params m in
  let cost =
    { env.cost with
      tick_size = params.exec.tick_size;
      tick_value = params.exec.tick_value; }
  in
  { session_start_min = env.session_start_min;
    session_end_min = env.session_end_min;
    qty = env.qty;
    cost;
    exec = Execution_params.default ~tick_size:cost.tick_size ();
    params; }

let session_window cfg = cfg.session_start_min, cfg.session_end_min
let qty cfg = cfg.qty
let cost cfg = cfg.cost
let exec cfg = cfg.exec
let params cfg = cfg.params
let with_cost ~cost cfg = { cfg with cost }
let with_qty ~qty cfg = { cfg with qty }
let with_params ~params cfg = { cfg with params }
let with_session ~start ~end_ cfg = { cfg with session_start_min = start; session_end_min = end_ }
let with_exec ~exec cfg = { cfg with exec }

let downgrade_if_needed plan ~minute_of_day =
  if plan.downgrade_after_b2 && Float.(plan.target_mult = 2.0) && minute_of_day > plan.b2_end_minute then
    let target_price =
      match plan.direction with
      | Long  -> plan.entry_price +. plan.r_pts
      | Short -> plan.entry_price -. plan.r_pts
    in
    { plan with target_mult = 1.0; target_price }
  else plan

(** Intent-only strategy: emits bracket orders and plan updates. *)
module Intent (Cfg : sig val cfg : config end) : SS.V2 = struct
  type state = {
    plan : trade_plan option;
    submitted : bool;
  }

  let init setup_opt =
    match setup_opt with
    | None -> { plan = None; submitted = false }
    | Some s ->
        (match Trade_logic.build_trade_plan ~params:Cfg.cfg.params.exec s with
         | None -> { plan = None; submitted = false }
         | Some plan -> { plan = Some plan; submitted = false })

  let step (env : SS.env) state (bar : bar_1m) =
    let minute = bar.ts.minute_of_day in
    let in_session =
      minute >= env.session_start_min
      && minute <= env.session_end_min
    in
    match state.plan with
    | None -> state, []
    | Some plan ->
        let cmds = ref [] in
        (* Downgrade target after B2 cutoff if applicable. *)
        if plan.downgrade_after_b2 && Float.(plan.target_mult = 2.0)
           && minute > plan.b2_end_minute
        then
          cmds :=
            SS.Update_all
              (fun p -> downgrade_if_needed p ~minute_of_day:minute)
            :: !cmds;

        (* Submit bracket once B2 window opens and we are in session. *)
        let state' =
          if (not state.submitted)
             && in_session
             && minute >= b2_min
          then
            let meta =
              SC.Trade_common.with_strategy strategy_id [
                ("target_mult", Float.to_string plan.target_mult);
                ("abr_prev", Float.to_string plan.abr_prev);
                ("b1_range", Float.to_string plan.b1_range);
                ("b2_follow",
                 match plan.b2_follow with
                 | Follow_good -> "good"
                 | Follow_poor -> "poor");
              ]
            in
            cmds :=
              SS.Submit_bracket { plan; qty = env.qty; meta } :: !cmds;
            { state with submitted = true }
          else
            state
        in
        state', List.rev !cmds

  let finalize_day _env state _last_bar =
    state, []
end

let make_pure_strategy cfg =
  let env =
    SC.Env.of_config
      ~session_start_min:cfg.session_start_min
      ~session_end_min:cfg.session_end_min
      ~qty:cfg.qty
      ~cost:cfg.cost
      ~exec:cfg.exec
      ()
  in
  let module S = Intent(struct let cfg = cfg end) in
  SC.Strategy_builder.make_pure ~id:strategy_id ~env
    ~build_setups:(SC.Setups.with_params ~params:cfg.params.setup Setup_builder.build)
    ~build_setups_stream:(fun () ->
      let streamer = Setup_builder.Streaming.create cfg.params.setup in
      { Engine_types.on_bar = (fun bar -> Setup_builder.Streaming.on_bar streamer bar);
        finalize = (fun () -> Setup_builder.Streaming.finalize streamer) })
    (module S)

let strategy_pure = make_pure_strategy default_config

let pure_strategy = make_pure_strategy
