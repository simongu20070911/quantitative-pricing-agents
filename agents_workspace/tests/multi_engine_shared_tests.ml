open Core
open Strategy_fast
open Types
module E = Strategy_fast.Engine.Engine
module T = Test_utils

let summary (r : Engine.Multi_engine.run_result) =
  let n = List.length r.trades in
  let total_R = List.fold r.trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_R) in
  let total_usd = List.fold r.trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_usd) in
  (n, total_R, total_usd)

let%test_unit "run_shared matches run_all for b1b2 on sample_es" =
  let file = T.find_fixture () in
  let strategies = [ Strategies.Strategy_b1b2.strategy_pure ] in
  let all = Engine.Multi_engine.run_all_pure strategies ~filename:file in
  let shared = Engine.Multi_engine.run_shared_pure strategies ~filename:file in
  match all, shared with
  | [a], [b] ->
      let (na, ra, ua) = summary a in
      let (nb, rb, ub) = summary b in
      let tol = 1e-6 in
      assert (Int.equal na nb);
      assert (Float.(abs (ra -. rb) < tol));
      assert (Float.(abs (ua -. ub) < tol))
  | _ -> assert false

let%test_unit "run_shared matches run_all for b1b2 + vwap on sample_es" =
  let file = T.find_fixture () in
  let strategies = [
    Strategies.Strategy_b1b2.strategy_pure;
    Strategies.Vwap_revert_strategy.strategy_pure;
  ] in
  let sort_by_id =
    List.sort ~compare:(fun (a : Engine.Multi_engine.run_result) b ->
        String.compare a.strategy_id b.strategy_id)
  in
  let all = Engine.Multi_engine.run_all_pure strategies ~filename:file |> sort_by_id in
  let shared = Engine.Multi_engine.run_shared_pure strategies ~filename:file |> sort_by_id in
  List.iter2_exn all shared ~f:(fun a b ->
      assert (String.(a.strategy_id = b.strategy_id));
      let (na, ra, ua) = summary a in
      let (nb, rb, ub) = summary b in
      let tol = 1e-6 in
      assert (Int.equal na nb);
      assert (Float.(abs (ra -. rb) < tol));
      assert (Float.(abs (ua -. ub) < tol)))

(* Determinism on synthetic in-memory stream exercising run_shared_with_stream. *)
module Dummy_intent : Strategy_fast.Engine.Strategy_sig.V2 = struct
  type state = bool (* entered? *)

  let init _setup_opt = false

  let step (env : Strategy_fast.Engine.Strategy_sig.env) entered (bar : Types.bar_1m) =
    if entered then entered, []
    else
      let plan =
        let stop = bar.close -. env.cost.tick_size in
        let target = bar.close +. env.cost.tick_size in
        {
          direction = Types.Long;
          entry_price = bar.close;
          cancel_level = bar.close -. (2. *. env.cost.tick_size);
          stop_init = stop;
          r_pts = env.cost.tick_size;
          target_mult = 1.0;
          target_price = target;
          be_trigger = target;
          b2_end_minute = env.session_end_min;
          downgrade_after_b2 = false;
          abr_prev = 0.0;
          b1_range = env.cost.tick_size;
          b2_follow = Types.Follow_good;
        }
      in
      let cmds = [ Strategy_fast.Engine.Strategy_sig.Submit_bracket { plan; qty = env.qty; meta = [ ("strategy", "dummy_v2") ] } ] in
      true, cmds

  let finalize_day _env entered _last_bar = entered, []
end

let make_dummy_strategy () : E.pure_strategy =
  let env = {
    Strategy_fast.Engine.Strategy_sig.session_start_min = 0;
    session_end_min = 1440;
    qty = 1.0;
    cost = {
      tick_size = 0.25;
      tick_value = 12.5;
      slippage_roundtrip_ticks = 0.0;
      fee_per_contract = 0.0;
      equity_base = None;
    };
    exec = Execution_params.legacy ~tick_size:0.25;
  } in
  { E._id = "dummy_v2";
    env;
    build_setups = Some (fun _ -> Date.Table.create ());
    strategy = (module Dummy_intent);
  }

let make_bars n : Types.bar_1m list =
  let date = Date.of_string "2020-01-02" in
  List.init n ~f:(fun i ->
      let ts = { date; minute_of_day = i } in
      { ts; open_ = 0.; high = Float.of_int (i + 1); low = Float.of_int i; close = Float.of_int i; volume = 0. })

module List_stream (B : sig val bars : Types.bar_1m list end) : Engine.Multi_engine.BAR_STREAM = struct
  let iter ~f = List.iter B.bars ~f
end

let%test_unit "run_shared_with_stream is deterministic on synthetic bars" =
  let bars = make_bars 7 in
  let open Engine.Multi_engine in
  let strat = make_dummy_strategy () in
  let run_once () =
    let module S = List_stream(struct let bars = bars end) in
    let make_setups _ = Date.Table.create () in
    run_shared_with_stream ~stream:(module S) ~make_setups [ strat ]
  in
  let r1 = run_once () in
  let r2 = run_once () in
  match r1, r2 with
  | [a], [b] ->
      assert (String.equal a.strategy_id "dummy_v2");
      let tol = 1e-6 in
      assert (Int.equal (List.length a.trades) (List.length b.trades));
      let pa = List.fold a.trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_R) in
      let pb = List.fold b.trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_R) in
      assert (Float.(abs (pa -. pb) < tol))
  | _ -> assert false
