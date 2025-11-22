open Core
open Strategy_fast
open Types
module E = Strategy_fast.Engine.Engine

let find_fixture () =
  let target = Filename.concat "agents_workspace" "sample_es.csv" in
  let rec ascend n dir =
    if n < 0 then None
    else
      let candidate = Filename.concat dir target in
      if Stdlib.Sys.file_exists candidate then Some candidate
      else ascend (n - 1) (Filename.dirname dir)
  in
  match ascend 6 (Stdlib.Sys.getcwd ()) with
  | Some path -> path
  | None ->
      failwithf "Fixture missing: %s (searched upwards from %s)"
        target (Stdlib.Sys.getcwd ()) ()

let summary (r : Engine.Multi_engine.run_result) =
  let n = List.length r.trades in
  let total_R = List.fold r.trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_R) in
  let total_usd = List.fold r.trades ~init:0.0 ~f:(fun acc t -> acc +. t.pnl_usd) in
  (n, total_R, total_usd)

let%test_unit "run_shared matches run_all for b1b2 on sample_es" =
  let file = find_fixture () in
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
  let file = find_fixture () in
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
module Dummy_policy : Strategy_fast.Engine.Policy_sig.S = struct
  type t = { count : int }

  let init_day _setup_opt = { count = 0 }

  let on_bar st (bar : Types.bar_1m) =
    let _ = bar in
    let count = st.count + 1 in
    ({ count }, [])

  let on_session_end st last_bar =
    match last_bar with
    | None -> st, []
    | Some (lb : Types.bar_1m) ->
        let trade : Types.trade =
          {
            date = lb.ts.date;
            direction = Types.Long;
            entry_ts = lb.ts;
            exit_ts = lb.ts;
            entry_price = lb.close;
            exit_price = lb.close;
            qty = 1.0;
            r_pts = 1.0;
            pnl_pts = 0.0;
            pnl_R = Float.of_int st.count; (* encodes bar count, proves last_bar passed *)
            pnl_usd = 0.0;
            pnl_pct = None;
            duration_min = 0.0;
            exit_reason = Types.Eod_flat;
            meta = [];
          }
        in
        (st, [ trade ])
end

let make_dummy_strategy () : E.strategy =
  { id = "dummy";
    session_start_min = 0;
    session_end_min = 1440;
    build_setups = None;
    policy = (module Dummy_policy);
  }

let make_bars n : Types.bar_1m list =
  let date = Date.of_string "2020-01-02" in
  List.init n ~f:(fun i ->
      let ts = { date; minute_of_day = i } in
      { ts; open_ = 0.; high = 0.; low = 0.; close = Float.of_int i; volume = 0. })

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
      assert (String.equal a.strategy_id "dummy");
      assert (List.length a.trades = 1);
      assert (List.length b.trades = 1);
      let tol = 1e-9 in
      let ra = (List.hd_exn a.trades).pnl_R in
      let rb = (List.hd_exn b.trades).pnl_R in
      assert (Float.(abs (ra -. 7.) < tol));
      assert (Float.(abs (rb -. 7.) < tol));
      assert (Float.(abs (ra -. rb) < tol));
      assert (List.equal Date.equal (List.map ~f:fst a.daily_pnl) (List.map ~f:fst b.daily_pnl))
  | _ -> assert false
