open Core

module Types = Strategy_fast.Types
module SS = Strategy_fast.Engine.Strategy_sig
module EP = Strategy_fast.Engine.Execution_params
module CM = Strategy_fast.Core.Cost_model
module ME = Strategy_fast.Engine.Multi_engine
module EV2 = Strategy_fast.Engine.Engine

let bar ~min ~open_ ~high ~low ~close ~volume : Types.bar_1m =
  let date = Date.of_string "2024-01-02" in
  { ts = { date; minute_of_day = min }; open_; high; low; close; volume }

module Make_strategy (Id : sig val id : string end) = struct
  type state = { submitted : bool }

  let init _ = { submitted = false }

  let step (env : SS.env) st (bar : Types.bar_1m) =
    if st.submitted then st, []
    else
      let tick = env.exec.tick_size in
      let stop = bar.low -. tick in
      let plan : Types.trade_plan =
        {
          direction = Types.Long;
          entry_price = bar.open_;
          cancel_level = stop;
          stop_init = stop;
          r_pts = bar.open_ -. stop;
          target_mult = 1.0;
          target_price = bar.high;
          be_trigger = bar.high;
          b2_end_minute = bar.ts.minute_of_day;
          downgrade_after_b2 = false;
          abr_prev = 1.0;
          b1_range = bar.high -. bar.low;
          b2_follow = Types.Follow_good;
        }
      in
      let meta = [ ("strategy", Id.id) ] in
      { submitted = true }, [ SS.Submit_bracket { plan; qty = env.qty; meta } ]

  let finalize_day _ st _ = st, []
end

let make_env ~(cost : CM.config) ~qty : SS.env =
  let exec = EP.default ~tick_size:cost.tick_size () in
  { SS.session_start_min = 0; session_end_min = 5; qty; cost; exec }

let%test_unit "multi_engine keeps per-instrument costs isolated" =
  let bars =
    [|
      bar ~min:0 ~open_:100. ~high:101. ~low:100.2 ~close:100.8 ~volume:1_000.;
    |]
  in
  let module Strat_a = Make_strategy (struct let id = "instrA" end) in
  let module Strat_b = Make_strategy (struct let id = "instrB" end) in

  let cost_a : CM.config =
    { tick_size = 0.5; tick_value = 5.0; fee_per_contract = 0.0; equity_base = None }
  in
  let cost_b : CM.config =
    { tick_size = 0.25; tick_value = 12.5; fee_per_contract = 0.0; equity_base = None }
  in

  let strat_a : EV2.pure_strategy =
    { _id = "instrA"; env = make_env ~cost:cost_a ~qty:1.0; build_setups = None; build_setups_stream = None; strategy = (module Strat_a) }
  in
  let strat_b : EV2.pure_strategy =
    { _id = "instrB"; env = make_env ~cost:cost_b ~qty:1.0; build_setups = None; build_setups_stream = None; strategy = (module Strat_b) }
  in

  let module Stream = struct
    let iter ~f = Array.iter bars ~f
  end in

  let results =
    ME.run_shared_with_stream ~stream:(module Stream)
      ~make_setups:(fun _ -> Date.Table.create ()) [ strat_a; strat_b ]
  in

  let by_id id = List.find_exn results ~f:(fun r -> String.equal r.strategy_id id) in
  let res_a = by_id "instrA" in
  let res_b = by_id "instrB" in

  assert (List.length res_a.trades = 1);
  assert (List.length res_b.trades = 1);

  let pnl_a = (List.hd_exn res_a.trades).pnl_usd in
  let pnl_b = (List.hd_exn res_b.trades).pnl_usd in

  assert (Float.(abs (pnl_a -. 10.0) < 1e-6));
  assert (Float.(abs (pnl_b -. 50.0) < 1e-6));
  (* ensure instruments stay independent: USD PnL scales with per-strategy tick values *)
  assert (Float.(pnl_b /. pnl_a >= 4.9 && pnl_b /. pnl_a <= 5.1))

let%test_unit "run_shared_with_stream calls make_setups per strategy and shares stream" =
  let bars =
    [|
      bar ~min:0 ~open_:50. ~high:51. ~low:49.5 ~close:50.5 ~volume:500.;
      bar ~min:1 ~open_:50.5 ~high:52.0 ~low:50.0 ~close:51.5 ~volume:600.;
    |]
  in
  let stream_iters = ref 0 in
  let module Stream = struct
    let iter ~f =
      Array.iter bars ~f:(fun b -> incr stream_iters; f b)
  end in

  let setups_called : int String.Table.t = String.Table.create () in
  let make_setups (strat : EV2.pure_strategy) =
    Hashtbl.update setups_called strat._id ~f:(function None -> 1 | Some n -> n + 1);
    Date.Table.create ()
  in

  let module Strat_a = Make_strategy (struct let id = "sa" end) in
  let module Strat_b = Make_strategy (struct let id = "sb" end) in

  let base_cost tick_size tick_value : CM.config =
    { tick_size; tick_value; fee_per_contract = 0.0; equity_base = None }
  in

  let strat_a : EV2.pure_strategy =
    { _id = "sa"; env = make_env ~cost:(base_cost 0.25 12.5) ~qty:1.0; build_setups = None; build_setups_stream = None; strategy = (module Strat_a) }
  in
  let strat_b : EV2.pure_strategy =
    { _id = "sb"; env = make_env ~cost:(base_cost 0.1 5.0) ~qty:2.0; build_setups = None; build_setups_stream = None; strategy = (module Strat_b) }
  in

  let results =
    ME.run_shared_with_stream ~stream:(module Stream) ~make_setups [ strat_a; strat_b ]
  in

  assert (!stream_iters = Array.length bars);
  assert (Int.equal (Hashtbl.find setups_called "sa" |> Option.value ~default:0) 1);
  assert (Int.equal (Hashtbl.find setups_called "sb" |> Option.value ~default:0) 1);
  let by_id id = List.find_exn results ~f:(fun r -> String.equal r.strategy_id id) in
  assert (List.length (by_id "sa").trades = 1);
  assert (List.length (by_id "sb").trades = 1)
