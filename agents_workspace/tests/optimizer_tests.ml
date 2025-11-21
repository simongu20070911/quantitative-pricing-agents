open Core
open Strategy_fast
module OP = Engine.Optimizer

let simple_specs =
  [ Parameters.make ~name:"x" ~default:1.0 ~bounds:(0., 2.) () ]

let make_cand v =
  let pnl = v -. 1. in
  let date = Date.of_string "2020-01-01" in
  let ts = { Types.date; minute_of_day = 0 } in
  {
    OP.params = String.Map.singleton "x" v;
    score = pnl;
    trades = [ { Types.
                 date; direction = Types.Long; entry_ts = ts; exit_ts = ts;
                 entry_price = 0.; exit_price = 0.; qty = 1.; r_pts = 1.; pnl_pts = pnl; pnl_R = pnl;
                 pnl_usd = pnl; pnl_pct = None;
                 duration_min = 0.; exit_reason = Types.Target; meta = [] } ];
    daily_pnl = [ date, pnl ];
  }

let eval params =
  let v = Map.find_exn params "x" in
  make_cand v

let g_default =
  { Guardrails.default with min_trades = 1; pvalue_threshold = 1.0; bootstrap_ci_target = -1.0 }

let%test_unit "latin hypercube search works and guardrails still applied" =
  let job = {
    OP.strategy_id = "test";
    specs = simple_specs;
    search = OP.Latin_hypercube { samples = 4; seed = 3 };
    objective = OP.Mean_pnl;
    guardrails = g_default;
    perm_reps = Some 50;
    bootstrap_reps = Some 50;
    robustness_bumps = None;
    cache = false;
  } in
  let res = OP.run job ~evaluate:eval in
  assert (res.tested = 4);
  assert (res.rejected_guardrail = 0);
  assert (res.rejected_robustness = 0);
  ignore (Map.find_exn res.best.params "x")

let%test_unit "guardrail rejection counts when no trades" =
  let eval_empty _ =
    { OP.params = Parameters.default_map simple_specs;
      score = 0.;
      trades = [];
      daily_pnl = [] }
  in
  let job = {
    OP.strategy_id = "test";
    specs = simple_specs;
    search = OP.Random { samples = 3; seed = 1 };
    objective = OP.Mean_pnl;
    guardrails = Guardrails.default;
    perm_reps = Some 10;
    bootstrap_reps = Some 10;
    robustness_bumps = None;
    cache = false;
  } in
  let res = OP.run job ~evaluate:eval_empty in
  (* all sampled candidates rejected; fallback adds one tested eval *)
  assert (res.rejected_guardrail = res.tested - 1)

let%test_unit "bayes search gravitates to optimum" =
  let specs = [ Parameters.make ~name:"x" ~default:0.5 ~bounds:(0.0, 1.0) () ] in
  let eval params =
    let x = Map.find_exn params "x" in
    let score = -. ((x -. 0.8) ** 2.) in
    let date = Date.of_string "2020-01-01" in
    let ts = { Types.date; minute_of_day = 0 } in
    {
      OP.params = params;
      score;
      trades = [ { Types.date; direction = Types.Long; entry_ts = ts; exit_ts = ts;
                   entry_price = 0.; exit_price = 0.; qty = 1.; r_pts = 1.; pnl_pts = score; pnl_R = score;
                   pnl_usd = score; pnl_pct = None;
                   duration_min = 0.; exit_reason = Types.Target; meta = [] } ];
      daily_pnl = [ date, score ];
    }
  in
  let job = {
    OP.strategy_id = "test";
    specs;
    search = OP.Bayes { samples = 30; seed = 7; init = Some 5; gamma = Some 0.25 };
    objective = OP.Mean_pnl;
    guardrails = { g_default with min_trades = 1 };
    perm_reps = Some 10;
    bootstrap_reps = Some 10;
    robustness_bumps = None;
    cache = false;
  } in
  let res = OP.run job ~evaluate:eval in
  let best_x = Map.find_exn res.best.params "x" in
  assert (Float.(abs (best_x -. 0.8) < 0.2))

let%test_unit "cache avoids duplicate evals" =
  let specs = [ Parameters.make ~name:"x" ~default:1.0 () ] in
  let calls = ref 0 in
  let eval params =
    incr calls;
    let date = Date.of_string "2020-01-01" in
    let ts = { Types.date; minute_of_day = 0 } in
    {
      OP.params = params;
      score = 1.;
      trades = [ { Types.date; direction = Types.Long; entry_ts = ts; exit_ts = ts;
                   entry_price = 0.; exit_price = 0.; qty = 1.; r_pts = 1.; pnl_pts = 1.; pnl_R = 1.;
                   pnl_usd = 1.; pnl_pct = None;
                   duration_min = 0.; exit_reason = Types.Target; meta = [] } ];
      daily_pnl = [ date, 1. ];
    }
  in
  let job = {
    OP.strategy_id = "test";
    specs;
    search = OP.Random { samples = 5; seed = 1 };
    objective = OP.Mean_pnl;
    guardrails = { g_default with min_trades = 1 };
    perm_reps = Some 5;
    bootstrap_reps = Some 5;
    robustness_bumps = None;
    cache = true;
  } in
  let res = OP.run job ~evaluate:eval in
  assert (res.tested = 1);
  assert (res.cache_hits >= 4);
  assert (!calls = 1)

let%test_unit "robustness rejection counts" =
  let specs = [ Parameters.make ~name:"x" ~default:1.0 ~bounds:(0.5, 1.5) () ] in
  let eval params =
    let x = Map.find_exn params "x" in
    let score = if Float.(x = 1.0) then 1.0 else 0.0 in
    let date = Date.of_string "2020-01-01" in
    let ts = { Types.date; minute_of_day = 0 } in
    {
      OP.params = params;
      score;
      trades = [ { Types.date; direction = Types.Long; entry_ts = ts; exit_ts = ts;
                   entry_price = 0.; exit_price = 0.; qty = 1.; r_pts = 1.; pnl_pts = score; pnl_R = score;
                   pnl_usd = score; pnl_pct = None;
                   duration_min = 0.; exit_reason = Types.Target; meta = [] } ];
      daily_pnl = [ date, score ];
    }
  in
  let job = {
    OP.strategy_id = "test";
    specs;
    search = OP.Grid 1;
    objective = OP.Mean_pnl;
    guardrails = { g_default with min_trades = 1 };
    perm_reps = Some 5;
    bootstrap_reps = Some 5;
    robustness_bumps = Some [1.2]; (* bump will drop score to 0 => cliff *)
    cache = false;
  } in
  let res = OP.run job ~evaluate:eval in
  assert (res.rejected_robustness = 1)
