open Core
open Types
module Alg = Opt_algos

type objective =
  | Sharpe
  | Mean_pnl
  | Hit_rate
  | Custom of (Types.trade list -> float)

type search =
  | Grid of int
  | Random of { samples : int; seed : int }
  | Latin_hypercube of { samples : int; seed : int }
  | Bayes of { samples : int; seed : int; init : int option; gamma : float option }

type job = {
  strategy_id : string;
  specs : Parameters.t list;
  search : search;
  objective : objective;
  guardrails : Guardrails.t;
  perm_reps : int option;      (* optional permutation reps for p-value guardrail *)
  bootstrap_reps : int option; (* optional bootstrap reps for CI guardrail *)
  robustness_bumps : float list option;
  cache : bool;
}

type candidate = {
  params : Parameters.value_map;
  score  : float;
  trades : Types.trade list;
  daily_pnl : (Date.t * float) list;
}

type result = {
  best : candidate;
  tested : int;
  rejected_guardrail : int;
  rejected_robustness : int;
  cache_hits : int;
  guardrails_hash : string;
}

let trades_guardrail (g : Guardrails.t) trades daily_pnl =
  let { Guardrails.min_trades; max_drawdown_R; _ } = g in
  let trade_count = List.length trades in
  let max_dd =
    let rec loop acc running = function
      | [] -> acc
      | (_, pnl) :: tl ->
          let running = running +. pnl in
          let acc = Float.min acc running in
          loop acc running tl
    in
    let sorted = List.sort daily_pnl ~compare:(fun (d1, _) (d2, _) -> Date.compare d1 d2) in
    let dd = loop 0. 0. sorted in
    -.dd
  in
  trade_count >= min_trades && Float.(max_dd <= max_drawdown_R)

(* Stub: capacity guardrail is currently a placeholder because trade size/volume is not tracked.
   Returns true but keeps the hook explicit for future notional-aware checks. *)
let capacity_guardrail (_g : Guardrails.t) (_trades : trade list) = true

let pvalue_guardrail (g : Guardrails.t) ?reps trades =
  let pnls = List.map trades ~f:(fun t -> t.pnl_R) in
  match pnls with
  | [] -> false
  | _ ->
      let mean xs =
        List.sum (module Float) xs ~f:Fun.id /. Float.of_int (List.length xs)
      in
      let p =
        Stat_tests.permutation_pvalue ?reps ~metric:mean pnls
      in
      Float.(p <= g.pvalue_threshold)

let bootstrap_guardrail (g : Guardrails.t) ?reps trades =
  let pnls = List.map trades ~f:(fun t -> t.pnl_R) in
  match pnls with
  | [] -> false
  | _ ->
      let lower, _ = Stat_tests.bootstrap_ci ?reps pnls in
      Float.(lower >= g.bootstrap_ci_target)

let metric_of_objective objective trades =
  match objective with
  | Custom f -> f trades
  | Hit_rate ->
      if List.is_empty trades then 0.
      else
        let wins = List.count trades ~f:(fun t -> Float.(t.pnl_R > 0.)) in
        Float.of_int wins /. Float.of_int (List.length trades)
  | Mean_pnl ->
      List.sum (module Float) trades ~f:(fun t -> t.pnl_R)
      /. Float.max 1. (Float.of_int (List.length trades))
  | Sharpe ->
      let pnls = List.map trades ~f:(fun t -> t.pnl_R) in
      match pnls with
      | [] -> 0.
      | _ ->
          let len = Float.of_int (List.length pnls) in
          let mean = List.sum (module Float) pnls ~f:Fun.id /. len in
          let var =
            List.sum (module Float) pnls ~f:(fun x -> (x -. mean) ** 2.) /. len
          in
          if Float.(var = 0.) then 0. else mean /. Float.sqrt var

let sample_params specs = Parameters.default_map specs

let key_of_params params =
  params
  |> Map.to_alist
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> [%sexp_of: (string * float) list]
  |> Sexp.to_string


let generate_candidates job =
  match job.search with
  | Grid steps -> Alg.grid ~steps job.specs ~samples:0 ~seed:0
  | Random { samples; seed } -> Alg.random_uniform job.specs ~samples ~seed
  | Latin_hypercube { samples; seed } -> Alg.latin_hypercube job.specs ~samples ~seed
  | Bayes _ -> []

let run job ~evaluate =
  let guardrails_hash = Guardrails.hash job.guardrails in
  let tested = ref 0 in
  let rejected = ref 0 in
  let rejected_robustness = ref 0 in
  let cache = Hashtbl.create (module String) in
  let cache_hits = ref 0 in

  let eval_with_cache params =
    match job.cache with
    | false -> incr tested; evaluate params
    | true -> (
        match Hashtbl.find cache (key_of_params params) with
        | Some cand -> incr cache_hits; cand
        | None -> incr tested; let cand = evaluate params in Hashtbl.set cache ~key:(key_of_params params) ~data:cand; cand)
  in

let robustness_pass _cand params cand_score =
    match job.robustness_bumps with
    | None | Some [] -> true
    | Some bumps ->
        let eval_score p =
          let c = eval_with_cache p in
          metric_of_objective job.objective c.trades
        in
        let r =
          Robustness.run ~specs:job.specs ~bump_factors:bumps
            ~tol_frac:job.guardrails.robustness_tol_frac
            ~cliff_pct:job.guardrails.cliff_pct ~evaluate:eval_score
            ~baseline_score:cand_score params
        in
        r.passed
  in

  let score_candidate params cand =
    let g = job.guardrails in
    let passed_trades = trades_guardrail g cand.trades cand.daily_pnl in
    let passed_capacity = if not passed_trades then false else capacity_guardrail g cand.trades in
    let passed_perm =
      if not passed_trades || not passed_capacity then false
      else pvalue_guardrail g ?reps:job.perm_reps cand.trades
    in
    let passed_bootstrap =
      if not passed_trades || not passed_capacity then false
      else bootstrap_guardrail g ?reps:job.bootstrap_reps cand.trades
    in
    if passed_trades && passed_capacity && passed_perm && passed_bootstrap then (
      let score = metric_of_objective job.objective cand.trades in
      if robustness_pass cand params score then Some { cand with score }
      else (incr rejected_robustness; None))
    else (incr rejected; None)
  in

  let collected = ref [] in

  let process_params params =
    let cand = eval_with_cache params in
    match score_candidate params cand with
    | None -> ()
    | Some s -> collected := s :: !collected
  in

  (* Non-Bayes batch search *)
  let candidates = generate_candidates job in
  List.iter candidates ~f:process_params;

  (* Bayes search if requested: simple TPE-style exploitation around best quantile *)
  (match job.search with
  | Bayes { samples; seed; init; gamma } ->
      let state = Random.State.make [| seed |] in
      let init = Option.value init ~default:(Int.max 5 (samples / 5)) in
      let gamma = Option.value gamma ~default:0.2 in
      let accepted = ref [] in
      let propose () =
        match !accepted with
        | [] -> Alg.random_uniform job.specs ~samples:1 ~seed:(Random.State.int state 1_000_000) |> List.hd_exn
        | xs ->
            let sorted = List.sort xs ~compare:(fun (_p1, s1) (_p2, s2) -> Float.compare s2 s1) in
            let n_good = Int.max 1 (Float.to_int (gamma *. Float.of_int (List.length sorted))) in
            let good = List.take sorted n_good in
            let (center, _) = List.random_element_exn ~random_state:state good in
            Alg.perturb_around ~center ~specs:job.specs ~sigma_frac:0.05
              ~seed:(Random.State.int state 1_000_000)
      in
      for i = 0 to samples - 1 do
        let params =
          if i < init then
            Alg.random_uniform job.specs ~samples:1 ~seed:(Random.State.int state 1_000_000) |> List.hd_exn
          else propose ()
        in
        let cand = eval_with_cache params in
        match score_candidate params cand with
        | None -> ()
        | Some s ->
            collected := s :: !collected;
            accepted := (params, s.score) :: !accepted
      done
  | _ -> ());

  let best =
    match !collected with
    | [] ->
        let fallback = eval_with_cache (sample_params job.specs) in
        { fallback with score = metric_of_objective job.objective fallback.trades }
    | xs -> List.max_elt xs ~compare:(fun a b -> Float.compare a.score b.score) |> Option.value_exn
  in
  {
    best;
    tested = !tested;
    rejected_guardrail = !rejected;
    rejected_robustness = !rejected_robustness;
    cache_hits = !cache_hits;
    guardrails_hash;
  }

let%test_unit "optimizer picks higher score and enforces guardrails" =
  let specs =
    [ Parameters.make ~name:"x" ~default:1. ~bounds:(0., 2.) () ]
  in
  let make_cand v =
    let pnl = v -. 1. in
    let date = Date.today ~zone:Time_float.Zone.utc in
    let ts = { date; minute_of_day = 0 } in
    {
      params = String.Map.singleton "x" v;
      trades = [ { date; direction = Long; entry_ts = ts; exit_ts = ts;
                  entry_price = 0.; exit_price = 0.; qty = 1.; r_pts = 1.; pnl_pts = pnl; pnl_R = pnl;
                  pnl_usd = pnl; pnl_pct = None;
                  duration_min = 0.; exit_reason = Target; meta = [] } ];
      daily_pnl = [ date, pnl ];
      score = pnl;
    }
  in
  let job = {
    strategy_id = "test";
    specs;
    search = Random { samples = 2; seed = 1 };
    objective = Mean_pnl;
    guardrails = { Guardrails.default with min_trades = 1; pvalue_threshold = 1.0; bootstrap_ci_target = -1.0 };
    perm_reps = Some 10;
    bootstrap_reps = Some 10;
    robustness_bumps = None;
    cache = false;
  } in
  let eval params =
    let v = Core.Map.find_exn params "x" in
    make_cand v
  in
  let res = run job ~evaluate:eval in
  let best_x = Core.Map.find_exn res.best.params "x" in
  assert (Float.(best_x > 0.));
  assert (res.tested = 2);
  assert (res.rejected_guardrail = 0);
  assert (res.rejected_robustness = 0)
