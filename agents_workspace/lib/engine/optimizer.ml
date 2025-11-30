open Core
open Types
module Alg = Opt_algos

type objective =
  | Sharpe
  | Mean_pnl
  | Hit_rate
  | Year_sharpe of { lambda : float; min_days : int }
  | Simon_ratio of { lambda : float }
  | Mean_trade_lcb of { confidence : float } (* one-sided lower confidence bound on mean trade pnl_R *)
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

(* --- Stats helpers --- *)

let norm_ppf p =
  (* Acklam's approximation for inverse normal CDF; max error ~4.5e-4 *)
  let a = [| -3.969683028665376e+01; 2.209460984245205e+02; -2.759285104469687e+02;
             1.383577518672690e+02; -3.066479806614716e+01; 2.506628277459239e+00 |] in
  let b = [| -5.447609879822406e+01; 1.615858368580409e+02; -1.556989798598866e+02;
             6.680131188771972e+01; -1.328068155288572e+01 |] in
  let c = [| -7.784894002430293e-03; -3.223964580411365e-01; -2.400758277161838e+00;
             -2.549732539343734e+00; 4.374664141464968e+00; 2.938163982698783e+00 |] in
  let d = [| 7.784695709041462e-03; 3.224671290700398e-01; 2.445134137142996e+00;
             3.754408661907416e+00 |] in
  let plow = 0.02425 in
  let phigh = 1. -. plow in
  if Float.(p <= 0.) || Float.(p >= 1.) then invalid_arg "norm_ppf expects 0<p<1";
  if Float.(p < plow) then
    let q = Float.sqrt (-2. *. Float.log p) in
    (((((c.(0)*.q +. c.(1))*.q +. c.(2))*.q +. c.(3))*.q +. c.(4))*.q +. c.(5))
    /. ((((d.(0)*.q +. d.(1))*.q +. d.(2))*.q +. d.(3))*.q +. 1.)
    |> fun x -> -.x
  else if Float.(p > phigh) then
    let q = Float.sqrt (-2. *. Float.log (1. -. p)) in
    (((((c.(0)*.q +. c.(1))*.q +. c.(2))*.q +. c.(3))*.q +. c.(4))*.q +. c.(5))
    /. ((((d.(0)*.q +. d.(1))*.q +. d.(2))*.q +. d.(3))*.q +. 1.)
  else
    let q = p -. 0.5 in
    let r = q *. q in
    (((((a.(0)*.r +. a.(1))*.r +. a.(2))*.r +. a.(3))*.r +. a.(4))*.r +. a.(5))*.q
    /. (((((b.(0)*.r +. b.(1))*.r +. b.(2))*.r +. b.(3))*.r +. b.(4))*.r +. 1.)

let t_critical ~p ~df =
  (* one-sided critical value using Cornish-Fisher expansion around normal *)
  let df = Float.max 1. df in
  let z = norm_ppf p in
  let v = df in
  let z2 = z *. z in
  let z3 = z2 *. z in
  let z5 = z3 *. z2 in
  let z7 = z5 *. z2 in
  let c1 = (z3 +. z) /. (4. *. v) in
  let c2 = (5. *. z5 +. 16. *. z3 +. 3. *. z) /. (96. *. v *. v) in
  let c3 = (3. *. z7 +. 19. *. z5 +. 17. *. z3 -. 15. *. z) /. (384. *. v ** 3.) in
  z +. c1 +. c2 +. c3

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
  match reps with
  | None -> true
  | Some r when r <= 0 -> true
  | Some _ ->
      let pnls = List.map trades ~f:(fun t -> t.pnl_R) in
      let mean xs =
        List.sum (module Float) xs ~f:Fun.id /. Float.of_int (List.length xs)
      in
      let p = Stat_tests.permutation_pvalue ?reps ~metric:mean pnls in
      Float.(p <= g.pvalue_threshold)

let bootstrap_guardrail (g : Guardrails.t) ?reps trades =
  match reps with
  | None -> true
  | Some r when r <= 0 -> true
  | Some _ ->
      let pnls = List.map trades ~f:(fun t -> t.pnl_R) in
      let lower, _ = Stat_tests.bootstrap_ci ?reps pnls in
      Float.(lower >= g.bootstrap_ci_target)

let year_sharpe_score ~lambda ~min_days ~trades_count (daily : (Date.t * float) list) =
  (* Soft coverage weighting + shrinkage to avoid brittle zero-score. *)
  let target_years = 3. in
  let target_days = 200. in
  let shrink_n = min_days in
  let per_year = Int.Table.create () in
  List.iter daily ~f:(fun (d, pnl) ->
      Hashtbl.add_multi per_year ~key:(Date.year d) ~data:pnl);
  let total_days = Float.of_int (List.length daily) in
  let sharpes, valid_years =
    Hashtbl.fold per_year ~init:([], 0) ~f:(fun ~key:_ ~data (acc, vy) ->
        let n = List.length data in
        let len = Float.of_int (n + shrink_n) in
        let sum = List.sum (module Float) data ~f:Fun.id in
        let mean = sum /. len in
        let sumsq =
          List.sum (module Float) data ~f:(fun x -> (x -. mean) ** 2.)
          +. Float.of_int shrink_n *. (mean ** 2.)
        in
        let var = sumsq /. len in
        let sharpe = if Float.(var = 0.) then 0. else mean /. Float.sqrt var in
        sharpe :: acc, (if n > 0 then vy + 1 else vy))
  in
  let coverage =
    let year_cov = Float.min 1. (Float.of_int valid_years /. target_years) in
    let day_cov = Float.min 1. (total_days /. target_days) in
    year_cov *. day_cov
  in
  if trades_count = 0 then -5.
  else
    match sharpes with
    | [] -> -5.
    | xs ->
        let len = Float.of_int (List.length xs) in
        let mean = List.sum (module Float) xs ~f:Fun.id /. len in
        let var = List.sum (module Float) xs ~f:(fun x -> (x -. mean) ** 2.) /. len in
        let std = Float.sqrt var in
        let raw = mean -. (lambda *. std) in
        if Float.(coverage = 0.) then -5. else coverage *. raw

let simon_ratio_score ~lambda (daily : (Date.t * float) list) =
  match daily with
  | [] -> -5.
  | _ ->
      let rec loop equity logs peaks mdd = function
        | [] -> logs, mdd
        | (_, pnl) :: tl ->
            let r = pnl /. equity in
            let eq' = equity *. (1. +. r) in
            if Float.(eq' <= 1e-9) then logs @ [Float.log 1e-9], 1.0
            else
              let logs' = (Float.log (1. +. r)) :: logs in
              let peak' = Float.max (List.hd peaks |> Option.value ~default:eq') eq' in
              let dd = (peak' -. eq') /. peak' in
              let mdd' = Float.max mdd dd in
              loop eq' logs' (peak' :: peaks) mdd' tl
      in
      let logs, mdd = loop 1. [] [] 0. daily in
      let t = Float.of_int (List.length logs) in
      if Float.(t = 0.) then -5.
      else
        let g = List.sum (module Float) logs ~f:Fun.id /. t in
        g -. (lambda *. mdd)

let metric_of_objective objective ~trades ~daily_pnl =
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
      (match pnls with
      | [] -> 0.
      | _ ->
          let len = Float.of_int (List.length pnls) in
          let mean = List.sum (module Float) pnls ~f:Fun.id /. len in
          let var =
            List.sum (module Float) pnls ~f:(fun x -> (x -. mean) ** 2.) /. len
          in
          if Float.(var = 0.) then 0. else mean /. Float.sqrt var)
  | Year_sharpe { lambda; min_days } ->
      year_sharpe_score ~lambda ~min_days ~trades_count:(List.length trades) daily_pnl
  | Simon_ratio { lambda } ->
      simon_ratio_score ~lambda daily_pnl
  | Mean_trade_lcb { confidence } ->
      let pnls = List.map trades ~f:(fun t -> t.pnl_R) |> Array.of_list in
      let n = Array.length pnls in
      if n < 2 then 0.
      else
        let mean = Owl_base_stats.mean pnls in
        let std = Owl_base_stats.std pnls in
        if Float.(std = 0.) then 0.
        else
          let se = std /. Float.sqrt (Float.of_int n) in
          let df = Float.of_int (n - 1) in
          let tcrit = t_critical ~p:confidence ~df in
          mean -. (tcrit *. se)

let sample_params specs = Parameters.default_map specs

let safe_hd = function
  | [] -> None
  | x :: _ -> Some x

let max_by_score candidates =
  match candidates with
  | [] -> None
  | _ ->
      List.max_elt candidates ~compare:(fun a b -> Float.compare a.score b.score)

let fallback_params specs =
  (* Deterministic fallback for empty samplers. *)
  sample_params specs

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
          metric_of_objective job.objective ~trades:c.trades ~daily_pnl:c.daily_pnl
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
      let score = metric_of_objective job.objective ~trades:cand.trades ~daily_pnl:cand.daily_pnl in
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
        | [] ->
            let sampled =
              Alg.random_uniform job.specs ~samples:1
                ~seed:(Random.State.int state 1_000_000)
            in
            Option.value (safe_hd sampled) ~default:(fallback_params job.specs)
        | xs ->
            let sorted =
              List.sort xs ~compare:(fun (_p1, s1) (_p2, s2) -> Float.compare s2 s1)
            in
            let n_good = Int.max 1 (Float.to_int (gamma *. Float.of_int (List.length sorted))) in
            let good = List.take sorted n_good in
            let center =
              match List.random_element ~random_state:state good with
              | None -> fallback_params job.specs
              | Some (c, _) -> c
            in
            Alg.perturb_around ~center ~specs:job.specs ~sigma_frac:0.05
              ~seed:(Random.State.int state 1_000_000)
      in
      for i = 0 to samples - 1 do
        let params =
          if i < init then
            let sampled =
              Alg.random_uniform job.specs ~samples:1
                ~seed:(Random.State.int state 1_000_000)
            in
            Option.value (safe_hd sampled) ~default:(fallback_params job.specs)
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
    match max_by_score !collected with
    | Some b -> b
    | None ->
        let fallback_params_v = fallback_params job.specs in
        let fallback = eval_with_cache fallback_params_v in
        { fallback with score = metric_of_objective job.objective ~trades:fallback.trades ~daily_pnl:fallback.daily_pnl }
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
