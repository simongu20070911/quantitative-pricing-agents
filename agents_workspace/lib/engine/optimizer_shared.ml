open Core
open Types

module Base = Optimizer
module Alg = Opt_algos

type objective = Base.objective
type search = Base.search

type job = {
  base : Base.job;
  datafile : string;
  batch_size : int;
  restarts : int;
  shared_stream : bool;
}

type candidate = {
  params : Parameters.value_map;
  score  : float;
  trades : trade list;
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

let key_of_params params =
  params
  |> Map.to_alist
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> [%sexp_of: (string * float) list]
  |> Sexp.to_string

let safe_hd = function
  | [] -> None
  | x :: _ -> Some x

let max_by_score (candidates : candidate list) =
  match candidates with
  | [] -> None
  | _ ->
      List.max_elt candidates ~compare:(fun a b -> Float.compare a.score b.score)

(* --- Guardrails and metrics (mirrors Base.Optimizer) --- *)

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
  let target_years = 3. in
  let target_days = 200. in
  let shrink_n = min_days in
  let per_year = Int.Table.create () in
  List.iter daily ~f:(fun (d, pnl) -> Hashtbl.add_multi per_year ~key:(Date.year d) ~data:pnl);
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

let metric_of_objective objective ~trades ~daily_pnl =
  match objective with
  | Base.Custom f -> f trades
  | Base.Hit_rate ->
      if List.is_empty trades then 0.
      else
        let wins = List.count trades ~f:(fun t -> Float.(t.pnl_R > 0.)) in
        Float.of_int wins /. Float.of_int (List.length trades)
  | Base.Mean_pnl ->
      List.sum (module Float) trades ~f:(fun t -> t.pnl_R)
      /. Float.max 1. (Float.of_int (List.length trades))
  | Base.Sharpe ->
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
  | Base.Year_sharpe { lambda; min_days } ->
      year_sharpe_score ~lambda ~min_days ~trades_count:(List.length trades) daily_pnl
  | Base.Simon_ratio { lambda } ->
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
      let logs, mdd = loop 1. [] [] 0. daily_pnl in
      let t = Float.of_int (List.length logs) in
      if Float.(t = 0.) then -5.
      else
        let g = List.sum (module Float) logs ~f:Fun.id /. t in
        g -. (lambda *. mdd)
  | Base.Mean_trade_lcb { confidence } ->
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
          let tcrit = Optimizer.t_critical ~p:confidence ~df in
          mean -. (tcrit *. se)

(* --- Helpers --- *)

let chunk ~size lst =
  let rec loop acc = function
    | [] -> List.rev acc
    | xs ->
        let chunk, rest = List.split_n xs size in
        loop (chunk :: acc) rest
  in
  loop [] lst

let fallback_params specs =
  Parameters.default_map specs

(* Build and run a batch of strategies in a shared stream *)
let eval_batch ~(strat_pack : Strategy_registry.pack) ~datafile params_list =
  let strategies =
    List.map params_list ~f:(fun p -> strat_pack.build p)
  in
  let results = Multi_engine.run_shared_pure strategies ~filename:datafile in
  List.map2_exn params_list results ~f:(fun params res ->
      { params; score = 0.; trades = res.trades; daily_pnl = res.daily_pnl })

let score_candidate (job : Base.job) (_params : Parameters.value_map) cand =
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
  if passed_trades && passed_capacity && passed_perm && passed_bootstrap then
    let score = metric_of_objective job.objective ~trades:cand.trades ~daily_pnl:cand.daily_pnl in
    Some { cand with score }
  else None

let run_grid_like ~(job : job) ~(strat_pack : Strategy_registry.pack) candidates =
  let tested = ref 0 in
  let rejected_guardrail = ref 0 in
  let rejected_robustness = ref 0 in
  let cache_hits = ref 0 in
  let cache = Hashtbl.create (module String) in
  let guardrails_hash = Guardrails.hash job.base.guardrails in
  let collected = ref [] in

  let eval_with_cache params =
    match job.base.cache with
    | false -> incr tested; eval_batch ~strat_pack ~datafile:job.datafile [ params ] |> List.hd_exn
    | true -> (
        match Hashtbl.find cache (key_of_params params) with
        | Some cand -> incr cache_hits; cand
        | None ->
            incr tested;
            let cand = eval_batch ~strat_pack ~datafile:job.datafile [ params ] |> List.hd_exn in
            Hashtbl.set cache ~key:(key_of_params params) ~data:cand;
            cand)
  in

  candidates
  |> chunk ~size:(Int.max 1 job.batch_size)
  |> List.iter ~f:(fun batch ->
         let cands =
           if job.shared_stream then eval_batch ~strat_pack ~datafile:job.datafile batch
           else List.map batch ~f:eval_with_cache
         in
         List.iter2_exn batch cands ~f:(fun params cand ->
             incr tested;
             let cand =
               if job.shared_stream then cand
               else cand (* already from cache path or single eval *)
             in
             match score_candidate job.base params cand with
             | None -> incr rejected_guardrail
             | Some s -> collected := s :: !collected));

  let best =
    match max_by_score !collected with
    | Some b -> b
    | None ->
        let fp = fallback_params job.base.specs in
        let fallback = eval_with_cache fp in
        { fallback with score = metric_of_objective job.base.objective ~trades:fallback.trades ~daily_pnl:fallback.daily_pnl }
  in
  {
    best;
    tested = !tested;
    rejected_guardrail = !rejected_guardrail;
    rejected_robustness = !rejected_robustness;
    cache_hits = !cache_hits;
    guardrails_hash;
  }

(* TuRBO-m style batched Bayes using simple GP + Thompson sampling. *)
let run_bayes ~(job : job) ~(strat_pack : Strategy_registry.pack) ~samples ~seed ~init:_ ~gamma:_ =
  let guardrails_hash = Guardrails.hash job.base.guardrails in
  let cfg =
    Botorch_kernel.
      {
        batch_size = job.batch_size;
        n_regions = Int.max 1 job.restarts;
        max_evals = samples;
        init_samples = None;
        shared_stream = job.shared_stream;
        min_trades = job.base.guardrails.min_trades;
        objective = job.base.objective;
        seed;
        host = None;
        port = None;
      }
  in
  let log msg =
    let ts =
      Time_float.(now () |> to_string_iso8601_basic ~zone:Time_float.Zone.utc)
    in
    let line = Stdlib.Format.asprintf "%s %s\n%!" ts msg in
    let outfile = Filename.concat "runs" "botorch_kernel.log" in
    Out_channel.with_file outfile ~append:true ~perm:0o644 ~f:(fun oc ->
        Out_channel.output_string oc line)
  in
  let r =
    Botorch_kernel.run ~log ~strat_pack ~datafile:job.datafile ~config:cfg ()
  in
  let tested = r.tested in
  let rejected_guardrail = ref 0 in
  let rejected_robustness = ref 0 in
  let cache_hits = 0 in
  let passed =
    List.filter_map r.history ~f:(fun e ->
        match score_candidate job.base e.params
                { params = e.params; score = e.score; trades = e.trades; daily_pnl = e.daily_pnl } with
        | None ->
            incr rejected_guardrail;
            None
        | Some s -> Some s)
  in
  let best =
    match max_by_score passed with
    | Some b -> b
    | None ->
        let fp = fallback_params job.base.specs in
        let cand =
          eval_batch ~strat_pack ~datafile:job.datafile [ fp ] |> List.hd_exn
        in
        { cand with score = metric_of_objective job.base.objective ~trades:cand.trades ~daily_pnl:cand.daily_pnl }
  in
  {
    best;
    tested;
    rejected_guardrail = !rejected_guardrail;
    rejected_robustness = !rejected_robustness;
    cache_hits;
    guardrails_hash;
  }

let run (job : job) ~(strat_pack : Strategy_registry.pack) : result =
  match job.base.search with
  | Base.Bayes { samples; seed; init; gamma } ->
      let init = Option.value init ~default:(Int.max 5 (samples / 5)) in
      let gamma = Option.value gamma ~default:0.2 in
      run_bayes ~job ~strat_pack ~samples ~seed ~init ~gamma
  | Base.Grid steps ->
      let candidates = Alg.grid ~steps job.base.specs ~samples:0 ~seed:0 in
      run_grid_like ~job ~strat_pack candidates
  | Base.Random { samples; seed } ->
      let candidates = Alg.random_uniform job.base.specs ~samples ~seed in
      run_grid_like ~job ~strat_pack candidates
  | Base.Latin_hypercube { samples; seed } ->
      let candidates = Alg.latin_hypercube job.base.specs ~samples ~seed in
      run_grid_like ~job ~strat_pack candidates
