open Core
open Strategy_fast
open Types
module Opt = Engine.Optimizer
module BK = Engine.Botorch_kernel
module Objectives = Engine.Objectives

(* Job file schema (JSON), restricted to LHS or botorch/TuRBO searches:
   {
     "strategy": "b1b2" | "vwap_revert" | ...,
     "data": "path/to/file.csv",
     "search": { "type": "lhs", "samples": 20, "seed": 1 }
            or { "type": "botorch", "batch_size": 4, "n_regions": 3,
                 "max_evals": 50, "init_samples": 12, "seed": 1,
                 "shared_stream": true, "host": "localhost", "port": 5555 },
     "objective": "sharpe" | "mean_pnl" | "hit_rate" | "trade_mean_lcb[@0.99]",
     "perm_reps": 200,
     "bootstrap_reps": 200,
     "robustness": { "bumps": [0.8, 0.9, 1.1, 1.2] },
     "cache": true
   }
*)

type search_choice =
  | Lhs of { samples : int; seed : int }
  | Botorch of {
      batch_size : int;
      n_regions : int;
      max_evals : int;
      init_samples : int option;
      shared_stream : bool;
      min_trades : int;
      seed : int;
      host : string option;
      port : int option;
    }

type best = {
  params : Parameters.value_map;
  score : float;
  trades : Types.trade list;
  daily_pnl : (Date.t * float) list;
}

type normalized = {
  best : best;
  tested : int;
  rejected_guardrail : int;
  rejected_robustness : int;
  cache_hits : int;
  guardrails_hash : string;
  method_name : string;
}

let parse_search json =
  let open Yojson.Safe.Util in
  let kind = json |> member "type" |> to_string |> String.lowercase in
  match kind with
  | "lhs" | "latin_hypercube" ->
      let samples = json |> member "samples" |> to_int_option |> Option.value ~default:20 in
      let seed = json |> member "seed" |> to_int_option |> Option.value ~default:42 in
      Lhs { samples; seed }
  | "botorch" | "bayes" | "turbo" ->
      let batch_size = json |> member "batch_size" |> to_int_option |> Option.value ~default:4 in
      let n_regions = json |> member "n_regions" |> to_int_option |> Option.value ~default:3 in
      let max_evals = json |> member "max_evals" |> to_int_option |> Option.value ~default:50 in
      let init_samples = json |> member "init_samples" |> to_int_option in
      let shared_stream = json |> member "shared_stream" |> to_bool_option |> Option.value ~default:true in
      let min_trades = json |> member "min_trades" |> to_int_option in
      let seed = json |> member "seed" |> to_int_option |> Option.value ~default:42 in
      let host = json |> member "host" |> to_string_option in
      let port = json |> member "port" |> to_int_option in
      Botorch { batch_size; n_regions; max_evals; init_samples; shared_stream; min_trades = Option.value ~default:0 min_trades; seed; host; port }
  | _ ->
      failwith "deprecated search.type; use \"lhs\" or \"botorch\""

let parse_objective s = Objectives.find_exn s

let parse_overrides specs json =
  match Yojson.Safe.Util.to_option Yojson.Safe.Util.to_assoc json with
  | None -> String.Map.empty
  | Some kvs ->
      let float_of_json = function
        | `Float f -> f
        | `Int i -> Float.of_int i
        | other -> failwithf "param override must be number, got %s" (Yojson.Safe.to_string other) ()
      in
      kvs
      |> List.map ~f:(fun (k,v) -> k, float_of_json v)
      |> String.Map.of_alist_exn
      |> Parameters.merge_overrides specs

let json_to_job ~job_json =
  let open Yojson.Safe.Util in
  let strategy = job_json |> member "strategy" |> to_string in
  let data = job_json |> member "data" |> to_string in
  let search = job_json |> member "search" |> parse_search in
  let objective =
    job_json |> member "objective" |> to_string_option
    |> Option.value ~default:"sharpe"
    |> String.lowercase
    |> parse_objective
  in
  let perm_reps = job_json |> member "perm_reps" |> to_int_option in
  let bootstrap_reps = job_json |> member "bootstrap_reps" |> to_int_option in
  let robustness_bumps =
    match member "robustness" job_json with
    | `Assoc _ as rob -> (
        match member "bumps" rob with
        | `Null -> None
        | `List xs -> Some (List.filter_map xs ~f:to_float_option)
        | _ -> None)
    | _ -> None
  in
  let cache = job_json |> member "cache" |> to_bool_option |> Option.value ~default:false in
  let shared_stream = job_json |> member "shared_stream" |> to_bool_option |> Option.value ~default:true in
  let save_steps = job_json |> member "save_steps" |> to_bool_option |> Option.value ~default:false in
  let step_stride = job_json |> member "step_stride" |> to_int_option |> Option.value ~default:10 in
  (strategy, data, search, objective, perm_reps, bootstrap_reps, robustness_bumps, cache, shared_stream, job_json |> member "params_override", save_steps, step_stride)

let run_job ~job_file =
  let json = Yojson.Safe.from_file job_file in
  let strategy_id, datafile, search, objective, perm_reps, bootstrap_reps, robustness_bumps, cache, _shared_stream, params_override_json, save_steps, step_stride =
    json_to_job ~job_json:json
  in
  if not (Stdlib.Sys.file_exists datafile) then
    failwithf "data file %s not found" datafile ();
  let strat_pack = Strategy_registry.find_exn strategy_id in
  let guardrails = Guardrails.load ~strategy_id () |> Result.ok_or_failwith in
  let base_job = {
    Opt.strategy_id = strategy_id;
    specs = strat_pack.specs;
    search = Opt.Latin_hypercube { samples = 0; seed = 0 }; (* placeholder, overridden for LHS *)
    objective;
    guardrails;
    perm_reps;
    bootstrap_reps;
    robustness_bumps;
    cache;
  } in
  let overrides = parse_overrides strat_pack.specs params_override_json in
  let apply_overrides params =
    Map.fold overrides ~init:params ~f:(fun ~key ~data acc -> Map.set acc ~key ~data)
  in
  let strat_pack_overrides =
    { strat_pack with build = (fun p -> strat_pack.build (apply_overrides p)) }
  in

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
  in
  let capacity_guardrail (_g : Guardrails.t) (_trades : Types.trade list) = true in
  let pvalue_guardrail (g : Guardrails.t) ?reps trades =
    match reps with
    | None -> true
    | Some r when r <= 0 -> true
    | Some _ ->
        let pnls = List.map trades ~f:(fun t -> t.pnl_R) in
        let mean xs = List.sum (module Float) xs ~f:Fun.id /. Float.of_int (List.length xs) in
        let p = Engine.Stat_tests.permutation_pvalue ?reps ~metric:mean pnls in
        Float.(p <= g.pvalue_threshold)
  in
  let bootstrap_guardrail (g : Guardrails.t) ?reps trades =
    match reps with
    | None -> true
    | Some r when r <= 0 -> true
    | Some _ ->
        let pnls = List.map trades ~f:(fun t -> t.pnl_R) in
        let lower, _ = Engine.Stat_tests.bootstrap_ci ?reps pnls in
        Float.(lower >= g.bootstrap_ci_target)
  in

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
  in

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
  in

  let trade_penalty trades_count score =
    if trades_count >= guardrails.min_trades then score
    else
      let shortfall = Float.of_int (guardrails.min_trades - trades_count) in
      score -. (1000. *. shortfall)
  in

  let metric_of_objective objective ~trades ~daily_pnl =
    let trades_count = List.length trades in
    match objective with
    | Opt.Custom _ -> failwith "custom objective not supported in botorch guardrails"
    | Opt.Hit_rate ->
        if List.is_empty trades then trade_penalty trades_count 0.
        else
          let wins = List.count trades ~f:(fun t -> Float.(t.pnl_R > 0.)) in
          let base = Float.of_int wins /. Float.of_int trades_count in
          trade_penalty trades_count base
    | Opt.Mean_pnl ->
        let base =
          List.sum (module Float) trades ~f:(fun t -> t.pnl_R)
          /. Float.max 1. (Float.of_int trades_count)
        in
        trade_penalty trades_count base
    | Opt.Sharpe ->
        let pnls = List.map trades ~f:(fun t -> t.pnl_R) in
        (match pnls with
        | [] -> trade_penalty trades_count 0.
        | _ ->
            let len = Float.of_int (List.length pnls) in
            let mean = List.sum (module Float) pnls ~f:Fun.id /. len in
            let var =
              List.sum (module Float) pnls ~f:(fun x -> (x -. mean) ** 2.) /. len
            in
            let base = if Float.(var = 0.) then 0. else mean /. Float.sqrt var in
            trade_penalty trades_count base)
    | Opt.Year_sharpe { lambda; min_days } ->
        let base = year_sharpe_score ~lambda ~min_days ~trades_count:trades_count daily_pnl in
        trade_penalty trades_count base
    | Opt.Simon_ratio { lambda } ->
        let base = simon_ratio_score ~lambda daily_pnl in
        trade_penalty trades_count base
    | Opt.Mean_trade_lcb { confidence } ->
        let pnls = List.map trades ~f:(fun t -> t.pnl_R) |> Array.of_list in
        let base =
          let n = Array.length pnls in
          if n < 2 then 0.
          else
            let mean = Owl_base_stats.mean pnls in
            let std = Owl_base_stats.std pnls in
            if Float.(std = 0.) then 0.
            else
              let se = std /. Float.sqrt (Float.of_int n) in
              let df = Float.of_int (n - 1) in
              let tcrit = Opt.t_critical ~p:confidence ~df in
              mean -. (tcrit *. se)
        in
        trade_penalty trades_count base
  in
  let ts =
    Time_float.now ()
    |> Time_float.to_string_iso8601_basic ~zone:Time_float.Zone.utc
  in
  let out_dir = Filename.concat (Filename.concat "runs" strategy_id) ts in
  Core_unix.mkdir_p out_dir;
  let steps_dir = Filename.concat out_dir "steps" in
  if save_steps then Core_unix.mkdir_p steps_dir;

  let stdout_log = Filename.concat out_dir "stdout.log" in
  let log_oc = Out_channel.create ~append:false stdout_log in
  let log_json event fields =
    let assoc = ("event", `String event) :: ("ts", `String (Time_float.now () |> Time_float.to_string_iso8601_basic ~zone:Time_float.Zone.utc)) :: fields in
    Out_channel.output_string log_oc (Yojson.Safe.to_string (`Assoc assoc));
    Out_channel.output_char log_oc '\n';
    Out_channel.flush log_oc
  in

  let lhs_result samples seed : normalized =
    let evaluate params =
      let p = apply_overrides params in
      let strategy = strat_pack.build p in
      let res = Engine.Engine.run_pure strategy ~filename:datafile in
      { Opt.params = p; score = 0.; trades = res.trades; daily_pnl = res.daily_pnl }
    in
    let job = { base_job with search = Opt.Latin_hypercube { samples; seed } } in
    let r = Opt.run job ~evaluate in
    log_json "done_lhs" [ ("tested", `Int r.tested); ("best_score", `Float r.best.score) ];
    { best = { params = r.best.params; score = r.best.score; trades = r.best.trades; daily_pnl = r.best.daily_pnl };
      tested = r.tested;
      rejected_guardrail = r.rejected_guardrail;
      rejected_robustness = r.rejected_robustness;
      cache_hits = r.cache_hits;
      guardrails_hash = r.guardrails_hash;
      method_name = Printf.sprintf "lhs:%d" samples; }
  in

  let botorch_result (cfg : search_choice) : normalized =
    match cfg with
    | Lhs _ -> failwith "internal: botorch_result called with lhs"
  | Botorch { batch_size; n_regions; max_evals; init_samples; shared_stream = shared_stream_search; min_trades = _; seed; host; port } ->
        let kernel_log = Filename.concat out_dir "kernel.log" in
        let log_out = Out_channel.create ~append:false kernel_log in
        let log msg =
          Out_channel.output_string log_out (msg ^ "\n");
          Out_channel.flush log_out;
          log_json "kernel" [ ("msg", `String msg) ]
        in
        let bk_cfg : BK.config = {
          batch_size;
          n_regions;
          max_evals;
          init_samples;
          shared_stream = shared_stream_search;
          min_trades = guardrails.min_trades;
          objective;
          seed;
          host;
          port;
        } in
        let on_new_best =
          if save_steps then
            Some (fun ~iter (ev : BK.eval) ->
              if iter mod step_stride = 0 then (
                let dir = Filename.concat steps_dir (Printf.sprintf "iter_%04d" iter) in
                Core_unix.mkdir_p dir;
                let trades_file = Filename.concat dir "trades.csv" in
                let daily_file = Filename.concat dir "daily.csv" in
                Summary.export_trades_csv ~outfile:trades_file ~trades:ev.trades;
                Summary.export_daily_csv ~outfile:daily_file ~daily:ev.daily_pnl ();
                let params_file = Filename.concat dir "params.json" in
                Yojson.Safe.to_file params_file (`Assoc (ev.params |> Map.to_alist |> List.map ~f:(fun (k,v) -> k, `Float v)));
                let score_file = Filename.concat dir "score.txt" in
                Out_channel.write_all score_file ~data:(Printf.sprintf "%.6f\n" ev.score);
                log_json "new_best_saved" [ ("iter", `Int iter); ("score", `Float ev.score); ("dir", `String dir) ]))
          else None
        in
        let r =
          BK.run ~log ?on_new_best
            ~strat_pack:strat_pack_overrides ~datafile ~config:bk_cfg ()
        in
        let evaluated = r.history in
        let rejected_guardrail = ref 0 in
        let rejected_robustness = ref 0 in
        let eval_score params =
          let p = apply_overrides params in
          let strat = strat_pack.build p in
          let res = Engine.Engine.run_pure strat ~filename:datafile in
          metric_of_objective objective ~trades:res.trades ~daily_pnl:res.daily_pnl
        in
        let robustness_pass params cand_score =
          match robustness_bumps with
          | None | Some [] -> true
          | Some bumps ->
              let r =
                Engine.Robustness.run ~specs:strat_pack.specs ~bump_factors:bumps
                  ~tol_frac:guardrails.robustness_tol_frac ~cliff_pct:guardrails.cliff_pct
                  ~evaluate:eval_score ~baseline_score:cand_score params
              in
              r.passed
        in
        let passed =
          List.filter evaluated ~f:(fun e ->
              let trades = e.BK.trades in
              let daily = e.daily_pnl in
              let pass_trades = trades_guardrail guardrails trades daily in
              let pass_cap = if pass_trades then capacity_guardrail guardrails trades else false in
              let pass_perm =
                if pass_trades && pass_cap then pvalue_guardrail guardrails ?reps:perm_reps trades else false
              in
              let pass_boot =
                if pass_trades && pass_cap then bootstrap_guardrail guardrails ?reps:bootstrap_reps trades else false
              in
              let pass_core = pass_trades && pass_cap && pass_perm && pass_boot in
              if not pass_core then (incr rejected_guardrail; false)
              else
                let score = e.score in
                if robustness_pass e.params score then true
                else (incr rejected_robustness; false))
        in
        let best =
          match List.max_elt passed ~compare:(fun a b -> Float.compare a.score b.score) with
          | Some b -> b
          | None ->
              let fp = Parameters.default_map strat_pack.specs in
              let strat = strat_pack.build fp in
              let res = Engine.Engine.run_pure strat ~filename:datafile in
              let score = metric_of_objective objective ~trades:res.trades ~daily_pnl:res.daily_pnl in
              { BK.params = fp; score; trades = res.trades; daily_pnl = res.daily_pnl; region_id = None }
        in
        log_json "done_botorch" [ ("tested", `Int r.tested); ("best_score", `Float best.score) ];
        { best = { params = best.params; score = best.score; trades = best.trades; daily_pnl = best.daily_pnl };
          tested = r.tested;
          rejected_guardrail = !rejected_guardrail;
          rejected_robustness = !rejected_robustness;
          cache_hits = 0;
          guardrails_hash = Guardrails.hash guardrails;
          method_name = "botorch"; }
  in

  let result : normalized =
    match search with
    | Lhs { samples; seed } -> lhs_result samples seed
    | Botorch _ as b -> botorch_result b
  in
  let shared_stream_used =
    match search with
    | Lhs _ -> false
    | Botorch { shared_stream = ss; _ } -> ss
  in

  Summary.export_trades_csv
    ~outfile:(Filename.concat out_dir "trades.csv")
    ~trades:result.best.trades;
  Summary.export_daily_csv
    ~outfile:(Filename.concat out_dir "daily.csv")
    ~daily:result.best.daily_pnl
    ();
  let params_json =
    `Assoc (result.best.params |> Map.to_alist |> List.map ~f:(fun (k, v) -> k, `Float v))
  in
  let params_file = Filename.concat out_dir "best_params.json" in
  Yojson.Safe.to_file params_file params_json;
  let summary =
    `Assoc [
      "strategy", `String strategy_id;
      "datafile", `String datafile;
      "search", `String result.method_name;
      "objective", `String (Objectives.to_string objective);
      "best_score", `Float result.best.score;
      "tested", `Int result.tested;
      "rejected_guardrail", `Int result.rejected_guardrail;
      "rejected_robustness", `Int result.rejected_robustness;
      "cache_hits", `Int result.cache_hits;
      "guardrails_hash", `String result.guardrails_hash;
      "shared_stream", `Bool shared_stream_used;
      "params", params_json;
      "perm_reps", (match perm_reps with Some r -> `Int r | None -> `Null);
      "bootstrap_reps", (match bootstrap_reps with Some r -> `Int r | None -> `Null);
      "timestamp", `String ts;
      "run_dir", `String out_dir;
      "save_steps", `Bool save_steps;
      "step_stride", `Int step_stride;
    ]
  in
  Yojson.Safe.to_file (Filename.concat out_dir "summary.json") summary;
  log_json "done" [ ("best_score", `Float result.best.score); ("tested", `Int result.tested); ("run_dir", `String out_dir) ];
  Out_channel.close log_oc;
  let params_str =
    result.best.params
    |> Map.to_alist
    |> List.map ~f:(fun (k,v) -> Printf.sprintf "%s=%.6g" k v)
    |> String.concat ~sep:", "
  in
  Stdio.printf "Done. Best score %.4f (tested %d, guardrail_rej %d, robustness_rej %d, cache_hits %d).\nParams: %s\nOutput -> %s\n"
    result.best.score result.tested result.rejected_guardrail result.rejected_robustness result.cache_hits
    params_str out_dir

let usage () =
  Stdio.eprintf "Usage: optimize_main.exe <job.json>\n";
  Stdlib.exit 1

let () =
  match Sys.get_argv () with
  | [| _; job |] -> run_job ~job_file:job
  | _ -> usage ()
