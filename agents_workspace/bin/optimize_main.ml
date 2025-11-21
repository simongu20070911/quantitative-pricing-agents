open Core
open Strategy_fast
module Opt = Engine.Optimizer

(* Job file schema (JSON):
   {
     "strategy": "b1b2" | "vwap_revert",
     "data": "path/to/file.csv",
     "search": { "type": "grid", "steps": 3 }
            or { "type": "random", "samples": 20, "seed": 1 },
            or { "type": "lhs", "samples": 20, "seed": 1 },
            or { "type": "bayes", "samples": 30, "seed": 1, "init": 5, "gamma": 0.2 },
     "objective": "sharpe" | "mean_pnl" | "hit_rate",
     "perm_reps": 200,
     "bootstrap_reps": 200,
     "robustness": { "bumps": [0.8, 0.9, 1.1, 1.2] },
     "cache": true
   }
*)

type strat_pack = {
  id : string;
  specs : Parameters.t list;
  build : Parameters.value_map -> Engine.Engine.pure_strategy;
}

let strat_registry : strat_pack list =
  [
    {
      id = Strategies.Strategy_b1b2.strategy_id;
      specs = Strategies.Strategy_b1b2.parameter_specs;
      build = (fun params ->
          let cfg = Strategies.Strategy_b1b2.config_of_params params in
          Strategies.Strategy_b1b2.make_pure_strategy cfg);
    };
    {
      id = Strategies.Vwap_revert_strategy.strategy_id;
      specs = Strategies.Vwap_revert_strategy.parameter_specs;
      build = (fun params ->
          let cfg = Strategies.Vwap_revert_strategy.config_of_params params in
          Strategies.Vwap_revert_strategy.make_pure_strategy cfg);
    };
  ]

let find_strategy id =
  List.find_exn strat_registry ~f:(fun s -> String.equal s.id id)

let parse_search json =
  let open Yojson.Safe.Util in
  let kind = json |> member "type" |> to_string in
  match String.lowercase kind with
  | "grid" ->
      let steps = json |> member "steps" |> to_int_option |> Option.value ~default:3 in
      Opt.Grid steps
  | "random" ->
      let samples = json |> member "samples" |> to_int_option |> Option.value ~default:20 in
      let seed = json |> member "seed" |> to_int_option |> Option.value ~default:42 in
      Opt.Random { samples; seed }
  | "lhs" | "latin_hypercube" ->
      let samples = json |> member "samples" |> to_int_option |> Option.value ~default:20 in
      let seed = json |> member "seed" |> to_int_option |> Option.value ~default:42 in
      Opt.Latin_hypercube { samples; seed }
  | "tpe" | "bayes" ->
      let samples = json |> member "samples" |> to_int_option |> Option.value ~default:30 in
      let seed = json |> member "seed" |> to_int_option |> Option.value ~default:42 in
      let init = json |> member "init" |> to_int_option in
      let gamma = json |> member "gamma" |> to_float_option in
      Opt.Bayes { samples; seed; init; gamma }
  | _ -> failwith "unknown search.type"

let parse_objective = function
  | "sharpe" -> Opt.Sharpe
  | "mean_pnl" -> Opt.Mean_pnl
  | "hit_rate" -> Opt.Hit_rate
  | s -> failwithf "unknown objective %s" s ()

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
    match member "robustness" job_json |> member "bumps" with
    | `Null -> None
    | `List xs -> Some (List.filter_map xs ~f:to_float_option)
    | _ -> None
  in
  let cache = job_json |> member "cache" |> to_bool_option |> Option.value ~default:false in
  (strategy, data, search, objective, perm_reps, bootstrap_reps, robustness_bumps, cache, job_json |> member "params_override")

let run_job ~job_file =
  let json = Yojson.Safe.from_file job_file in
  let strategy_id, datafile, search, objective, perm_reps, bootstrap_reps, robustness_bumps, cache, params_override_json =
    json_to_job ~job_json:json
  in
  if not (Stdlib.Sys.file_exists datafile) then
    failwithf "data file %s not found" datafile ();
  let strat_pack = find_strategy strategy_id in
  let guardrails = Guardrails.load ~strategy_id () |> Result.ok_or_failwith in
  let job = {
    Opt.strategy_id = strategy_id;
    specs = strat_pack.specs;
    search;
    objective;
    guardrails;
    perm_reps;
    bootstrap_reps;
    robustness_bumps;
    cache;
  } in
  let overrides = parse_overrides strat_pack.specs params_override_json in
  let evaluate params =
    let p = Map.fold overrides ~init:params ~f:(fun ~key ~data acc -> Map.set acc ~key ~data) in
    let strategy = strat_pack.build p in
    let { Engine.Engine.setups = _; trades; daily_pnl; daily_pnl_usd = _; daily_pnl_pct = _ } =
      Engine.Engine.run_pure strategy ~filename:datafile
    in
    { Opt.params = p; score = 0.; trades; daily_pnl }
  in
  let result = Opt.run job ~evaluate in
  let ts =
    Time_float.now ()
    |> Time_float.to_string_iso8601_basic ~zone:Time_float.Zone.utc
  in
  let out_dir = Filename.concat (Filename.concat "runs" strategy_id) ts in
  Core_unix.mkdir_p out_dir;
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
  let summary =
    `Assoc [
      "strategy", `String strategy_id;
      "datafile", `String datafile;
      "search", `String (match search with
                         | Opt.Grid n -> sprintf "grid:%d" n
                         | Opt.Random {samples; _} -> sprintf "random:%d" samples
                         | Opt.Latin_hypercube {samples; _} -> sprintf "lhs:%d" samples
                         | Opt.Bayes {samples; _} -> sprintf "bayes:%d" samples);
      "objective", `String (match objective with | Opt.Sharpe -> "sharpe" | Opt.Mean_pnl -> "mean_pnl" | Opt.Hit_rate -> "hit_rate" | Opt.Custom _ -> "custom");
      "best_score", `Float result.best.score;
      "tested", `Int result.tested;
      "rejected_guardrail", `Int result.rejected_guardrail;
      "rejected_robustness", `Int result.rejected_robustness;
      "cache_hits", `Int result.cache_hits;
      "guardrails_hash", `String result.guardrails_hash;
      "params", params_json;
      "perm_reps", (match perm_reps with Some r -> `Int r | None -> `Null);
      "bootstrap_reps", (match bootstrap_reps with Some r -> `Int r | None -> `Null);
      "timestamp", `String ts;
    ]
  in
  Yojson.Safe.to_file (Filename.concat out_dir "summary.json") summary;
  Stdio.printf "Done. Best score %.4f (tested %d, guardrail_rej %d, robustness_rej %d, cache_hits %d). Output -> %s\n"
    result.best.score result.tested result.rejected_guardrail result.rejected_robustness result.cache_hits out_dir

let usage () =
  Stdio.eprintf "Usage: optimize_main.exe <job.json>\n";
  Stdlib.exit 1

let () =
  match Sys.get_argv () with
  | [| _; job |] -> run_job ~job_file:job
  | _ -> usage ()
