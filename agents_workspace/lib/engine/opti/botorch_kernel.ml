open Core
open Types

let now_iso () =
  Time_float.(now () |> to_string_iso8601_basic ~zone:Time_float.Zone.utc)

let log_ts log msg = log (Printf.sprintf "%s %s" (now_iso ()) msg)

module BC = Botorch_client

type config = {
  batch_size : int;
  n_regions : int;
  max_evals : int;
  init_samples : int option;
  shared_stream : bool;
  objective : Optimizer.objective;
  seed : int;
  host : string option;
  port : int option;
}

type eval = {
  params : Parameters.value_map;
  score : float;
  trades : trade list;
  daily_pnl : (Date.t * float) list;
  region_id : int option;
}

type result = {
  best : eval;
  history : eval list;
  tested : int;
  state : BC.state;
}

let metric_of_objective (objective : Optimizer.objective) (trades : trade list) =
  match objective with
  | Optimizer.Custom f -> f trades
  | Optimizer.Hit_rate ->
      if List.is_empty trades then 0.
      else
        let wins = List.count trades ~f:(fun t -> Float.(t.pnl_R > 0.)) in
        Float.of_int wins /. Float.of_int (List.length trades)
  | Optimizer.Mean_pnl ->
      List.sum (module Float) trades ~f:(fun t -> t.pnl_R)
      /. Float.max 1. (Float.of_int (List.length trades))
  | Optimizer.Sharpe ->
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

let bounds_of_specs (specs : Parameters.t list) =
  List.filter_map specs ~f:(fun s ->
      if s.Parameters.fixed || not s.Parameters.tunable then None
      else
        let lower, upper =
          match s.Parameters.bounds with
          | Some b -> b
          | None -> s.default -. 1., s.default +. 1.
        in
        let domain =
          match s.Parameters.domain with
          | Parameters.Continuous -> BC.Continuous
          | Parameters.Integer -> BC.Integer
          | Parameters.Discrete vals -> BC.Discrete vals
          | Parameters.Categorical cats -> BC.Categorical cats
        in
        Some { BC.name = s.name; lower; upper; integer = s.integer; domain })

let params_of_array (specs : Parameters.t list) (arr : float array) =
  let tunable_specs =
    List.filter specs ~f:(fun s -> s.Parameters.tunable && not s.Parameters.fixed)
  in
  List.foldi tunable_specs ~init:String.Map.empty ~f:(fun i acc spec ->
      let raw = arr.(i) in
      let v =
        match spec.domain with
        | Parameters.Categorical cats ->
              let idx = Int.clamp_exn ~min:0 ~max:(List.length cats - 1) (Int.of_float raw) in
              Float.of_int idx
          | Parameters.Discrete vals ->
              let idx = Int.clamp_exn ~min:0 ~max:(List.length vals - 1) (Int.of_float raw) in
              List.nth_exn vals idx
          | _ ->
              let v = if spec.integer then Float.round_nearest raw else raw in
              Parameters.clamp spec v
        in
        Map.set acc ~key:spec.name ~data:v)

let array_of_params (specs : Parameters.t list) (p : Parameters.value_map) =
  specs
  |> List.filter ~f:(fun s -> s.Parameters.tunable && not s.Parameters.fixed)
  |> List.map ~f:(fun s ->
         match Map.find p s.name with
         | Some v -> v
         | None -> failwithf "parameter %s missing" s.name ())
  |> Array.of_list

let eval_batch ~(strat_pack : Strategy_registry.pack) ~(datafile : string)
    ~(objective : Optimizer.objective) ~(shared_stream : bool)
    (params_list : Parameters.value_map list) : eval list =
  let strategies = List.map params_list ~f:strat_pack.build in
  let results =
    if shared_stream then
      Multi_engine.run_shared_pure strategies ~filename:datafile
    else
      Multi_engine.run_all_pure strategies ~filename:datafile
  in
  List.map3_exn params_list strategies results ~f:(fun params _ res ->
      let score = metric_of_objective objective res.Multi_engine.trades in
      { params; score; trades = res.trades; daily_pnl = res.daily_pnl; region_id = None })

let max_by_score (xs : eval list) =
  List.max_elt xs ~compare:(fun a b -> Float.compare a.score b.score)

let run ?(log = fun _ -> ()) ~(strat_pack : Strategy_registry.pack)
    ~(datafile : string) ~(config : config) () : result =
  let specs = strat_pack.specs in
  let bounds = bounds_of_specs specs in
  let bc_state =
    BC.init ?host:config.host ?port:config.port ~bounds
      ~batch_size:config.batch_size ~n_regions:config.n_regions ()
  in
  let init_samples =
    let default_init =
      Int.max (config.batch_size * config.n_regions) (2 * List.length specs)
    in
    Option.value config.init_samples ~default:default_init
    |> Int.min config.max_evals
    |> Int.max 1
  in
  let initial_params =
    Opt_algos.latin_hypercube specs ~samples:init_samples ~seed:config.seed
  in
  log_ts log
    (Printf.sprintf
       "botorch_kernel: warmup_start init_samples=%d shared_stream=%b batch_size=%d max_evals=%d"
       init_samples config.shared_stream config.batch_size config.max_evals);
  let warmup_t0 = Time_float.now () in
  let initial_evals =
    eval_batch ~strat_pack ~datafile ~objective:config.objective
      ~shared_stream:config.shared_stream initial_params
  in
  let warmup_dur = Time_float.diff (Time_float.now ()) warmup_t0 |> Time_float.Span.to_sec in
  let x_data = ref (List.map initial_params ~f:(array_of_params specs)) in
  let y_data = ref (List.map initial_evals ~f:(fun e -> e.score)) in
  let tested = ref (List.length initial_evals) in
  let best =
    match max_by_score initial_evals with
    | Some b -> ref b
    | None -> failwith "no initial evaluations produced"
  in
  let history = ref initial_evals in
  log_ts log
    (Printf.sprintf "botorch_kernel: warmup_done tested=%d warmup_s=%.3f" !tested
       warmup_dur);
  let bc_state =
    BC.update ?host:config.host ?port:config.port ~state:bc_state
      ~y_new:(Array.of_list !y_data) ()
  in
  let iter = ref 0 in
  let rec loop (state : BC.state) =
    if !tested >= config.max_evals then state
    else (
      incr iter;
      log_ts log
        (Printf.sprintf
           "botorch_kernel: iter %d tested=%d/%d -> suggest" !iter !tested
           config.max_evals);
      let x_mat = Array.of_list !x_data in
      let y_arr = Array.of_list !y_data in
      let suggest_t0 = Time_float.now () in
      let suggested =
        BC.suggest ?host:config.host ?port:config.port ~state ~x:x_mat ~y:y_arr ()
      in
      let suggest_dur = Time_float.diff (Time_float.now ()) suggest_t0 |> Time_float.Span.to_sec in
      let remaining = config.max_evals - !tested in
      let cand_with_region =
        List.zip_exn suggested.candidates suggested.region_ids
        |> Fn.flip List.take remaining
      in
      if List.is_empty cand_with_region then (
        log_ts log
          (Printf.sprintf
             "botorch_kernel: iter %d suggest returned 0 candidates; stopping (%.3fs)"
             !iter suggest_dur);
        state)
      else
        let params_batch =
          List.map cand_with_region ~f:(fun (arr, _) -> params_of_array specs arr)
        in
        log_ts log
          (Printf.sprintf
             "botorch_kernel: iter %d evaluating batch=%d suggest_s=%.3f" !iter
             (List.length params_batch) suggest_dur);
        let eval_t0 = Time_float.now () in
        let evals =
          eval_batch ~strat_pack ~datafile ~objective:config.objective
            ~shared_stream:config.shared_stream params_batch
        in
        let eval_dur = Time_float.diff (Time_float.now ()) eval_t0 |> Time_float.Span.to_sec in
        let evals =
          List.map2_exn evals cand_with_region ~f:(fun e (_, rid) ->
              { e with region_id = Some rid })
        in
        let y_new = List.map evals ~f:(fun e -> e.score) in
        x_data := !x_data @ List.map params_batch ~f:(array_of_params specs);
        y_data := !y_data @ y_new;
        tested := !tested + List.length evals;
        history := !history @ evals;
        log_ts log
          (Printf.sprintf
             "botorch_kernel: iter %d finished eval batch tested=%d/%d eval_s=%.3f"
             !iter !tested config.max_evals eval_dur);
        (match max_by_score evals with
        | Some b when Float.(b.score > (!best).score) ->
            best := b;
            log_ts log
              (Printf.sprintf
                 "botorch_kernel: iter %d new_best=%.4f after %d evals"
                 !iter b.score !tested)
        | _ -> ());
        let state' =
          BC.update ?host:config.host ?port:config.port ~state:suggested.state
            ~y_new:(Array.of_list y_new) ()
        in
        loop state')
  in
  let final_state = loop bc_state in
  { best = !best; history = !history; tested = !tested; state = final_state }
