open Core

module GP = Gp
module Acq = Acquisition

type bounds = { lower : float array; upper : float array }

type region = {
  center : float array;      (* normalized [0,1]^d *)
  lengths : float array;     (* per-dim length in normalized space *)
  successes : int;
  failures : int;
  points : (float array * float) list;  (* normalized points with scores *)
}

type config = {
  dim : int;
  batch_size : int;
  length_max : float;
  length_min : float;
  length_init : float;
  success_t : int;
  failure_t : int;
  shrink : float;
  expand : float;
  top_k : int option; (* number of TS-ranked candidates to keep; None => keep all, Some 1 => strict TS *)
}

let default_config ~dim =
  let batch_size = 5 in
  let length_init = 0.8 in
  let length_min = 0.5 ** 7. in
  let length_max = 1.6 in
  let failure_t =
    let v = Float.max (4. /. Float.of_int batch_size) (Float.of_int dim /. Float.of_int batch_size) in
    Int.of_float (Float.round_up v)
  in
  let success_t = 10 in
  { dim; batch_size; length_max; length_min; length_init;
    success_t; failure_t; shrink = 0.5; expand = 2.0; top_k = Some 1 }

let normalize_point ~bounds x =
  Array.mapi x ~f:(fun i xi ->
      let span = bounds.upper.(i) -. bounds.lower.(i) in
      (xi -. bounds.lower.(i)) /. span)

let denorm_point ~bounds z =
  Array.mapi z ~f:(fun i zi ->
      let span = bounds.upper.(i) -. bounds.lower.(i) in
      bounds.lower.(i) +. zi *. span)

let random_in_box ~rng ?scale center lengths =
  Array.mapi center ~f:(fun i ci ->
      let u = Random.State.float rng 1. in
      let s = match scale with None -> 1. | Some v -> v.(i) in
      let v = ci +. (u -. 0.5) *. lengths.(i) *. s in
      Float.min 1.0 (Float.max 0.0 v))

let best_in pts =
  List.fold pts ~init:Float.neg_infinity ~f:(fun acc (_, f) -> Float.max acc f)

let best_point pts =
  List.fold pts ~init:None ~f:(fun acc ((_, f) as p) ->
      match acc with
      | None -> Some p
      | Some (_, f_best) -> if Float.(f > f_best) then Some p else acc)

let update_region cfg region new_pts =
  if List.is_empty new_pts then region
  else (
  let points = region.points @ new_pts in
  let old_best = best_in region.points in
  let new_best = best_in new_pts in
  let tol = if Float.is_finite old_best then 1e-3 *. Float.abs old_best else 0. in
  let improved = not (List.is_empty new_pts) && Float.(new_best > old_best +. tol) in
  let successes = if improved then region.successes + 1 else 0 in
  let failures = if improved then 0 else region.failures + 1 in
  let successes, failures, lengths =
    if successes >= cfg.success_t then (
      let lengths' =
        Array.map region.lengths ~f:(fun l -> Float.min cfg.length_max (l *. cfg.expand))
      in
      (0, failures, lengths'))
    else if failures >= cfg.failure_t then (
      let lengths' =
        Array.map region.lengths ~f:(fun l -> Float.max cfg.length_min (l *. cfg.shrink))
      in
      (successes, 0, lengths'))
    else (successes, failures, region.lengths)
  in
  let center =
    if improved then
      match best_point points with
      | Some (x, _) -> x
      | None -> region.center
    else region.center
  in
  { center; lengths; successes; failures; points })

let should_restart cfg region =
  Array.exists region.lengths ~f:(fun l -> Float.(l < cfg.length_min))

let fit_region_gp region hyper =
  match region.points with
  | [] -> None
  | pts ->
      let x = Array.of_list (List.map pts ~f:fst) in
      let y = Array.of_list (List.map pts ~f:snd) in
      let gp, hyper', log_mll = GP.optimize_and_fit ~normalize:false ~max_iter:50 x y hyper in
      Some (gp, hyper', log_mll)

let propose_batch cfg ~rng ~bounds region gp_opt =
  let n_cand =
    Int.min 5000 (Int.max 2000 (200 * cfg.dim))
  in
  let scale =
    match gp_opt with
    | None -> None
    | Some (_, hyper, _) ->
        let gmean =
          Array.fold hyper.GP.log_ell ~init:0. ~f:( +. ) /. Float.of_int (Array.length hyper.log_ell)
          |> Float.exp
        in
        Some (Array.map hyper.log_ell ~f:(fun le -> Float.exp le /. gmean))
  in
  let samples_norm =
    Array.init n_cand ~f:(fun _ ->
        random_in_box ~rng ?scale region.center region.lengths)
  in
  let idx, draw =
    match gp_opt with
    | None ->
        let i = Random.State.int rng n_cand in
        i, None
    | Some (gp, _hyper, _log_mll) ->
        let j, z = Acq.thompson_sample ~gp ~xs:samples_norm ~rng in
        j, Some z
  in
  let k =
    match cfg.top_k with
    | Some k -> Int.min k n_cand
    | None -> 1
  in
  let selected_idx =
    match draw with
    | None -> [ idx ]
    | Some z ->
        let pairs =
          Array.to_list (Array.init n_cand ~f:(fun i -> (z.{i+1}, i)))
          |> List.sort ~compare:(fun (a,_) (b,_) -> Float.compare b a)
        in
        pairs |> fun p -> List.take p k |> List.map ~f:snd
  in
  let selected_norm = List.map selected_idx ~f:(fun i -> samples_norm.(i)) in
  let selected = List.map selected_norm ~f:(denorm_point ~bounds) in
  selected, selected_norm

type candidate = {
  x : float array;
  region_id : int;
}

let step ?log_fit ?iter cfg ~rng regions hyper bounds =
  let candidates = ref [] in
  regions
  |> List.iteri ~f:(fun rid r ->
         let gp_opt = fit_region_gp r hyper in
         (match gp_opt, log_fit, iter with
          | Some (_, hyper', log_mll), Some lf, Some it -> lf ~iter:it ~region_id:rid ~hyper:hyper' ~log_mll
          | _ -> ());
         let xs_raw, _xs_norm = propose_batch cfg ~rng ~bounds r gp_opt in
         List.iter xs_raw ~f:(fun x ->
             candidates := { x; region_id = rid } :: !candidates));
  List.rev !candidates

type log_fn = iter:int -> region:region -> region_id:int -> x:float array -> f:float -> unit
type log_fit = iter:int -> region_id:int -> hyper:Gp.hyper -> log_mll:float -> unit

let update ?log ?iter ~rng cfg regions bounds _hyper evaluated =
  let by_region =
    List.fold evaluated ~init:(Int.Map.empty) ~f:(fun acc (cand, f) ->
        let z = normalize_point ~bounds cand.x in
        Map.update acc cand.region_id ~f:(function
            | None -> [ z, f ]
            | Some l -> (z, f) :: l))
  in
  regions
  |> List.mapi ~f:(fun rid r ->
         let pts = Map.find by_region rid |> Option.value ~default:[] in
         let rand_restart () =
           let center = Array.init cfg.dim ~f:(fun _ -> Random.State.float rng 1.) in
           let lengths = Array.create ~len:cfg.dim cfg.length_init in
           { center; lengths; successes = 0; failures = 0; points = [] }
         in
         (match log, iter with
         | Some lf, Some it ->
             List.iter pts ~f:(fun (x, f) ->
                lf ~iter:it ~region:r ~region_id:rid ~x:(denorm_point ~bounds x) ~f)
         | _ -> ());
         let updated = update_region cfg r pts in
         if should_restart cfg updated then rand_restart () else updated)

let init_regions ~dim ~m ~bounds:_ =
  let rng = Random.State.make_self_init () in
  let regions =
    List.init m ~f:(fun _ ->
        let center =
          Array.init dim ~f:(fun _ -> Random.State.float rng 1.)
        in
        let lengths = Array.create ~len:dim 0.8 in
        { center; lengths; successes = 0; failures = 0; points = [] })
  in
  regions
