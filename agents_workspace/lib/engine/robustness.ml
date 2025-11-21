open Core

type perturbation = {
  params : Parameters.value_map;
  score  : float;
}

type result = {
  baseline_score : float;
  stability      : float;
  worst_cliff    : float;
  samples        : perturbation list;
  passed         : bool;
}

let generate_perturbations ~specs ~bump_factors base =
  let non_fixed =
    List.filter specs ~f:(fun (s : Parameters.t) ->
        let { Parameters.fixed; _ } = s in
        not fixed)
  in
  List.concat_map non_fixed ~f:(fun (spec : Parameters.t) ->
      let { Parameters.integer; name; _ } = spec in
      List.filter_map bump_factors ~f:(fun f ->
          let base_v = Map.find_exn base name in
          let bumped =
            let raw = base_v *. f in
            let raw = if integer then Float.round_nearest raw else raw in
            Parameters.clamp spec raw
          in
          if Float.(bumped = base_v) then None
          else
            Some (Map.set base ~key:name ~data:bumped)))

let run ~specs ~bump_factors ~tol_frac ~cliff_pct ~evaluate ?baseline_score base_params =
  let baseline_score = Option.value baseline_score ~default:(evaluate base_params) in
  let perturb_params = generate_perturbations ~specs ~bump_factors base_params in
  let samples =
    List.map perturb_params ~f:(fun p ->
        let score = evaluate p in
        { params = p; score })
  in
  let stability =
    match samples with
    | [] -> 1.0
    | xs ->
        let good =
          List.count xs ~f:(fun s -> Float.(s.score >= baseline_score *. tol_frac))
        in
        Float.of_int good /. Float.of_int (List.length xs)
  in
  let worst_cliff =
    match samples with
    | [] -> 0.0
    | xs ->
        List.fold xs ~init:1.0 ~f:(fun acc s ->
            let ratio = if Float.(baseline_score = 0.) then 1.0 else s.score /. baseline_score in
            Float.min acc ratio)
  in
  let cliff_fail =
    Float.(worst_cliff < 1.0 -. cliff_pct)
  in
  let passed = Float.(stability >= tol_frac) && not cliff_fail in
  { baseline_score; stability; worst_cliff; samples; passed }

let%test_unit "robustness detects stability and cliffs" =
  let specs = [
    Parameters.make ~name:"k" ~default:1.0 ~bounds:(0.5, 2.0) ();
  ] in
  let base = Parameters.default_map specs in
  let evaluate params = Map.find_exn params "k" in
  let res = run ~specs ~bump_factors:[0.8; 1.0; 1.2] ~tol_frac:0.8 ~cliff_pct:0.3 ~evaluate base in
  assert res.passed;
  let evaluate_cliff params = (-1.) *. Map.find_exn params "k" in
  let res2 = run ~specs ~bump_factors:[1.2] ~tol_frac:0.5 ~cliff_pct:0.1 ~evaluate:evaluate_cliff base in
  assert (not res2.passed)
