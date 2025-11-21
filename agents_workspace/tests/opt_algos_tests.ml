open Core
open Strategy_fast
module OA = Engine.Opt_algos

let int_bounds_spec =
  Parameters.make ~name:"k" ~default:1.0 ~bounds:(0., 10.) ~integer:true ()

let float_bounds_spec =
  Parameters.make ~name:"x" ~default:0.5 ~bounds:(0., 1.) ()

let%test_unit "random_uniform respects bounds and integer flag" =
  let samples = 50 in
  let draws = OA.random_uniform [ int_bounds_spec ] ~samples ~seed:7 in
  List.iter draws ~f:(fun m ->
      let v = Map.find_exn m "k" in
      assert (Float.(v >= 0. && v <= 10.));
      assert (Float.(Float.round_nearest v = v)))

let%test_unit "lhs covers all buckets once" =
  let samples = 5 in
  let draws = OA.latin_hypercube [ float_bounds_spec ] ~samples ~seed:123 in
  let idxs =
    List.map draws ~f:(fun m ->
        let v = Map.find_exn m "x" in
        Int.clamp_exn ~min:0 ~max:(samples - 1) (Int.of_float (v *. Float.of_int samples)))
  in
  let distinct =
    List.dedup_and_sort idxs ~compare:Int.compare
  in
  assert (List.length distinct = samples);
  List.iter idxs ~f:(fun i -> assert (i >= 0 && i < samples))

let%test_unit "grid produces expected count and endpoints" =
  let steps = 3 in
  let draws = OA.grid ~steps [ float_bounds_spec ] ~samples:0 ~seed:0 in
  let vs = List.map draws ~f:(fun m -> Map.find_exn m "x") |> List.sort ~compare:Float.compare in
  assert (List.length vs = steps);
  assert (Float.(List.hd_exn vs = 0.));
  assert (Float.(List.last_exn vs = 1.))
