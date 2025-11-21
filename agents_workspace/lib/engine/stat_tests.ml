open Core

let permutation_pvalue ?(reps = 500) ?(seed = 42) ~metric sample =
  match sample with
  | [] -> 1.0
  | _ ->
      let baseline = metric sample in
      let state = Random.State.make [| seed |] in
      let worse =
        List.init reps ~f:(fun _ ->
            let permuted = List.permute ~random_state:state sample in
            Float.(metric permuted >= baseline))
      in
      let count = List.count worse ~f:Fn.id in
      Float.of_int (count + 1) /. Float.of_int (reps + 1)

let bootstrap_ci ?(reps = 500) ?(alpha = 0.05) ?(seed = 43) sample =
  match sample with
  | [] -> (0., 0.)
  | _ ->
      let state = Random.State.make [| seed |] in
      let draws =
        List.init reps ~f:(fun _ ->
            let resample =
                List.init (List.length sample) ~f:(fun _ ->
                  List.random_element_exn ~random_state:state sample)
            in
            List.sum (module Float) resample ~f:Fun.id
            /. Float.of_int (List.length resample))
      in
      let sorted = List.sort draws ~compare:Float.compare in
      let idx_lower = Int.clamp_exn ~min:0 ~max:(reps - 1) (Int.of_float (alpha /. 2. *. Float.of_int reps)) in
      let idx_upper = Int.clamp_exn ~min:0 ~max:(reps - 1) (Int.of_float ((1. -. alpha /. 2.) *. Float.of_int reps)) in
      (List.nth_exn sorted idx_lower, List.nth_exn sorted idx_upper)

let%test_unit "permutation pvalue near 0.5 for symmetric sample" =
  let sample = [1.; -1.; 1.; -1.; 0.] in
  let p = permutation_pvalue ~reps:200 ~metric:(fun xs -> List.sum (module Float) xs ~f:Fun.id) sample in
  assert (Float.(p > 0.1 && p < 0.9))

let%test_unit "bootstrap_ci bounds order" =
  let low, high = bootstrap_ci ~reps:200 [1.; 2.; 3.; 4.; 5.] in
  assert (Float.(low <= high))
