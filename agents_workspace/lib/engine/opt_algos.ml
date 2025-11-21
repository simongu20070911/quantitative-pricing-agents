open Core
open Parameters

let clamp_round (spec : Parameters.t) v =
  let v = if spec.integer then Float.round_nearest v else v in
  Parameters.clamp spec v

let sample_one (state : Random.State.t) (spec : Parameters.t) =
  match spec.bounds with
  | None -> spec.default
  | Some (lo, hi) ->
      let u = Random.State.float state 1.0 in
      let raw =
        match spec.scale with
        | Parameters.Scale.Lin -> lo +. u *. (hi -. lo)
        | Parameters.Scale.Log ->
            let llo = Float.log lo and lhi = Float.log hi in
            Float.exp (llo +. u *. (lhi -. llo))
      in
      clamp_round spec raw

let random_uniform specs ~samples ~seed =
  let state = Random.State.make [| seed |] in
  List.init samples ~f:(fun _ ->
      List.fold specs ~init:String.Map.empty ~f:(fun acc spec ->
          let v = sample_one state spec in
          Map.set acc ~key:spec.name ~data:v))

let latin_hypercube specs ~samples ~seed =
  let state = Random.State.make [| seed |] in
  let buckets () =
    List.init samples ~f:(fun i ->
        (Float.of_int i +. Random.State.float state 1.0) /. Float.of_int samples)
    |> List.permute ~random_state:state
  in
  (* Generate a bucket list per dim, then zip by index *)
  let bucket_table =
    List.map specs ~f:(fun spec ->
        match spec.bounds with
        | None -> Array.create ~len:samples spec.default
        | Some (lo, hi) ->
            buckets ()
            |> List.map ~f:(fun u ->
                match spec.scale with
                | Parameters.Scale.Lin -> lo +. u *. (hi -. lo)
                | Parameters.Scale.Log ->
                    let llo = Float.log lo and lhi = Float.log hi in
                    Float.exp (llo +. u *. (lhi -. llo)))
            |> Array.of_list)
  in
  List.init samples ~f:(fun idx ->
      List.fold2_exn specs bucket_table ~init:String.Map.empty
        ~f:(fun acc spec vals ->
            let v = clamp_round spec vals.(idx) in
            Map.set acc ~key:spec.name ~data:v))

let grid ~steps specs ~samples:_ ~seed:_ =
  let points spec =
    match spec.Parameters.bounds with
    | None -> [ spec.default ]
    | Some (lo, hi) ->
        if steps <= 1 then [ spec.default ]
        else
          let vals =
            match spec.scale with
            | Parameters.Scale.Lin ->
                List.init steps ~f:(fun i ->
                    let t = Float.of_int i /. Float.of_int (steps - 1) in
                    lo +. t *. (hi -. lo))
            | Parameters.Scale.Log ->
                let llo = Float.log lo and lhi = Float.log hi in
                List.init steps ~f:(fun i ->
                    let t = Float.of_int i /. Float.of_int (steps - 1) in
                    Float.exp (llo +. t *. (lhi -. llo)))
          in
          List.map vals ~f:(clamp_round spec)
  in
  let rec build specs =
    match specs with
    | [] -> [ String.Map.empty ]
    | spec :: tl ->
        let rest = build tl in
        List.concat_map (points spec) ~f:(fun v ->
            List.map rest ~f:(fun base -> Map.set base ~key:spec.name ~data:v))
  in
  build specs

let perturb_around ~center ~specs ~sigma_frac ~seed =
  let state = Random.State.make [| seed |] in
let sample_dim spec value =
    match spec.bounds with
    | None -> value
    | Some (lo, hi) ->
        let range = hi -. lo in
        let sigma = sigma_frac *. range in
        let noise = Random.State.float state 1.0 |> fun u ->
          (* Box-Muller *)
          let u2 = Random.State.float state 1.0 in
          let z = Float.sqrt (-2. *. Float.log (Float.max u 1e-12)) *. Float.cos (2. *. Float.pi *. u2) in
          z *. sigma
        in
        let proposal = value +. noise in
        clamp_round spec proposal
  in
  List.fold specs ~init:String.Map.empty ~f:(fun acc spec ->
      let base_v = Map.find_exn center spec.name in
      let v = sample_dim spec base_v in
      Map.set acc ~key:spec.name ~data:v)
type sampler = Parameters.t list -> samples:int -> seed:int -> Parameters.value_map list
