open Core
open Lacaml.D

(* ARD Matérn-5/2 kernel with ML-II hyperparameter optimization and optional input/output normalization. *)

type hyper = {
  log_sigma_f : float;        (* signal std dev *)
  log_ell : float array;      (* per-dim log lengthscale *)
  log_sigma_n : float;        (* noise std dev *)
}

let default_hyper ~dim =
  { log_sigma_f = 0.0; log_ell = Array.create ~len:dim 0.0; log_sigma_n = log 1e-3 }

let sigma_f2 h = Float.exp (2. *. h.log_sigma_f)
let sigma_n2 h = Float.exp (2. *. h.log_sigma_n)

let matern52_ard ~hyper x y =
  let a = Float.sqrt 5. in
  let b = 5. /. 3. in
  let r2 = ref 0. in
  for i = 0 to Array.length x - 1 do
    let ell = Float.exp hyper.log_ell.(i) in
    let d = (x.(i) -. y.(i)) /. ell in
    r2 := !r2 +. d *. d
  done;
  let r = Float.sqrt !r2 in
  let c = 1. +. a *. r +. b *. !r2 in
  sigma_f2 hyper *. c *. Float.exp (-. a *. r)

let kernel = matern52_ard

type t = {
  x : float array array;      (* normalized training inputs *)
  y_norm : float array;       (* normalized outputs *)
  chol : mat;                 (* lower Cholesky of K + sigma_n^2 I *)
  alpha : vec;                (* (K + sn^2 I)^{-1} y_norm *)
  hyper : hyper;
  normalize_x : bool;
  x_mean : float array;
  x_scale : float array;
  y_mean : float;
  y_std : float;
}

let normalize_point ~mean ~scale v =
  Array.mapi v ~f:(fun i xi -> (xi -. mean.(i)) /. scale.(i))

let compute_normalization ~normalize (x : float array array) =
  let dim = Array.length x.(0) in
  let mean = Array.create ~len:dim 0. in
  let m = Float.of_int (Array.length x) in
  if normalize then (
    Array.iter x ~f:(fun v ->
        for i = 0 to dim - 1 do
          mean.(i) <- mean.(i) +. v.(i)
        done);
    for i = 0 to dim - 1 do
      mean.(i) <- mean.(i) /. m
    done);
  let var = Array.create ~len:dim 0. in
  if normalize then (
    Array.iter x ~f:(fun v ->
        for i = 0 to dim - 1 do
          let d = v.(i) -. mean.(i) in
          var.(i) <- var.(i) +. (d *. d)
        done);
    for i = 0 to dim - 1 do
      var.(i) <- var.(i) /. Float.max 1. m
    done);
  let scale =
    Array.map var ~f:(fun v ->
        if Float.(v <= 0.) then 1. else Float.sqrt v |> Float.max 1e-12)
  in
  mean, scale

let normalize_inputs ~normalize ~mean ~scale x =
  if normalize then Array.map x ~f:(normalize_point ~mean ~scale) else Array.copy x

let normalize_dataset ~normalize x =
  let mean, scale = compute_normalization ~normalize x in
  normalize_inputs ~normalize ~mean ~scale x, mean, scale

let standardize_y y =
  let n = Array.length y in
  let mu = Array.fold y ~init:0. ~f:( +. ) /. Float.of_int n in
  let var =
    Array.fold y ~init:0. ~f:(fun acc v -> acc +. (v -. mu) ** 2.) /. Float.max 1. (Float.of_int n)
  in
  let sigma = Float.sqrt var |> Float.max 1e-12 in
  let yn = Array.map y ~f:(fun v -> (v -. mu) /. sigma) in
  yn, mu, sigma

let build_k ~hyper x =
  let n = Array.length x in
  let k = Mat.create n n in
  for i = 1 to n do
    for j = 1 to i do
      let v = kernel ~hyper x.(i-1) x.(j-1) in
      k.{i,j} <- v;
      if i <> j then k.{j,i} <- v
    done
  done;
  k

let fit ?(jitter=1e-8) ?(normalize=false) ~hyper x y =
  let n = Array.length x in
  if n = 0 then invalid_arg "Gp.fit: empty training set";
  let x, mean, scale = normalize_dataset ~normalize x in
  let y_norm, y_mean, y_std = standardize_y y in
  let k = build_k ~hyper x in
  let sigma_n2 = sigma_n2 hyper in
  let diag_max = ref 0. in
  for i = 1 to n do
    k.{i,i} <- k.{i,i} +. sigma_n2;
    diag_max := Float.max !diag_max k.{i,i}
  done;
  let jitter_eff = Float.max jitter (1e-6 *. !diag_max) in
  for i = 1 to n do
    k.{i,i} <- k.{i,i} +. jitter_eff
  done;
  let rec factor ?(bump=1.) () =
    let chol = Lacaml.D.lacpy k in
    if Float.(bump <> 1.) then
      for i = 1 to n do
        chol.{i,i} <- chol.{i,i} +. (bump -. 1.) *. jitter_eff
      done;
    try
      potrf ~up:false chol;
      chol
    with exn ->
      if Float.(bump < 1e6) then factor ~bump:(bump *. 10.) () else raise exn
  in
  let chol = factor () in
  let yv = Vec.of_array y_norm in
  let rhs = Mat.of_col_vecs [| yv |] in
  potrs ~up:false chol rhs;
  let alpha = Mat.col rhs 1 |> Vec.to_array |> Vec.of_array in
  { x; y_norm; chol; alpha; hyper; normalize_x = normalize; x_mean = mean; x_scale = scale; y_mean; y_std }

let normalize_input gp x = normalize_point ~mean:gp.x_mean ~scale:gp.x_scale x

let predict gp x_star =
  let x_star = normalize_input gp x_star in
  let n = Array.length gp.x in
  let k_star = Vec.create n in
  for i = 1 to n do
    k_star.{i} <- kernel ~hyper:gp.hyper gp.x.(i-1) x_star
  done;
  let mean_norm = dot k_star gp.alpha in
  let v = Vec.create n in
  for i = 1 to n do v.{i} <- k_star.{i} done;
  trsv ~up:false ~trans:`N ~diag:`N gp.chol v;
  let k_ss = kernel ~hyper:gp.hyper x_star x_star in
  let var_norm = k_ss -. (dot v v) in
  let mean = gp.y_mean +. gp.y_std *. mean_norm in
  let var = Float.max 0. (var_norm *. gp.y_std *. gp.y_std) in
  mean, var

(* ---------- Hyperparameter optimization (ML-II) ---------- *)

type grad = { d_log_sigma_f : float; d_log_ell : float array; d_log_sigma_n : float }

let log_marginal_and_grad ?(jitter=1e-8) ?(normalize=false) ~hyper x y =
  let n = Array.length x in
  let x, _, _ = normalize_dataset ~normalize x in
  let y_norm, _ym, _ys = standardize_y y in
  let k_sig = build_k ~hyper x in
  let sn2 = sigma_n2 hyper in
  let diag_max = ref 0. in
  for i = 1 to n do
    k_sig.{i,i} <- k_sig.{i,i} +. sn2;
    diag_max := Float.max !diag_max k_sig.{i,i}
  done;
  let jitter_eff = Float.max jitter (1e-6 *. !diag_max) in
  for i = 1 to n do
    k_sig.{i,i} <- k_sig.{i,i} +. jitter_eff
  done;
  let rec factor ?(bump=1.) () =
    let m = Lacaml.D.lacpy k_sig in
    if Float.(bump <> 1.) then
      for i = 1 to n do
        m.{i,i} <- m.{i,i} +. (bump -. 1.) *. jitter_eff
      done;
    try
      potrf ~up:false m;
      m, bump
    with exn ->
      if Float.(bump < 1e6) then factor ~bump:(bump *. 10.) () else raise exn
  in
  let chol, bump = factor () in
  let yv = Vec.of_array y_norm in
  let rhs = Mat.of_col_vecs [| yv |] in
  potrs ~up:false chol rhs;
  let alpha = Mat.col rhs 1 in
  let logdet =
    let s = ref 0. in
    for i = 1 to n do
      s := !s +. log chol.{i,i}
    done;
    2. *. !s
  in
  let quad = dot yv alpha in
  let logp = -.0.5 *. quad -. 0.5 *. logdet -. 0.5 *. (Float.of_int n) *. log (2. *. Float.pi) in
  let kinv = Lacaml.D.lacpy chol in
  potri ~up:false kinv;
  let grad_sf = ref 0. in
  let grad_sn = ref 0. in
  let grad_ell = Array.create ~len:(Array.length hyper.log_ell) 0. in
  let a = Float.sqrt 5. in
  let b = 5. /. 3. in
  for i = 1 to n do
    for j = 1 to i do
      let xi = x.(i-1) and xj = x.(j-1) in
      let r2 = ref 0. in
      for d = 0 to Array.length xi - 1 do
        let ell = Float.exp hyper.log_ell.(d) in
        let q = (xi.(d) -. xj.(d)) /. ell in
        r2 := !r2 +. q *. q
      done;
      let r = Float.sqrt !r2 in
      let k_ij =
        if i = j then k_sig.{i,j} -. sn2 -. (bump *. jitter_eff) else k_sig.{i,j}
      in
      let s_ij = alpha.{i} *. alpha.{j} -. kinv.{i,j} in
      let weight = if i = j then 1. else 2. in
      grad_sf := !grad_sf +. 0.5 *. weight *. s_ij *. (2. *. k_ij);
      let denom = 1. +. a *. r +. b *. !r2 in
      if Float.(denom <> 0.) then
        for d = 0 to Array.length xi - 1 do
          let ell = Float.exp hyper.log_ell.(d) in
          let q = (xi.(d) -. xj.(d)) /. ell in
          let qi = q *. q in
          let factor = k_ij *. qi *. (5. /. 3.) *. (1. +. a *. r) /. denom in
          grad_ell.(d) <- grad_ell.(d) +. 0.5 *. weight *. s_ij *. factor
        done;
      if i = j then (
        let d_sn = 2. *. sn2 in
        grad_sn := !grad_sn +. 0.5 *. s_ij *. d_sn)
    done
  done;
  let grad = { d_log_sigma_f = !grad_sf; d_log_ell = grad_ell; d_log_sigma_n = !grad_sn } in
  logp, grad

let clamp_log (lo, hi) v = Float.max lo (Float.min hi v)

let optimize_hyper
    ?(normalize=false)
    ?(jitter=1e-8)
    ?(max_iter=100)
    ?(lr=0.05)
    ?(beta1=0.9)
    ?(beta2=0.999)
    ?(eps=1e-8)
    ?(log_bounds=(-20., 5.))
    x y init_hyper =
  let dim = Array.length init_hyper.log_ell in
  let m1_sf = ref 0. and m2_sf = ref 0. in
  let m1_sn = ref 0. and m2_sn = ref 0. in
  let m1_ell = Array.create ~len:dim 0. in
  let m2_ell = Array.create ~len:dim 0. in
  let best = ref (Float.neg_infinity, init_hyper) in
  let hyper = ref init_hyper in
  for t = 1 to max_iter do
    let logp, g = log_marginal_and_grad ~normalize ~jitter ~hyper:!hyper x y in
    if Float.(logp > fst !best) then best := (logp, !hyper);
    let b1t = 1. -. (beta1 ** Float.of_int t) in
    let b2t = 1. -. (beta2 ** Float.of_int t) in
    let upd_scalar m1 m2 gcomp =
      m1 := beta1 *. !m1 +. (1. -. beta1) *. gcomp;
      m2 := beta2 *. !m2 +. (1. -. beta2) *. (gcomp *. gcomp);
      let m1h = !m1 /. b1t in
      let m2h = !m2 /. b2t in
      lr *. m1h /. (Float.sqrt m2h +. eps)
    in
    let step_sf = upd_scalar m1_sf m2_sf g.d_log_sigma_f in
    let step_sn = upd_scalar m1_sn m2_sn g.d_log_sigma_n in
    let step_ell =
      Array.mapi g.d_log_ell ~f:(fun i ge ->
          let m1 = ref m1_ell.(i) in
          let m2 = ref m2_ell.(i) in
          let s = upd_scalar m1 m2 ge in
          m1_ell.(i) <- !m1; m2_ell.(i) <- !m2; s)
    in
    let next =
      {
        log_sigma_f = clamp_log log_bounds (!hyper.log_sigma_f +. step_sf);
        log_sigma_n = clamp_log log_bounds (!hyper.log_sigma_n +. step_sn);
        log_ell =
          Array.mapi step_ell ~f:(fun i s ->
              clamp_log log_bounds (!hyper.log_ell.(i) +. s));
      }
    in
    hyper := next;
    if Float.(abs step_sf < 1e-6 && abs step_sn < 1e-6) &&
       Array.for_all step_ell ~f:(fun s -> Float.(abs s < 1e-6))
    then ()
  done;
  snd !best

let optimize_and_fit ?normalize ?jitter ?max_iter ?lr ?beta1 ?beta2 ?eps ?log_bounds x y init_hyper =
  let hyper =
    optimize_hyper ?normalize ?jitter ?max_iter ?lr ?beta1 ?beta2 ?eps ?log_bounds x y init_hyper
  in
  let gp = fit ?normalize ?jitter ~hyper x y in
  let log_mll, _ = log_marginal_and_grad ?normalize ?jitter ~hyper x y in
  gp, hyper, log_mll
