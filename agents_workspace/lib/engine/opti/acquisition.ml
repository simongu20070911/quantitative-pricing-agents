open Core
open Lacaml.D

(* Batch Thompson sampling using GP draws at candidate locations. *)

let gaussian ~rng () =
  let u1 = Float.max (Random.State.float rng 1.0) 1e-12 in
  let u2 = Random.State.float rng 1.0 in
  let r = Float.sqrt (-2. *. Float.log u1) in
  let theta = 2. *. Float.pi *. u2 in
  r *. Float.cos theta

let thompson_sample ~(gp : Gp.t) ~(xs : float array array) ~(rng:Random.State.t) : int * Vec.t =
  let n = Array.length gp.x in
  let m = Array.length xs in
  let xs =
    Array.map xs ~f:(fun x -> Gp.normalize_input gp x)
  in
  (* Build K_* (n x m) and K_** diagonal *)
  let k_star = Mat.create n m in
  let k_diag = Vec.create m in
  let k_ss = Mat.create m m in
  for j = 1 to m do
    let xj = xs.(j-1) in
    k_diag.{j} <- Gp.kernel ~hyper:gp.hyper xj xj;
    for i = 1 to n do
      k_star.{i,j} <- Gp.kernel ~hyper:gp.hyper gp.x.(i-1) xj
    done
  done;
  for j = 1 to m do
    for i = 1 to j do
      let v = Gp.kernel ~hyper:gp.hyper xs.(i-1) xs.(j-1) in
      k_ss.{i,j} <- v;
      if i <> j then k_ss.{j,i} <- v
    done
  done;
  (* Compute predictive mean/var via solves *)
  let mean = Vec.create m in
  for j = 1 to m do
    let col = Mat.col k_star j in
    mean.{j} <- dot col gp.alpha
  done;
  let tmp = Lacaml.D.lacpy k_star in
  (* v = L^{-1} K_* *)
  trsm ~side:`L ~up:false ~transa:`N ~diag:`N gp.chol tmp;
  for j = 1 to m do
    let col = Mat.col tmp j in
    let var = k_diag.{j} -. (dot col col) in
    k_diag.{j} <- Float.max var 0.
  done;
  (* Full covariance: K_ss - K_s^T (K + sn^2 I)^{-1} K_s *)
  let cov = Lacaml.D.lacpy k_ss in
  let cross = gemm ~transa:`T tmp tmp in
  for i = 1 to m do
    for j = 1 to m do
      cov.{i,j} <- cov.{i,j} -. cross.{i,j}
    done
  done;
  let max_diag =
    let md = ref 0. in
    for i = 1 to m do
      md := Float.max !md cov.{i,i}
    done;
    !md
  in
  let jitter = Float.max 1e-12 (1e-6 *. max_diag) in
  for i = 1 to m do
    cov.{i,i} <- cov.{i,i} +. jitter
  done;
  let z = Vec.init m (fun _ -> gaussian ~rng ()) in
  (try
     potrf ~up:false cov;
     let delta = Vec.create m in
     for i = 1 to m do delta.{i} <- z.{i} done;
     trmv ~up:false ~trans:`N ~diag:`N cov delta;
     for j = 1 to m do
       z.{j} <- mean.{j} +. delta.{j}
     done
   with _ ->
     (* Fallback to independent draws if covariance is not SPD. *)
     for j = 1 to m do
       z.{j} <- mean.{j} +. Float.sqrt k_diag.{j} *. z.{j}
     done);
  let best_idx = ref 0 in
  let best = ref Float.neg_infinity in
  for j = 1 to m do
    let v = z.{j} in
    if Float.(v > !best) then (best := v; best_idx := j - 1)
  done;
  !best_idx, z
