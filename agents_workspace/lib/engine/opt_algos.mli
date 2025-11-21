type sampler = Parameters.t list -> samples:int -> seed:int -> Parameters.value_map list

val random_uniform : sampler
(** IID uniform within bounds (log-space if scale=Log; integer rounded). *)

val latin_hypercube : sampler
(** Simple LHS stratification per dimension; reduces clumping vs IID. *)

val grid :
  steps:int ->
  sampler
(** Cartesian grid with [steps] points per bounded dimension; unbounded params use defaults. *)

val perturb_around :
  center:Parameters.value_map ->
  specs:Parameters.t list ->
  sigma_frac:float ->
  seed:int ->
  Parameters.value_map
(** Gaussian perturbation per dim around a center. Sigma is sigma_frac * range. Clamped & integer aware. Useful for adaptive samplers. *)
