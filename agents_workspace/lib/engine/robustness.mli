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

val run :
  specs:Parameters.t list ->
  bump_factors:float list ->
  tol_frac:float ->
  cliff_pct:float ->
  evaluate:(Parameters.value_map -> float) ->
  ?baseline_score:float ->
  Parameters.value_map ->
  result
