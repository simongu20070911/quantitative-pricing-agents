val permutation_pvalue :
  ?reps:int ->
  ?seed:int ->
  metric:(float list -> float) ->
  float list ->
  float

val bootstrap_ci :
  ?reps:int ->
  ?alpha:float ->
  ?seed:int ->
  float list ->
  float * float
