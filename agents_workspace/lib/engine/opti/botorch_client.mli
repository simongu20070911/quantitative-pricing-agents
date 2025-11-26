type domain =
  | Continuous
  | Integer
  | Discrete of float list
  | Categorical of string list

type bounds = {
  name : string;
  lower : float;
  upper : float;
  integer : bool;
  domain : domain;
}
type state

val init :
  ?host:string ->
  ?port:int ->
  bounds:bounds list ->
  batch_size:int ->
  n_regions:int ->
  unit ->
  state

type suggest_result = {
  candidates : float array list;
  region_ids : int list;
  state : state;
}

val suggest :
  ?host:string ->
  ?port:int ->
  state:state ->
  x:float array array ->
  y:float array ->
  unit ->
  suggest_result

val update :
  ?host:string ->
  ?port:int ->
  state:state ->
  y_new:float array ->
  unit ->
  state
