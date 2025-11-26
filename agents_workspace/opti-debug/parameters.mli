open Core

module Scale : sig
  type t = Lin | Log [@@deriving sexp]
end

type domain =
  | Continuous
  | Integer
  | Discrete of float list
  | Categorical of string list
[@@deriving sexp]

type t = {
  name : string;
  default : float;
  bounds : (float * float) option;
  scale : Scale.t;
  fixed : bool;
  integer : bool;
  tunable : bool;
  domain : domain;
  description : string;
}

type value_map = float String.Map.t

val make :
  ?bounds:(float * float) ->
  ?scale:Scale.t ->
  ?fixed:bool ->
  ?integer:bool ->
  ?tunable:bool ->
  ?domain:domain ->
  ?description:string ->
  name:string ->
  default:float ->
  unit ->
  t

val clamp : t -> float -> float

val validate : t -> float -> (unit, string) result

val to_string : t -> string

val default_map : t list -> value_map

val merge_overrides : t list -> value_map -> value_map
