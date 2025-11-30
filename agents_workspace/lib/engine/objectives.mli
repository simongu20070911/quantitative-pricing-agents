(* Central registry of optimizer objectives with parsing/printing helpers. *)

type t = Optimizer.objective

type desc = {
  id : string;
  objective : t;
  description : string;
}

val all : unit -> desc list

val find_exn : string -> t

val to_string : t -> string
