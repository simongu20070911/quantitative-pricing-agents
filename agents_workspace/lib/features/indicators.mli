module Abr : sig
  type t
  val create : n:int -> t
  val update : t -> float -> unit
  val value  : t -> float option
end

module Adr : sig
  type t
  val create : n:int -> t
  val update : t -> float -> unit
  val value  : t -> float option
end
