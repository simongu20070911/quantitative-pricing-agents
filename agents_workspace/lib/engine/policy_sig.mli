open Types

module type S = sig
  type t
  val init_day : setup option -> t
  val on_bar : t -> bar_1m -> t * trade list
  val on_session_end : t -> bar_1m option -> t * trade list
end
