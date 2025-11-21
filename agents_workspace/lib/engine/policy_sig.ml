open Types

module type S = sig
  type t

  (** Called at start of each trading day (may be None if no setup). *)
  val init_day : setup option -> t

  (** Process one bar, possibly returning completed trades. *)
  val on_bar : t -> bar_1m -> t * trade list

  (** Triggered at session end to force exits/cleanup. [last_bar] is the final
      session bar observed for the day if any. *)
  val on_session_end : t -> bar_1m option -> t * trade list
end
