open Core

[@@@warning "-27-32-69"]

(* Rolling Average Bar Range (ABR) over last N bars. *)
module Abr = struct
  type t = {
    window : float Deque.t;
    n : int;
  }

  let create ~n = { window = Deque.create (); n }

  let update t range =
    Deque.enqueue_back t.window range;
    if Deque.length t.window > t.n then ignore (Deque.dequeue_front t.window)

  let value t =
    if Deque.is_empty t.window then None
    else
      let sum = Deque.fold t.window ~init:0.0 ~f:( +. ) in
      Some (sum /. Float.of_int (Deque.length t.window))
end

(* Rolling Average Daily Range (ADR) for last N daily ranges. *)
module Adr = struct
  type t = {
    window : float Deque.t;
    n : int;
  }

  let create ~n = { window = Deque.create (); n }

  let update t daily_range =
    Deque.enqueue_back t.window daily_range;
    if Deque.length t.window > t.n then ignore (Deque.dequeue_front t.window)

  let value t =
    if Deque.length t.window < t.n then None
    else
      let sum = Deque.fold t.window ~init:0.0 ~f:( +. ) in
      Some (sum /. Float.of_int (Deque.length t.window))
end
