open Core
open Types

module Params = Execution_params

type side = Buy | Sell
type rng = Random.State.t

let make_rng ?seed () =
  match seed with
  | None -> Random.State.make_self_init ()
  | Some s -> Random.State.make [| s |]

let path_prices (bar : bar_1m) : float array =
  if Float.(bar.close >= bar.open_) then
    [| bar.open_; bar.high; bar.low; bar.close |]
  else
    [| bar.open_; bar.low; bar.high; bar.close |]

let volume_slices ~(params : Params.t) (bar : bar_1m) : float array =
  let path = path_prices bar in
  match params.volume_model with
  | Params.Equal_quarters ->
      Array.create ~len:(Array.length path) (bar.volume /. 4.)
  | Params.Range_weighted ->
      let weights =
        Array.init (Array.length path) ~f:(fun i ->
            match i with
            | 0 -> Float.abs (path.(1) -. path.(0))
            | 1 -> Float.abs (path.(2) -. path.(1))
            | 2 -> Float.abs (path.(3) -. path.(2))
            | _ -> Float.abs (path.(3) -. path.(2)))
      in
      let total = Array.fold ~init:0.0 ~f:( +. ) weights in
      if Float.(total <= 0.) then Array.create ~len:(Array.length path) (bar.volume /. 4.)
      else
        Array.map weights ~f:(fun w -> bar.volume *. (w /. total))

let adjust_price ~(params : Params.t) ~side ~rng price =
  let slip_ticks =
    match params.slip_model with
    | Params.No_slip -> 0.0
    | Params.Constant_ticks t -> t
    | Params.Prob_one_tick { prob } ->
        if Float.(Random.State.float rng 1.0 <= prob) then 1.0 else 0.0
  in
  let half_spread_ticks = params.spread_ticks /. 2.0 in
  let effective_ticks = half_spread_ticks +. slip_ticks in
  let slip = Params.apply_tick params effective_ticks in
  match side with
  | Buy -> price +. slip
  | Sell -> price -. slip
