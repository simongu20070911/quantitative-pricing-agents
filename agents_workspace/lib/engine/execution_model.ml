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

let adjust_price ~(params : Params.t) ~side ~rng price =
  let half_spread = Params.apply_tick params (params.spread_ticks /. 2.) in
  let slip_ticks =
    match params.slip_model with
    | Params.No_slip -> 0.0
    | Params.Constant_ticks t -> t
    | Params.Prob_one_tick { prob } ->
        if Float.(Random.State.float rng 1.0 <= prob) then 1.0 else 0.0
  in
  let slip = Params.apply_tick params slip_ticks in
  match side with
  | Buy -> price +. half_spread +. slip
  | Sell -> price -. half_spread -. slip
