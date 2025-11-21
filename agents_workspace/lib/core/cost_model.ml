open Core
open Types

[@@@warning "-27-32-69"]

type config = {
  tick_size : float;
  tick_value : float;
  slippage_roundtrip_ticks : float;
  fee_per_contract : float;
  equity_base : float option; (** optional account equity to compute pct PnL *)
}

let apply ~(qty : float) (cfg : config) (t : trade) : trade =
  let dollars_per_point = cfg.tick_value /. cfg.tick_size in
  let gross_usd = t.pnl_pts *. dollars_per_point *. qty in
  let cost_slippage_usd = cfg.slippage_roundtrip_ticks *. cfg.tick_value *. qty in
  let cost_fees_usd = cfg.fee_per_contract *. qty in
  let net_usd = gross_usd -. cost_slippage_usd -. cost_fees_usd in
  let net_pts_per_contract = net_usd /. dollars_per_point /. qty in
  let net_R = if Float.(t.r_pts > 0.) then net_pts_per_contract /. t.r_pts else t.pnl_R in
  let pct =
    match cfg.equity_base with
    | None -> None
    | Some eq when Float.(eq = 0.) -> None
    | Some eq -> Some (net_usd /. eq)
  in
  { t with
    qty;
    pnl_pts = net_pts_per_contract;
    pnl_R = net_R;
    pnl_usd = net_usd;
    pnl_pct = pct;
  }
