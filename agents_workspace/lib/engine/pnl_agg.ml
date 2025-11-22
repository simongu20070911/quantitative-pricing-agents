open Core
open Types

type t = {
  trades : trade list;
  daily_pnl : float Date.Map.t;
  daily_pnl_usd : float Date.Map.t;
  daily_pnl_pct : float Date.Map.t;
}

let empty = {
  trades = [];
  daily_pnl = Date.Map.empty;
  daily_pnl_usd = Date.Map.empty;
  daily_pnl_pct = Date.Map.empty;
}

let add_trade acc t =
  let daily_pnl =
    Map.update acc.daily_pnl t.date ~f:(function
        | None -> t.pnl_R
        | Some x -> x +. t.pnl_R)
  in
  let daily_pnl_usd =
    Map.update acc.daily_pnl_usd t.date ~f:(function
        | None -> t.pnl_usd
        | Some x -> x +. t.pnl_usd)
  in
  let daily_pnl_pct =
    match t.pnl_pct with
    | None -> acc.daily_pnl_pct
    | Some pct ->
        Map.update acc.daily_pnl_pct t.date ~f:(function
            | None -> pct
            | Some x -> x +. pct)
  in
  {
    trades = t :: acc.trades;
    daily_pnl;
    daily_pnl_usd;
    daily_pnl_pct;
  }

let add_trades acc trades =
  List.fold trades ~init:acc ~f:add_trade

let to_alists_unsorted t =
  (Map.to_alist t.daily_pnl,
   Map.to_alist t.daily_pnl_usd,
   Map.to_alist t.daily_pnl_pct)
