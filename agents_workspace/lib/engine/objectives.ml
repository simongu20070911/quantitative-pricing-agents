open Core

type t = Optimizer.objective

type desc = {
  id : string;
  objective : t;
  description : string;
}

let registry : desc list = [
  {
    id = "sharpe";
    objective = Optimizer.Sharpe;
    description = "Full-sample Sharpe on trade pnls.";
  };
  {
    id = "mean_pnl";
    objective = Optimizer.Mean_pnl;
    description = "Average R per trade.";
  };
  {
    id = "hit_rate";
    objective = Optimizer.Hit_rate;
    description = "Win rate across trades.";
  };
  {
    id = "year_sharpe";
    objective = Optimizer.Year_sharpe { lambda = 0.55; min_days = 40 };
    description = "Year-by-year Sharpe with lambda=0.55 and min_days=40; coverage-weighted (see Optimizer).";
  };
  {
    id = "simon_ratio";
    objective = Optimizer.Simon_ratio { lambda = 1.5 };
    description = "Log-growth minus drawdown penalty: J = g - lambda * MDD, where g is mean log(1+r_t) over the equity path and MDD is max drawdown fraction.";
  };
  {
    id = "trade_mean_lcb";
    objective = Optimizer.Mean_trade_lcb { confidence = 0.99 };
    description = "One-sided lower confidence bound on mean trade pnl_R: mean - t_crit(conf) * se; default conf=0.99.";
  };
]

let all () = registry

let parse_trade_mean_lcb id =
  match String.lsplit2 id ~on:'@' with
  | Some ("trade_mean_lcb", conf_s) -> (
      try
        let c = Float.of_string conf_s in
        if Float.(c <= 0.5 || c >= 1.) then None
        else Some (Optimizer.Mean_trade_lcb { confidence = c })
      with _ -> None)
  | _ -> None

let find_exn id =
  match parse_trade_mean_lcb id with
  | Some obj -> obj
  | None ->
      (match List.find registry ~f:(fun d -> String.(d.id = id)) with
       | Some d -> d.objective
       | None -> failwithf "unknown objective %s" id ())

let to_string (obj : t) =
  match obj with
  | Optimizer.Sharpe -> "sharpe"
  | Optimizer.Mean_pnl -> "mean_pnl"
  | Optimizer.Hit_rate -> "hit_rate"
  | Optimizer.Year_sharpe _ -> "year_sharpe"
  | Optimizer.Simon_ratio _ -> "simon_ratio"
  | Optimizer.Mean_trade_lcb { confidence } ->
      if Float.(confidence = 0.99) then "trade_mean_lcb"
      else Printf.sprintf "trade_mean_lcb@%.6f" confidence
  | Optimizer.Custom _ -> "custom"
