open Core

type objective =
  | Sharpe
  | Mean_pnl
  | Hit_rate
  | Custom of (Types.trade list -> float)

type search =
  | Grid of int
  | Random of { samples : int; seed : int }
  | Latin_hypercube of { samples : int; seed : int }
  | Bayes of { samples : int; seed : int; init : int option; gamma : float option }

type job = {
  strategy_id : string;
  specs : Parameters.t list;
  search : search;
  objective : objective;
  guardrails : Guardrails.t;
  perm_reps : int option;
  bootstrap_reps : int option;
  robustness_bumps : float list option;
  cache : bool;
}

type candidate = {
  params : Parameters.value_map;
  score  : float;
  trades : Types.trade list;
  daily_pnl : (Date.t * float) list;
}

type result = {
  best : candidate;
  tested : int;
  rejected_guardrail : int;
  rejected_robustness : int;
  cache_hits : int;
  guardrails_hash : string;
}

val run :
  job ->
  evaluate:(Parameters.value_map -> candidate) ->
  result
