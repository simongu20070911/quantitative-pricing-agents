open Core

type t = {
  min_trades : int;
  max_trial_runs : int;
  pvalue_threshold : float;
  bootstrap_ci_target : float;
  max_drawdown_R : float;
  robustness_tolerance : float;
  robustness_tol_frac : float;
  cliff_pct : float;
  capacity_impact_bps : float;
}

val default : t

val apply_overrides : t -> Yojson.Safe.t -> t

val load :
  ?path:string ->
  ?strategy_id:string ->
  unit ->
  (t, string) result

val hash : t -> string
