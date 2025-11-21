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

let default = {
  min_trades = 300;
  max_trial_runs = 200;
  pvalue_threshold = 0.025;
  bootstrap_ci_target = 0.0;
  max_drawdown_R = 15.0;
  robustness_tolerance = 0.7;
  robustness_tol_frac = 0.9;
  cliff_pct = 0.2;
  capacity_impact_bps = 2.0;
}

let float_field name f json =
  match Yojson.Safe.Util.member name json with
  | `Null -> None
  | v -> Some (f v)
  | exception _ -> None

let int_field name json =
  match Yojson.Safe.Util.member name json with
  | `Null -> None
  | v -> Some (Yojson.Safe.Util.to_int v)
  | exception _ -> None

let apply_overrides base json =
  let open Yojson.Safe.Util in
  {
    min_trades = Option.value (int_field "min_trades" json) ~default:base.min_trades;
    max_trial_runs = Option.value (int_field "max_trial_runs" json) ~default:base.max_trial_runs;
    pvalue_threshold =
      Option.value (float_field "pvalue_threshold" to_float json) ~default:base.pvalue_threshold;
    bootstrap_ci_target =
      Option.value (float_field "bootstrap_ci_target" to_float json) ~default:base.bootstrap_ci_target;
    max_drawdown_R =
      Option.value (float_field "max_drawdown_R" to_float json) ~default:base.max_drawdown_R;
    robustness_tolerance =
      Option.value (float_field "robustness_tolerance" to_float json) ~default:base.robustness_tolerance;
    robustness_tol_frac =
      Option.value (float_field "robustness_tol_frac" to_float json) ~default:base.robustness_tol_frac;
    cliff_pct =
      Option.value (float_field "cliff_pct" to_float json) ~default:base.cliff_pct;
    capacity_impact_bps =
      Option.value (float_field "capacity_impact_bps" to_float json) ~default:base.capacity_impact_bps;
  }

let load ?(path = "config/optimization_guardrails.yaml") ?strategy_id () =
  let exists = Stdlib.Sys.file_exists path in
  if not exists then Ok default
  else
    try
      let json = Yojson.Safe.from_file path in
      let defaults =
        Yojson.Safe.Util.member "defaults" json
        |> apply_overrides default
      in
      let merged =
        match strategy_id with
        | None -> defaults
        | Some id ->
            let overrides =
              match Yojson.Safe.Util.member "overrides" json with
              | `Null -> []
              | obj -> Yojson.Safe.Util.to_assoc obj
            in
            List.find overrides ~f:(fun (k, _) -> String.equal k id)
            |> Option.map ~f:(fun (_k, v) -> apply_overrides defaults v)
            |> Option.value ~default:defaults
      in
      Ok merged
    with
    | Yojson.Json_error msg
    | Yojson.Safe.Util.Type_error (msg, _)
    | Sys_error msg -> Error msg

let hash t =
  let json =
    `Assoc [
      "min_trades", `Int t.min_trades;
      "max_trial_runs", `Int t.max_trial_runs;
      "pvalue_threshold", `Float t.pvalue_threshold;
      "bootstrap_ci_target", `Float t.bootstrap_ci_target;
      "max_drawdown_R", `Float t.max_drawdown_R;
      "robustness_tolerance", `Float t.robustness_tolerance;
      "robustness_tol_frac", `Float t.robustness_tol_frac;
      "cliff_pct", `Float t.cliff_pct;
      "capacity_impact_bps", `Float t.capacity_impact_bps;
    ]
  in
  Yojson.Safe.to_string json |> Md5.digest_string |> Md5.to_hex

let%test_unit "apply_overrides merges defaults and overrides" =
  let base = default in
  let override =
    `Assoc [ "min_trades", `Int 10; "pvalue_threshold", `Float 0.1 ]
  in
  let merged = apply_overrides base override in
  assert (merged.min_trades = 10);
  assert (Float.(merged.pvalue_threshold = 0.1))
