open Core

type pack = {
  id : string;
  specs : Parameters.t list;
  build : Parameters.value_map -> Engine.pure_strategy;
}

let registry : pack list =
  [
    {
      id = Strategy_b1b2.strategy_id;
      specs = Strategy_b1b2.parameter_specs;
      build = (fun params ->
          let cfg = Strategy_b1b2.config_of_params params in
          Strategy_b1b2.pure_strategy cfg);
    };
    {
      id = Vwap_revert_strategy.strategy_id;
      specs = Vwap_revert_strategy.parameter_specs;
      build = (fun params ->
          let cfg = Vwap_revert_strategy.config_of_params params in
          Vwap_revert_strategy.pure_strategy cfg);
    };
  ]

let find id = List.find registry ~f:(fun s -> String.equal s.id id)

let find_exn id = List.find_exn registry ~f:(fun s -> String.equal s.id id)

let all () = registry
