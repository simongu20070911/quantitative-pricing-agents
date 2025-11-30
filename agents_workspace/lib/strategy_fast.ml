(* Umbrella module exporting curated API *)

module Types = Types
module Time_utils = Time_utils
module Csv_parser = Csv_parser
module Summary = Summary
module Parameters = Parameters
module Guardrails = Guardrails
module Amplitude_labeler = Amplitude_labeler
module Execution_params = Execution_params
module Execution_model = Execution_model
module Trade_base = Trade_base
module Order_book = Order_book
module Execution_engine = Execution_engine
module Engine_v2 = Engine_v2

module Features = struct
  module Indicators = Indicators
  module Features = Features
end

module Pattern_types = Pattern_types
module Patterns = Patterns
module Gap_patterns = Gap_patterns
module Context = Context

module Core = struct
  module Position_sizing = Position_sizing
  module Cost_model = Cost_model
end

module Engine = struct
  module Engine = Engine_v2
  module Strategy_sig = Strategy_sig
  module Multi_engine = Multi_engine
  module Trade_transition = Trade_transition
  module Execution_params = Execution_params
  module Execution_model = Execution_model
  module Opt_algos = Opt_algos
  module Stat_tests = Stat_tests
  module Robustness = Robustness
  module Optimizer = Optimizer
  module Optimizer_shared = Optimizer_shared
  module Botorch_kernel = Botorch_kernel
  module Botorch_client = Botorch_client
  module Objectives = Objectives
end

module Plot = struct
  module Plotter = Plotter
end

module Cli_helpers = Cli_helpers

module Strategies = struct
  module Strategy_b1b2 = Strategy_b1b2
  module Vwap_revert_strategy = Vwap_revert_strategy
  module Open_range_breakout_strategy = Open_range_breakout_strategy
  module B1b2_params = B1b2_params
  module Setup_builder_b1b2 = Setup_builder_b1b2
end

module Trade_logic = Trade_logic
module Strategy_registry = Strategy_registry
