(* Umbrella module exporting curated API *)

module Types = Types
module Time_utils = Time_utils
module Csv_parser = Csv_parser
module Summary = Summary
module Parameters = Parameters
module Guardrails = Guardrails

module Features = struct
  module Indicators = Indicators
  module Features = Features
end

module Core = struct
  module Position_sizing = Position_sizing
  module Cost_model = Cost_model
end

module Engine = struct
  module Engine = Engine
  module Strategy_sig = Strategy_sig
  module Multi_engine = Multi_engine
  module Policy_sig = Policy_sig
  module Backtest = Backtest
  module Trade_transition = Trade_transition
  module Opt_algos = Opt_algos
  module Stat_tests = Stat_tests
  module Robustness = Robustness
  module Optimizer = Optimizer
end

module Plot = struct
  module Plotter = Plotter
end

module Strategies = struct
  module Strategy_b1b2 = Strategy_b1b2
  module Vwap_revert_strategy = Vwap_revert_strategy
end

module Trade_logic = Trade_logic
