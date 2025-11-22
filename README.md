Quantitative Pricing Agents

This repo is about an experimental step in constructing an AI research environment for asset price prediction. The AGENTS.md features a structured environmental setup where the agents are forced to keep inventory and use a functional programming language that’s compile-first.

The AgentsWorkspace folder includes a strategy folder, sitting in an optimization framework with event-based filtering. Backtesting is conducted in a streaming logic.

OCaml is used for: 1) algebraic data types to ensure correctness at compile time, avoiding messy runtime debugging that’s expensive; 2) exhaustive safety and modular induction; 3) agents often hallucinate, OCaml’s correctness-first principle exposes weak points immediately.

The repository contains several ongoing pricing analysis methods that can be seen in the strategy folder.

The way the AGENTS.md is constructed is to ensure efficient modularization/planning, aiming to reduce the usual “bloat” AI-written codes tend to produce.

For the detailed strategy and analysis methods, refer to the strategy.md doc.