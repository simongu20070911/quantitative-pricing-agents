Quantitative Pricing Agents

This repo is about an experimental step in constructing an AI research environment on asset price prediction. the AGENTS.md features a structured environmental setup where the agents are forced to keep inventory, use a functional programming language that's compile-first.

the Agentsworkspace folder includes a strategy folder, sitting in a optimization framework, with event based filtering. backtesting is conducted in a streaming logic. 

Ocaml is used for 1: algebraic data types to ensure correctness at compile time, avoiding messy runtime debugging that's expensive
2: exhaustive safety and modular induction 3: agents often hallucinate, ocaml's correctness first principle exposes weak points immediately. 


The repository contains several ongoing pricing analysis methods that can be seen in the strategy folder. 

The way the AGENTS.md constructed is to ensure efficient modularization\planning, aiming to reduce the usual "bloat" AI written codes tend to produce. 

for the detailed strategy and analysis methods, refer to the strategy.md doc. 
