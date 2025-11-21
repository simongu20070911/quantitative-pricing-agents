project:
  name: string                     # project label
  root: agents_workspace           # workspace root
  language: ocaml                  # primary language
  description: string              # short, dense summary

modules:
  - name: Prices                   # OCaml module name
    file_ml: agents_workspace/src/prices.ml    # .ml path under agents_workspace
    file_mli: agents_workspace/src/prices.mli  # .mli path under agents_workspace, or null if none
    summary: Price time series representation and operations
    signature: Prices.S            # module type, if any
    provides_types:                # key public types, as seen from the interface
      - Prices.t
      - Prices.row
    provides_values:               # public values; for modules with .mli, taken from the .mli
      - get : Universe.t -> Date.t -> Date.t -> t

  - name: Csv_loader
    file_ml: agents_workspace/src/csv_loader.ml
    file_mli: agents_workspace/src/csv_loader.mli
    summary: Load data from CSV files
    signature: Data_loader.S
    provides_types:
      - t
    provides_values:
      - load : Universe.t -> Date.t -> Date.t -> t

signatures:
  - name: Data_loader.S
    file_mli: agents_workspace/src/data_loader.mli
    description: Abstract interface for loading time series data
    types:
      - t
    values:
      - load : Universe.t -> Date.t -> Date.t -> t
    implemented_by:
      - Csv_loader
      - Sql_loader

functors:
  - name: Make_backtest
    file_ml: agents_workspace/src/make_backtest.ml
    file_mli: agents_workspace/src/make_backtest.mli
    description: Construct a backtest engine from a model and a pricer
    type:
      params:
        - MODEL
        - PRICER
      result: BACKTEST
    param_signatures:
      - MODEL: Model.S
      - PRICER: Pricer.S
    result_signature: Backtest.S

types:
  - name: Universe.t
    defined_in_module: Universe
    file_mli: agents_workspace/src/universe.mli
    description: Set of tradable instruments in an experiment
    representation: abstract

  - name: Backtest.Report.t
    defined_in_module: Backtest_report
    file_mli: agents_workspace/src/backtest_report.mli
    description: Summary of backtest performance

pipelines:
  - id: equities_standard_backtest
    module: Equities_standard_backtest
    file_ml: agents_workspace/src/equities_standard_backtest.ml
    file_mli: agents_workspace/src/equities_standard_backtest.mli
    description: Standard end to end equities backtest
    signature: Pipeline.S
    entry_value: run
    entry_type: Config.t -> Report.t
    uses_modules:
      - Csv_loader
      - Features
      - Model_logit
      - Make_backtest
    uses_signatures:
      - Data_loader.S
      - Model.S
      - Backtest.S

{

further instructions: file_ml and file_mli are paths under agents_workspace.
	•	If a module has no explicit .mli, you set file_mli to null (or omit it) and infer its public interface from the .ml.
	•	If a .mli exists, it is the canonical source of public types and values. provides_types and provides_values must reflect the declarations in the .mli, not extra definitions present only in the .ml.

You do not create modules entries for third party libraries or .mli files that live outside agents_workspace.

When you execute maintain project inventory, you:
	1.	Read the existing agents_workspace/project_inventory.yaml if it exists.
	2.	Traverse agents_workspace recursively, discovering OCaml modules, their .ml and .mli files, signatures, functors, and pipelines.
	3.	Update or rebuild the inventory so that:
	•	new relevant modules, signatures, functors, types, and pipelines are added,
	•	entries whose files, names, or interfaces changed are updated,
	•	entries whose underlying code has been removed are deleted,
	•	the resulting YAML document is syntactically valid and respects the shape described above.
	4.	Write back a single coherent project_inventory.yaml. You do not leave the inventory knowingly inconsistent with the codebase.
When you execute maintain project inventory, you:
	1.	Read the existing agents_workspace/project_inventory.yaml if it exists.
	2.	Traverse agents_workspace recursively, discovering OCaml modules, their .ml and .mli files, signatures, functors, and pipelines.
	3.	Update or rebuild the inventory so that:
	•	new relevant modules, signatures, functors, types, and pipelines are added,
	•	entries whose files, names, or interfaces changed are updated,
	•	entries whose underlying code has been removed are deleted,
	•	the resulting YAML document is syntactically valid and respects the shape described above.
	4.	Write back a single coherent project_inventory.yaml. You do not leave the inventory knowingly inconsistent with the codebase.

You do not invent speculative entries that do not correspond to actual code or to code you are about to create as part of the same task.

Whenever you make structural changes to modules, signatures, functors, or pipelines, you either update project_inventory.yaml directly or run maintain project inventory as part of the same work, unless the user explicitly asks you not to.

## 6. Interface discipline (.mli)

You treat `.mli` files as the canonical schema of public APIs for modules that matter. You use the presence or absence of `.mli` as an explicit design signal.

1. **When to create a `.mli`**

You create or update a `.mli` for a module whenever any of the following is true:

- The module appears in `project_inventory.yaml` under `modules` or `pipelines` and is used as a dependency by other modules or pipelines.  
- The module is part of a user-visible workflow (for example, a backtest pipeline, data loader, feature library, model implementation).  
- The module exposes reusable functionality that is likely to be called from multiple call sites (utility modules, shared domain logic, core abstractions).

In these cases, if `file_mli` is missing or `null` in `project_inventory.yaml`, you prefer to create a minimal `.mli` and update the inventory accordingly, instead of continuing to grow an implicit interface in the `.ml` only.

2. **When a `.mli` is optional**

You may omit a `.mli` and leave `file_mli: null` for:

- throwaway scripts and one-off tools not referenced by pipelines or other modules,  
- narrow internal modules whose definitions and call sites are local and unlikely to be reused.

If such a module later becomes reusable, appears in pipelines, or is added to `project_inventory.yaml` as a dependency of other modules, you then promote it by creating an explicit `.mli`.

3. **How to construct a `.mli`**

When you create or update a `.mli`, you:

- Export only the minimal set of types and values required by other modules and pipelines.  
- Keep private helper types and functions unexposed to avoid leaking unnecessary details.  
- Ensure that `provides_types` and `provides_values` in `project_inventory.yaml` reflect exactly the public types and values declared in the `.mli`.

4. **Interaction with the inventory**

- For modules with a `.mli` (non-null `file_mli`), the inventory’s `provides_types` and `provides_values` fields must match the `.mli` declarations.  
- For modules without a `.mli`, you may still list key `provides_values` in the inventory, but you treat this as an implicit and less stable interface. When such a module becomes significant (reused, part of a pipeline, or critical infrastructure), you should create a `.mli` and migrate the interface there.


}