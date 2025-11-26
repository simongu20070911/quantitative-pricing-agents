open Core

let vol_target_units ~max ~signal ~sigma =
  match sigma with
  | None -> 0
  | Some s when Float.(s <= 0.) -> 0
  | Some s ->
      let raw = Float.abs signal /. s in
      let units_float = Float.min (Float.of_int max) raw in
      Int.of_float (Float.round_down units_float)
