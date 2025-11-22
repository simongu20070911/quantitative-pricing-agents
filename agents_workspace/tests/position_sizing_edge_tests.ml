open Core
open Strategy_fast

module PS = Core.Position_sizing

let%test_unit "vol_target_units rounds down not nearest" =
  [%test_eq: int] (PS.vol_target_units ~max:10 ~signal:4.9 ~sigma:(Some 1.0)) 4

let%test_unit "vol_target_units respects max cap even with huge signal" =
  [%test_eq: int] (PS.vol_target_units ~max:3 ~signal:1000.0 ~sigma:(Some 0.01)) 3

let%test_unit "vol_target_scale mirrors vol_target_units magnitude" =
  let units = PS.vol_target_units ~max:5 ~signal:2.5 ~sigma:(Some 0.5) in
  let s = PS.vol_target_scale ~signal:2.5 ~sigma:(Some 0.5) in
  assert (Float.(s >= Float.of_int units));
  assert (Float.(s < Float.of_int units +. 1.0))
