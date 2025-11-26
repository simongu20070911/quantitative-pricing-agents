open Types

type config = {
  tick_size : float;
  tick_value : float;
  fee_per_contract : float;
  equity_base : float option;
}

val apply : qty:float -> config -> trade -> trade
