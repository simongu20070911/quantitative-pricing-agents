open Types

type config = {
  tick_size : float;
  tick_value : float;
  slippage_roundtrip_ticks : float;
  fee_per_contract : float;
  equity_base : float option;
}

val apply : qty:float -> config -> trade -> trade
