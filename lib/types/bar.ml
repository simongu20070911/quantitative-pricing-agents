(*The type of candlestick bars* with timestamp, open, high, low, close, volume,interval_length*)
type candlestick_bar = {
  timestamp: float;
  open_price: float;
  high_price: float;
  low_price: float;
  close_price: float;
  volume: float;
  interval_length: float;
}

