(** Direction of a single bar in pattern space. *)
type bar_trend =
  | Bull
  | Bear
  | Doji

(** Basic OHLC-derived geometry for a bar. *)
type bar_shape = {
  trend      : bar_trend;
  body       : float;
  range      : float;
  upper_wick : float;
  lower_wick : float;
}

(** Side of a wedge-style pattern. *)
type wedge_side =
  | Up   (** bullish wedge, three pushes up followed by expectable reversal down *)
  | Down (** bearish wedge, three pushes down followed by expectable reversal up *)

(** Minimal metadata for a detected wedge on a discrete bar sequence. *)
type wedge = {
  side       : wedge_side;
  last_index : int;   (** index (in the original sequence) of the last bar of the wedge *)
  length     : int;   (** number of bars spanned by the wedge pattern *)
}

(** Coarse leg direction at the current bar. *)
type leg_side =
  | Leg_up
  | Leg_down
  | Leg_none

(** High-level trading-range vs trend-day style regime. *)
type range_regime =
  | Range
  | Trend
  | Regime_unknown

(** Coarse day-level regime (RTH) in Brooks sense. *)
type day_regime =
  | Day_spike     (** early spike dominates range and net change *)
  | Day_trend     (** strong net trend day without a huge initial spike *)
  | Day_range     (** mostly sideways trading range day *)
  | Day_unknown

(** Intraday phase within the RTH session. *)
type intraday_phase =
  | Pre_rth
  | Rth_open
  | Rth_mid
  | Rth_late
  | Post_rth

(** Always-in state in Brooks sense. *)
type ai_state =
  | Ai_long
  | Ai_short
  | Ai_flat

(** Causal snapshot of Brooks-style geometric context for a single bar. *)
type snapshot = {
  leg_side        : leg_side;
  leg_len_bars    : int;           (** bars since leg start, 0 if unknown *)
  leg_range       : float;         (** price move in leg direction, 0 if unknown *)
  range_regime    : range_regime;
  pos_in_range    : float option;  (** position in recent range [0,1], 0=bottom,1=top *)
  range_tests_high : int;          (** recent tests of range high *)
  range_tests_low  : int;          (** recent tests of range low *)
  range_failed_high : int;         (** recent failed breakouts above range high *)
  range_failed_low  : int;         (** recent failed breakouts below range low *)
  range_tight       : bool;        (** narrow trading range vs broader structure *)
  range_test_rate_high : float;    (** tests_high / window_size *)
  range_test_rate_low  : float;    (** tests_low / window_size *)
  range_fail_rate_high : float;    (** fails_high / window_size *)
  range_fail_rate_low  : float;    (** fails_low / window_size *)
  range_width_ratio   : float;     (** range width / avg bar range in window; >1 = wide, <1 = tight *)
  day_regime      : day_regime;    (** spike / trend / range classification for current RTH day *)
  day_net_change  : float option;  (** day_close - day_open in points (RTH), if available *)
  day_range       : float option;  (** day_high - day_low in points (RTH), if available *)
  day_pos_in_range : float option; (** position of close in day range [0,1], 0=low,1=high *)
  day_inside_prev_range : bool option; (** whether today's close is inside prior RTH high/low *)
  day_trend_run_len : int;         (** #consecutive prior trend/spike days, 0 if none *)
  dist_prev_day_high : float option; (** close - prev_day_high, if available *)
  dist_prev_day_low  : float option; (** close - prev_day_low, if available *)
  intraday_phase : intraday_phase; (** coarse phase: open/mid/late etc. *)
  wedge_up        : bool;
  wedge_down      : bool;
  h1              : bool;          (** first higher-low / pullback attempt in current leg *)
  h2              : bool;          (** second higher-low / pullback attempt in current leg *)
  l1              : bool;          (** first lower-high attempt in down leg *)
  l2              : bool;          (** second lower-high attempt in down leg *)
  h_pullback_count : int;          (** persistent count of higher-low pullbacks in up leg *)
  l_pullback_count : int;          (** persistent count of lower-high pullbacks in down leg *)
  micro_up_len    : int;           (** consecutive up closes *)
  micro_down_len  : int;           (** consecutive down closes *)
  soft_micro_up_len : int;         (** tolerant bull microchannel length *)
  soft_micro_down_len : int;       (** tolerant bear microchannel length *)
  soft_micro_up_len_max_leg : int;   (** max tolerant bull length in current leg *)
  soft_micro_down_len_max_leg : int; (** max tolerant bear length in current leg *)
  soft_micro_bias : int;           (** soft_micro_up_len - soft_micro_down_len *)
  inside_bar      : bool;          (** current bar inside prior bar's range *)
  outside_bar     : bool;          (** current bar outside prior bar's range *)
  ii_pattern      : bool;          (** inside-inside cluster just completed *)
  ioi_pattern     : bool;          (** inside-outside-inside cluster just completed *)
  consecutive_outside : bool;      (** part of a run of outside bars *)
  bar_body        : float;         (** current bar body = close - open *)
  bar_range       : float;         (** current bar range = high - low *)
  bar_body_frac   : float option;  (** |body| / range when range > 0 *)
  bar_close_pos   : float option;  (** (close - low) / range in [0,1] when range > 0 *)
  bar_is_trend    : bool;          (** large body vs range, simple trend-bar proxy *)
  bar_is_doji     : bool;          (** very small body vs range *)
  micro_channel_midline : float option;  (** regression midline over current bar-level leg *)
  micro_channel_slope   : float option;  (** slope of bar-level regression channel *)
  micro_channel_z       : float option;  (** |close-midline| / avg_range; channel residual in bar-range units *)
  micro_channel_overshoot : bool;        (** rare flag when micro_channel_z is large *)
  major_channel_midline : float option;  (** swing-based major channel line at this bar *)
  major_channel_slope   : float option;  (** slope of swing-based major channel *)
  major_channel_z       : float option;  (** |close-midline| / avg_range for major channel *)
  major_channel_overshoot : bool;        (** close far beyond major channel in either direction *)
  leg_mm_up      : float option;   (** latest completed up-leg measured-move target, if any *)
  leg_mm_down    : float option;   (** latest completed down-leg measured-move target, if any *)
  range_mid      : float option;   (** current window range midpoint, TR magnet *)
  range_mm_up    : float option;   (** range high + width (1x range extension up) *)
  range_mm_down  : float option;   (** range low - width (1x range extension down) *)
  soft_break_up_severity   : float option; (** last bull soft microchannel break severity (ATR-like units) *)
  soft_break_up_trend      : bool;         (** whether last bull break bar was trend bar *)
  soft_break_down_severity : float option; (** last bear soft microchannel break severity (ATR-like units) *)
  soft_break_down_trend    : bool;         (** whether last bear break bar was trend bar *)
  recent_bull_count : int;         (** count of bull bars in recent window *)
  recent_bear_count : int;         (** count of bear bars in recent window *)
  recent_doji_count : int;         (** count of doji/small bars in recent window *)
  recent_body_sum   : float;       (** sum of bodies (close-open) over recent window *)
  recent_strength_score : float option; (** avg signed body over recent non-doji bars *)
  always_in       : ai_state;      (** Always-in bias: long/short/flat *)
  htf_leg_side_5m     : leg_side;  (** coarser 5m leg direction *)
  htf_leg_len_bars_5m : int;       (** coarser 5m leg length in bars *)
  htf_wedge_up_5m     : bool;      (** simple 5m wedge up on tail *)
  htf_wedge_down_5m   : bool;      (** simple 5m wedge down on tail *)
}
