(** Domain constants for the B1/B2 strategy, kept in one place to avoid magic numbers. *)

val tick_size : float
val tick_value : float

val be_trigger_mult : float           (** break-even trigger in R *)
val downgrade_cutoff_offset_min : int (** minutes after b2_min to allow downgrade *)
val two_r_range_factor : float        (** allow 2R target if B1 range <= factor * ABR_prev *)
val climactic_range_factor : float    (** B1 range must be <= factor * ABR_prev to be valid *)
val gap_min_pct_adr : float           (** min |gap| as % of ADR to qualify *)
val gap_max_pct_adr : float           (** max |gap| as % of ADR to qualify *)
val body_pct_min : float              (** minimum body / range to count as trend *)
val ibs_bull_min : float              (** IBS threshold for bullish B1 *)
val ibs_bear_max : float              (** IBS threshold for bearish B1 *)

val abr_window_n : int
val adr_window_n : int
