open Types

(** High-level context aggregator that composes intraday features, Brooks-style
    geometric patterns, and daily gap/runaway information into a single
    strictly causal snapshot. *)

type t = {
  feat_state : Features.state;
  patt_state : Patterns.state;
  mutable daily_gap : Gap_patterns.gap_snapshot option;
}

type snapshot = {
  features : Features.snapshot;
  patterns : Pattern_types.snapshot;
  gap      : Gap_patterns.gap_snapshot option;
}

let create () : t =
  { feat_state = Features.create ();
    patt_state = Patterns.create ();
    daily_gap = None;
  }

let update_bar (t : t) (bar : bar_1m) : t =
  let feat_state' = Features.update t.feat_state bar in
  let patt_state' = Patterns.update t.patt_state bar in
  { t with feat_state = feat_state'; patt_state = patt_state' }

let snapshot (t : t) : snapshot =
  { features = Features.snapshot t.feat_state;
    patterns = Patterns.snapshot t.patt_state;
    gap = t.daily_gap;
  }

(** Set the daily gap/runaway context at the RTH open of a new day.

    The caller is responsible for supplying prior day's high/low/close and the
    current day's open, plus optional ATR20 and 20-day trend magnitudes. *)
let set_daily_gap
    (t : t)
    ~(prev_high : float)
    ~(prev_low : float)
    ~(prev_close : float)
    ~(open_ : float)
    ?atr20
    ?trend20
    () : unit =
  let gap =
    Gap_patterns.classify_gap
      ~prev_high ~prev_low ~prev_close ~open_ ?atr20 ?trend20 ()
  in
  t.daily_gap <- Some gap
