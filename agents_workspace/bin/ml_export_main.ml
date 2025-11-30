open Core
open Strategy_fast
module PT = Strategy_fast.Pattern_types

module T = Types
module Time_utils = Time_utils
module Ctx = Context
module Amp = Amplitude_labeler

let rth_start = Time_utils.rth_start_min
let rth_end   = Time_utils.rth_end_min

type hit_first =
  | Hit_up
  | Hit_down
  | Hit_both
  | Hit_none

let string_of_hit_first = function
  | Hit_up -> "up"
  | Hit_down -> "down"
  | Hit_both -> "both"
  | Hit_none -> "none"

let compute_tr (bars : T.bar_1m array) =
  let n = Array.length bars in
  let tr = Array.create ~len:n 0.0 in
  for i = 0 to n - 1 do
    let b = bars.(i) in
    let h = b.T.high and l = b.T.low in
    let prev_close =
      if i = 0 then b.T.close else bars.(i - 1).T.close
    in
    let r1 = h -. l in
    let r2 = Float.abs (h -. prev_close) in
    let r3 = Float.abs (prev_close -. l) in
    tr.(i) <- Float.max r1 (Float.max r2 r3)
  done;
  tr

let compute_atr tr ~period =
  let n = Array.length tr in
  let atr = Array.create ~len:n None in
  let sum = ref 0.0 in
  for i = 0 to n - 1 do
    sum := !sum +. tr.(i);
    if i >= period then sum := !sum -. tr.(i - period);
    if i >= period - 1 then
      let v = !sum /. Float.of_int period in
      atr.(i) <- Some v
  done;
  atr

let hit_first_bracket ~(bars : T.bar_1m array) ~i ~h_mult ~atr10 ~n_horizon =
  match atr10.(i) with
  | None -> Hit_none
  | Some atr when Float.(atr <= 0.) -> Hit_none
  | Some atr ->
      let p0 = bars.(i).T.close in
      let h = h_mult *. atr in
      let up_level = p0 +. h in
      let dn_level = p0 -. h in
      let n = Array.length bars in
      let last = Int.min (n - 1) (i + n_horizon) in
      let t_up = ref None in
      let t_dn = ref None in
      let rec loop j =
        if j > last then ()
        else begin
          let b = bars.(j) in
          if Option.is_none !t_up && Float.(b.T.high >= up_level) then
            t_up := Some j;
          if Option.is_none !t_dn && Float.(b.T.low <= dn_level) then
            t_dn := Some j;
          if Option.is_some !t_up && Option.is_some !t_dn then ()
          else loop (j + 1)
        end
      in
      loop (i + 1);
      match !t_up, !t_dn with
      | None, None -> Hit_none
      | Some _tu, None -> Hit_up
      | None, Some _td -> Hit_down
      | Some tu, Some td ->
          if tu < td then Hit_up
          else if td < tu then Hit_down
          else Hit_both

let string_of_leg_side = function
  | PT.Leg_up -> "up"
  | PT.Leg_down -> "down"
  | PT.Leg_none -> "none"

let string_of_range_regime = function
  | PT.Range -> "range"
  | PT.Trend -> "trend"
  | PT.Regime_unknown -> "unknown"

let string_of_day_regime = function
  | PT.Day_spike -> "spike"
  | PT.Day_trend -> "trend"
  | PT.Day_range -> "range"
  | PT.Day_unknown -> "unknown"

let string_of_intraday_phase = function
  | PT.Pre_rth -> "pre_rth"
  | PT.Rth_open -> "rth_open"
  | PT.Rth_mid -> "rth_mid"
  | PT.Rth_late -> "rth_late"
  | PT.Post_rth -> "post_rth"

let string_of_ai_state = function
  | PT.Ai_long -> "long"
  | PT.Ai_short -> "short"
  | PT.Ai_flat -> "flat"

let string_of_option_float = function
  | None -> ""
  | Some x -> Float.to_string_hum x

let string_of_option_bool = function
  | None -> ""
  | Some b -> Bool.to_string b

let () =
  let argv = Array.to_list (Sys.get_argv ()) in
  let infile, outfile =
    match argv with
    | [_; in_; out_] -> in_, out_
    | [_; in_] -> in_, "ml_export.csv"
    | _ -> "sample_es.csv", "ml_export.csv"
  in
  let bars_rev = ref [] in
  let snaps_rev = ref [] in
  let ctx = ref (Ctx.create ()) in
  Csv_parser.iter_bars infile ~f:(fun b ->
      ctx := Ctx.update_bar !ctx b;
      let snap = Ctx.snapshot !ctx in
      bars_rev := b :: !bars_rev;
      snaps_rev := snap :: !snaps_rev);
  let bars = Array.of_list (List.rev !bars_rev) in
  let snaps = Array.of_list (List.rev !snaps_rev) in
  let n = Array.length bars in
  let tr = compute_tr bars in
  let atr10 = compute_atr tr ~period:10 in
  let oc = Out_channel.create outfile in
  (* header *)
  Out_channel.output_string oc
    "date,time,split,label_small,label_amp,close,atr10,z_vwap,rv10,rv60,rv_ratio,trend_feat,gap,dist_onh,dist_onl,dist_ema20,ema20_slope,leg_side,leg_len_bars,leg_range,range_regime,pos_in_range,range_test_rate_high,range_test_rate_low,range_fail_rate_high,range_fail_rate_low,range_width_ratio,range_tight,micro_up_len,micro_down_len,soft_micro_up_len,soft_micro_down_len,soft_micro_bias,bar_body_frac,bar_close_pos,bar_is_trend,bar_is_doji,micro_channel_slope,major_channel_slope,micro_channel_z,major_channel_z,micro_channel_overshoot,major_channel_overshoot,leg_mm_up,leg_mm_down,recent_strength_score,always_in,htf_leg_side_5m,htf_leg_len_bars_5m,day_regime,day_net_change,day_range,day_pos_in_range,day_inside_prev_range,day_trend_run_len,dist_prev_day_high,dist_prev_day_low,intraday_phase,range_mid,range_mm_up,range_mm_down,soft_break_up_severity,soft_break_up_trend,soft_break_down_severity,soft_break_down_trend,recent_bull_count,recent_bear_count,recent_doji_count,recent_body_sum\n";
  let train_end_date = Date.of_string "2018-12-31" in
  (* Pre-compute amplitude-based labels on closes (Jonathan Shore style, in bps). *)
  let closes = Array.map bars ~f:(fun b -> b.T.close) in
  let amp_minamp_bps = 25.0 in
  let amp_tinactive = 20 in
  let amp_labels = Amp.label_prices ~minamp_bps:amp_minamp_bps ~tinactive:amp_tinactive closes in
  let h_small_mult = 1.0 in
  let n_small = 10 in
  for i = 0 to n - 1 do
    let bar = bars.(i) in
    let minute = bar.T.ts.minute_of_day in
    (* only RTH samples *)
    if minute >= rth_start && minute <= rth_end && i < n - 1 then begin
      let snap = snaps.(i) in
      let feat = snap.Ctx.features in
      let patt = snap.Ctx.patterns in
      let gate_soft = patt.soft_micro_up_len >= 5 || patt.soft_micro_down_len >= 5 in
      let gate_edge =
        match patt.pos_in_range with
        | None -> false
        | Some p -> Float.(abs (p -. 0.5) >= 0.3)
      in
      let gated = gate_soft || gate_edge in
      (match atr10.(i) with
       | None -> ()
       | Some atr when Float.(atr <= 0.) -> ()
       | Some _ when not gated -> ()
       | Some _ ->
           let lbl_small =
             hit_first_bracket ~bars ~i ~h_mult:h_small_mult ~atr10 ~n_horizon:n_small
           in
           (match lbl_small with
            | Hit_none -> ()
            | _ ->
               let date = bar.T.ts.date in
               let split =
                 if Date.compare date train_end_date <= 0 then "train" else "test"
               in
               let date_str = Date.to_string date in
               let hh = bar.T.ts.minute_of_day / 60 in
               let mm = bar.T.ts.minute_of_day mod 60 in
               let time_str = sprintf "%02d:%02d" hh mm in
               let lbl_str = string_of_hit_first lbl_small in
               let amp_lbl =
                 let v = amp_labels.(i) in
                 if Float.(v > 0.) then "up"
                 else if Float.(v < 0.) then "down"
                 else "flat"
               in
               let openf = Out_channel.output_string in
               openf oc date_str; openf oc ",";
               openf oc time_str; openf oc ",";
               openf oc split; openf oc ",";
               openf oc lbl_str; openf oc ",";
               openf oc amp_lbl; openf oc ",";
               openf oc (Float.to_string_hum bar.T.close); openf oc ",";
               openf oc (string_of_option_float atr10.(i)); openf oc ",";
               openf oc (string_of_option_float feat.z_vwap); openf oc ",";
               openf oc (string_of_option_float feat.rv10); openf oc ",";
               openf oc (string_of_option_float feat.rv60); openf oc ",";
               openf oc (string_of_option_float feat.rv_ratio); openf oc ",";
               openf oc (string_of_option_float feat.trend); openf oc ",";
               openf oc (string_of_option_float feat.gap); openf oc ",";
               openf oc (string_of_option_float feat.dist_onh); openf oc ",";
               openf oc (string_of_option_float feat.dist_onl); openf oc ",";
               openf oc (string_of_option_float feat.dist_ema20); openf oc ",";
               openf oc (string_of_option_float feat.ema20_slope); openf oc ",";
               openf oc (string_of_leg_side patt.leg_side); openf oc ",";
               openf oc (Int.to_string patt.leg_len_bars); openf oc ",";
               openf oc (Float.to_string_hum patt.leg_range); openf oc ",";
               openf oc (string_of_range_regime patt.range_regime); openf oc ",";
               openf oc (string_of_option_float patt.pos_in_range); openf oc ",";
               openf oc (Float.to_string_hum patt.range_test_rate_high); openf oc ",";
               openf oc (Float.to_string_hum patt.range_test_rate_low); openf oc ",";
               openf oc (Float.to_string_hum patt.range_fail_rate_high); openf oc ",";
               openf oc (Float.to_string_hum patt.range_fail_rate_low); openf oc ",";
               openf oc (Float.to_string_hum patt.range_width_ratio); openf oc ",";
               openf oc (Bool.to_string patt.range_tight); openf oc ",";
               openf oc (Int.to_string patt.micro_up_len); openf oc ",";
               openf oc (Int.to_string patt.micro_down_len); openf oc ",";
               openf oc (Int.to_string patt.soft_micro_up_len); openf oc ",";
               openf oc (Int.to_string patt.soft_micro_down_len); openf oc ",";
               openf oc (Int.to_string patt.soft_micro_bias); openf oc ",";
               openf oc (string_of_option_float patt.bar_body_frac); openf oc ",";
               openf oc (string_of_option_float patt.bar_close_pos); openf oc ",";
               openf oc (Bool.to_string patt.bar_is_trend); openf oc ",";
               openf oc (Bool.to_string patt.bar_is_doji); openf oc ",";
               openf oc (string_of_option_float patt.micro_channel_slope); openf oc ",";
               openf oc (string_of_option_float patt.major_channel_slope); openf oc ",";
               openf oc (string_of_option_float patt.micro_channel_z); openf oc ",";
               openf oc (string_of_option_float patt.major_channel_z); openf oc ",";
               openf oc (Bool.to_string patt.micro_channel_overshoot); openf oc ",";
               openf oc (Bool.to_string patt.major_channel_overshoot); openf oc ",";
               openf oc (string_of_option_float patt.leg_mm_up); openf oc ",";
               openf oc (string_of_option_float patt.leg_mm_down); openf oc ",";
               openf oc (string_of_option_float patt.recent_strength_score); openf oc ",";
               openf oc (string_of_ai_state patt.always_in); openf oc ",";
               openf oc (string_of_leg_side patt.htf_leg_side_5m); openf oc ",";
               openf oc (Int.to_string patt.htf_leg_len_bars_5m); openf oc ",";
               openf oc (string_of_day_regime patt.day_regime); openf oc ",";
               openf oc (string_of_option_float patt.day_net_change); openf oc ",";
               openf oc (string_of_option_float patt.day_range); openf oc ",";
               openf oc (string_of_option_float patt.day_pos_in_range); openf oc ",";
               openf oc (string_of_option_bool patt.day_inside_prev_range); openf oc ",";
               openf oc (Int.to_string patt.day_trend_run_len); openf oc ",";
               openf oc (string_of_option_float patt.dist_prev_day_high); openf oc ",";
               openf oc (string_of_option_float patt.dist_prev_day_low); openf oc ",";
               openf oc (string_of_intraday_phase patt.intraday_phase); openf oc ",";
               openf oc (string_of_option_float patt.range_mid); openf oc ",";
               openf oc (string_of_option_float patt.range_mm_up); openf oc ",";
               openf oc (string_of_option_float patt.range_mm_down); openf oc ",";
               openf oc (string_of_option_float patt.soft_break_up_severity); openf oc ",";
               openf oc (Bool.to_string patt.soft_break_up_trend); openf oc ",";
               openf oc (string_of_option_float patt.soft_break_down_severity); openf oc ",";
               openf oc (Bool.to_string patt.soft_break_down_trend); openf oc ",";
               openf oc (Int.to_string patt.recent_bull_count); openf oc ",";
               openf oc (Int.to_string patt.recent_bear_count); openf oc ",";
               openf oc (Int.to_string patt.recent_doji_count); openf oc ",";
               openf oc (Float.to_string_hum patt.recent_body_sum); openf oc "\n"))
    end
  done;
  Out_channel.close oc
