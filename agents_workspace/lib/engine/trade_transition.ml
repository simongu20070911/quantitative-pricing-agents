open Core
open Types

let step ~plan ~state ~(bar : bar_1m) ~record_trade : trade_state * 'a list =
  match state with
  | No_trade | Done -> state, []
  | Pending ->
      (match plan.direction with
       | Long ->
           if Float.(bar.high >= plan.entry_price) then
             Active { stop_price = plan.stop_init; moved_to_be = false; entry_ts = bar.ts }, []
           else if Float.(bar.low <= plan.cancel_level) then Done, []
           else state, []
       | Short ->
           if Float.(bar.low <= plan.entry_price) then
             Active { stop_price = plan.stop_init; moved_to_be = false; entry_ts = bar.ts }, []
           else if Float.(bar.high >= plan.cancel_level) then Done, []
           else state, [])
  | Active active ->
      let stop_hit =
        match plan.direction with
        | Long  -> Float.(bar.low <= active.stop_price)
        | Short -> Float.(bar.high >= active.stop_price)
      in
      if stop_hit then
        let trade = record_trade ~active ~exit_ts:bar.ts ~exit_price:active.stop_price ~exit_reason:Stop in
        Done, [ trade ]
      else
        let active' =
          if active.moved_to_be then active
          else
            match plan.direction with
            | Long when Float.(bar.high >= plan.be_trigger) ->
                { active with stop_price = plan.entry_price; moved_to_be = true }
            | Short when Float.(bar.low <= plan.be_trigger) ->
                { active with stop_price = plan.entry_price; moved_to_be = true }
            | _ -> active
        in
        let target_hit =
          match plan.direction with
          | Long  -> Float.(bar.high >= plan.target_price)
          | Short -> Float.(bar.low <= plan.target_price)
        in
        if target_hit then
          let trade = record_trade ~active:active' ~exit_ts:bar.ts ~exit_price:plan.target_price ~exit_reason:Target in
          Done, [ trade ]
        else
          Active active', []
