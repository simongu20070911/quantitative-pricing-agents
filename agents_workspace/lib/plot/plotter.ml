open Core
open Types

module C = Cairo

let with_surface ~w ~h ~outfile f =
  let surf = C.Image.create C.Image.ARGB32 ~w ~h in
  let cr = C.create surf in
  f cr;
  C.PNG.write surf outfile

let clear_background cr ~w ~h =
  C.set_source_rgb cr 1.0 1.0 1.0;
  C.rectangle cr 0. 0. ~w:(Float.of_int w) ~h:(Float.of_int h);
  C.fill cr;
  ()

let draw_axes cr ~w ~h ~xlabel ~ylabel ~yticks =
  let x0 = 60. and y0 = Float.of_int (h - 40) in
  let x1 = Float.of_int (w - 20) and y1 = 20. in
  C.set_source_rgb cr 0.7 0.7 0.7;
  C.set_line_width cr 1.;
  (* x-axis *)
  C.move_to cr x0 y0; C.line_to cr x1 y0;
  (* y-axis *)
  C.move_to cr x0 y1; C.line_to cr x0 y0;
  C.stroke cr;
  (* axis labels *)
  C.select_font_face cr ~slant:C.Upright ~weight:C.Normal "Sans";
  C.set_font_size cr 12.;
  C.move_to cr ((x0 +. x1) /. 2.) (Float.of_int h -. 15.);
  C.show_text cr xlabel;
  C.move_to cr 10. ((y0 +. y1) /. 2.);
  C.save cr; C.rotate cr (-. Float.pi /. 2.);
  C.show_text cr ylabel;
  C.restore cr;
  (* y ticks *)
  List.iter yticks ~f:(fun (v, label) ->
      let y = y0 -. v in
      C.set_source_rgb cr 0.75 0.75 0.75;
      C.set_line_width cr 0.5;
      C.move_to cr x0 y; C.line_to cr x1 y; C.stroke cr;
      C.set_source_rgb cr 0.3 0.3 0.3;
      C.move_to cr (x0 -. 50.) (y +. 4.); C.show_text cr label)

let line_poly cr pts ~color =
  match pts with
  | [] | [ _ ] -> ()
  | (x0, y0) :: rest ->
      let r, g, b = color in
      C.set_source_rgb cr r g b;
      C.set_line_width cr 2.;
      C.move_to cr x0 y0;
      List.iter rest ~f:(fun (x, y) -> C.line_to cr x y);
      C.stroke cr

let bounds pts =
  let xs = List.map pts ~f:fst and ys = List.map pts ~f:snd in
  match xs, ys with
  | [], _ | _, [] -> None
  | _ ->
      Some
        (List.reduce_exn xs ~f:Float.min,
         List.reduce_exn xs ~f:Float.max,
         List.reduce_exn ys ~f:Float.min,
         List.reduce_exn ys ~f:Float.max)

let scale_points pts ~w ~h ~xmin ~xmax ~ymin ~ymax =
  let xspan = if Float.(xmax = xmin) then 1. else xmax -. xmin in
  let yspan = if Float.(ymax = ymin) then 1. else ymax -. ymin in
  let x0 = 70. and y0 = Float.of_int (h - 50) in
  let usable_w = Float.of_int (w - 90) in
  let usable_h = Float.of_int (h - 90) in
  List.map pts ~f:(fun (x, y) ->
      let nx = (x -. xmin) /. xspan in
      let ny = (y -. ymin) /. yspan in
      let px = x0 +. nx *. usable_w in
      let py = y0 -. ny *. usable_h in
      px, py)

let render_equity ~outfile ~trades =
  let w, h = 1200, 600 in
  let pts =
    trades
    |> List.sort ~compare:(fun a b ->
           let c = Date.compare a.exit_ts.date b.exit_ts.date in
           if c <> 0 then c else Int.compare a.exit_ts.minute_of_day b.exit_ts.minute_of_day)
    |> List.folding_map ~init:0.0 ~f:(fun acc t ->
           let acc' = acc +. t.pnl_R in acc', acc')
    |> List.mapi ~f:(fun i y -> Float.of_int i, y)
  in
  with_surface ~w ~h ~outfile (fun cr ->
      clear_background cr ~w ~h;
      match bounds pts with
      | None -> ()
      | Some (_, _, ymin, ymax) ->
          let yticks =
            let step = (ymax -. ymin) /. 4. in
            List.init 5 ~f:(fun i ->
                let v = ymin +. Float.of_int i *. step in
                let lab = Printf.sprintf "%.2f R" v in
                ( (v -. ymin) /. (if Float.(ymax = ymin) then 1. else ymax -. ymin)
                  *. Float.of_int (h - 90), lab))
          in
          draw_axes cr ~w ~h ~xlabel:"Trades" ~ylabel:"Cumulative R" ~yticks;
          let xmin,xmax,ymin,ymax = Option.value_exn (bounds pts) in
          let scaled = scale_points pts ~w ~h ~xmin ~xmax ~ymin ~ymax in
          line_poly cr scaled ~color:(0.1, 0.4, 0.8))

let render_daily_curve ~outfile ~daily =
  let w, h = 1200, 600 in
  let pts =
    daily
    |> List.map ~f:(fun (_d, r) -> r)
    |> List.folding_map ~init:0.0 ~f:(fun acc r -> let acc' = acc +. r in acc', acc')
    |> List.mapi ~f:(fun i y -> Float.of_int i, y)
  in
  with_surface ~w ~h ~outfile (fun cr ->
      clear_background cr ~w ~h;
      match bounds pts with
      | None -> ()
      | Some (_, _, ymin, ymax) ->
          let yticks =
            let step = (ymax -. ymin) /. 4. in
            List.init 5 ~f:(fun i ->
                let v = ymin +. Float.of_int i *. step in
                let lab = Printf.sprintf "%.2f R" v in
                ( (v -. ymin) /. (if Float.(ymax = ymin) then 1. else ymax -. ymin)
                  *. Float.of_int (h - 90), lab))
          in
          draw_axes cr ~w ~h ~xlabel:"Days" ~ylabel:"Cumulative R" ~yticks;
          let xmin,xmax,ymin,ymax = Option.value_exn (bounds pts) in
          let scaled = scale_points pts ~w ~h ~xmin ~xmax ~ymin ~ymax in
          line_poly cr scaled ~color:(0.2, 0.6, 0.2))

let render_histogram ~outfile ~values ~bins =
  let w, h = 800, 500 in
  if Array.length values = 0 then
    with_surface ~w ~h ~outfile (fun cr -> clear_background cr ~w ~h)
  else
    let vmin = Array.reduce_exn values ~f:Float.min in
    let vmax = Array.reduce_exn values ~f:Float.max in
    let span = if Float.(vmax = vmin) then 1. else vmax -. vmin in
    let bin_w = span /. Float.of_int bins in
    let counts = Array.create ~len:bins 0 in
    Array.iter values ~f:(fun v ->
        let raw = int_of_float ((v -. vmin) /. bin_w) in
        let idx =
          if raw < 0 then 0
          else if raw >= bins then bins - 1
          else raw
        in
        counts.(idx) <- counts.(idx) + 1);
    let max_count = Array.reduce_exn counts ~f:Int.max |> Float.of_int in
    with_surface ~w ~h ~outfile (fun cr ->
        clear_background cr ~w ~h;
        let left, right = 60., Float.of_int (w - 30) in
        let bottom, top = Float.of_int (h - 40), 30. in
        let usable_w = right -. left in
        let usable_h = bottom -. top in
        Array.iteri counts ~f:(fun i c ->
            let x0 = left +. (Float.of_int i /. Float.of_int bins) *. usable_w in
            let x1 = left +. (Float.of_int (i+1) /. Float.of_int bins) *. usable_w in
            let bar_w = x1 -. x0 -. 2. in
            let bar_h = if Float.(max_count = 0.) then 0.
              else (Float.of_int c /. max_count) *. usable_h in
            let y = bottom -. bar_h in
            C.set_source_rgb cr 0.9 0.4 0.2;
            C.rectangle cr (x0 +. 1.) y ~w:bar_w ~h:bar_h;
            C.fill cr))
