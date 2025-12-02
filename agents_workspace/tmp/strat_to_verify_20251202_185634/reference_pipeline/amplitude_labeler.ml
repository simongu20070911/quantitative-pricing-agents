open Core

(* Amplitude-based trend labeler, ported from Jonathan Shore's
   AmplitudeBasedLabeler (MIT). It operates on cumulative returns
   in "bps" space and returns labels in {-1.0, 0.0, +1.0}. *)

let _apply_label (labels : float array) (istart : int) (iend : int) (dir : float) =
  if istart < 0 || iend < 0 then ()
  else if iend < istart then ()
  else
    for i = istart to iend do
      labels.(i) <- dir
    done

let label_cumr ~(minamp : float) ~(tinactive : int) (cumr : float array) :
    float array =
  let len = Array.length cumr in
  if len = 0 then [||]
  else
    let labels = Array.create ~len 0.0 in
    (* Pass 1: brute-force segmentation by amplitude / inactivity. *)
    let pass1 () =
      let istart = ref 0 in
      let icursor = ref 0 in
      let imin = ref 0 in
      let imax = ref 0 in
      let vmin = ref cumr.(0) in
      let vmax = ref cumr.(0) in
      while !icursor < len do
        let v = cumr.(!icursor) in
        (* retracement splits *)
        if Float.(!vmax -. !vmin >= minamp)
           && !imin > !imax
           && Float.(v -. !vmin >= minamp)
        then (
          _apply_label labels !istart (!imax - 1) 0.0;
          _apply_label labels !imax !imin (-1.0);
          istart := !imin;
          imax := !icursor;
          vmax := v)
        else if Float.(!vmax -. !vmin >= minamp)
                && !imax > !imin
                && Float.(!vmax -. v >= minamp)
        then (
          _apply_label labels !istart (!imin - 1) 0.0;
          _apply_label labels !imin !imax 1.0;
          istart := !imax;
          imin := !icursor;
          vmin := v)
        (* inactivity in upward direction *)
        else if !imax > !imin
                && !icursor - !imax >= tinactive
                && Float.(v <= !vmax)
        then (
          if Float.(!vmax -. !vmin >= minamp) then (
            _apply_label labels !istart (!imin - 1) 0.0;
            _apply_label labels !imin !imax 1.0;
            _apply_label labels (!imax + 1) !icursor 0.0 )
          else
            _apply_label labels !istart !icursor 0.0;
          istart := !icursor;
          imax := !icursor;
          imin := !icursor;
          vmax := v;
          vmin := v)
        (* inactivity in downward direction *)
        else if !imin > !imax
                && !icursor - !imin >= tinactive
                && Float.(v >= !vmin)
        then (
          if Float.(!vmax -. !vmin >= minamp) then (
            _apply_label labels !istart (!imax - 1) 0.0;
            _apply_label labels !imax !imin (-1.0);
            _apply_label labels (!imin + 1) !icursor 0.0 )
          else
            _apply_label labels !istart !icursor 0.0;
          istart := !icursor;
          imax := !icursor;
          imin := !icursor;
          vmax := v;
          vmin := v);
        (* adjust local extrema *)
        if Float.(v >= !vmax) then (
          imax := !icursor;
          vmax := v );
        if Float.(v <= !vmin) then (
          imin := !icursor;
          vmin := v );
        incr icursor
      done;
      (* finish end *)
      if Float.(!vmax -. !vmin >= minamp) && !imin > !imax then (
        _apply_label labels !istart (!imax - 1) 0.0;
        _apply_label labels !imax !imin (-1.0);
        _apply_label labels (!imin + 1) (!icursor - 1) 0.0 )
      else if Float.(!vmax -. !vmin >= minamp) && !imax > !imin then (
        _apply_label labels !istart (!imin - 1) 0.0;
        _apply_label labels !imin !imax 1.0;
        _apply_label labels (!imax + 1) (!icursor - 1) 0.0 )
      else
        _apply_label labels !istart (!icursor - 1) 0.0
    in
    (* Pass 2: OLS-based filtering of momentum regions. *)
    let filter_pass () =
      let ipos = ref 0 in
      while !ipos < len do
        let dir = labels.(!ipos) in
        if Float.(dir = 0.) then
          incr ipos
        else (
          let istart = ref !ipos in
          let iend = ref !ipos in
          while !iend < len && Float.(labels.(!iend) = dir) do
            incr iend
          done;
          decr iend;
          let imaxfwd = ref !istart in
          let imaxback = ref !iend in
          let vmaxfwd = ref 0.0 in
          let vmaxback = ref 0.0 in
          (* forward OLS *)
          let fexy = ref 0.0
          and fexx = ref 0.0
          and fex = ref 0.0
          and fey = ref 0.0 in
          let distance = ref 0.0 in
          for i = !istart to !iend do
            let xc = float (i - !istart) in
            let yc = cumr.(i) in
            fexy := !fexy +. (xc *. yc);
            fexx := !fexx +. (xc *. xc);
            fex := !fex +. xc;
            fey := !fey +. yc;
            if Float.(xc > 0.) then (
              let denom = !fexx -. (!fex *. !fex /. (xc +. 1.0)) in
              if Float.(denom <> 0.) then (
                let beta =
                  (!fexy -. (!fex *. !fey /. (xc +. 1.0)))
                  /. denom
                in
                distance := dir *. beta *. xc;
                if Float.(!distance > !vmaxfwd) then (
                  vmaxfwd := !distance;
                  imaxfwd := i ) ) )
          done;
          (* backward OLS *)
          let bexy = ref 0.0
          and bexx = ref 0.0
          and bex = ref 0.0
          and bey = ref 0.0 in
          distance := 0.0;
          for i = !iend downto !istart do
            let xc = float (!iend - i) in
            let yc = cumr.(i) in
            bexy := !bexy +. (xc *. yc);
            bexx := !bexx +. (xc *. xc);
            bex := !bex +. xc;
            bey := !bey +. yc;
            if Float.(xc > 0.) then (
              let denom = !bexx -. (!bex *. !bex /. (xc +. 1.0)) in
              if Float.(denom <> 0.) then (
                let beta =
                  (!bexy -. (!bex *. !bey /. (xc +. 1.0)))
                  /. denom
                in
                distance := -. dir *. beta *. xc;
                if Float.(!distance > !vmaxback) then (
                  vmaxback := !distance;
                  imaxback := i ) ) )
          done;
          if Float.(!vmaxfwd < minamp) && Float.(!vmaxback < minamp) then
            _apply_label labels !istart !iend 0.0
          else (
            if Float.(!vmaxfwd >= minamp) then (
              _apply_label labels !istart !imaxfwd dir;
              _apply_label labels (!imaxfwd + 1) (!imaxback - 1) 0.0 )
            else
              _apply_label labels !istart !imaxback 0.0;
            if Float.(!vmaxback >= minamp) then
              _apply_label labels !imaxback !iend dir
            else
              _apply_label
                labels
                (Int.max !imaxback (!imaxfwd + 1))
                !iend
                0.0 );
          ipos := !iend + 1 )
      done
    in
    pass1 ();
    filter_pass ();
    labels

let label_prices ~(minamp_bps : float) ~(tinactive : int) (prices : float array) :
    float array =
  let n = Array.length prices in
  if n = 0 then [||]
  else
    let base = prices.(0) in
    let cumr = Array.create ~len:n 0.0 in
    for i = 0 to n - 1 do
      let p = prices.(i) in
      (* guard against pathological non-positive prices *)
      let p = if Float.(p <= 0.) then base else p in
      cumr.(i) <- Float.log (p /. base) *. 1e4
    done;
    label_cumr ~minamp:minamp_bps ~tinactive cumr

