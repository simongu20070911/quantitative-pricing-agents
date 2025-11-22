open Core

let usage () =
  eprintf "Usage: %s <es_1min_csv>\n%!"
    (Filename.basename (Core.Sys.get_argv ()).(0));
  exit 1

let () =
  let file =
    match Core.Sys.get_argv () with
    | [| _; f |] -> f
    | _ -> usage ()
  in
  if not (Stdlib.Sys.file_exists file) then
    failwithf "data file %s not found" file ();

  let strat = Strategy_fast.Strategies.Strategy_b1b2.strategy_pure in
  let result = Strategy_fast.Engine.Engine.run_pure strat ~filename:file in
  let setup_days =
    Hashtbl.keys result.setups |> List.sort ~compare:Date.compare
  in

  (* Build set of all dates present in the CSV to identify filtered-out days. *)
  let all_days_tbl = Date.Table.create () in
  Strategy_fast.Csv_parser.iter_bars file ~f:(fun bar ->
      Hashtbl.set all_days_tbl ~key:bar.ts.date ~data:());
  let all_days = Hashtbl.keys all_days_tbl |> List.sort ~compare:Date.compare in

  let setup_set = Date.Set.of_list setup_days in
  let filtered =
    List.filter all_days ~f:(fun d -> not (Set.mem setup_set d))
  in

  printf "Data days        : %d (%s .. %s)\n"
    (List.length all_days)
    (Option.value_exn (List.hd all_days |> Option.map ~f:Date.to_string))
    (Option.value_exn (List.last all_days |> Option.map ~f:Date.to_string));
  printf "Setup days       : %d\n" (List.length setup_days);
  printf "Filtered-out days: %d\n\n" (List.length filtered);

  let take_first lst n =
    List.take lst n |> List.map ~f:Date.to_string
  in
  let take_last lst n =
    let rev = List.rev lst in
    let first = List.take rev n in
    let back = List.rev first in
    List.map back ~f:Date.to_string
  in

  printf "First 5 setup days      : [%s]\n"
    (String.concat ~sep:"; " (take_first setup_days 5));
  printf "Last 5 setup days       : [%s]\n"
    (String.concat ~sep:"; " (take_last setup_days 5));
  printf "First 5 filtered-out    : [%s]\n"
    (String.concat ~sep:"; " (take_first filtered 5));
  printf "Last 5 filtered-out     : [%s]\n"
    (String.concat ~sep:"; " (take_last filtered 5));

  let hash dates =
    let concat = String.concat ~sep:"," (List.map dates ~f:Date.to_string) in
    Md5.to_hex (Md5.digest_string concat)
  in
  printf "\nSetup-day hash    : %s\n" (hash setup_days);
  printf "Filtered-day hash : %s\n" (hash filtered);
