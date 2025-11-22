open Core

let find_fixture () =
  let target = Filename.concat "agents_workspace" "sample_es.csv" in
  let rec ascend n dir =
    if n < 0 then None
    else
      let candidate = Filename.concat dir target in
      if Stdlib.Sys.file_exists candidate then Some candidate
      else ascend (n - 1) (Filename.dirname dir)
  in
  match ascend 6 (Stdlib.Sys.getcwd ()) with
  | Some p -> p
  | None -> failwith "fixture not found"
