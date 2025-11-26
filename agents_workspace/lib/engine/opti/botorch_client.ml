open Core

(* Lightweight OCaml wrapper that talks to the BoTorch TuRBO-m FastAPI service
   (tools/botorch_service.py) via curl/HTTP + JSON. *)

type domain =
  | Continuous
  | Integer
  | Discrete of float list
  | Categorical of string list

type bounds = {
  name : string;
  lower : float;
  upper : float;
  integer : bool;
  domain : domain;
}
type state = Yojson.Safe.t

let default_host = "127.0.0.1"
let default_port = 10001

let endpoint ?(host = default_host) ?(port = default_port) path =
  sprintf "http://%s:%d%s" host port path

let run_curl ~url ~body =
  let tmp_in = Stdlib.Filename.temp_file ~temp_dir:"/tmp" "botorch_req" ".json" in
  let tmp_out = Stdlib.Filename.temp_file ~temp_dir:"/tmp" "botorch_resp" ".json" in
  Out_channel.write_all tmp_in ~data:body;
  let cmd =
    sprintf
      "curl -s -X POST -H 'Content-Type: application/json' --data @%s %s > %s"
      (Filename.quote tmp_in) (Filename.quote url) (Filename.quote tmp_out)
  in
  let resp = Stdlib.Sys.command cmd in
  let data = In_channel.read_all tmp_out in
  Stdlib.Sys.remove tmp_in;
  Stdlib.Sys.remove tmp_out;
  match resp with
  | 0 -> data
  | _ -> failwith (sprintf "curl failed (code %d): %s" resp cmd)

let bounds_to_yojson b =
  `List
    (List.map b ~f:(fun { name; lower; upper; integer; domain } ->
         `Assoc
           [
             ("name", `String name);
             ("lower", `Float lower);
             ("upper", `Float upper);
             ("integer", `Bool integer);
             ( "domain",
               match domain with
               | Continuous -> `Assoc [ ("kind", `String "continuous") ]
               | Integer -> `Assoc [ ("kind", `String "integer") ]
               | Discrete vals ->
                   `Assoc [ ("kind", `String "discrete"); ("values", `List (List.map vals ~f:(fun v -> `Float v))) ]
               | Categorical cats ->
                   `Assoc [ ("kind", `String "categorical"); ("values", `List (List.map cats ~f:(fun c -> `String c))) ] );
           ]))

let array_to_json arr = `List (Array.to_list arr |> List.map ~f:(fun x -> `Float x))
let mat_to_json m = `List (Array.to_list m |> List.map ~f:array_to_json)

let init ?host ?port ~bounds ~batch_size ~n_regions () : state =
  let url = endpoint ?host ?port "/init" in
  let body =
    `Assoc
      [
        ("bounds", bounds_to_yojson bounds);
        ("batch_size", `Int batch_size);
        ("n_regions", `Int n_regions);
      ]
    |> Yojson.Safe.to_string
  in
  let resp = run_curl ~url ~body in
  let json = Yojson.Safe.from_string resp in
  match json with
  | `Assoc l -> (match List.Assoc.find l ~equal:String.equal "state" with Some s -> s | _ -> json)
  | _ -> json

type suggest_result = {
  candidates : float array list;
  region_ids : int list;
  state : state;
}

let suggest ?host ?port ~state ~x ~y () : suggest_result =
  let url = endpoint ?host ?port "/suggest" in
  let body =
    `Assoc
      [
        ("X", mat_to_json x);
        ("Y", `List (Array.to_list y |> List.map ~f:(fun v -> `Float v)));
        ("state", state);
      ]
    |> Yojson.Safe.to_string
  in
  let resp = run_curl ~url ~body in
  let json = Yojson.Safe.from_string resp in
  let candidates =
    match Yojson.Safe.Util.member "candidates" json with
    | `List lst ->
        List.map lst ~f:(function
          | `List xs -> Array.of_list (List.map xs ~f:Yojson.Safe.Util.to_float)
          | _ -> failwith "bad candidate")
    | _ -> []
  in
  let region_ids =
    match Yojson.Safe.Util.member "region_ids" json with
    | `List lst -> List.map lst ~f:Yojson.Safe.Util.to_int
    | _ -> []
  in
  let state =
    match Yojson.Safe.Util.member "state" json with
    | s -> s
  in
  { candidates; region_ids; state }

let update ?host ?port ~state ~y_new () : state =
  let url = endpoint ?host ?port "/update" in
  let body =
    `Assoc
      [
        ("state", state);
        ("Y_new", `List (Array.to_list y_new |> List.map ~f:(fun v -> `Float v)));
      ]
    |> Yojson.Safe.to_string
  in
  let resp = run_curl ~url ~body in
  let json = Yojson.Safe.from_string resp in
  match Yojson.Safe.Util.member "state" json with s -> s
