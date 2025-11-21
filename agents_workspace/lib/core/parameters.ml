open Core

module Scale = struct
  type t = Lin | Log [@@deriving sexp]
end

type t = {
  name : string;
  default : float;
  bounds : (float * float) option;
  scale : Scale.t;
  fixed : bool;
  integer : bool;
  description : string;
}

type value_map = float String.Map.t

let make ?bounds ?scale ?fixed ?integer ?description ~name ~default () =
  let scale = Option.value scale ~default:Scale.Lin in
  let fixed = Option.value fixed ~default:false in
  let integer = Option.value integer ~default:false in
  let description = Option.value description ~default:"" in
  { name; default; bounds; scale; fixed; integer; description }

let clamp spec v =
  match spec.bounds with
  | None -> v
  | Some (lo, hi) -> Float.(max lo (min hi v))

let validate spec v =
  match spec.bounds with
  | None -> Ok ()
  | Some (lo, hi) ->
      if Float.(v < lo || v > hi) then
        Or_error.errorf "%s=%f outside bounds [%f,%f]" spec.name v lo hi
        |> Result.map_error ~f:Error.to_string_hum
      else Ok ()

let to_string spec =
  let bounds =
    match spec.bounds with
    | None -> ""
    | Some (l, h) -> sprintf " [%g,%g]" l h
  in
  let fixed = if spec.fixed then " fixed" else "" in
  let int_flag = if spec.integer then " int" else "" in
  let desc = if String.is_empty spec.description then "" else " " ^ spec.description in
  sprintf "%s=%.4g%s%s%s%s" spec.name spec.default bounds fixed int_flag desc

let default_map specs =
  List.fold specs ~init:String.Map.empty ~f:(fun acc s ->
      Map.set acc ~key:s.name ~data:s.default)

let merge_overrides specs overrides =
  Map.fold overrides ~init:(default_map specs) ~f:(fun ~key ~data acc ->
      match List.find specs ~f:(fun s -> String.equal s.name key) with
      | None -> acc
      | Some spec ->
          let v = if spec.integer then Float.round_nearest data else data in
          let v = clamp spec v in
          Map.set acc ~key ~data:v)

let%test_unit "clamp and merge_overrides work" =
  let specs = [
    make ~name:"x" ~default:1.0 ~bounds:(0., 2.) ();
    make ~name:"y" ~default:5.0 ();
  ] in
  let overrides = String.Map.of_alist_exn [ "x", 3.0; "y", 6.0 ] in
  let merged = merge_overrides specs overrides in
  assert (Float.(Core.Map.find_exn merged "x" = 2.0));
  assert (Float.(Core.Map.find_exn merged "y" = 6.0))
