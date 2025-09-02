open Prelude
module Attrs = Markup_ext.Attrs

type t = {
  href : string;
  time : string;
  description : string option;
  extended : string option;
  tag : string list;
  hash : string option;
  shared : bool;
  toread : bool;
}

let empty =
  {
    href = String.empty;
    time = String.empty;
    description = None;
    extended = None;
    tag = [];
    hash = None;
    shared = false;
    toread = false;
  }

let href p = p.href
let time p = p.time
let description p = p.description
let extended p = p.extended
let tag p = p.tag
let hash p = p.hash
let shared p = p.shared
let toread p = p.toread

let equal x y =
  String.equal x.href y.href
  && String.equal x.time y.time
  && Option.equal String.equal x.description y.description
  && Option.equal String.equal x.extended y.extended
  && List.equal String.equal x.tag y.tag
  && Option.equal String.equal x.hash y.hash
  && Bool.equal x.shared y.shared
  && Bool.equal x.toread y.toread

let pp fmt p =
  let open Format in
  let none fmt () = fprintf fmt "None" in
  let some fmt elem = fprintf fmt "%S" elem in
  let pp_string_option = pp_print_option ~none some in
  let pp_sep fmt () = fprintf fmt ";@;<1 2>" in
  let pp_elem fmt elem = fprintf fmt "%S" elem in
  let pp_tag = pp_print_list ~pp_sep pp_elem in
  fprintf fmt "@[<hv>{";
  fprintf fmt "@;<1 2>@[href =@ %S@];" p.href;
  fprintf fmt "@;<1 2>@[time =@ %S@];" p.time;
  fprintf fmt "@;<1 2>@[description =@ %a@];" pp_string_option p.description;
  fprintf fmt "@;<1 2>@[extended =@ %a@];" pp_string_option p.extended;
  fprintf fmt "@;<1 2>@[tag =@ @[<hv>[@;<0 2>%a@;<0 0>]@]@];" pp_tag p.tag;
  fprintf fmt "@;<1 2>@[hash =@ %a@];" pp_string_option p.hash;
  fprintf fmt "@;<1 2>@[shared =@ %B@];" p.shared;
  fprintf fmt "@;<1 2>@[toread =@ %B@];" p.toread;
  fprintf fmt "@;<1 0>}@]"

let to_string = Format.asprintf "%a" pp

let accumulate_pinboard_attr (pinboard : t)
    (((_namespace, key), value) : (string * string) * string) : t =
  match String.lowercase_ascii key with
  | "href" -> { pinboard with href = value }
  | "time" -> { pinboard with time = value }
  | "description" when value <> "" -> { pinboard with description = Some value }
  | "extended" when value <> "" -> { pinboard with extended = Some value }
  | "tag" when value <> "" ->
      let tag = Str.split (Str.regexp "[ \t]+") value in
      { pinboard with tag }
  | "hash" when value <> "" -> { pinboard with hash = Some value }
  | "shared" -> { pinboard with shared = value = "yes" }
  | "toread" -> { pinboard with toread = value = "yes" }
  | _ -> pinboard

let t_of_attrs (attrs : Attrs.t) : t = List.fold_left accumulate_pinboard_attr empty attrs

let from_xml content =
  let stream = Markup.string content in
  let xml = Markup.parse_xml stream in
  let signals = Markup.signals xml in
  let continue = ref true in
  let acc = ref [] in
  while !continue do
    match Markup.next signals with
    | None -> continue := false
    | Some (`Start_element ((_, "post"), attrs)) -> acc := t_of_attrs attrs :: !acc
    | Some (`Start_element ((_, "posts"), _)) -> ()
    | Some (`Start_element ((_, s), _)) -> failwith ("unexpected Start_element: " ^ s)
    | Some _ -> ()
  done;
  !acc

let t_of_yaml (value : Yaml.value) : t =
  let open Yaml_ext in
  let href = get_field ~key:"href" value |> Yaml.Util.to_string_exn in
  let time = get_field ~key:"time" value |> Yaml.Util.to_string_exn in
  let description = map_optional_field_exn ~key:"description" ~f:Yaml.Util.to_string_exn value in
  let extended = map_optional_field_exn ~key:"extended" ~f:Yaml.Util.to_string_exn value in
  let tags = get_field ~key:"tags" value |> Yaml.Util.to_string_exn in
  let tag = Str.split (Str.regexp "[ \t]+") tags in
  let hash = map_optional_field_exn ~key:"hash" ~f:Yaml.Util.to_string_exn value in
  let shared = get_field ~key:"shared" value |> Yaml.Util.to_string_exn = "yes" in
  let toread = get_field ~key:"toread" value |> Yaml.Util.to_string_exn = "yes" in
  { href; time; description; extended; tag; hash; shared; toread }

let from_json content = Ezjsonm.from_string content |> Yaml_ext.map_array_exn t_of_yaml
