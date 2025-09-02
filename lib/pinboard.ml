open Prelude
module Attrs = Markup_ext.Attrs

exception Unexpected_xml_element of string

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

let accumulate_pinboard_attr (pinboard : t) (((_, key), value) : Attrs.elt) : t =
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
    | Some (`Start_element ((_, s), _)) -> raise (Unexpected_xml_element s)
    | Some _ -> ()
  done;
  !acc

let accumulate_pinboard_yaml (pinboard : t) ((key, value) : string * Yaml.value) : t =
  match key with
  | "href" -> { pinboard with href = Yaml.Util.to_string_exn value }
  | "time" -> { pinboard with time = Yaml.Util.to_string_exn value }
  | "description" ->
      { pinboard with description = Prelude.option_of_string (Yaml.Util.to_string_exn value) }
  | "extended" ->
      { pinboard with extended = Prelude.option_of_string (Yaml.Util.to_string_exn value) }
  | "tags" ->
      let tags = Yaml.Util.to_string_exn value in
      let tag = Str.split (Str.regexp "[ \t]+") tags in
      { pinboard with tag }
  | "hash" -> { pinboard with hash = Prelude.option_of_string (Yaml.Util.to_string_exn value) }
  | "shared" -> { pinboard with shared = Yaml.Util.to_string_exn value = "yes" }
  | "toread" -> { pinboard with toread = Yaml.Util.to_string_exn value = "yes" }
  | _ -> pinboard

let t_of_yaml (value : Yaml.value) : t =
  let open Yaml_ext in
  fold_object_exn accumulate_pinboard_yaml empty value

let from_json content = Ezjsonm.from_string content |> Yaml_ext.map_array_exn t_of_yaml
