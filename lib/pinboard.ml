open Prelude
module Attrs = Markup_ext.Attrs

exception Unexpected_xml_element of string

type t = {
  mutable href : string;
  mutable time : string;
  mutable description : string option;
  mutable extended : string option;
  mutable tag : string list;
  mutable hash : string option;
  mutable shared : bool;
  mutable toread : bool;
}

let fresh () =
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

let to_entity (p : t) : Entity.t =
  let uri = Uri.of_string (href p) in
  let created_at = Entity.Time.of_string (time p) in
  let maybe_name = Option.map Entity.Name.of_string (description p) in
  let labels = Entity.Label_set.of_list (List.map Entity.Label.of_string (tag p)) in
  let extended = Option.map Entity.Extended.of_string (extended p) in
  let shared = shared p in
  let to_read = toread p in
  Entity.make uri created_at ~maybe_name ~labels ~extended ~shared ~to_read ()

let to_collection (posts : t list) : Collection.t =
  let ret = Collection.create () in
  let sorted = List.sort (fun a b -> String.compare a.time b.time) posts in
  List.iter (fun post -> ignore (Collection.insert ret (to_entity post))) sorted;
  ret

module Json = struct
  let t_of_yaml (value : Yaml.value) : t =
    let remaining =
      match value with
      | `O assoc -> ref assoc
      | _ -> raise (Yaml.Util.Value_error "Expected an object")
    in
    let pinboard = fresh () in
    while not (List.is_empty !remaining) do
      let head_opt, tail = Prelude.List_ext.uncons !remaining in
      remaining := tail;
      match head_opt with
      | Some (key, value) -> begin
          match key with
          | "href" -> pinboard.href <- Yaml.Util.to_string_exn value
          | "time" -> pinboard.time <- Yaml.Util.to_string_exn value
          | "description" ->
              pinboard.description <- Prelude.option_of_string (Yaml.Util.to_string_exn value)
          | "extended" ->
              pinboard.extended <- Prelude.option_of_string (Yaml.Util.to_string_exn value)
          | "tags" ->
              let tags = Yaml.Util.to_string_exn value in
              let tag = Str.split (Str.regexp "[ \t]+") tags in
              pinboard.tag <- tag
          | "hash" -> pinboard.hash <- Prelude.option_of_string (Yaml.Util.to_string_exn value)
          | "shared" -> pinboard.shared <- Yaml.Util.to_string_exn value = "yes"
          | "toread" -> pinboard.toread <- Yaml.Util.to_string_exn value = "yes"
          | _ -> ()
        end
      | None -> ()
    done;
    pinboard

  let from_json content = Yaml_ext.map_array_exn t_of_yaml (Ezjsonm.from_string content)
  let parse content = to_collection (from_json content)
end

module Xml = struct
  let t_of_attrs (attrs : Attrs.t) : t =
    let pinboard = fresh () in
    let remaining = ref attrs in
    while not (List.is_empty !remaining) do
      let head_opt, tail = Prelude.List_ext.uncons !remaining in
      remaining := tail;
      match head_opt with
      | Some ((_, key), value) -> begin
          match String.lowercase_ascii key with
          | "href" -> pinboard.href <- value
          | "time" -> pinboard.time <- value
          | "description" when value <> String.empty -> pinboard.description <- Some value
          | "extended" when value <> String.empty -> pinboard.extended <- Some value
          | "tag" when value <> String.empty ->
              let tag = Str.split (Str.regexp "[ \t]+") value in
              pinboard.tag <- tag
          | "hash" when value <> String.empty -> pinboard.hash <- Some value
          | "shared" -> pinboard.shared <- value = "yes"
          | "toread" -> pinboard.toread <- value = "yes"
          | _ -> ()
        end
      | None -> ()
    done;
    pinboard

  let from_xml content =
    if String.length content = 0 then
      []
    else
      let input = Xmlm.make_input (`String (0, content)) in
      let continue = ref true in
      let acc = ref [] in
      while !continue do
        if Xmlm.eoi input then
          continue := false
        else
          match Xmlm.input input with
          | `El_start ((_, "post"), attrs) -> acc := t_of_attrs attrs :: !acc
          | `El_start ((_, "posts"), _) -> ()
          | `El_start ((_, s), _) -> raise (Unexpected_xml_element s)
          | `El_end -> ()
          | `Data _ -> ()
          | `Dtd _ -> ()
      done;
      List.rev !acc

  let parse content = to_collection (from_xml content)
end
