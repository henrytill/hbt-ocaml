open Prelude
module Attrs = Markup_ext.Attrs

exception Unexpected_xml_element of string

type t = {
  href : string;
  time : string;
  description : string option;
  extended : string option;
  tag : string list;
  meta : string option;
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
    meta = None;
    hash = None;
    shared = false;
    toread = false;
  }

let href p = p.href
let time p = p.time
let description p = p.description
let extended p = p.extended
let tag p = p.tag
let meta p = p.meta
let hash p = p.hash
let shared p = p.shared
let toread p = p.toread

let equal x y =
  String.equal x.href y.href
  && String.equal x.time y.time
  && Option.equal String.equal x.description y.description
  && Option.equal String.equal x.extended y.extended
  && List.equal String.equal x.tag y.tag
  && Option.equal String.equal x.meta y.meta
  && Option.equal String.equal x.hash y.hash
  && Bool.equal x.shared y.shared
  && Bool.equal x.toread y.toread

let pp =
  Fmt.record
    [
      Fmt.(field "href" (fun p -> p.href) (quote string));
      Fmt.(field "time" (fun p -> p.time) (quote string));
      Fmt.(field "description" (fun p -> p.description) (option (quote string)));
      Fmt.(field "extended" (fun p -> p.extended) (option (quote string)));
      Fmt.(field "tag" (fun p -> p.tag) (list ~sep:semi (quote string)));
      Fmt.(field "meta" (fun p -> p.meta) (option (quote string)));
      Fmt.(field "hash" (fun p -> p.hash) (option (quote string)));
      Fmt.(field "shared" (fun p -> p.shared) bool);
      Fmt.(field "toread" (fun p -> p.toread) bool);
    ]

let to_string = Fmt.str "%a" pp

let to_entity (p : t) : Entity.t =
  let uri = Entity.Uri.of_string (href p) in
  let created_at = Entity.Time.of_string (time p) in
  let maybe_name = Option.map Entity.Name.of_string (description p) in
  let labels = Entity.Label_set.of_list (List.map Entity.Label.of_string (tag p)) in
  let extended = Option.map Entity.Extended.of_string (extended p) in
  let shared = shared p in
  let to_read = toread p in
  Entity.make uri created_at ~maybe_name ~labels ~extended ~shared ~to_read ()

let to_collection (ps : t list) : Collection.t =
  let coll = Collection.create () in
  let sorted = List.sort (fun a b -> String.compare a.time b.time) ps in
  List.iter (fun post -> ignore (Collection.insert coll (to_entity post))) sorted;
  coll

module Json = struct
  let build p (k, v) =
    match k with
    | "href" -> { p with href = Yaml.Util.to_string_exn v }
    | "time" -> { p with time = Yaml.Util.to_string_exn v }
    | "description" -> { p with description = option_of_string (Yaml.Util.to_string_exn v) }
    | "extended" -> { p with extended = option_of_string (Yaml.Util.to_string_exn v) }
    | "tags" ->
        let tags = Yaml.Util.to_string_exn v in
        let tag = Str.split (Str.regexp "[ \t]+") tags in
        { p with tag }
    | "meta" -> { p with meta = option_of_string (Yaml.Util.to_string_exn v) }
    | "hash" -> { p with hash = option_of_string (Yaml.Util.to_string_exn v) }
    | "shared" -> { p with shared = Yaml.Util.to_string_exn v = "yes" }
    | "toread" -> { p with toread = Yaml.Util.to_string_exn v = "yes" }
    | _ -> p

  let t_of_yaml (value : Yaml.value) : t =
    let assoc =
      match value with
      | `O assoc -> assoc
      | _ -> raise (Yaml.Util.Value_error "Expected an object")
    in
    List.fold_left build empty assoc

  let from_json content = Yaml_ext.map_array_exn t_of_yaml (Ezjsonm.from_string content)
  let parse content = to_collection (from_json content)
end

module Xml = struct
  let build p ((_, k), v) =
    match String.lowercase_ascii k with
    | "href" -> { p with href = v }
    | "time" -> { p with time = v }
    | "description" when v <> String.empty -> { p with description = Some v }
    | "extended" when v <> String.empty -> { p with extended = Some v }
    | "tag" when v <> String.empty ->
        let tag = Str.split (Str.regexp "[ \t]+") v in
        { p with tag }
    | "meta" when v <> String.empty -> { p with meta = Some v }
    | "hash" when v <> String.empty -> { p with hash = Some v }
    | "shared" -> { p with shared = v = "yes" }
    | "toread" -> { p with toread = v = "yes" }
    | _ -> p

  let t_of_attrs (attrs : Attrs.t) : t = List.fold_left build empty attrs

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
