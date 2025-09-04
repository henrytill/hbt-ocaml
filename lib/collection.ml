module Yaml_ext = Prelude.Yaml_ext

module Version = struct
  type t = Semver.t

  exception Unsupported

  let expected : t = (0, 1, 0)

  let check version =
    if not (Semver.equal version expected) then
      raise Unsupported

  let t_of_yaml value = Option.get (Semver.of_string (Yaml.Util.to_string_exn value))
  let yaml_of_t version = Yaml.Util.string (Semver.to_string version)
end

module Id = struct
  type t = int

  let of_int (i : int) : t = i
  let to_int = Fun.id
  let equal = Int.equal
  let compare = Int.compare
  let pp = Format.pp_print_int
  let t_of_yaml value = int_of_float (Yaml.Util.to_float_exn value)
  let yaml_of_t id = Yaml.Util.float (float_of_int id)
end

module Uri_hashtbl = Hashtbl.Make (struct
  type t = Uri.t

  let equal = Uri.equal

  let hash uri =
    let _ = Uri.query uri in
    Hashtbl.hash uri
end)

type edges = Id.t Dynarray.t

type t = {
  nodes : Entity.t Dynarray.t;
  edges : edges Dynarray.t;
  uris : Id.t Uri_hashtbl.t;
}

let create () =
  let nodes = Dynarray.create () in
  let edges = Dynarray.create () in
  let uris = Uri_hashtbl.create 1024 in
  { nodes; edges; uris }

let make n =
  let nodes = Dynarray.make n Entity.empty in
  let edges = Dynarray.make n (Dynarray.create ()) in
  let uris = Uri_hashtbl.create n in
  { nodes; edges; uris }

let length c =
  let ret = Dynarray.length c.nodes in
  assert (ret = Dynarray.length c.edges);
  ret

let is_empty c =
  let ret = Dynarray.is_empty c.nodes in
  assert (ret = Dynarray.is_empty c.edges);
  ret

let id c uri = Uri_hashtbl.find_opt c.uris uri
let contains c uri = Option.is_some (id c uri)

let insert c e =
  let id = Id.of_int (length c) in
  Dynarray.add_last c.nodes e;
  Dynarray.add_last c.edges (Dynarray.create ());
  let uri = Entity.uri (Dynarray.get c.nodes id) in
  Uri_hashtbl.add c.uris uri id;
  id

let upsert c e =
  match id c (Entity.uri e) with
  | None -> insert c e
  | Some id ->
      let existing = Dynarray.get c.nodes id in
      let updated = Entity.absorb e existing in
      let () =
        if not (Entity.equal updated existing) then
          Dynarray.(set c.nodes id updated)
      in
      id

let add_edge c from target =
  let from_edges = Dynarray.get c.edges from in
  if not (Dynarray.exists (Id.equal target) from_edges) then
    Dynarray.add_last from_edges target

let add_edges c from target =
  add_edge c from target;
  add_edge c target from

let entity c id = Dynarray.get c.nodes id
let edges c id = Dynarray.(to_array (get c.edges id))
let entities c = Dynarray.to_array c.nodes

let t_of_yaml value =
  let open Yaml_ext in
  begin
    let version = get_field ~key:"version" value |> Version.t_of_yaml in
    Version.check version
  end;
  let length = get_field ~key:"length" value |> int_of_float_exn in
  let ret = make length in
  let process_item pairs =
    let i = get_field ~key:"id" pairs |> int_of_float_exn in
    let entity = get_field ~key:"entity" pairs |> Entity.t_of_yaml in
    let edges =
      get_field ~key:"edges" pairs |> map_array_exn int_of_float_exn |> Dynarray.of_list
    in
    let uri = Entity.uri entity in
    Dynarray.set ret.nodes i entity;
    Dynarray.set ret.edges i edges;
    Uri_hashtbl.add ret.uris uri i
  in
  get_field ~key:"value" value |> iter_array_exn process_item;
  ret

let yaml_of_t c =
  let f i entity =
    assert (Option.equal Id.equal (id c (Entity.uri entity)) (Some (Id.of_int i)));
    let entity_yaml = Entity.yaml_of_t entity in
    let edges_yaml = Dynarray.(to_list (map (fun e -> `Float (float_of_int e)) (get c.edges i))) in
    `O [ ("id", `Float (float_of_int i)); ("entity", entity_yaml); ("edges", `A edges_yaml) ]
  in
  let items = Dynarray.(to_list (mapi f c.nodes)) in
  `O
    [
      ("version", Version.(yaml_of_t expected));
      ("length", `Float (float_of_int (length c)));
      ("value", `A items);
    ]

let map_labels (f : Entity.Label_set.t -> Entity.Label_set.t) (c : t) : t =
  let nodes = Dynarray.map (Entity.map_labels f) c.nodes in
  { c with nodes }

let yaml_to_map (yaml : Yaml.value) : Entity.Label.t Entity.Label_map.t =
  let open Entity in
  let f acc (k, v) =
    let k = Label.of_string k in
    let v = Label.t_of_yaml v in
    Label_map.add k v acc
  in
  Yaml_ext.fold_object_exn f Label_map.empty yaml

let update_labels (yaml : Yaml.value) : t -> t =
  let mapping = yaml_to_map yaml in
  let f label = Option.value ~default:label (Entity.Label_map.find_opt label mapping) in
  map_labels (Entity.Label_set.map f)

module Netscape = struct
  open Prelude
  module Attrs = Markup_ext.Attrs

  let element_of_string = function
    | "h3" -> `H3
    | "dt" -> `Dt
    | "a" -> `A
    | "dd" -> `Dd
    | "dl" -> `Dl
    | name -> `Other name

  let mk_labels ls s = Entity.(Label_set.add (Label.of_string s) ls)

  let from_html content =
    let collection = create () in
    let maybe_description = ref None in
    let maybe_extended = ref None in
    let attributes = ref Attrs.empty in
    let folder_stack = Stack.create () in
    let waiting_for = ref `Nothing in

    let add_pending () =
      let folder_labels = Stack.fold mk_labels Entity.Label_set.empty folder_stack in
      let entity =
        Entity.Html.create_entity !attributes !maybe_description folder_labels !maybe_extended
      in
      ignore (upsert collection entity);
      attributes := Attrs.empty;
      maybe_description := None;
      maybe_extended := None
    in

    let stream = Markup.string content in
    let html = Markup.parse_html stream in
    let signals = Markup.signals html in

    let element_stack = Stack.create () in
    let continue = ref true in

    while !continue do
      match Markup.next signals with
      | None ->
          assert (Attrs.is_empty !attributes);
          continue := false
      | Some (`Start_element ((_, name), _)) when element_of_string name = `H3 ->
          Stack.push `H3 element_stack;
          waiting_for := `Folder_name
      | Some (`Start_element ((_, name), _)) when element_of_string name = `Dt ->
          Stack.push `Dt element_stack;
          unless (Attrs.is_empty !attributes) add_pending
      | Some (`Start_element ((_, name), attrs)) when element_of_string name = `A ->
          Stack.push `A element_stack;
          attributes := attrs;
          waiting_for := `Bookmark_description
      | Some (`Start_element ((_, name), _)) when element_of_string name = `Dd ->
          Stack.push `Dd element_stack;
          let@ () = unless (Attrs.is_empty !attributes) in
          waiting_for := `Extended_description
      | Some (`Start_element ((_, name), _)) -> Stack.push (element_of_string name) element_stack
      | Some (`Text xs) -> begin
          match !waiting_for with
          | `Folder_name ->
              let folder_name = String.trim (String.concat String.empty xs) in
              Stack.push folder_name folder_stack;
              waiting_for := `Nothing
          | `Bookmark_description ->
              let description = String.trim (String.concat String.empty xs) in
              maybe_description := Some description;
              waiting_for := `Nothing
          | `Extended_description ->
              let extended = String.trim (String.concat String.empty xs) in
              maybe_extended := Some extended;
              unless (Attrs.is_empty !attributes) add_pending;
              waiting_for := `Nothing
          | `Nothing -> ()
        end
      | Some `End_element ->
          let maybe_head = Stack.pop_opt element_stack in
          if maybe_head = Some `Dl then begin
            unless (Attrs.is_empty !attributes) add_pending;
            ignore (Stack.pop_opt folder_stack)
          end
      | Some _ -> ()
    done;

    collection

  module Template_entity = struct
    open Prelude

    type t = {
      uri : string;
      title : string;
      created_at : string;
      last_modified : string option;
      tags : string option;
      description : string option;
      last_visit : string option;
      shared : bool;
      to_read : bool;
      is_feed : bool;
    }

    let of_entity (entity : Entity.t) : t =
      let open Entity in
      let uri = Uri.to_string (Entity.uri entity) in
      let created_at = Time.to_string (Entity.created_at entity) in
      let title =
        match Name_set.elements (Entity.names entity) with
        | [] -> uri
        | names ->
            let name_strings = List.map Name.to_string names in
            List.hd (List.sort String.compare name_strings)
      in
      let last_modified =
        match Entity.updated_at entity with
        | [] -> None
        | times ->
            let latest = List.hd (List.sort (fun a b -> Time.compare b a) times) in
            Some (Time.to_string latest)
      in
      let tags =
        let labels = Label_set.elements (Entity.labels entity) in
        match labels with
        | [] -> None
        | _ -> Some (String.concat "," (List.map Label.to_string labels))
      in
      let description = Option.map Extended.to_string (Entity.extended entity) in
      let last_visit = Option.map Time.to_string (Entity.last_visited_at entity) in
      {
        uri;
        title;
        created_at;
        last_modified;
        tags;
        description;
        last_visit;
        shared = Entity.shared entity;
        to_read = Entity.to_read entity;
        is_feed = Entity.is_feed entity;
      }

    let yaml_of_t (template_entity : t) : Yaml.value =
      let base_fields =
        [
          ("uri", `String template_entity.uri);
          ("createdAt", `String template_entity.created_at);
          ("shared", `Bool template_entity.shared);
          ("toRead", `Bool template_entity.to_read);
          ("isFeed", `Bool template_entity.is_feed);
          ("title", `String template_entity.title);
        ]
      in
      let optional_fields =
        List_ext.filter_some
          [
            Option.map (fun v -> ("lastModified", `String v)) template_entity.last_modified;
            Option.map (fun v -> ("tags", `String v)) template_entity.tags;
            Option.map (fun v -> ("description", `String v)) template_entity.description;
            Option.map (fun v -> ("lastVisit", `String v)) template_entity.last_visit;
          ]
      in
      `O (base_fields @ optional_fields)
  end

  let to_html c =
    let f e acc = Template_entity.(yaml_of_t (of_entity e)) :: acc in
    let entities_mustache = Array.fold_right f (entities c) [] in
    let json = `O [ ("entities", `A entities_mustache) ] in
    let template = Mustache.of_string Templates.netscape_bookmarks in
    Mustache.render ~strict:false template json
end

let from_html = Netscape.from_html
let to_html = Netscape.to_html
