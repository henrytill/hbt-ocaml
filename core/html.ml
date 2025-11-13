open Prelude
module Attrs = Markup_ext.Attrs

module Elt = struct
  type t =
    | Dl
    | Dt
    | Dd
    | H3
    | A
    | Other

  let of_string = function
    | "dl" -> Dl
    | "dt" -> Dt
    | "dd" -> Dd
    | "h3" -> H3
    | "a" -> A
    | _ -> Other

  let equals a b =
    match (a, b) with
    | Dl, Dl | Dt, Dt | Dd, Dd | H3, H3 | A, A | Other, Other -> true
    | _, _ -> false
end

let mk_labels acc s = Entity.(Label_set.add (Label.of_string s) acc)

let parse content =
  let coll = Collection.create () in
  let maybe_description = ref None in
  let maybe_extended = ref None in
  let attributes = ref Attrs.empty in
  let folder_stack = Stack.create () in
  let waiting_for = ref `Nothing in

  let add_pending () =
    let entity =
      let open Entity in
      let some s = Name_set.singleton (Name.of_string s) in
      let names = Option.fold ~none:Name_set.empty ~some !maybe_description in
      let folder_labels = Stack.fold mk_labels Entity.Label_set.empty folder_stack in
      let extended = Option.map Extended.of_string !maybe_extended in
      Html.entity_of_attrs !attributes names folder_labels extended
    in
    ignore (Collection.upsert coll entity);
    attributes := Attrs.empty;
    maybe_description := None;
    maybe_extended := None
  in

  let stream = Markup.string content in
  let html = Markup.parse_html stream in
  let signals = Markup.signals html in

  let elt_stack = Stack.create () in
  let continue = ref true in

  while !continue do
    match Markup.next signals with
    | None ->
        assert (Attrs.is_empty !attributes);
        continue := false
    | Some (`Start_element ((_, name), _)) when Elt.(equals (of_string name) H3) ->
        Stack.push Elt.H3 elt_stack;
        waiting_for := `Folder_name
    | Some (`Start_element ((_, name), _)) when Elt.(equals (of_string name) Dt) ->
        Stack.push Elt.Dt elt_stack;
        unless (Attrs.is_empty !attributes) add_pending
    | Some (`Start_element ((_, name), attrs)) when Elt.(equals (of_string name) A) ->
        Stack.push Elt.A elt_stack;
        attributes := attrs;
        waiting_for := `Bookmark_description
    | Some (`Start_element ((_, name), _)) when Elt.(equals (of_string name) Dd) ->
        Stack.push Elt.Dd elt_stack;
        unless (Attrs.is_empty !attributes) (fun () -> waiting_for := `Extended_description)
    | Some (`Start_element ((_, name), _)) -> Stack.push (Elt.of_string name) elt_stack
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
        let maybe_head = Stack.pop_opt elt_stack in
        if maybe_head = Some Dl then begin
          unless (Attrs.is_empty !attributes) add_pending;
          ignore (Stack.pop_opt folder_stack)
        end
    | Some _ -> ()
  done;

  coll

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
    let uri = Entity.Uri.to_string (Entity.uri entity) in
    let title =
      match Entity.Name_set.elements (Entity.names entity) with
      | [] -> uri
      | names ->
          let name_strings = List.map Entity.Name.to_string names in
          List.hd (List.sort String.compare name_strings)
    in
    let last_modified =
      match Entity.updated_at entity with
      | [] -> None
      | times ->
          let latest = List.hd (List.sort (fun a b -> Entity.Time.compare b a) times) in
          Some (Entity.Time.to_string latest)
    in
    let tags =
      let labels = Entity.Label_set.elements (Entity.labels entity) in
      match labels with
      | [] -> None
      | _ -> Some (String.concat "," (List.map Entity.Label.to_string labels))
    in
    {
      uri;
      title;
      created_at = Entity.Time.to_string (Entity.created_at entity);
      last_modified;
      tags;
      description = Option.map Entity.Extended.to_string (Entity.extended entity);
      last_visit = Option.map Entity.Time.to_string (Entity.last_visited_at entity);
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

let format c =
  let f e acc = Template_entity.(yaml_of_t (of_entity e)) :: acc in
  let entities_mustache = Array.fold_right f (Collection.entities c) [] in
  let json = `O [ ("entities", `A entities_mustache) ] in
  let template = Mustache.of_string Templates.netscape_bookmarks in
  Mustache.render ~strict:false template json
