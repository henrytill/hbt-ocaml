module Yaml_ext = Prelude.Yaml_ext

let pp_print_set pp_item fmt items =
  let open Format in
  let pp_sep fmt () = fprintf fmt ";@ " in
  fprintf fmt "@[<h>{%a}@]" (pp_print_list ~pp_sep pp_item) items

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

module Name = struct
  type t = string

  let of_string (s : string) : t = s
  let to_string = Fun.id
  let equal = String.equal
  let compare = String.compare
  let pp fmt = Format.fprintf fmt "%S"
  let t_of_yaml = Yaml.Util.to_string_exn
  let yaml_of_t = Yaml.Util.string
end

module Name_set = struct
  include Set.Make (Name)

  let pp fmt s = pp_print_set Name.pp fmt (elements s)
  let t_of_yaml value = of_list (Yaml_ext.map_array_exn Name.t_of_yaml value)
  let yaml_of_t set = Yaml.Util.list Name.yaml_of_t (to_list set)
end

module Label = struct
  type t = string

  let of_string (s : string) : t = s
  let to_string = Fun.id
  let equal = String.equal
  let compare = String.compare
  let pp fmt = Format.fprintf fmt "%S"
  let t_of_yaml = Yaml.Util.to_string_exn
  let yaml_of_t = Yaml.Util.string
end

module Label_set = struct
  include Set.Make (Label)

  let pp fmt s = pp_print_set Label.pp fmt (elements s)
  let t_of_yaml value = of_list (Yaml_ext.map_array_exn Label.t_of_yaml value)
  let yaml_of_t set = Yaml.Util.list Label.yaml_of_t (to_list set)
end

module Label_map = Map.Make (Label)

module Time = struct
  type t = float * Unix.tm

  let empty =
    let t = 0.0 in
    (t, Unix.gmtime t)

  let int_of_month = function
    | "January" -> 0
    | "February" -> 1
    | "March" -> 2
    | "April" -> 3
    | "May" -> 4
    | "June" -> 5
    | "July" -> 6
    | "August" -> 7
    | "September" -> 8
    | "October" -> 9
    | "November" -> 10
    | "December" -> 11
    | _ -> invalid_arg "Time.int_of_month: invalid month name"

  let parse_date s =
    Scanf.sscanf s "%s %d, %d" (fun month day year -> (int_of_month month, day, year))

  let parse_iso8601 s =
    try
      (* Try to parse ISO 8601ish format *)
      let f year month day hour min sec = (year, month - 1, day, hour, min, sec) in
      Scanf.sscanf s "%d-%d-%dT%d:%d:%dZ" f
    with _ ->
      (* Fallback to a date without time *)
      let f year month day = (year, month - 1, day, 0, 0, 0) in
      Scanf.sscanf s "%d-%d-%d" f

  let of_string (s : string) : t =
    let open Unix in
    try
      (* Try ISO 8601ish format first (for Pinboard XML) *)
      let year, tm_mon, tm_mday, tm_hour, tm_min, tm_sec = parse_iso8601 s in
      let tm_year = year - 1900 in
      let tm =
        {
          tm_sec;
          tm_min;
          tm_hour;
          tm_mday;
          tm_mon;
          tm_year;
          tm_wday = 0;
          tm_yday = 0;
          tm_isdst = false;
        }
      in
      mktime tm
    with _ ->
      (* Fallback to original format "Month Day, Year" *)
      let tm_mon, tm_mday, year = parse_date s in
      let tm_year = year - 1900 in
      let tm =
        {
          tm_sec = 0;
          tm_min = 0;
          tm_hour = 0;
          tm_mday;
          tm_mon;
          tm_year;
          tm_wday = 0;
          tm_yday = 0;
          tm_isdst = false;
        }
      in
      mktime tm

  let to_string t = fst t |> int_of_float |> string_of_int
  let equal x y = Float.equal (fst x) (fst y)
  let compare x y = Float.compare (fst x) (fst y)
  let pp fmt t = Format.fprintf fmt "%S" (to_string t)

  let t_of_yaml value =
    let f = Yaml.Util.to_float_exn value in
    (f, Unix.gmtime f)

  let yaml_of_t time = Yaml.Util.float (fst time)
end

module Extended = struct
  type t = string

  let of_string (s : string) : t = s
  let to_string = Fun.id
  let equal = String.equal
  let compare = String.compare
  let pp fmt = Format.fprintf fmt "%S"
  let t_of_yaml = Yaml.Util.to_string_exn
  let yaml_of_t = Yaml.Util.string
end

module Entity = struct
  type t = {
    uri : Uri.t;
    created_at : Time.t;
    updated_at : Time.t list;
    names : Name_set.t;
    labels : Label_set.t;
    extended : Extended.t option;
    shared : bool;
    to_read : bool;
    last_visited_at : Time.t option;
    is_feed : bool;
  }

  let make uri created_at maybe_name labels =
    let uri = Uri.canonicalize uri in
    let updated_at = [] in
    let names = Option.fold ~none:Name_set.empty ~some:Name_set.singleton maybe_name in
    let extended = None in
    let shared = false in
    let to_read = false in
    let last_visited_at = None in
    let is_feed = false in
    {
      uri;
      created_at;
      updated_at;
      names;
      labels;
      extended;
      shared;
      to_read;
      last_visited_at;
      is_feed;
    }

  let empty =
    let uri = Uri.empty in
    let created_at = Time.empty in
    let updated_at = [] in
    let names = Name_set.empty in
    let labels = Label_set.empty in
    let extended = None in
    let shared = false in
    let to_read = false in
    let last_visited_at = None in
    let is_feed = false in
    {
      uri;
      created_at;
      updated_at;
      names;
      labels;
      extended;
      shared;
      to_read;
      last_visited_at;
      is_feed;
    }

  let of_pinboard (p : Pinboard.t) : t =
    let open Pinboard in
    let uri = Uri.of_string (href p) in
    let created_at = Time.of_string (time p) in
    let updated_at = [] in
    let names = Option.fold ~none:Name_set.empty ~some:Name_set.singleton (description p) in
    let labels = Label_set.of_list (tag p) in
    let extended = Option.map Extended.of_string (extended p) in
    let shared = shared p in
    let to_read = toread p in
    let last_visited_at = None in
    let is_feed = false in
    {
      uri;
      created_at;
      updated_at;
      names;
      labels;
      extended;
      shared;
      to_read;
      last_visited_at;
      is_feed;
    }

  let equal x y =
    Uri.equal x.uri y.uri
    && Time.equal x.created_at y.created_at
    && List.equal Time.equal x.updated_at y.updated_at
    && Name_set.equal x.names y.names
    && Label_set.equal x.labels y.labels
    && Option.equal Extended.equal x.extended y.extended
    && Bool.equal x.shared y.shared
    && Bool.equal x.to_read y.to_read
    && Option.equal Time.equal x.last_visited_at y.last_visited_at
    && Bool.equal x.is_feed y.is_feed

  let pp fmt e =
    let open Format in
    let pp_sep fmt () = fprintf fmt ";@;<1 2>" in
    let pp_updated_at = pp_print_list ~pp_sep Time.pp in
    let none fmt () = fprintf fmt "None" in
    let some fmt = fprintf fmt "Some %a" Extended.pp in
    let pp_extended_opt = pp_print_option ~none some in
    let pp_last_visit_opt = pp_print_option ~none Time.pp in
    fprintf fmt "@[<hv>{";
    fprintf fmt "@;<1 2>@[uri =@ %a@];" Uri.pp e.uri;
    fprintf fmt "@;<1 2>@[created_at =@ %a@];" Time.pp e.created_at;
    fprintf fmt "@;<1 2>@[updated_at =@ @[<hv>[@;<0 2>%a@;<0 0>]@]@];" pp_updated_at e.updated_at;
    fprintf fmt "@;<1 2>@[names =@ %a@];" Name_set.pp e.names;
    fprintf fmt "@;<1 2>@[labels =@ %a@];" Label_set.pp e.labels;
    fprintf fmt "@;<1 2>@[extended =@ %a@];" pp_extended_opt e.extended;
    fprintf fmt "@;<1 2>@[shared =@ %a@];" pp_print_bool e.shared;
    fprintf fmt "@;<1 2>@[to_read =@ %a@];" pp_print_bool e.to_read;
    fprintf fmt "@;<1 2>@[last_visited_at =@ %a@];" pp_last_visit_opt e.last_visited_at;
    fprintf fmt "@;<1 2>@[is_feed =@ %a@];" pp_print_bool e.is_feed;
    fprintf fmt "@;<1 0>}@]"

  let t_of_yaml value =
    let open Yaml_ext in
    {
      uri = get_field ~key:"uri" value |> Yaml.Util.to_string_exn |> Uri.of_string;
      created_at = get_field ~key:"createdAt" value |> Time.t_of_yaml;
      updated_at = get_field ~key:"updatedAt" value |> map_array_exn Time.t_of_yaml;
      names = get_field ~key:"names" value |> Name_set.t_of_yaml;
      labels = get_field ~key:"labels" value |> Label_set.t_of_yaml;
      extended = map_optional_field_exn ~key:"extended" ~f:Extended.t_of_yaml value;
      shared = get_field ~key:"shared" value |> Yaml.Util.to_bool_exn;
      to_read = get_field ~key:"toRead" value |> Yaml.Util.to_bool_exn;
      last_visited_at = map_optional_field_exn ~key:"lastVisitedAt" ~f:Time.t_of_yaml value;
      is_feed = get_field ~key:"isFeed" value |> Yaml.Util.to_bool_exn;
    }

  let yaml_of_t entity =
    let maybe_extended =
      match entity.extended with
      | Some extended -> [ ("extended", Extended.yaml_of_t extended) ]
      | None -> []
    in
    let maybe_last_visit =
      match entity.last_visited_at with
      | Some last_visit -> [ ("lastVisitedAt", Time.yaml_of_t last_visit) ]
      | None -> []
    in
    `O
      ([
         ("uri", `String (Uri.to_string entity.uri));
         ("createdAt", Time.yaml_of_t entity.created_at);
         ("updatedAt", `A (List.map Time.yaml_of_t entity.updated_at));
         ("names", Name_set.yaml_of_t entity.names);
         ("labels", Label_set.yaml_of_t entity.labels);
         ("shared", `Bool entity.shared);
         ("toRead", `Bool entity.to_read);
         ("isFeed", `Bool entity.is_feed);
       ]
      @ maybe_extended
      @ maybe_last_visit)

  let update updated_at names labels e =
    let names = Name_set.union e.names names in
    let labels = Label_set.union e.labels labels in
    if Time.compare updated_at e.created_at < 0 then
      {
        e with
        updated_at = List.sort Time.compare (e.created_at :: e.updated_at);
        created_at = updated_at;
        names;
        labels;
      }
    else
      { e with updated_at = List.sort Time.compare (updated_at :: e.updated_at); names; labels }

  let absorb other existing =
    if not (equal other existing) then
      update other.created_at other.names other.labels existing
    else
      existing

  let uri e = e.uri
  let created_at e = e.created_at
  let updated_at e = e.updated_at
  let names e = e.names
  let labels e = e.labels
  let extended e = e.extended
  let shared e = e.shared
  let to_read e = e.to_read
  let last_visited_at e = e.last_visited_at
  let is_feed e = e.is_feed
  let map_labels f e = { e with labels = f e.labels }
end

module Uri_hashtbl = Hashtbl.Make (struct
  type t = Uri.t

  let equal = Uri.equal

  let hash uri =
    (* force thunk *)
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
  let uri = (Dynarray.get c.nodes id).uri in
  Uri_hashtbl.add c.uris uri id;
  id

let upsert c e =
  match id c (Entity.uri e) with
  | Some id ->
      let existing = Dynarray.get c.nodes id in
      let updated = Entity.absorb e existing in
      let () =
        if not (Entity.equal updated existing) then
          Dynarray.(set c.nodes id updated)
      in
      id
  | None -> insert c e

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

let map_labels (f : Label_set.t -> Label_set.t) (c : t) : t =
  let nodes = Dynarray.map (Entity.map_labels f) c.nodes in
  { c with nodes }

let yaml_to_map (yaml : Yaml.value) : Label.t Label_map.t =
  let f acc (k, v) =
    let k = Label.of_string k in
    let v = Label.t_of_yaml v in
    Label_map.add k v acc
  in
  Yaml_ext.fold_object_exn f Label_map.empty yaml

let update_labels (yaml : Yaml.value) : t -> t =
  let mapping = yaml_to_map yaml in
  let f label = Option.value ~default:label (Label_map.find_opt label mapping) in
  map_labels (Label_set.map f)

module Template_entity = struct
  type t = {
    uri : string;
    title : string;
    createdAt : string;
    lastModified : string option;
    tags : string option;
    description : string option;
    lastVisit : string option;
    shared : bool;
    toRead : bool;
    isFeed : bool;
  }

  let of_entity (entity : Entity.t) : t =
    let uri_string = Uri.to_string (Entity.uri entity) in
    let createdAt = Time.to_string (Entity.created_at entity) in
    let title =
      match Name_set.elements (Entity.names entity) with
      | [] -> uri_string
      | names ->
          let name_strings = List.map Name.to_string names in
          List.hd (List.sort String.compare name_strings)
    in
    let lastModified =
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
    let lastVisit = Option.map Time.to_string (Entity.last_visited_at entity) in
    {
      uri = uri_string;
      title;
      createdAt;
      lastModified;
      tags;
      description;
      lastVisit;
      shared = Entity.shared entity;
      toRead = Entity.to_read entity;
      isFeed = Entity.is_feed entity;
    }

  let yaml_of_t template_entity =
    let base_fields =
      [
        ("uri", `String template_entity.uri);
        ("createdAt", `String template_entity.createdAt);
        ("shared", `Bool template_entity.shared);
        ("toRead", `Bool template_entity.toRead);
        ("isFeed", `Bool template_entity.isFeed);
        ("title", `String template_entity.title);
      ]
    in
    let optional_fields =
      List.filter_map
        Fun.id
        [
          Option.map (fun v -> ("lastModified", `String v)) template_entity.lastModified;
          Option.map (fun v -> ("tags", `String v)) template_entity.tags;
          Option.map (fun v -> ("description", `String v)) template_entity.description;
          Option.map (fun v -> ("lastVisit", `String v)) template_entity.lastVisit;
        ]
    in
    `O (base_fields @ optional_fields)
end

module Netscape = struct
  module Attrs = Prelude.Markup_ext.Attrs

  let parse_timestamp_opt (attrs : Attrs.t) (key : string) : Time.t option =
    match Attrs.get_opt key attrs with
    | Some timestamp_str when timestamp_str <> "" ->
        let timestamp = Float.of_string timestamp_str in
        Some (timestamp, Unix.gmtime timestamp)
    | _ -> None

  let parse_timestamp (attrs : Attrs.t) (key : string) : Time.t =
    match parse_timestamp_opt attrs key with
    | Some time -> time
    | None -> Time.empty

  let create_bookmark (folder_labels : string list) (attrs : Attrs.t) (description : string option)
      (extended : string option) : Entity.t =
    let href = Attrs.get "href" attrs in
    let uri = Uri.of_string href in
    let created_at = parse_timestamp attrs "add_date" in
    let last_modified = parse_timestamp_opt attrs "last_modified" in
    let last_visited_at = parse_timestamp_opt attrs "last_visit" in
    let tag_string = Attrs.get "tags" attrs in
    let tag = if tag_string = "" then [] else Str.split (Str.regexp "[,]+") tag_string in
    let filtered_tags = List.filter (fun t -> t <> "toread") tag in
    let label_strings = filtered_tags @ folder_labels in
    let labels = Label_set.of_list (List.map Label.of_string label_strings) in
    let extended = Option.map Extended.of_string extended in
    let shared =
      match Attrs.get_opt "private" attrs with
      | Some "1" -> false
      | Some "0" -> true
      | Some "" -> true
      | None -> true
      | Some _ -> true
    in
    let to_read = Attrs.get "toread" attrs = "1" in
    let is_feed =
      match Attrs.get_opt "feed" attrs with
      | Some "true" -> true
      | _ -> false
    in
    let base_entity = Entity.make uri created_at (Option.map Name.of_string description) labels in
    let updated_at = Option.to_list last_modified in
    { base_entity with updated_at; extended; shared; to_read; last_visited_at; is_feed }

  let add_pending (collection : t) (folder_stack : string list) (attrs : Attrs.t)
      (bookmark_description : string option) (extended : string option) : unit =
    let entity = create_bookmark folder_stack attrs bookmark_description extended in
    ignore (upsert collection entity)

  let element_of_string = function
    | "h3" -> `H3
    | "dt" -> `Dt
    | "a" -> `A
    | "dd" -> `Dd
    | "dl" -> `Dl
    | name -> `Other name

  let from_html content =
    let stream = Markup.string content in
    let html = Markup.parse_html stream in
    let signals = Markup.signals html in

    let collection = create () in
    let bookmark_attrs = ref None in
    let bookmark_description = ref None in
    let waiting_for = ref `Nothing in
    let element_stack = ref [] in
    let folder_stack = ref [] in
    let continue = ref true in

    while !continue do
      match Markup.next signals with
      | Some (`Start_element ((_, name), _)) when element_of_string name = `H3 ->
          element_stack := `H3 :: !element_stack;
          waiting_for := `Folder_name
      | Some (`Start_element ((_, name), _)) when element_of_string name = `Dt ->
          element_stack := `Dt :: !element_stack;
          begin
            (* If we have a previous bookmark without extended description, create it now *)
            match !bookmark_attrs with
            | Some attrs ->
                add_pending collection !folder_stack attrs !bookmark_description None;
                bookmark_attrs := None;
                bookmark_description := None
            | None -> ()
          end
      | Some (`Start_element ((_, name), attrs)) when element_of_string name = `A ->
          element_stack := `A :: !element_stack;
          bookmark_attrs := Some attrs;
          waiting_for := `Bookmark_description
      | Some (`Start_element ((_, name), _))
        when element_of_string name = `Dd && Option.is_some !bookmark_attrs ->
          (* DD elements contain extended descriptions and should only be processed
             when we have pending bookmark_attrs from a previous DT>A sequence *)
          element_stack := `Dd :: !element_stack;
          waiting_for := `Extended_description
      | Some (`Start_element ((_, name), _)) ->
          element_stack := element_of_string name :: !element_stack
      | Some (`Text xs) -> begin
          match !waiting_for with
          | `Folder_name ->
              let folder_name = String.trim (String.concat String.empty xs) in
              folder_stack := folder_name :: !folder_stack;
              waiting_for := `Nothing
          | `Bookmark_description ->
              bookmark_description := Some (String.trim (String.concat String.empty xs));
              waiting_for := `Nothing
          | `Extended_description ->
              begin
                match !bookmark_attrs with
                | Some attrs ->
                    let extended = Some (String.trim (String.concat String.empty xs)) in
                    add_pending collection !folder_stack attrs !bookmark_description extended;
                    bookmark_attrs := None;
                    bookmark_description := None
                | None -> ()
              end;
              waiting_for := `Nothing
          | `Nothing -> ()
        end
      | Some `End_element -> begin
          match !element_stack with
          | `Dl :: rest ->
              element_stack := rest;
              begin
                match !bookmark_attrs with
                | Some attrs ->
                    add_pending collection !folder_stack attrs !bookmark_description None;
                    bookmark_attrs := None;
                    bookmark_description := None
                | None -> ()
              end;
              folder_stack := List.drop 1 !folder_stack
          | _ :: rest -> element_stack := rest
          | [] -> ()
        end
      | Some _ -> () (* Skip other nodes *)
      | None ->
          begin
            (* End of parsing - create any pending bookmark *)
            match !bookmark_attrs with
            | Some attrs ->
                add_pending collection !folder_stack attrs !bookmark_description None;
                bookmark_attrs := None;
                bookmark_description := None
            | None -> ()
          end;
          (* Prepare to exit the loop *)
          continue := false
    done;

    collection

  let to_html c =
    let entities_array = entities c in
    let template_entities = Array.to_list (Array.map Template_entity.of_entity entities_array) in
    let entities_mustache = List.map Template_entity.yaml_of_t template_entities in
    let json = `O [ ("entities", `A entities_mustache) ] in
    let template = Mustache.of_string Templates.netscape_bookmarks in
    Mustache.render ~strict:false template json
end

let from_html = Netscape.from_html
let to_html = Netscape.to_html
