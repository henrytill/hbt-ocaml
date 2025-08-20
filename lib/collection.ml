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

  let t_of_yojson json = Semver.of_string (Yojson.Safe.Util.to_string json)
  let yojson_of_t version = `String (Semver.to_string version)
end

module Id = struct
  type t = int

  let of_int (i : int) : t = i
  let to_int = Fun.id
  let equal = Int.equal
  let compare = Int.compare
  let pp = Format.pp_print_int
  let t_of_yojson json = Yojson.Safe.Util.to_int json
  let yojson_of_t id = `Int (to_int id)
end

module Name = struct
  type t = string

  let of_string (s : string) : t = s
  let to_string = Fun.id
  let equal = String.equal
  let compare = String.compare
  let pp fmt = Format.fprintf fmt "%S"
  let t_of_yojson json = Yojson.Safe.Util.to_string json
  let yojson_of_t name = `String (to_string name)
end

module Name_set = struct
  include Set.Make (Name)

  let pp fmt s = pp_print_set Name.pp fmt (elements s)

  let t_of_yojson json =
    List.fold_left
      (fun acc name -> add (Name.t_of_yojson name) acc)
      empty
      (Yojson.Safe.Util.to_list json)

  let yojson_of_t set = `List (List.map Name.yojson_of_t (elements set))
end

module Label = struct
  type t = string

  let of_string (s : string) : t = s
  let to_string = Fun.id
  let equal = String.equal
  let compare = String.compare
  let pp fmt = Format.fprintf fmt "%S"
  let t_of_yojson json = Yojson.Safe.Util.to_string json
  let yojson_of_t label = `String (to_string label)
end

module Label_set = struct
  include Set.Make (Label)

  let pp fmt s = pp_print_set Label.pp fmt (elements s)

  let t_of_yojson json =
    List.fold_left
      (fun acc label -> add (Label.t_of_yojson label) acc)
      empty
      (Yojson.Safe.Util.to_list json)

  let yojson_of_t set = `List (List.map Label.yojson_of_t (elements set))
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

  let of_string (s : string) : t =
    let open Unix in
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
  let max x y = if compare x y < 0 then y else x
  let pp fmt t = Format.fprintf fmt "%S" (to_string t)

  let t_of_yojson json =
    let t = Yojson.Safe.Util.to_float json in
    (t, Unix.gmtime t)

  let yojson_of_t time = `Float (fst time)
end

module Extended = struct
  type t = string

  let of_string (s : string) : t = s
  let to_string = Fun.id
  let equal = String.equal
  let compare = String.compare
  let pp fmt = Format.fprintf fmt "%S"
  let t_of_yojson json = Yojson.Safe.Util.to_string json
  let yojson_of_t extended = `String (to_string extended)
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
    toread : bool;
    last_visited_at : Time.t option;
    is_feed : bool;
  }

  let make uri created_at maybe_name labels =
    let uri = Uri.canonicalize uri in
    let updated_at = [] in
    let names = Option.fold ~none:Name_set.empty ~some:Name_set.singleton maybe_name in
    let extended = None in
    let shared = false in
    let toread = false in
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
      toread;
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
    let toread = false in
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
      toread;
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
    let toread = toread p in
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
      toread;
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
    && Bool.equal x.toread y.toread
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
    fprintf fmt "@;<1 2>@[toread =@ %a@];" pp_print_bool e.toread;
    fprintf fmt "@;<1 2>@[last_visited_at =@ %a@];" pp_last_visit_opt e.last_visited_at;
    fprintf fmt "@;<1 2>@[is_feed =@ %a@];" pp_print_bool e.is_feed;
    fprintf fmt "@;<1 0>}@]"

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    {
      uri = json |> member "uri" |> to_string |> Uri.of_string;
      created_at = json |> member "createdAt" |> Time.t_of_yojson;
      updated_at = json |> member "updatedAt" |> to_list |> List.map Time.t_of_yojson;
      names = json |> member "names" |> Name_set.t_of_yojson;
      labels = json |> member "labels" |> Label_set.t_of_yojson;
      extended = json |> member "extended" |> to_option Extended.t_of_yojson;
      shared = json |> member "shared" |> to_bool;
      toread = json |> member "toread" |> to_bool;
      last_visited_at = json |> member "lastVisitedAt" |> to_option Time.t_of_yojson;
      is_feed = json |> member "isFeed" |> to_bool;
    }

  let yojson_of_t entity =
    let maybe_extended =
      match entity.extended with
      | Some extended -> [ ("extended", Extended.yojson_of_t extended) ]
      | None -> []
    in
    let maybe_last_visit =
      match entity.last_visited_at with
      | Some last_visit -> [ ("lastVisitedAt", Time.yojson_of_t last_visit) ]
      | None -> []
    in
    `Assoc
      ([
         ("uri", `String (Uri.to_string entity.uri));
         ("createdAt", Time.yojson_of_t entity.created_at);
         ("updatedAt", `List (List.map Time.yojson_of_t entity.updated_at));
         ("names", Name_set.yojson_of_t entity.names);
         ("labels", Label_set.yojson_of_t entity.labels);
         ("shared", `Bool entity.shared);
         ("toread", `Bool entity.toread);
         ("isFeed", `Bool entity.is_feed);
       ]
      @ maybe_extended
      @ maybe_last_visit)

  let update updated_at names labels e =
    let names = Name_set.union e.names names in
    let labels = Label_set.union e.labels labels in
    if Time.compare updated_at e.created_at < 0 then
      { e with updated_at = e.created_at :: e.updated_at; created_at = updated_at; names; labels }
    else
      { e with updated_at = updated_at :: e.updated_at; names; labels }

  let absorb other existing =
    if not (equal other existing) then
      update other.created_at other.names other.labels existing
    else
      existing

  let uri e = e.uri
  let created_at e = e.created_at
  let updated_at e = e.updated_at

  let last_updated_at e =
    match e.updated_at with
    | [] -> None
    | x :: xs -> Some (List.fold_left Time.max x xs)

  let names e = e.names
  let labels e = e.labels
  let extended e = e.extended
  let shared e = e.shared
  let toread e = e.toread
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

let t_of_yojson json =
  let open Yojson.Safe.Util in
  begin
    let maybe_version = json |> member "version" |> Version.t_of_yojson in
    match maybe_version with
    | Some version -> Version.check version
    | None -> invalid_arg "Collection.t_of_yojson: unable to parse version"
  end;
  let length = json |> member "length" |> to_int in
  let ret = make length in
  let f item =
    let i = item |> member "id" |> to_int in
    let entity = item |> member "entity" |> Entity.t_of_yojson in
    let edges = item |> member "edges" |> to_list |> List.map to_int |> Dynarray.of_list in
    let uri = Entity.uri entity in
    Dynarray.set ret.nodes i entity;
    Dynarray.set ret.edges i edges;
    Uri_hashtbl.add ret.uris uri i
  in
  json |> member "value" |> to_list |> List.iter f;
  ret

let yojson_of_t c =
  let items = ref [] in
  let f i entity =
    assert (Option.equal Id.equal (id c (Entity.uri entity)) (Some (Id.of_int i)));
    let entity_json = Entity.yojson_of_t entity in
    let edges_json = `List Dynarray.(fold_right (fun e acc -> `Int e :: acc) (get c.edges i) []) in
    let item = `Assoc [ ("id", `Int i); ("entity", entity_json); ("edges", edges_json) ] in
    items := item :: !items
  in
  Dynarray.iteri f c.nodes;
  `Assoc
    [
      ("version", Version.(yojson_of_t expected));
      ("length", `Int (length c));
      ("value", `List !items);
    ]

let map_labels (f : Label_set.t -> Label_set.t) (c : t) : t =
  let nodes = Dynarray.map (Entity.map_labels f) c.nodes in
  { c with nodes }

let json_to_map (json : Yojson.Basic.t) : Label.t Label_map.t =
  let pairs =
    match json with
    | `Assoc pairs -> pairs
    | _ -> invalid_arg "Collection.json_to_map: expected a JSON object"
  in
  let f acc (k, v) =
    match v with
    | `String s ->
        let k = Label.of_string k in
        let v = Label.of_string s in
        Label_map.add k v acc
    | _ -> invalid_arg "Collection.json_to_map: all values must be strings"
  in
  List.fold_left f Label_map.empty pairs

let update_labels (json : Yojson.Basic.t) : t -> t =
  let mapping = json_to_map json in
  let f label = Option.value ~default:label (Label_map.find_opt label mapping) in
  map_labels (Label_set.map f)

let make_dt e =
  let open Tyxml in
  let href = Entity.uri e |> Uri.to_string |> Html.a_href in
  let created_at = Entity.created_at e in
  let add_date = created_at |> Time.to_string |> Html.Unsafe.string_attrib "add_date" in
  let last_modified =
    Entity.last_updated_at e
    |> Option.value ~default:created_at
    |> Time.to_string
    |> Html.Unsafe.string_attrib "last_modified"
  in
  let tags =
    Entity.labels e
    |> Label_set.to_list
    |> List.map Label.to_string
    |> String.concat ","
    |> Html.Unsafe.string_attrib "tags"
  in
  let name = Entity.names e |> Name_set.to_list |> List.hd |> Name.to_string |> Html.txt in
  Html.(dt [ a ~a:[ href; add_date; last_modified; tags ] [ name ] ])

let to_html c =
  let top =
    [
      {|<!DOCTYPE NETSCAPE-Bookmark-file-1>|};
      {|<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">|};
      {|<TITLE>Bookmarks</TITLE>|};
    ]
  in
  let dts = Array.fold_right (fun e acc -> make_dt e :: acc) (entities c) [] in
  let open Tyxml in
  Format.pp_set_margin Format.str_formatter 200;
  Format.fprintf
    Format.str_formatter
    "%s@[<h>%a@]\n"
    (String.concat "\n" top)
    (Html.pp_elt ~indent:true ())
    (Html.dl dts);
  Format.flush_str_formatter ()

module Netscape = struct
  module Attrs = struct
    type t = ((string * string) * string) list

    let get_opt (k : string) (attrs : t) : string option = List.assoc_opt (String.empty, k) attrs
    let get (k : string) (attrs : t) : string = Option.value ~default:String.empty (get_opt k attrs)
  end

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
    (* Add folder path as labels *)
    let label_strings = tag @ folder_labels in
    let labels = Label_set.of_list (List.map Label.of_string label_strings) in
    let extended_field = Option.map Extended.of_string extended in
    let shared =
      match Attrs.get_opt "private" attrs with
      | Some "1" -> false
      | Some "0" -> true
      | Some "" -> true
      | None -> true
      | Some _ -> true
    in
    let toread = Attrs.get "toread" attrs = "1" in
    let is_feed =
      match Attrs.get_opt "feed" attrs with
      | Some "true" -> true
      | _ -> false
    in
    let base_entity = Entity.make uri created_at (Option.map Name.of_string description) labels in
    (* Update with additional fields that Entity.make doesn't set *)
    {
      base_entity with
      updated_at =
        (match last_modified with
        | Some t -> [ t ]
        | None -> []);
      extended = extended_field;
      shared;
      toread;
      last_visited_at;
      is_feed;
    }

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

  let from_html file =
    let ic = open_in file in
    let channel = Markup.channel ic in
    let html = Markup.parse_html channel in
    let signals = Markup.signals html in
    let collection = create () in
    let folder_stack = ref [] in
    let element_stack = ref [] in
    let continue = ref true in

    let bookmark_attrs = ref None in
    let bookmark_description = ref None in
    let waiting_for = ref `Nothing in

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
          begin
            match !element_stack with
            | `A :: `Dt :: _ ->
                bookmark_attrs := Some attrs;
                waiting_for := `Bookmark_description
            | _ -> ()
          end
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
                (* End of folder - create any pending bookmark *)
                match !bookmark_attrs with
                | Some attrs ->
                    add_pending collection !folder_stack attrs !bookmark_description None;
                    bookmark_attrs := None;
                    bookmark_description := None
                | None -> ()
              end;
              begin
                (* Pop folder from stack *)
                match !folder_stack with
                | _ :: rest_folders -> folder_stack := rest_folders
                | [] -> ()
              end
          | `Dt :: rest ->
              element_stack := rest;
              (* Don't create bookmark yet - wait to see if there's a dd element *)
              ()
          | _ :: rest ->
              element_stack := rest;
              ()
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

    close_in ic;
    collection
end

let from_html = Netscape.from_html
