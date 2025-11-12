open Prelude

let pp_print_set pp_item = Fmt.(braces (list ~sep:semi pp_item))

module Uri = struct
  type t = Uri.t

  let empty = Uri.empty
  let of_string = Uri.of_string
  let to_string uri = Uri.to_string uri
  let canonicalize = Uri.canonicalize
  let equal = Uri.equal
  let compare x y = String.compare (to_string x) (to_string y)
  let pp = Uri.pp

  let hash uri =
    let _ = Uri.query uri in
    Hashtbl.hash uri

  let t_of_yaml value = of_string (Yaml.Util.to_string_exn value)
  let yaml_of_t uri = Yaml.Util.string (to_string uri)
end

module Name = struct
  type t = string

  let of_string (s : string) : t = s
  let to_string = Fun.id
  let equal = String.equal
  let compare = String.compare
  let pp = Fmt.(quote string)
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
  let pp = Fmt.(quote string)
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

  exception Invalid_month_name of string

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
    | month -> raise (Invalid_month_name month)

  let parse_date s =
    Scanf.sscanf s "%s %d, %d" (fun month day year -> (int_of_month month, day, year))

  let parse_iso8601 s =
    try
      let f year month day hour min sec = (year, month - 1, day, hour, min, sec) in
      Scanf.sscanf s "%d-%d-%dT%d:%d:%dZ" f
    with _ ->
      let f year month day = (year, month - 1, day, 0, 0, 0) in
      Scanf.sscanf s "%d-%d-%d" f

  let of_string (s : string) : t =
    let open Unix in
    try
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
  let pp = Fmt.(using to_string (quote string))

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
  let pp = Fmt.(quote string)
  let t_of_yaml = Yaml.Util.to_string_exn
  let yaml_of_t = Yaml.Util.string
end

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

let make uri created_at ?(updated_at = []) ?(maybe_name = None) ?(labels = Label_set.empty)
    ?(extended = None) ?(shared = false) ?(to_read = false) ?(last_visited_at = None)
    ?(is_feed = false) () =
  let uri = Uri.canonicalize uri in
  let names = Option.fold ~none:Name_set.empty ~some:Name_set.singleton maybe_name in
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
  {
    uri = Uri.empty;
    created_at = Time.empty;
    updated_at = [];
    names = Name_set.empty;
    labels = Label_set.empty;
    extended = None;
    shared = false;
    to_read = false;
    last_visited_at = None;
    is_feed = false;
  }

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

let pp =
  Fmt.(
    record
      [
        field "uri" uri Uri.pp;
        field "created_at" created_at Time.pp;
        field "updated_at" updated_at (list ~sep:semi Time.pp);
        field "names" names Name_set.pp;
        field "labels" labels Label_set.pp;
        field "extended" extended (option Extended.pp);
        field "shared" shared bool;
        field "to_read" to_read bool;
        field "last_visited_at" last_visited_at (option Time.pp);
        field "is_feed" is_feed bool;
      ])

let build entity (key, value) =
  match key with
  | "uri" -> { entity with uri = Uri.t_of_yaml value }
  | "createdAt" -> { entity with created_at = Time.t_of_yaml value }
  | "updatedAt" -> { entity with updated_at = Yaml_ext.map_array_exn Time.t_of_yaml value }
  | "names" -> { entity with names = Name_set.t_of_yaml value }
  | "labels" -> { entity with labels = Label_set.t_of_yaml value }
  | "extended" -> { entity with extended = option_of_string (Extended.t_of_yaml value) }
  | "shared" -> { entity with shared = Yaml.Util.to_bool_exn value }
  | "toRead" -> { entity with to_read = Yaml.Util.to_bool_exn value }
  | "lastVisitedAt" -> { entity with last_visited_at = Some (Time.t_of_yaml value) }
  | "isFeed" -> { entity with is_feed = Yaml.Util.to_bool_exn value }
  | _ -> entity

let t_of_yaml value =
  let assoc =
    match value with
    | `O assoc -> assoc
    | _ -> raise (Yaml.Util.Value_error "Expected an object")
  in
  List.fold_left build empty assoc

let yaml_of_t entity =
  let base_fields =
    [
      ("uri", Uri.yaml_of_t entity.uri);
      ("createdAt", Time.yaml_of_t entity.created_at);
      ("updatedAt", `A (List.map Time.yaml_of_t entity.updated_at));
      ("names", Name_set.yaml_of_t entity.names);
      ("labels", Label_set.yaml_of_t entity.labels);
      ("shared", `Bool entity.shared);
      ("toRead", `Bool entity.to_read);
      ("isFeed", `Bool entity.is_feed);
    ]
  in
  let optional_fields =
    List_ext.filter_some
      [
        Option.map (fun e -> ("extended", Extended.yaml_of_t e)) entity.extended;
        Option.map (fun t -> ("lastVisitedAt", Time.yaml_of_t t)) entity.last_visited_at;
      ]
  in
  `O (base_fields @ optional_fields)

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

let map_labels f e = { e with labels = f e.labels }

let of_pinboard (p : Pinboard.t) : t =
  let uri = Uri.of_string (Pinboard.href p) in
  let created_at = Time.of_string (Pinboard.time p) in
  let maybe_name = Option.map Name.of_string (Pinboard.description p) in
  let labels = Label_set.of_list (List.map Label.of_string (Pinboard.tag p)) in
  let extended = Option.map Extended.of_string (Pinboard.extended p) in
  let shared = Pinboard.shared p in
  let to_read = Pinboard.toread p in
  make uri created_at ~maybe_name ~labels ~extended ~shared ~to_read ()

module Html = struct
  module Attrs = Prelude.Markup_ext.Attrs

  let parse_timestamp (value : string) : Time.t =
    match Float.of_string_opt value with
    | None -> Time.empty
    | Some timestamp -> (timestamp, Unix.gmtime timestamp)

  let build entity ((_, key), value) =
    match String.lowercase_ascii key with
    | "href" -> { entity with uri = Uri.canonicalize (Uri.of_string value) }
    | "add_date" -> { entity with created_at = parse_timestamp value }
    | "last_modified" when value <> String.empty ->
        let time = parse_timestamp value in
        { entity with updated_at = [ time ] }
    | "last_visit" when value <> String.empty ->
        let time = parse_timestamp value in
        { entity with last_visited_at = Some time }
    | "tags" when value <> String.empty ->
        let tag_list = Str.split (Str.regexp "[,]+") value in
        let labels =
          Label_set.of_list
            (List.filter_map
               (fun tag -> if tag <> "toread" then Some (Label.of_string tag) else None)
               tag_list)
        in
        let to_read = entity.to_read || List.mem "toread" tag_list in
        { entity with labels; to_read }
    | "private" -> { entity with shared = value <> "1" }
    | "toread" -> { entity with to_read = value = "1" }
    | "feed" -> { entity with is_feed = value = "true" }
    | _ -> entity

  let entity_of_attrs attributes names folder_labels extended : t =
    let entity = List.fold_left build { empty with shared = true; names; extended } attributes in
    let labels = Label_set.union entity.labels folder_labels in
    { entity with labels }
end
