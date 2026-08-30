module Post = Pinboard.Post
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

  (* Uri.t memoizes its query in a lazy field, and Hashtbl.hash traverses it,
     so an unforced and a forced value of the same URI hash differently.
     Forcing it first makes the hash stable for the value's whole lifetime -
     which Collection's uri index depends on, since a hash that drifted after
     insertion would silently stop finding the entry. Do not remove this as
     dead code: it is called for its effect on uri, not for its result. *)
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

  (* Scanf signals a failed parse with any of these. *)
  let is_scan_failure = function
    | Scanf.Scan_failure _ | Failure _ | End_of_file -> true
    | _ -> false

  let parse_date s =
    Scanf.sscanf s "%s %d, %d" (fun month day year -> (int_of_month month, day, year))

  let parse_iso8601 s =
    try
      let f year month day hour min sec = (year, month - 1, day, hour, min, sec) in
      Scanf.sscanf s "%d-%d-%dT%d:%d:%dZ" f
    with e when is_scan_failure e ->
      let f year month day = (year, month - 1, day, 0, 0, 0) in
      Scanf.sscanf s "%d-%d-%d" f

  (* Days from the Unix epoch to a proleptic Gregorian date, after Howard
     Hinnant's days_from_civil. [month] is 0-based, as in Unix.tm.tm_mon. *)
  let days_from_civil year month day =
    let m = month + 1 in
    let y =
      if m <= 2 then
        year - 1
      else
        year
    in
    let era =
      (if y >= 0 then
         y
       else
         y - 399)
      / 400
    in
    let yoe = y - (era * 400) in
    let mp = (m + 9) mod 12 in
    let doy = (((153 * mp) + 2) / 5) + day - 1 in
    let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
    (era * 146097) + doe - 719468

  (* The UTC counterpart of Unix.mktime. Unix offers no timegm, and mktime
     interprets its argument as local time, which made parsed timestamps -
     and therefore all output - depend on the caller's TZ. *)
  let timegm ~year ~month ~day ~hour ~min ~sec =
    let days = days_from_civil year month day in
    float_of_int ((days * 86400) + (hour * 3600) + (min * 60) + sec)

  let of_string (s : string) : t =
    let year, month, day, hour, min, sec =
      try parse_iso8601 s
      with e when is_scan_failure e ->
        let month, day, year = parse_date s in
        (year, month, day, 0, 0, 0)
    in
    let t = timegm ~year ~month ~day ~hour ~min ~sec in
    (t, Unix.gmtime t)

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

module Extended_set = struct
  include Set.Make (Extended)

  let pp fmt s = pp_print_set Extended.pp fmt (elements s)
  let t_of_yaml value = of_list (Yaml_ext.map_array_exn Extended.t_of_yaml value)
  let yaml_of_t set = Yaml.Util.list Extended.yaml_of_t (to_list set)
end

module Flag = struct
  type t = bool option

  let of_bool (b : bool) = Some b
  let empty = None
  let get = Fun.id
  let equal = Option.equal Bool.equal
  let pp = Fmt.(option bool)

  let concat a b =
    match (a, b) with
    | None, None -> None
    | Some x, None | None, Some x -> Some x
    | Some x, Some y -> Some (x || y)
end

module Shared = Flag
module To_read = Flag
module Is_feed = Flag

module Last_visited_at = struct
  type t = Time.t option

  let of_time (t : Time.t) = Some t
  let empty = None
  let get = Fun.id
  let equal = Option.equal Time.equal
  let pp = Fmt.(option Time.pp)

  let concat a b =
    match (a, b) with
    | None, None -> None
    | Some t, None | None, Some t -> Some t
    | Some t1, Some t2 -> Some (if Time.compare t1 t2 < 0 then t2 else t1)
end

type t = {
  uri : Uri.t;
  created_at : Time.t;
  updated_at : Time.t list;
  names : Name_set.t;
  labels : Label_set.t;
  extended : Extended_set.t;
  shared : Shared.t;
  to_read : To_read.t;
  last_visited_at : Last_visited_at.t;
  is_feed : Is_feed.t;
}

let make uri created_at ?(updated_at = []) ?(maybe_name = None) ?(labels = Label_set.empty)
    ?(extended = Extended_set.empty) ?(shared = Shared.empty) ?(to_read = To_read.empty)
    ?(last_visited_at = Last_visited_at.empty) ?(is_feed = Is_feed.empty) () =
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
    extended = Extended_set.empty;
    shared = Shared.empty;
    to_read = To_read.empty;
    last_visited_at = Last_visited_at.empty;
    is_feed = Is_feed.empty;
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
  && Extended_set.equal x.extended y.extended
  && Shared.equal x.shared y.shared
  && To_read.equal x.to_read y.to_read
  && Last_visited_at.equal x.last_visited_at y.last_visited_at
  && Is_feed.equal x.is_feed y.is_feed

let pp =
  let open Fmt in
  record
    [
      field "uri" uri Uri.pp;
      field "created_at" created_at Time.pp;
      field "updated_at" updated_at (list ~sep:semi Time.pp);
      field "names" names Name_set.pp;
      field "labels" labels Label_set.pp;
      field "extended" extended Extended_set.pp;
      field "shared" shared Shared.pp;
      field "to_read" to_read To_read.pp;
      field "last_visited_at" last_visited_at Last_visited_at.pp;
      field "is_feed" is_feed Is_feed.pp;
    ]

let build e (k, v) =
  match k with
  | "uri" -> { e with uri = Uri.t_of_yaml v }
  | "createdAt" -> { e with created_at = Time.t_of_yaml v }
  | "updatedAt" -> { e with updated_at = Yaml_ext.map_array_exn Time.t_of_yaml v }
  | "names" -> { e with names = Name_set.t_of_yaml v }
  | "labels" -> { e with labels = Label_set.t_of_yaml v }
  | "extended" -> { e with extended = Extended_set.t_of_yaml v }
  | "shared" -> { e with shared = Shared.of_bool (Yaml.Util.to_bool_exn v) }
  | "toRead" -> { e with to_read = To_read.of_bool (Yaml.Util.to_bool_exn v) }
  | "lastVisitedAt" -> { e with last_visited_at = Last_visited_at.of_time (Time.t_of_yaml v) }
  | "isFeed" -> { e with is_feed = Is_feed.of_bool (Yaml.Util.to_bool_exn v) }
  | _ -> e

exception Missing_uri

let t_of_yaml value =
  let assoc =
    match value with
    | `O assoc -> assoc
    | _ -> raise (Yaml.Util.Value_error "Expected an object")
  in
  let entity = List.fold_left build empty assoc in
  (* A URI is intrinsic to an entity - it is the identity every producer
     keys on - so enforce it here rather than in any one caller. *)
  if Uri.equal entity.uri Uri.empty then
    raise Missing_uri;
  entity

let yaml_of_t entity =
  let base_fields =
    [
      ("uri", Uri.yaml_of_t entity.uri);
      ("createdAt", Time.yaml_of_t entity.created_at);
      ("updatedAt", `A (List.map Time.yaml_of_t entity.updated_at));
      ("names", Name_set.yaml_of_t entity.names);
      ("labels", Label_set.yaml_of_t entity.labels);
    ]
  in
  let shared =
    match Shared.get entity.shared with
    | None -> []
    | Some b -> [ ("shared", `Bool b) ]
  in
  let to_read =
    match To_read.get entity.to_read with
    | None -> []
    | Some b -> [ ("toRead", `Bool b) ]
  in
  let is_feed =
    match Is_feed.get entity.is_feed with
    | None -> []
    | Some b -> [ ("isFeed", `Bool b) ]
  in
  let extended =
    if Extended_set.is_empty entity.extended then
      []
    else
      [ ("extended", Extended_set.yaml_of_t entity.extended) ]
  in
  let last_visited =
    match Last_visited_at.get entity.last_visited_at with
    | None -> []
    | Some t -> [ ("lastVisitedAt", Time.yaml_of_t t) ]
  in
  `O (base_fields @ shared @ to_read @ is_feed @ extended @ last_visited)

let update updated_at names labels extended e =
  let names = Name_set.union e.names names in
  let labels = Label_set.union e.labels labels in
  let extended = Extended_set.union e.extended extended in
  let base = { e with names; labels; extended } in
  let c = Time.compare updated_at base.created_at in
  if c < 0 then
    (* An earlier timestamp becomes created_at, and the one it displaces becomes an update. *)
    {
      base with
      updated_at = List.sort Time.compare (base.created_at :: base.updated_at);
      created_at = updated_at;
    }
  else if c > 0 then
    { base with updated_at = List.sort Time.compare (updated_at :: base.updated_at) }
  else
    (* A timestamp equal to created_at is deliberately not recorded: an "update" whose timestamp
       merely repeats created_at carries no information. Settled as henrytill/hbt-go#57. *)
    base

let absorb other existing =
  if not (equal other existing) then
    let base = update other.created_at other.names other.labels other.extended existing in
    {
      base with
      shared = Shared.concat existing.shared other.shared;
      to_read = To_read.concat existing.to_read other.to_read;
      is_feed = Is_feed.concat existing.is_feed other.is_feed;
      last_visited_at = Last_visited_at.concat existing.last_visited_at other.last_visited_at;
    }
  else
    existing

let map_labels f e = { e with labels = f e.labels }

let of_post (p : Pinboard.Post.t) : t =
  let uri = Uri.of_string (Post.href p) in
  let created_at = Time.of_string (Post.time p) in
  let maybe_name = Option.map Name.of_string (Post.description p) in
  let labels = Label_set.of_list (List.map Label.of_string (Post.tag p)) in
  let extended =
    Option.fold
      ~none:Extended_set.empty
      ~some:(fun s -> Extended_set.singleton (Extended.of_string s))
      (Post.extended p)
  in
  let shared = Shared.of_bool (Post.shared p) in
  let to_read = To_read.of_bool (Post.toread p) in
  let is_feed = Is_feed.of_bool false in
  make uri created_at ~maybe_name ~labels ~extended ~shared ~to_read ~is_feed ()

module Html = struct
  module Attrs = Prelude.Markup_ext.Attrs

  (* Deliberately lenient, unlike Time.of_string: exported bookmark files in
     the wild carry missing or malformed ADD_DATE values, and falling back to
     the epoch imports the bookmark rather than rejecting the whole file. *)
  let parse_timestamp (value : string) : Time.t =
    match Float.of_string_opt value with
    | None -> Time.empty
    | Some timestamp -> (timestamp, Unix.gmtime timestamp)

  (* Split a TAGS attribute, trimming each tag and dropping empty ones. A
     value like "x, toread" is one tag "x" and the toread marker, not a tag
     named " toread". *)
  let split_tags r v =
    let f tag =
      match String.trim tag with
      | "" -> None
      | tag -> Some tag
    in
    List.filter_map f (Str.split (Lazy.force r) v)

  let toread_tag = "toread"

  (* The accumulator carries whether a toread tag was seen alongside the
     entity, so the decision does not depend on whether TAGS or TOREAD came
     first in the attribute list. *)
  let build r (e, tag_to_read) ((_, k), v) =
    match String.lowercase_ascii k with
    | "href" -> ({ e with uri = Uri.canonicalize (Uri.of_string v) }, tag_to_read)
    | "add_date" -> ({ e with created_at = parse_timestamp v }, tag_to_read)
    | "last_modified" when v <> String.empty ->
        let time = parse_timestamp v in
        ({ e with updated_at = [ time ] }, tag_to_read)
    | "last_visit" when v <> String.empty ->
        let time = parse_timestamp v in
        ({ e with last_visited_at = Last_visited_at.of_time time }, tag_to_read)
    | "tags" when v <> String.empty ->
        let tags = split_tags r v in
        let labels =
          Label_set.of_list
            (List.filter_map
               (fun tag ->
                 if String.equal tag toread_tag then
                   None
                 else
                   Some (Label.of_string tag))
               tags)
        in
        (* Both decisions come from the same exact per-tag comparison, so a
           tag like "toreading" is a label and not the toread marker. *)
        ({ e with labels }, tag_to_read || List.exists (String.equal toread_tag) tags)
    | "private" -> ({ e with shared = Shared.of_bool (v <> "1") }, tag_to_read)
    | "toread" -> ({ e with to_read = To_read.of_bool (v = "1") }, tag_to_read)
    | "feed" -> ({ e with is_feed = Is_feed.of_bool (v = "true") }, tag_to_read)
    | _ -> (e, tag_to_read)

  let tag_splitter = lazy (Str.regexp "[,]+")

  let entity_of_attrs attributes names folder_labels extended : t =
    let f = build tag_splitter in
    let entity, tag_to_read = List.fold_left f ({ empty with names; extended }, false) attributes in
    (* An explicit TOREAD attribute is authoritative; the tag only decides
       when the attribute is absent. *)
    let to_read =
      match To_read.get entity.to_read with
      | Some _ -> entity.to_read
      | None when tag_to_read -> To_read.of_bool true
      | None -> entity.to_read
    in
    let labels = Label_set.union entity.labels folder_labels in
    { entity with labels; to_read }
end
