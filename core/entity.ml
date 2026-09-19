module Post = Pinboard.Post
open Prelude

let pp_print_set pp_item = Fmt.(braces (list ~sep:semi pp_item))

(* Names, labels and descriptions are nonempty by construction. An empty one
   is a value the formatters write and readers drop, so a collection carrying
   one does not round-trip, and every producer used to have to remember to
   guard: Collection.update_labels did not, and put an empty label into the
   collection whenever a mappings file mapped one to "" (#50, and
   henrytill/hbt-go#66 where it was found). [of_string] returns an option, for
   producers with untrusted text in hand; [of_string_exn] is for the
   deserialization path, where an empty value is bad input with nothing to
   recover to. *)
exception Empty of string

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

  let of_string : string -> t option = option_of_string

  let of_string_exn (s : string) : t =
    match of_string s with
    | None -> raise (Empty "name")
    | Some t -> t

  let to_string = Fun.id
  let equal = String.equal
  let compare = String.compare
  let pp = Fmt.(quote string)
  let t_of_yaml value = of_string_exn (Yaml.Util.to_string_exn value)
  let yaml_of_t = Yaml.Util.string
end

module Name_set = struct
  include Set.Make (Name)

  let pp fmt s = pp_print_set Name.pp fmt (elements s)

  (* Empty entries are dropped rather than refused: nothing this project
     writes produces one, but hand-written YAML can, and hbt-go's reader
     drops them too (henrytill/hbt-go#73). *)
  let t_of_yaml value =
    of_list
      (Yaml_ext.filter_map_array_exn (fun v -> Name.of_string (Yaml.Util.to_string_exn v)) value)

  let yaml_of_t set = Yaml.Util.list Name.yaml_of_t (to_list set)
end

module Label = struct
  type t = string

  let of_string : string -> t option = option_of_string

  let of_string_exn (s : string) : t =
    match of_string s with
    | None -> raise (Empty "label")
    | Some t -> t

  let to_string = Fun.id
  let equal = String.equal
  let compare = String.compare
  let pp = Fmt.(quote string)
  let t_of_yaml value = of_string_exn (Yaml.Util.to_string_exn value)
  let yaml_of_t = Yaml.Util.string
end

module Label_set = struct
  include Set.Make (Label)

  let pp fmt s = pp_print_set Label.pp fmt (elements s)

  (* Empty entries are dropped rather than refused: nothing this project
     writes produces one, but hand-written YAML can, and hbt-go's reader
     drops them too (henrytill/hbt-go#73). *)
  let t_of_yaml value =
    of_list
      (Yaml_ext.filter_map_array_exn (fun v -> Label.of_string (Yaml.Util.to_string_exn v)) value)

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

module Time_set = struct
  include Set.Make (Time)

  let pp fmt s = pp_print_set Time.pp fmt (elements s)
  let t_of_yaml value = of_list (Yaml_ext.map_array_exn Time.t_of_yaml value)
  let yaml_of_t set = Yaml.Util.list Time.yaml_of_t (to_list set)
end

module Extended = struct
  type t = string

  let of_string : string -> t option = option_of_string

  let of_string_exn (s : string) : t =
    match of_string s with
    | None -> raise (Empty "extended")
    | Some t -> t

  let to_string = Fun.id
  let equal = String.equal
  let compare = String.compare
  let pp = Fmt.(quote string)
  let t_of_yaml value = of_string_exn (Yaml.Util.to_string_exn value)
  let yaml_of_t = Yaml.Util.string
end

module Extended_set = struct
  include Set.Make (Extended)

  let pp fmt s = pp_print_set Extended.pp fmt (elements s)

  (* Empty entries are dropped rather than refused: nothing this project
     writes produces one, but hand-written YAML can, and hbt-go's reader
     drops them too (henrytill/hbt-go#73). *)
  let t_of_yaml value =
    of_list
      (Yaml_ext.filter_map_array_exn
         (fun v -> Extended.of_string (Yaml.Util.to_string_exn v))
         value)

  let yaml_of_t set = Yaml.Util.list Extended.yaml_of_t (to_list set)
  let of_option = Option.fold ~none:empty ~some:singleton
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
  updated_at : Time_set.t;
  names : Name_set.t;
  labels : Label_set.t;
  extended : Extended_set.t;
  shared : Shared.t;
  to_read : To_read.t;
  last_visited_at : Last_visited_at.t;
  is_feed : Is_feed.t;
}

(* Drop an update that merely repeats the creation time.

   A timestamp equal to created_at carries no information that created_at does not
   (henrytill/hbt-go#57). An update strictly *below* created_at is a different thing and is
   untouched: henrytill/hbt-data#34.

   This is the whole of the normal form (henrytill/hbt-data#38), and three places maintain it -
   the three that take a history from input. [absorb]'s merging branch ends here, so a merge that
   demotes the later creation time to an update does not then record the earlier one twice. Its
   other branch, the equality guard, returns [existing] untouched and so deliberately does not:
   see [absorb]. [t_of_yaml] ends here
   because a serialized history is input like any other. [Html.entity_of_attrs] ends here because
   HTML reads ADD_DATE and LAST_MODIFIED independently, so one anchor may state the same instant
   in both - html/bookmarks_simple.

   [empty] and [of_post] are normal for a weaker reason: they record no updates at all. [make]
   deliberately does *not* normalize, even though its ?updated_at could carry a repeat. No
   production caller passes that argument: [of_post] and the Markdown parser call [make] without
   it, and the HTML parser and the decoder fold the record directly. So nothing the program
   builds is non-normal. What it buys is that the tests, which live outside
   this module and so have no other way in, can still construct the un-normalized values the
   merge properties have to range over: associativity and the identical-entity guard are claims
   about every value of the type, not only the reachable ones. Normalizing here would leave both
   tests passing while testing nothing. *)
let normalize e = { e with updated_at = Time_set.remove e.created_at e.updated_at }

let make uri created_at ?(updated_at = Time_set.empty) ?(maybe_name = None)
    ?(labels = Label_set.empty) ?(extended = Extended_set.empty) ?(shared = Shared.empty)
    ?(to_read = To_read.empty) ?(last_visited_at = Last_visited_at.empty) ?(is_feed = Is_feed.empty)
    () =
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
    updated_at = Time_set.empty;
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
  && Time_set.equal x.updated_at y.updated_at
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
      field "updated_at" updated_at Time_set.pp;
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
  | "updatedAt" -> { e with updated_at = Time_set.t_of_yaml v }
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
  (* A serialized history is input like any other, so decoding must not reintroduce an entity
     whose updated_at holds its created_at.

     This is a real CLI path here, not an internal one: hbt-ocaml accepts [-f yaml], so
     [hbt -f yaml -t yaml] over a collection whose updatedAt repeats its createdAt now emits the
     normalized form. It is also the only implementation that accepts YAML as *input* - hbt-rs,
     hbt-go and hbt-hs all reject [-f yaml] - which is why no shared fixture can pin this half
     and a unit test does instead. *)
  normalize entity

let yaml_of_t entity =
  let base_fields =
    [
      ("uri", Uri.yaml_of_t entity.uri);
      ("createdAt", Time.yaml_of_t entity.created_at);
      ("updatedAt", Time_set.yaml_of_t entity.updated_at);
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

(* Both histories and both creation times. Putting both creation times back into the history, and
   leaving it to [normalize] to take the winner back out, is what makes merging associative:
   however a sequence of mentions is bracketed, the result is every history and every creation
   time in it minus the smallest creation time -- an update below that one stays,
   henrytill/hbt-data#34. Removing the winner only when the two differ is not associative, and
   neither is removing every update at or below created_at; henrytill/hbt-data#36 pins both, and
   henrytill/hbt-go#57 is the case where the winner was merely repeated.

   The removal is deliberately not spelled here: [absorb] is field-wise, then normalized, and two
   spellings of one rule is what a later change would have to keep in step. *)
let merged_timestamps a b =
  let winner = if Time.compare a.created_at b.created_at <= 0 then a.created_at else b.created_at in
  let updated =
    Time_set.union a.updated_at b.updated_at
    |> Time_set.add a.created_at
    |> Time_set.add b.created_at
  in
  (winner, updated)

(* Merging an entity that already equals [existing] is a no-op, and that is not redundant: the
   rule drops an update equal to created_at, so for an entity whose history repeats its own
   creation time the same mention twice would not read like it once.

   Normalizing at the parse and decode boundaries means such an entity no longer arrives from
   input - html/bookmarks_simple, which used to parse to that shape, no longer does - so what the
   guard protects is now reachable only through [make] with an ?updated_at carrying the repeat.
   That is exactly what [test_entity_absorb_identical] builds, which is why [make] does not
   normalize. hbt-hs guards the same way in its own [absorb], outside the Semigroup instance, and
   hbt-rs and hbt-go each guard in [merge]. It cannot affect associativity either way, since any
   later merge puts both creation times back regardless. *)
let absorb other existing =
  if not (equal other existing) then
    let created_at, updated_at = merged_timestamps existing other in
    normalize
      {
        existing with
        created_at;
        updated_at;
        names = Name_set.union existing.names other.names;
        labels = Label_set.union existing.labels other.labels;
        extended = Extended_set.union existing.extended other.extended;
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
  let maybe_name = Option.bind (Post.description p) Name.of_string in
  let labels = Label_set.of_list (List.filter_map Label.of_string (Post.tag p)) in
  let extended = Extended_set.of_option (Option.bind (Post.extended p) Extended.of_string) in
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
        ({ e with updated_at = Time_set.singleton time }, tag_to_read)
    | "last_visit" when v <> String.empty ->
        let time = parse_timestamp v in
        ({ e with last_visited_at = Last_visited_at.of_time time }, tag_to_read)
    | "tags" when v <> String.empty ->
        let tags = split_tags r v in
        let label_of_tag tag =
          if String.equal tag toread_tag then
            None
          else
            Label.of_string tag
        in
        let labels = Label_set.of_list (List.filter_map label_of_tag tags) in
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
    (* ADD_DATE and LAST_MODIFIED are read independently above, so an anchor stating the same
       instant in both arrives here with the repeat - the html/bookmarks_simple shape.
       Normalizing once the whole anchor is read is what drops it. *)
    normalize { entity with labels; to_read }
end
