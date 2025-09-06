module Yaml_ext = Prelude.Yaml_ext

let pp_print_set pp_item fmt items =
  let open Format in
  let pp_sep fmt () = fprintf fmt ";@ " in
  fprintf fmt "@[<h>{%a}@]" (pp_print_list ~pp_sep pp_item) items

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

type t = {
  mutable uri : Uri.t;
  mutable created_at : Time.t;
  mutable updated_at : Time.t list;
  mutable names : Name_set.t;
  mutable labels : Label_set.t;
  mutable extended : Extended.t option;
  mutable shared : bool;
  mutable to_read : bool;
  mutable last_visited_at : Time.t option;
  mutable is_feed : bool;
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

let fresh () =
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

let empty = fresh ()

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
  let remaining =
    match value with
    | `O assoc -> ref assoc
    | _ -> raise (Yaml.Util.Value_error "Expected an object")
  in
  let entity = fresh () in
  while not (List.is_empty !remaining) do
    let head_opt, tail = Prelude.List_ext.uncons !remaining in
    remaining := tail;
    match head_opt with
    | Some (key, value) -> begin
        match key with
        | "uri" -> entity.uri <- Uri.of_string (Yaml.Util.to_string_exn value)
        | "createdAt" -> entity.created_at <- Time.t_of_yaml value
        | "updatedAt" -> entity.updated_at <- Yaml_ext.map_array_exn Time.t_of_yaml value
        | "names" -> entity.names <- Name_set.t_of_yaml value
        | "labels" -> entity.labels <- Label_set.t_of_yaml value
        | "extended" -> entity.extended <- Prelude.option_of_string (Extended.t_of_yaml value)
        | "shared" -> entity.shared <- Yaml.Util.to_bool_exn value
        | "toRead" -> entity.to_read <- Yaml.Util.to_bool_exn value
        | "lastVisitedAt" -> entity.last_visited_at <- Some (Time.t_of_yaml value)
        | "isFeed" -> entity.is_feed <- Yaml.Util.to_bool_exn value
        | _ -> ()
      end
    | None -> ()
  done;
  entity

let yaml_of_t entity =
  let base_fields =
    [
      ("uri", `String (Uri.to_string entity.uri));
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
    Prelude.List_ext.filter_some
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

module Html = struct
  module Attrs = Prelude.Markup_ext.Attrs

  let parse_timestamp (value : string) : Time.t =
    match Float.of_string_opt value with
    | None -> Time.empty
    | Some timestamp -> (timestamp, Unix.gmtime timestamp)

  let entity_of_attrs attributes names folder_labels extended : t =
    let entity = fresh () in
    entity.shared <- true;
    let remaining = ref attributes in
    while not (List.is_empty !remaining) do
      let head_opt, tail = Prelude.List_ext.uncons !remaining in
      remaining := tail;
      match head_opt with
      | Some ((_, key), value) -> begin
          match String.lowercase_ascii key with
          | "href" -> entity.uri <- Uri.canonicalize (Uri.of_string value)
          | "add_date" -> entity.created_at <- parse_timestamp value
          | "last_modified" when value <> "" ->
              let time = parse_timestamp value in
              entity.updated_at <- [ time ]
          | "last_visit" when value <> "" ->
              let time = parse_timestamp value in
              entity.last_visited_at <- Some time
          | "tags" when value <> "" ->
              let tag_list = Str.split (Str.regexp "[,]+") value in
              let filtered = List.filter (( <> ) "toread") tag_list in
              let labels = Label_set.of_list (List.map Label.of_string filtered) in
              let to_read = entity.to_read || List.mem "toread" tag_list in
              entity.labels <- labels;
              entity.to_read <- to_read
          | "private" -> entity.shared <- value <> "1"
          | "toread" -> entity.to_read <- value = "1"
          | "feed" -> entity.is_feed <- value = "true"
          | _ -> ()
        end
      | None -> ()
    done;
    let labels = Label_set.union entity.labels folder_labels in
    entity.labels <- labels;
    entity.names <- names;
    entity.extended <- extended;
    entity
end
