let pp_print_set pp_item fmt items =
  let open Format in
  let pp_sep fmt () = fprintf fmt ";@ " in
  fprintf fmt "@[<h>{%a}@]" (pp_print_list ~pp_sep pp_item) items

module Id = struct
  type t = int

  let of_int (i : int) : t = i
  let to_int = Fun.id
  let equal = Int.equal
  let compare = Int.compare
  let pp = Format.pp_print_int
end

module Name = struct
  type t = string

  let of_string (s : string) : t = s
  let to_string = Fun.id
  let equal = String.equal
  let compare = String.compare
  let pp fmt = Format.fprintf fmt "%S"
end

module Name_set = struct
  include Set.Make (Name)

  let pp fmt s = pp_print_set Name.pp fmt (elements s)
end

module Label = struct
  type t = string

  let of_string (s : string) : t = s
  let to_string = Fun.id
  let equal = String.equal
  let compare = String.compare
  let pp fmt = Format.fprintf fmt "%S"
end

module Label_set = struct
  include Set.Make (Label)

  let pp fmt s = pp_print_set Label.pp fmt (elements s)
end

module Time = struct
  type t = float * Unix.tm

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
    | _ -> failwith "Invalid month name"

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
end

module Entity = struct
  type t = {
    uri : Uri.t;
    created_at : Time.t;
    updated_at : Time.t list;
    names : Name_set.t;
    labels : Label_set.t;
  }

  let make uri created_at maybe_name labels =
    let uri = Uri.canonicalize uri in
    let updated_at = [] in
    let names =
      match maybe_name with
      | Some name -> Name_set.(empty |> add name)
      | None -> Name_set.empty
    in
    { uri; created_at; updated_at; names; labels }

  let equal x y =
    Uri.equal x.uri y.uri
    && Time.equal x.created_at y.created_at
    && List.equal Time.equal x.updated_at y.updated_at
    && Name_set.equal x.names y.names
    && Label_set.equal x.labels y.labels

  let pp fmt e =
    let open Format in
    let pp_sep fmt () = fprintf fmt ";@;<1 2>" in
    let pp_updated_at = pp_print_list ~pp_sep Time.pp in
    fprintf fmt "@[<hv>{";
    fprintf fmt "@;<1 2>@[uri =@ %a@];" Uri.pp e.uri;
    fprintf fmt "@;<1 2>@[created_at =@ %a@];" Time.pp e.created_at;
    fprintf fmt "@;<1 2>@[updated_at =@ @[<hv>[@;<0 2>%a@;<0 0>]@]@];" pp_updated_at e.updated_at;
    fprintf fmt "@;<1 2>@[names =@ %a@];" Name_set.pp e.names;
    fprintf fmt "@;<1 2>@[labels =@ %a@];" Label_set.pp e.labels;
    fprintf fmt "@;<1 0>}@]"

  let update updated_at names labels e =
    let names = Name_set.union e.names names in
    let labels = Label_set.union e.labels labels in
    if updated_at < e.created_at then
      { e with updated_at = e.created_at :: e.updated_at; created_at = updated_at; names; labels }
    else
      { e with updated_at = updated_at :: e.updated_at; names; labels }

  let absorb other = update other.created_at other.names other.labels
  let uri e = e.uri
  let created_at e = e.created_at
  let updated_at e = e.updated_at

  let last_updated_at e =
    match e.updated_at with
    | [] -> None
    | x :: xs -> Some (List.fold_left Time.max x xs)

  let names e = e.names
  let labels e = e.labels
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

type t = {
  nodes : Entity.t Dynarray.t;
  edges : Id.t Dynarray.t Dynarray.t;
  uris : Id.t Uri_hashtbl.t;
}

let make () =
  let nodes = Dynarray.create () in
  let edges = Dynarray.create () in
  let uris = Uri_hashtbl.create 1024 in
  { nodes; edges; uris }

let length c =
  let ret = Dynarray.length c.nodes in
  assert (ret = Dynarray.length c.edges);
  ret

let is_empty c =
  let ret = Dynarray.is_empty c.nodes in
  assert (ret = Dynarray.is_empty c.edges);
  ret

let contains c uri = Uri_hashtbl.find_opt c.uris uri |> Option.is_some
let id c uri = Uri_hashtbl.find_opt c.uris uri

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
      Dynarray.(get c.nodes id |> Entity.absorb e |> set c.nodes id);
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
let edges c id = Dynarray.get c.edges id |> Dynarray.to_array
let entities c = Dynarray.to_array c.nodes

let map_labels (f : Label_set.t -> Label_set.t) (c : t) : t =
  let nodes = Dynarray.map (Entity.map_labels f) c.nodes in
  { c with nodes }

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
  Format.asprintf
    "%s\n@[<hv>%a@]\n"
    (String.concat "\n" top)
    (Html.pp_elt ~indent:true ())
    (Html.dl dts)
