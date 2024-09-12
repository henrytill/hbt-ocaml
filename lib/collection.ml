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

  let of_string (name : string) : t = name
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

  let of_string (label : string) : t = label
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

  let parse_date date_str =
    Scanf.sscanf date_str "%s %d, %d" (fun month day year -> (int_of_month month, day, year))

  let of_string (date_str : string) =
    let open Unix in
    let tm_mon, tm_mday, year = parse_date date_str in
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

  let to_string self = fst self |> int_of_float |> string_of_int
  let equal x y = Float.equal (fst x) (fst y)
  let compare x y = Float.compare (fst x) (fst y)
  let pp fmt self = Format.fprintf fmt "%S" (to_string self)
end

module Entity = struct
  type t = {
    uri : Uri.t;
    mutable created_at : Time.t;
    mutable updated_at : Time.t list;
    mutable names : Name_set.t;
    mutable labels : Label_set.t;
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

  let pp fmt self =
    let open Format in
    let pp_sep fmt () = fprintf fmt ";@;<1 2>" in
    let pp_updated_at = pp_print_list ~pp_sep Time.pp in
    fprintf fmt "@[<hv>{";
    fprintf fmt "@;<1 2>@[uri =@ %a@];" Uri.pp self.uri;
    fprintf fmt "@;<1 2>@[created_at =@ %a@];" Time.pp self.created_at;
    fprintf fmt "@;<1 2>@[updated_at =@ @[<hv>[@;<0 2>%a@;<0 0>]@]@];" pp_updated_at self.updated_at;
    fprintf fmt "@;<1 2>@[names =@ %a@];" Name_set.pp self.names;
    fprintf fmt "@;<1 2>@[labels =@ %a@];" Label_set.pp self.labels;
    fprintf fmt "@;<1 0>}@]"

  let update self updated_at names labels =
    if updated_at < self.created_at then (
      self.updated_at <- self.created_at :: self.updated_at;
      self.created_at <- updated_at)
    else
      self.updated_at <- updated_at :: self.updated_at;
    self.names <- Name_set.union self.names names;
    self.labels <- Label_set.union self.labels labels

  let absorb self other = update self other.created_at other.names other.labels
  let uri self = self.uri
  let created_at self = self.created_at
  let updated_at self = self.updated_at

  let last_updated_at self =
    match self.updated_at with
    | [] -> None
    | hd :: tl ->
        let max x y = if Time.compare x y < 0 then y else x in
        let ret = List.fold_left max hd tl in
        Some ret

  let names self = self.names
  let labels self = self.labels
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

let length self =
  let ret = Dynarray.length self.nodes in
  assert (ret = Dynarray.length self.edges);
  ret

let is_empty self =
  let ret = Dynarray.is_empty self.nodes in
  assert (ret = Dynarray.is_empty self.edges);
  ret

let contains self uri = Uri_hashtbl.find_opt self.uris uri |> Option.is_some
let id self uri = Uri_hashtbl.find_opt self.uris uri

let insert self entity =
  let id = Id.of_int (length self) in
  Dynarray.add_last self.nodes entity;
  Dynarray.add_last self.edges (Dynarray.create ());
  let uri = (Dynarray.get self.nodes id).uri in
  Uri_hashtbl.add self.uris uri id;
  id

let upsert self other =
  match id self (Entity.uri other) with
  | Some id ->
      let entity = Dynarray.get self.nodes id in
      Entity.absorb entity other;
      id
  | None -> insert self other

let add_edge self from target =
  let from_edges = Dynarray.get self.edges from in
  if not (Dynarray.exists (Id.equal target) from_edges) then
    Dynarray.add_last from_edges target

let add_edges self from target =
  add_edge self from target;
  add_edge self target from

let entity self id = Dynarray.get self.nodes id
let edges self id = Dynarray.get self.edges id |> Dynarray.to_array
let entities self = Dynarray.to_array self.nodes

let to_html self =
  let top =
    [
      {|<!DOCTYPE NETSCAPE-Bookmark-file-1>|};
      {|<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">|};
      {|<TITLE>Bookmarks</TITLE>|};
    ]
  in
  let open Tyxml in
  let create_dt et =
    let href = Entity.uri et |> Uri.to_string |> Html.a_href in
    let created_at = Entity.created_at et in
    let add_date = created_at |> Time.to_string |> Html.Unsafe.string_attrib "add_date" in
    let last_modified =
      Entity.last_updated_at et
      |> Option.value ~default:created_at
      |> Time.to_string
      |> Html.Unsafe.string_attrib "last_modified"
    in
    let tags =
      Entity.labels et
      |> Label_set.to_list
      |> List.map Label.to_string
      |> String.concat ","
      |> Html.Unsafe.string_attrib "tags"
    in
    let name = Entity.names et |> Name_set.to_list |> List.hd |> Name.to_string |> Html.txt in
    Html.(dt [ a ~a:[ href; add_date; last_modified; tags ] [ name ] ])
  in
  let dts = Array.fold_right (fun et acc -> create_dt et :: acc) (entities self) [] in
  Format.asprintf
    "%s\n@[<hv>%a@]\n"
    (String.concat "\n" top)
    (Html.pp_elt ~indent:true ())
    (Html.dl dts)
