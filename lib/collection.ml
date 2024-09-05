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
  type t = string

  let of_string (time : string) : t = time
  let equal = String.equal
  let compare = String.compare
  let pp fmt = Format.fprintf fmt "%S"
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
    fprintf fmt "@[<hv>{";
    fprintf fmt "@;<1 2>@[uri =@ %a@];" Uri.pp self.uri;
    fprintf fmt "@;<1 2>@[created_at =@ %a@];" Time.pp self.created_at;
    let pp_sep fmt () = fprintf fmt ";@;<1 2>" in
    let pp_updated_at = pp_print_list ~pp_sep Time.pp in
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
  let names self = self.names
  let labels self = self.labels
end

module Uri_hashtbl = Hashtbl.Make (struct
  type t = Uri.t

  let equal = Uri.equal

  let hash (uri : Uri.t) =
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
