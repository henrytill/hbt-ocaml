module Post = Pinboard.Post
module Yaml_ext = Prelude.Yaml_ext

module Version = struct
  type t = Semver.t

  exception Unsupported of string
  exception Malformed of string

  let expected : t = (0, 1, 0)
  let to_string = Semver.to_string

  let check version =
    if not (Semver.equal version expected) then
      raise (Unsupported (Semver.to_string version))

  let t_of_yaml value =
    let s = Yaml.Util.to_string_exn value in
    match Semver.of_string s with
    | Some version -> version
    | None -> raise (Malformed s)

  let yaml_of_t version = Yaml.Util.string (Semver.to_string version)
end

module Uri_hashtbl = Hashtbl.Make (Entity.Uri)

type edges = int Dynarray.t

type collection = {
  nodes : Entity.t Dynarray.t;
  edges : edges Dynarray.t;
  uris : int Uri_hashtbl.t;
}

module Id = struct
  type t = {
    owner : collection;
    index : int;
  }

  let make owner index = { owner; index }
  let equal a b = a.owner == b.owner && Int.equal a.index b.index
  let pp fmt id = Fmt.int fmt id.index
end

type t = collection

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

let id c uri = Option.map (Id.make c) (Uri_hashtbl.find_opt c.uris uri)
let contains c uri = Option.is_some (id c uri)

let insert c e =
  let index = length c in
  Dynarray.add_last c.nodes e;
  Dynarray.add_last c.edges (Dynarray.create ());
  let uri = Entity.uri (Dynarray.get c.nodes index) in
  Uri_hashtbl.add c.uris uri index;
  Id.make c index

let upsert c e =
  match id c (Entity.uri e) with
  | None -> insert c e
  | Some id ->
      let existing = Dynarray.get c.nodes id.index in
      let updated = Entity.absorb e existing in
      let () =
        if not (Entity.equal updated existing) then
          Dynarray.(set c.nodes id.index updated)
      in
      id

let check_id c Id.{ owner; _ } =
  if owner != c then
    invalid_arg "Collection: id belongs to a different collection"

let add_edge c from target =
  check_id c from;
  check_id c target;
  let from_edges = Dynarray.get c.edges from.index in
  let target_index = target.index in
  if not (Dynarray.exists (Int.equal target_index) from_edges) then
    Dynarray.add_last from_edges target_index

let add_edges c from target =
  add_edge c from target;
  add_edge c target from

let entity c id =
  check_id c id;
  Dynarray.get c.nodes id.index

let edges c id =
  check_id c id;
  Dynarray.(to_array (map (Id.make c) (get c.edges id.index)))

let entities c = Dynarray.to_array c.nodes

exception Invalid of string

let invalid fmt = Printf.ksprintf (fun msg -> raise (Invalid msg)) fmt

let t_of_yaml value =
  let open Yaml_ext in
  begin
    let version = get_field ~key:"version" value |> Version.t_of_yaml in
    Version.check version
  end;
  let length = get_field ~key:"length" value |> int_of_float_exn in
  if length < 0 then
    invalid "negative length %d" length;
  let coll = make length in
  let seen = Array.make length false in
  let count = ref 0 in
  let process_item pairs =
    let i = get_field ~key:"id" pairs |> int_of_float_exn in
    if i < 0 || i >= length then
      invalid "node id %d out of bounds for length %d" i length;
    if seen.(i) then
      invalid "duplicate node id %d" i;
    let entity =
      try get_field ~key:"entity" pairs |> Entity.t_of_yaml
      with Entity.Missing_uri -> invalid "node %d has no uri" i
    in
    let edges = get_field ~key:"edges" pairs |> map_array_exn int_of_float_exn in
    let check_edge target =
      if target < 0 || target >= length then
        invalid "node %d has an edge to %d, out of bounds for length %d" i target length
    in
    List.iter check_edge edges;
    let uri = Entity.uri entity in
    if Uri_hashtbl.mem coll.uris uri then
      invalid "duplicate uri %s at node %d" (Entity.Uri.to_string uri) i;
    Dynarray.set coll.nodes i entity;
    Dynarray.set coll.edges i (Dynarray.of_list edges);
    Uri_hashtbl.add coll.uris uri i;
    seen.(i) <- true;
    incr count
  in
  get_field ~key:"value" value |> iter_array_exn process_item;
  (* Ids are in bounds and distinct, so matching the count means every slot
     was filled - no node is left as the Entity.empty that make installed. *)
  if !count <> length then
    invalid "declared length %d but found %d nodes" length !count;
  coll

let yaml_of_t c =
  let f i entity =
    assert (Option.equal Id.equal (id c (Entity.uri entity)) (Some (Id.make c i)));
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

let iter_labels (f : Entity.Label_set.t -> Entity.Label_set.t) (c : t) : unit =
  Dynarray.iteri (fun i e -> Dynarray.set c.nodes i (Entity.map_labels f e)) c.nodes

let yaml_to_map (yaml : Yaml.value) : Entity.Label.t Entity.Label_map.t =
  let f acc (k, v) =
    let k = Entity.Label.of_string k in
    let v = Entity.Label.t_of_yaml v in
    Entity.Label_map.add k v acc
  in
  Yaml_ext.fold_object_exn f Entity.Label_map.empty yaml

let update_labels (c : t) (yaml : Yaml.value) : unit =
  let mapping = yaml_to_map yaml in
  let f label = Option.value ~default:label (Entity.Label_map.find_opt label mapping) in
  iter_labels (Entity.Label_set.map f) c

let of_posts (ps : Post.t list) : t =
  let coll = create () in
  let sorted = List.stable_sort (fun a b -> String.compare (Post.time a) (Post.time b)) ps in
  List.iter (fun post -> ignore (upsert coll (Entity.of_post post))) sorted;
  coll
