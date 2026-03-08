module Yaml_ext = Prelude.Yaml_ext

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

module Uri_map = Map.Make (Entity.Uri)
module Persistent = Sek.Persistent

let next_id = Atomic.make 0

type collection = {
  id : int;
  nodes : Entity.t Persistent.t;
  edges : int list Persistent.t;
  uris : int Uri_map.t;
}

module Id = struct
  type t = {
    owner_id : int;
    index : int;
  }

  let make owner_id index = { owner_id; index }
  let equal a b = Int.equal a.owner_id b.owner_id && Int.equal a.index b.index
  let pp fmt id = Fmt.int fmt id.index
end

type t = collection

let create () =
  {
    id = Atomic.fetch_and_add next_id 1;
    nodes = Persistent.create Entity.empty;
    edges = Persistent.create [];
    uris = Uri_map.empty;
  }

let length c =
  let ret = Persistent.length c.nodes in
  assert (ret = Persistent.length c.edges);
  ret

let is_empty c = Persistent.length c.nodes = 0
let id c uri = Option.map (Id.make c.id) (Uri_map.find_opt uri c.uris)
let contains c uri = Option.is_some (id c uri)

let insert c e =
  let index = length c in
  let nodes = Persistent.push Sek.back c.nodes e in
  let edges = Persistent.push Sek.back c.edges [] in
  let uris = Uri_map.add (Entity.uri e) index c.uris in
  let c' = { c with nodes; edges; uris } in
  (c', Id.make c.id index)

let upsert c e =
  match id c (Entity.uri e) with
  | None -> insert c e
  | Some existing_id ->
      let existing = Persistent.get c.nodes existing_id.index in
      let updated = Entity.absorb e existing in
      if Entity.equal updated existing then
        (c, existing_id)
      else
        ({ c with nodes = Persistent.set c.nodes existing_id.index updated }, existing_id)

let check_id c Id.{ owner_id; _ } =
  if owner_id <> c.id then
    invalid_arg "Collection: id belongs to a different collection"

let add_edge c from target =
  check_id c from;
  check_id c target;
  let current = Persistent.get c.edges from.index in
  if List.mem target.index current then
    c
  else
    { c with edges = Persistent.set c.edges from.index (current @ [ target.index ]) }

let add_edges c from target =
  let c = add_edge c from target in
  add_edge c target from

let entity c id =
  check_id c id;
  Persistent.get c.nodes id.index

let edges c id =
  check_id c id;
  let edge_list = Persistent.get c.edges id.index in
  Array.of_list (List.map (Id.make c.id) edge_list)

let entities c =
  let n = length c in
  Array.init n (fun i -> Persistent.get c.nodes i)

let t_of_yaml value =
  let open Yaml_ext in
  begin
    let version = get_field ~key:"version" value |> Version.t_of_yaml in
    Version.check version
  end;
  let n = get_field ~key:"length" value |> int_of_float_exn in
  let nodes_arr = Array.make n Entity.empty in
  let edges_arr = Array.make n [] in
  let uris = ref Uri_map.empty in
  let process_item pairs =
    let i = get_field ~key:"id" pairs |> int_of_float_exn in
    let entity = get_field ~key:"entity" pairs |> Entity.t_of_yaml in
    let edges = get_field ~key:"edges" pairs |> map_array_exn int_of_float_exn in
    nodes_arr.(i) <- entity;
    edges_arr.(i) <- edges;
    uris := Uri_map.add (Entity.uri entity) i !uris
  in
  get_field ~key:"value" value |> iter_array_exn process_item;
  let nodes = Persistent.of_array Entity.empty nodes_arr in
  let edges = Persistent.of_array [] edges_arr in
  { id = Atomic.fetch_and_add next_id 1; nodes; edges; uris = !uris }

let yaml_of_t c =
  let n = length c in
  let items =
    List.init n (fun i ->
        let entity = Persistent.get c.nodes i in
        let edge_list = Persistent.get c.edges i in
        assert (Option.equal Id.equal (id c (Entity.uri entity)) (Some (Id.make c.id i)));
        let entity_yaml = Entity.yaml_of_t entity in
        let edges_yaml = List.map (fun e -> `Float (float_of_int e)) edge_list in
        `O [ ("id", `Float (float_of_int i)); ("entity", entity_yaml); ("edges", `A edges_yaml) ])
  in
  `O
    [
      ("version", Version.(yaml_of_t expected));
      ("length", `Float (float_of_int n));
      ("value", `A items);
    ]

let map_labels (f : Entity.Label_set.t -> Entity.Label_set.t) (c : t) : t =
  let n = length c in
  let nodes =
    List.fold_left
      (fun s i -> Persistent.set s i (Entity.map_labels f (Persistent.get s i)))
      c.nodes
      (List.init n Fun.id)
  in
  { c with nodes }

let yaml_to_map (yaml : Yaml.value) : Entity.Label.t Entity.Label_map.t =
  let f acc (k, v) =
    let k = Entity.Label.of_string k in
    let v = Entity.Label.t_of_yaml v in
    Entity.Label_map.add k v acc
  in
  Yaml_ext.fold_object_exn f Entity.Label_map.empty yaml

let update_labels (c : t) (yaml : Yaml.value) : t =
  let mapping = yaml_to_map yaml in
  let f label = Option.value ~default:label (Entity.Label_map.find_opt label mapping) in
  map_labels (Entity.Label_set.map f) c

let of_posts (ps : Pinboard.Post.t list) : t =
  let module Post = Pinboard.Post in
  let sorted = List.sort (fun a b -> String.compare (Post.time a) (Post.time b)) ps in
  List.fold_left (fun coll post -> fst (insert coll (Entity.of_post post))) (create ()) sorted
