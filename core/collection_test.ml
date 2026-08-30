open Hbt

let same_uri = "same uri"
let same_created_at = "same created_at"
let same_updated_at = "same updated_at"
let same_names = "same names"
let same_labels = "same labels"
let same_length = "same length"
let same_entity = "same entity"
let same_edges = "same edges"

let test_entity_equal () =
  let open Entity in
  let uri = Uri.of_string "https://foo.org" in
  let maybe_name = Some (Name.of_string "foo") in
  let created = Time.of_string "September 2, 2024" in
  let labels = Label_set.singleton (Label.of_string "foo") in
  let a = Entity.make uri created ~maybe_name ~labels () in
  let b = Entity.make uri created ~maybe_name ~labels () in
  Alcotest.(check (module Entity)) same_entity a b

let test_entity_update () =
  let open Entity in
  let uri = Uri.of_string "https://foo.org" in
  let maybe_name = Some (Name.of_string "foo") in
  let maybe_names = Option.fold ~none:Name_set.empty ~some:Name_set.singleton maybe_name in
  let created = Time.of_string "September 2, 2024" in
  let labels = Label_set.singleton (Label.of_string "foo") in
  let a = Entity.make uri created ~maybe_name ~labels () in
  let updated = Time.of_string "September 4, 2024" in
  let names_update = Name_set.of_list [ Name.of_string "Foo.org"; Name.of_string "F00" ] in
  let labels_update = Label_set.of_list [ Label.of_string "foozer"; Label.of_string "bar" ] in
  let extended_update = Extended_set.empty in
  let a = Entity.update updated names_update labels_update extended_update a in
  Alcotest.(check (module Uri)) same_uri (Uri.canonicalize uri) (Entity.uri a);
  Alcotest.(check (module Time)) same_created_at created (Entity.created_at a);
  Alcotest.(check (list (module Time))) same_updated_at [ updated ] (Entity.updated_at a);
  Alcotest.(check (module Name_set))
    same_names
    (Name_set.union maybe_names names_update)
    (Entity.names a);
  Alcotest.(check (module Label_set))
    same_labels
    (Label_set.union labels labels_update)
    (Entity.labels a)

(* An incoming timestamp equal to created_at records no update: such an entry would only repeat
   created_at. Settled as henrytill/hbt-go#57, where this implementation was one of the two that
   appended. The absorb guard does not cover this, since the entities differ. *)
let test_entity_update_equal_timestamp () =
  let open Entity in
  let uri = Uri.of_string "https://foo.org" in
  let created = Time.of_string "September 2, 2024" in
  let a = Entity.make uri created ~maybe_name:(Some (Name.of_string "foo")) () in
  let names_update = Name_set.singleton (Name.of_string "bar") in
  let a = Entity.update created names_update Label_set.empty Extended_set.empty a in
  Alcotest.(check (module Time)) same_created_at created (Entity.created_at a);
  Alcotest.(check (list (module Time))) same_updated_at [] (Entity.updated_at a);
  Alcotest.(check (module Name_set))
    same_names
    (Name_set.of_list [ Name.of_string "foo"; Name.of_string "bar" ])
    (Entity.names a)

let test_entity_absorb () =
  let open Entity in
  let uri = Uri.of_string "https://foo.org" in
  let name = Name.of_string "foo" in
  let names = Name_set.singleton name in
  let created_a = Time.of_string "September 4, 2024" in
  let created_b = Time.of_string "September 2, 2024" in
  let labels_foo = Label_set.singleton (Label.of_string "foo") in
  let labels_bar = Label_set.singleton (Label.of_string "bar") in
  let a = Entity.make uri created_a ~labels:labels_foo () in
  let b = Entity.make uri created_b ~maybe_name:(Some name) ~labels:labels_bar () in
  let a = Entity.absorb b a in
  Alcotest.(check (module Uri)) same_uri (Uri.canonicalize uri) (Entity.uri a);
  Alcotest.(check (module Time)) same_created_at created_b (Entity.created_at a);
  Alcotest.(check (list (module Time))) same_updated_at [ created_a ] (Entity.updated_at a);
  Alcotest.(check (module Name_set)) same_names names (Entity.names a);
  Alcotest.(check (module Label_set))
    same_labels
    (Label_set.union labels_foo labels_bar)
    (Entity.labels a)

let test_entity_absorb_extended () =
  let open Entity in
  let uri = Uri.of_string "https://foo.org" in
  let created_a = Time.of_string "September 4, 2024" in
  let created_b = Time.of_string "September 2, 2024" in
  let extended_a = Extended_set.singleton (Extended.of_string "description from source A") in
  let extended_b = Extended_set.singleton (Extended.of_string "description from source B") in
  let a = Entity.make uri created_a ~extended:extended_a () in
  let b = Entity.make uri created_b ~extended:extended_b () in
  let merged = Entity.absorb b a in
  let expected_extended = Extended_set.union extended_a extended_b in
  Alcotest.(check (module Extended_set))
    "extended sets unioned"
    expected_extended
    (Entity.extended merged)

(* Two entities that share a description but differ elsewhere are not equal, so
   absorb's equality guard does not fire and the merge runs. The description
   must still appear once. *)
let test_entity_absorb_extended_shared () =
  let open Entity in
  let uri = Uri.of_string "https://foo.org" in
  let created = Time.of_string "September 2, 2024" in
  let extended = Extended_set.singleton (Extended.of_string "a shared description") in
  let described label =
    Entity.make uri created ~labels:(Label_set.singleton (Label.of_string label)) ~extended ()
  in
  let merged = Entity.absorb (described "b") (described "a") in
  Alcotest.(check (module Extended_set))
    "a shared description is not duplicated"
    extended
    (Entity.extended merged)

let test_collection_upsert () =
  let open Entity in
  let uri = Uri.of_string "https://foo.org" in
  let name = Name.of_string "foo" in
  let names = Name_set.singleton name in
  let created_a = Time.of_string "September 4, 2024" in
  let created_b = Time.of_string "September 2, 2024" in
  let labels_foo = Label_set.singleton (Label.of_string "foo") in
  let labels_bar = Label_set.singleton (Label.of_string "bar") in
  let a = Entity.make uri created_a ~labels:labels_foo () in
  let b = Entity.make uri created_b ~maybe_name:(Some name) ~labels:labels_bar () in
  let coll = Collection.create () in
  let id_a = Collection.upsert coll a in
  let id_b = Collection.upsert coll b in
  let expected_length = 1 in
  Alcotest.(check int) same_length expected_length (Collection.length coll);
  Alcotest.(check (module Collection.Id)) "same id" id_a id_b;
  let e = Collection.entity coll id_a in
  Alcotest.(check (module Uri)) same_uri (Uri.canonicalize uri) (Entity.uri e);
  Alcotest.(check (module Time)) same_created_at created_b (Entity.created_at e);
  Alcotest.(check (list (module Time))) same_updated_at [ created_a ] (Entity.updated_at e);
  Alcotest.(check (module Name_set)) same_names names (Entity.names e);
  Alcotest.(check (module Label_set))
    same_labels
    (Label_set.union labels_foo labels_bar)
    (Entity.labels e)

let test_collection_add_edge () =
  let testable_id : Collection.Id.t Alcotest.testable = (module Collection.Id) in
  let open Entity in
  let uri_a = Uri.of_string "https://foo.org" in
  let uri_b = Uri.of_string "https://foo.net" in
  let created_a = Time.of_string "September 4, 2024" in
  let created_b = Time.of_string "September 2, 2024" in
  let a = Entity.make uri_a created_a () in
  let b = Entity.make uri_b created_b () in
  let coll = Collection.create () in
  let id_a = Collection.upsert coll a in
  let id_b = Collection.upsert coll b in
  let expected_length = 2 in
  Alcotest.(check int) same_length expected_length (Collection.length coll);
  let () = Collection.add_edge coll id_a id_b in
  let () = Collection.add_edge coll id_b id_a in
  Alcotest.(check (neg testable_id)) "different id" id_a id_b;
  let edges_a = [| id_b |] in
  let edges_b = [| id_a |] in
  Alcotest.(check (array testable_id)) same_edges edges_a (Collection.edges coll id_a);
  Alcotest.(check (array testable_id)) same_edges edges_b (Collection.edges coll id_b);
  let () = Collection.add_edge coll id_a id_b in
  let () = Collection.add_edge coll id_b id_a in
  Alcotest.(check (array testable_id)) same_edges edges_a (Collection.edges coll id_a);
  Alcotest.(check (array testable_id)) same_edges edges_b (Collection.edges coll id_b)

let test_collection_id_protection () =
  let open Entity in
  let uri_a = Uri.of_string "https://foo.org" in
  let uri_b = Uri.of_string "https://bar.org" in
  let created = Time.of_string "September 2, 2024" in
  let coll_a = Collection.create () in
  let coll_b = Collection.create () in
  let id_a = Collection.insert coll_a (Entity.make uri_a created ()) in
  let id_b = Collection.insert coll_b (Entity.make uri_b created ()) in
  Alcotest.(check (neg (module Collection.Id)))
    "ids from different collections are unequal"
    id_a
    id_b;
  let foreign_id_err = Invalid_argument "Collection: id belongs to a different collection" in
  Alcotest.check_raises "entity rejects foreign id" foreign_id_err (fun () ->
      ignore (Collection.entity coll_b id_a));
  Alcotest.check_raises "edges rejects foreign id" foreign_id_err (fun () ->
      ignore (Collection.edges coll_b id_a));
  Alcotest.check_raises "add_edge rejects foreign from" foreign_id_err (fun () ->
      Collection.add_edge coll_b id_a id_b);
  Alcotest.check_raises "add_edge rejects foreign target" foreign_id_err (fun () ->
      Collection.add_edge coll_b id_b id_a)

let test_time_of_string () =
  let open Entity in
  (* Expected values are UTC epoch seconds. They must not depend on the TZ
     this test runs under. *)
  let cases =
    [
      ("2023-11-15T00:00:00Z", 1700006400.);
      ("2023-11-15", 1700006400.);
      ("November 15, 2023", 1700006400.);
      ("1970-01-01T00:00:00Z", 0.);
      ("1969-12-31T00:00:00Z", -86400.);
      ("2024-02-29T12:34:56Z", 1709210096.);
      ("2000-03-01", 951868800.);
      ("1999-12-31", 946598400.);
      (* Midsummer: under the old local-time parse this shifted by the
         DST offset wherever the tests happened to run. *)
      ("2023-07-01", 1688169600.);
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check (float 0.))
        (Printf.sprintf "%S parses to %.0f" input expected)
        expected
        (float_of_string (Time.to_string (Time.of_string input))))
    cases

let test_time_of_string_rejects_garbage () =
  let open Entity in
  Alcotest.check_raises "unknown month name" (Time.Invalid_month_name "Smarch") (fun () ->
      ignore (Time.of_string "Smarch 3, 2023"))

let post_json href description time tags =
  Printf.sprintf
    {|{"href":%S,"description":%S,"time":%S,"extended":"","tags":%S,"shared":"yes","toread":"no"}|}
    href
    description
    time
    tags

let test_of_posts_merges_duplicates () =
  let open Entity in
  let posts =
    Pinboard.Post.from_json
      (Printf.sprintf
         "[%s,%s]"
         (post_json "https://foo.org" "First" "2024-09-02T00:00:00Z" "one")
         (post_json "https://foo.org" "Second" "2024-09-04T00:00:00Z" "two"))
  in
  let coll = Collection.of_posts posts in
  Alcotest.(check int) "duplicate hrefs collapse to one entity" 1 (Collection.length coll);
  let uri = Uri.of_string "https://foo.org" in
  let id = Option.get (Collection.id coll (Uri.canonicalize uri)) in
  let e = Collection.entity coll id in
  Alcotest.(check (module Time))
    "earliest post wins created_at"
    (Time.of_string "2024-09-02T00:00:00Z")
    (Entity.created_at e);
  Alcotest.(check (list (module Time)))
    same_updated_at
    [ Time.of_string "2024-09-04T00:00:00Z" ]
    (Entity.updated_at e);
  Alcotest.(check (module Name_set))
    same_names
    (Name_set.of_list [ Name.of_string "First"; Name.of_string "Second" ])
    (Entity.names e);
  Alcotest.(check (module Label_set))
    same_labels
    (Label_set.of_list [ Label.of_string "one"; Label.of_string "two" ])
    (Entity.labels e)

let test_of_posts_roundtrips_duplicates () =
  let posts =
    Pinboard.Post.from_json
      (Printf.sprintf
         "[%s,%s]"
         (post_json "https://foo.org" "First" "2024-09-02T00:00:00Z" "one")
         (post_json "https://foo.org" "Second" "2024-09-04T00:00:00Z" "two"))
  in
  let coll = Collection.of_posts posts in
  (* Before duplicates were merged this raised, leaving the collection
     unserializable: the second node shadowed the first in the uri index. *)
  let reparsed = Collection.t_of_yaml (Collection.yaml_of_t coll) in
  Alcotest.(check int) same_length (Collection.length coll) (Collection.length reparsed)

let entity_yaml uri =
  Printf.sprintf {|{uri: "%s", createdAt: 0, updatedAt: [], names: [], labels: []}|} uri

let node_yaml ?(uri = "https://a.org/") ?(edges = "[]") id =
  Printf.sprintf "- {id: %d, entity: %s, edges: %s}" id (entity_yaml uri) edges

let collection_yaml ~length nodes =
  Printf.sprintf "version: 0.1.0\nlength: %d\nvalue:\n%s\n" length (String.concat "\n" nodes)

let of_yaml_string s = Collection.t_of_yaml (Yaml.of_string_exn s)

let check_invalid name message s =
  Alcotest.check_raises name (Collection.Invalid message) (fun () -> ignore (of_yaml_string s))

let test_yaml_roundtrip () =
  let open Entity in
  let coll = Collection.create () in
  let a =
    Entity.make
      (Uri.of_string "https://foo.org")
      (Time.of_string "September 2, 2024")
      ~maybe_name:(Some (Name.of_string "Foo"))
      ~labels:(Label_set.singleton (Label.of_string "one"))
      ~extended:(Extended_set.singleton (Extended.of_string "a description"))
      ~shared:(Shared.of_bool true)
      ~to_read:(To_read.of_bool false)
      ()
  in
  let b = Entity.make (Uri.of_string "https://bar.org") (Time.of_string "September 4, 2024") () in
  let id_a = Collection.upsert coll a in
  let id_b = Collection.upsert coll b in
  Collection.add_edges coll id_a id_b;
  let reparsed = Collection.t_of_yaml (Collection.yaml_of_t coll) in
  Alcotest.(check int) same_length (Collection.length coll) (Collection.length reparsed);
  let id_a' = Option.get (Collection.id reparsed (Entity.uri a)) in
  Alcotest.(check (module Entity))
    same_entity
    (Collection.entity coll id_a)
    (Collection.entity reparsed id_a');
  Alcotest.(check int)
    same_edges
    (Array.length (Collection.edges coll id_a))
    (Array.length (Collection.edges reparsed id_a'))

let test_yaml_accepts_valid () =
  let coll = of_yaml_string (collection_yaml ~length:1 [ node_yaml 0 ~edges:"[0]" ]) in
  Alcotest.(check int) same_length 1 (Collection.length coll)

let test_yaml_rejects_length_mismatch () =
  check_invalid
    "length mismatch"
    "declared length 3 but found 1 nodes"
    (collection_yaml ~length:3 [ node_yaml 0 ])

let test_yaml_rejects_negative_length () =
  check_invalid "negative length" "negative length -1" "version: 0.1.0\nlength: -1\nvalue: []\n"

let test_yaml_rejects_out_of_bounds_id () =
  check_invalid
    "id out of bounds"
    "node id 5 out of bounds for length 1"
    (collection_yaml ~length:1 [ node_yaml 5 ])

let test_yaml_rejects_out_of_bounds_edge () =
  check_invalid
    "edge out of bounds"
    "node 0 has an edge to 99, out of bounds for length 1"
    (collection_yaml ~length:1 [ node_yaml 0 ~edges:"[99]" ])

let test_yaml_rejects_duplicate_id () =
  check_invalid
    "duplicate id"
    "duplicate node id 0"
    (collection_yaml ~length:2 [ node_yaml 0; node_yaml 0 ~uri:"https://b.org/" ])

let test_yaml_rejects_duplicate_uri () =
  check_invalid
    "duplicate uri"
    "duplicate uri https://a.org/ at node 1"
    (collection_yaml ~length:2 [ node_yaml 0; node_yaml 1 ])

let test_yaml_rejects_missing_uri () =
  check_invalid
    "missing uri"
    "node 0 has no uri"
    (collection_yaml
       ~length:1
       [ "- {id: 0, entity: {createdAt: 0, updatedAt: [], names: [], labels: []}, edges: []}" ])

let test_entity_yaml_rejects_missing_uri () =
  Alcotest.check_raises "entity without a uri" Entity.Missing_uri (fun () ->
      ignore (Entity.t_of_yaml (Yaml.of_string_exn "{createdAt: 0, names: [], labels: []}")))

let test_yaml_rejects_bad_version () =
  Alcotest.check_raises "malformed version" (Collection.Version.Malformed "not-semver") (fun () ->
      ignore (of_yaml_string "version: not-semver\nlength: 0\nvalue: []\n"));
  Alcotest.check_raises "unsupported version" (Collection.Version.Unsupported "9.9.9") (fun () ->
      ignore (of_yaml_string "version: 9.9.9\nlength: 0\nvalue: []\n"))

let tests =
  let open Alcotest in
  [
    ( "Entity",
      [
        test_case "equal" `Quick test_entity_equal;
        test_case "update" `Quick test_entity_update;
        test_case "update with an equal timestamp" `Quick test_entity_update_equal_timestamp;
        test_case "absorb" `Quick test_entity_absorb;
        test_case "absorb extended" `Quick test_entity_absorb_extended;
        test_case "absorb shared extended" `Quick test_entity_absorb_extended_shared;
      ] );
    ( "Time",
      [
        test_case "of_string" `Quick test_time_of_string;
        test_case "of_string rejects garbage" `Quick test_time_of_string_rejects_garbage;
      ] );
    ( "Collection",
      [
        test_case "insert" `Quick test_collection_upsert;
        test_case "add_edge" `Quick test_collection_add_edge;
        test_case "id protection" `Quick test_collection_id_protection;
        test_case "of_posts merges duplicates" `Quick test_of_posts_merges_duplicates;
        test_case "of_posts roundtrips duplicates" `Quick test_of_posts_roundtrips_duplicates;
      ] );
    ( "Collection YAML",
      [
        test_case "roundtrip" `Quick test_yaml_roundtrip;
        test_case "accepts valid" `Quick test_yaml_accepts_valid;
        test_case "rejects length mismatch" `Quick test_yaml_rejects_length_mismatch;
        test_case "rejects negative length" `Quick test_yaml_rejects_negative_length;
        test_case "rejects out-of-bounds id" `Quick test_yaml_rejects_out_of_bounds_id;
        test_case "rejects out-of-bounds edge" `Quick test_yaml_rejects_out_of_bounds_edge;
        test_case "rejects duplicate id" `Quick test_yaml_rejects_duplicate_id;
        test_case "rejects duplicate uri" `Quick test_yaml_rejects_duplicate_uri;
        test_case "rejects missing uri" `Quick test_yaml_rejects_missing_uri;
        test_case "entity rejects missing uri" `Quick test_entity_yaml_rejects_missing_uri;
        test_case "rejects bad version" `Quick test_yaml_rejects_bad_version;
      ] );
  ]

let () = Alcotest.run "Collection" tests
