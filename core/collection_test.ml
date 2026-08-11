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
  let extended_update = [] in
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
  let extended_a = [ Extended.of_string "description from source A" ] in
  let extended_b = [ Extended.of_string "description from source B" ] in
  let a = Entity.make uri created_a ~extended:extended_a () in
  let b = Entity.make uri created_b ~extended:extended_b () in
  let merged = Entity.absorb b a in
  let expected_extended = extended_a @ extended_b in
  Alcotest.(check (list (module Extended)))
    "extended lists concatenated"
    expected_extended
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

let tests =
  let open Alcotest in
  [
    ( "Entity",
      [
        test_case "equal" `Quick test_entity_equal;
        test_case "update" `Quick test_entity_update;
        test_case "absorb" `Quick test_entity_absorb;
        test_case "absorb extended" `Quick test_entity_absorb_extended;
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
  ]

let () = Alcotest.run "Collection" tests
