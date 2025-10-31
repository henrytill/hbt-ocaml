open Hbt
open Hbt.Collection

let testable_id = Alcotest.testable Id.pp Id.equal
let testable_uri = Alcotest.testable Entity.Uri.pp Entity.Uri.equal
let testable_time = Alcotest.testable Entity.Time.pp Entity.Time.equal
let testable_name_set = Alcotest.testable Entity.Name_set.pp Entity.Name_set.equal
let testable_label_set = Alcotest.testable Entity.Label_set.pp Entity.Label_set.equal
let testable_entity = Alcotest.testable Entity.pp Entity.equal
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
  let labels = Label_set.of_list [ Label.of_string "foo" ] in
  let a = Entity.make uri created ~maybe_name ~labels () in
  let b = Entity.make uri created ~maybe_name ~labels () in
  Alcotest.(check testable_entity) same_entity a b

let test_entity_update () =
  let open Entity in
  let uri = Uri.of_string "https://foo.org" in
  let maybe_name = Some (Name.of_string "foo") in
  let created = Time.of_string "September 2, 2024" in
  let labels = Label_set.of_list [ Label.of_string "foo" ] in
  let a = Entity.make uri created ~maybe_name ~labels () in
  let updated = Time.of_string "September 4, 2024" in
  let names_update = Name_set.of_list [ Name.of_string "Foo.org"; Name.of_string "F00" ] in
  let labels_update = Label_set.of_list [ Label.of_string "foozer"; Label.of_string "bar" ] in
  let a = Entity.update updated names_update labels_update a in
  Alcotest.(check testable_uri) same_uri (Uri.canonicalize uri) (Entity.uri a);
  Alcotest.(check testable_time) same_created_at created (Entity.created_at a);
  Alcotest.(check (list testable_time)) same_updated_at [ updated ] (Entity.updated_at a);
  Alcotest.(check testable_name_set)
    same_names
    (Name_set.union
       (Option.fold ~none:Name_set.empty ~some:Name_set.singleton maybe_name)
       names_update)
    (Entity.names a);
  Alcotest.(check testable_label_set)
    same_labels
    (Label_set.union labels labels_update)
    (Entity.labels a)

let test_entity_absorb () =
  let open Entity in
  let uri = Uri.of_string "https://foo.org" in
  let name = Name.of_string "foo" in
  let created_a = Time.of_string "September 4, 2024" in
  let created_b = Time.of_string "September 2, 2024" in
  let labels_foo = Label_set.of_list [ Label.of_string "foo" ] in
  let labels_bar = Label_set.of_list [ Label.of_string "bar" ] in
  let a = Entity.make uri created_a ~labels:labels_foo () in
  let b = Entity.make uri created_b ~maybe_name:(Some name) ~labels:labels_bar () in
  let a = Entity.absorb b a in
  Alcotest.(check testable_uri) same_uri (Uri.canonicalize uri) (Entity.uri a);
  Alcotest.(check testable_time) same_created_at created_b (Entity.created_at a);
  Alcotest.(check (list testable_time)) same_updated_at [ created_a ] (Entity.updated_at a);
  Alcotest.(check testable_name_set) same_names (Name_set.of_list [ name ]) (Entity.names a);
  Alcotest.(check testable_label_set)
    same_labels
    (Label_set.union labels_foo labels_bar)
    (Entity.labels a)

let test_collection_upsert () =
  let open Entity in
  let uri = Uri.of_string "https://foo.org" in
  let name = Name.of_string "foo" in
  let created_a = Time.of_string "September 4, 2024" in
  let created_b = Time.of_string "September 2, 2024" in
  let labels_foo = Label_set.of_list [ Label.of_string "foo" ] in
  let labels_bar = Label_set.of_list [ Label.of_string "bar" ] in
  let a = Entity.make uri created_a ~labels:labels_foo () in
  let b = Entity.make uri created_b ~maybe_name:(Some name) ~labels:labels_bar () in
  let coll = create () in
  let id_a = upsert coll a in
  let id_b = upsert coll b in
  let expected_length = 1 in
  Alcotest.(check int) same_length expected_length (length coll);
  Alcotest.(check testable_id) "same id" id_a id_b;
  let e = entity coll id_a in
  Alcotest.(check testable_uri) same_uri (Uri.canonicalize uri) (Entity.uri e);
  Alcotest.(check testable_time) same_created_at created_b (Entity.created_at e);
  Alcotest.(check (list testable_time)) same_updated_at [ created_a ] (Entity.updated_at e);
  Alcotest.(check testable_name_set) same_names (Name_set.of_list [ name ]) (Entity.names e);
  Alcotest.(check testable_label_set)
    same_labels
    (Label_set.union labels_foo labels_bar)
    (Entity.labels e)

let test_collection_add_edge () =
  let open Entity in
  let uri_a = Uri.of_string "https://foo.org" in
  let uri_b = Uri.of_string "https://foo.net" in
  let created_a = Time.of_string "September 4, 2024" in
  let created_b = Time.of_string "September 2, 2024" in
  let a = Entity.make uri_a created_a () in
  let b = Entity.make uri_b created_b () in
  let coll = create () in
  let id_a = upsert coll a in
  let id_b = upsert coll b in
  let expected_length = 2 in
  Alcotest.(check int) same_length expected_length (length coll);
  let () = add_edge coll id_a id_b in
  let () = add_edge coll id_b id_a in
  Alcotest.(check (neg testable_id)) "different id" id_a id_b;
  let edges_a = [| id_b |] in
  let edges_b = [| id_a |] in
  Alcotest.(check (array testable_id)) same_edges edges_a (edges coll id_a);
  Alcotest.(check (array testable_id)) same_edges edges_b (edges coll id_b);
  let () = add_edge coll id_a id_b in
  let () = add_edge coll id_b id_a in
  Alcotest.(check (array testable_id)) same_edges edges_a (edges coll id_a);
  Alcotest.(check (array testable_id)) same_edges edges_b (edges coll id_b)

let tests =
  let open Alcotest in
  [
    ( "Entity",
      [
        test_case "equal" `Quick test_entity_equal;
        test_case "update" `Quick test_entity_update;
        test_case "absorb" `Quick test_entity_absorb;
      ] );
    ( "Collection",
      [
        test_case "insert" `Quick test_collection_upsert;
        test_case "add_edge" `Quick test_collection_add_edge;
      ] );
  ]

let () = Alcotest.run "Collection" tests
