open Hbt.Collection

let same_entity_count = "same entity count"
let same_uri = "same uri"
let same_shared = "same shared"
let same_is_feed = "same is_feed"
let same_extended = "same extended"
let has_label = "has label"
let has_last_visit = "has last visit time"

let test_simple_bookmarks () =
  let collection = from_html "bookmarks_simple.html" in
  let entities_array = entities collection in
  let entity_count = Array.length entities_array in
  Alcotest.(check int) same_entity_count 3 entity_count;

  (* Check first bookmark *)
  let entity1 = entities_array.(0) in
  let uri1 = Entity.uri entity1 |> Uri.to_string in
  Alcotest.(check string) same_uri "https://example.com/" uri1;
  Alcotest.(check bool) same_shared true (Entity.shared entity1);
  Alcotest.(check bool) same_is_feed false (Entity.is_feed entity1);

  (* Check labels include tags *)
  let labels1 = Entity.labels entity1 in
  let has_example = Label_set.mem (Label.of_string "example") labels1 in
  let has_test = Label_set.mem (Label.of_string "test") labels1 in
  Alcotest.(check bool) has_label true has_example;
  Alcotest.(check bool) has_label true has_test

let find_entity_by_uri entities_array uri_string =
  let rec find_in_array i =
    if i >= Array.length entities_array then
      failwith ("Entity not found: " ^ uri_string)
    else
      let entity = entities_array.(i) in
      if Uri.to_string (Entity.uri entity) = uri_string then
        entity
      else
        find_in_array (i + 1)
  in
  find_in_array 0

let test_folder_structure () =
  let collection = from_html "bookmarks_folders.html" in
  let entities_array = entities collection in

  (* Find GitHub bookmark which should be in Programming folder *)
  let github_entity = find_entity_by_uri entities_array "https://github.com/" in

  let labels = Entity.labels github_entity in
  let has_folder_label = Label_set.mem (Label.of_string "Programming") labels in
  let has_git_tag = Label_set.mem (Label.of_string "git") labels in
  let has_hosting_tag = Label_set.mem (Label.of_string "hosting") labels in
  Alcotest.(check bool) has_label true has_folder_label;
  Alcotest.(check bool) has_label true has_git_tag;
  Alcotest.(check bool) has_label true has_hosting_tag

let test_nested_folders () =
  let collection = from_html "bookmarks_folders.html" in
  let entities_array = entities collection in

  (* Find OCaml bookmark which should be in Programming/Languages folder *)
  let ocaml_entity = find_entity_by_uri entities_array "https://ocaml.org/" in

  let labels = Entity.labels ocaml_entity in
  let has_languages_folder = Label_set.mem (Label.of_string "Languages") labels in
  let has_programming_folder = Label_set.mem (Label.of_string "Programming") labels in
  let has_functional_tag = Label_set.mem (Label.of_string "functional") labels in
  Alcotest.(check bool) has_label true has_languages_folder;
  Alcotest.(check bool) has_label true has_programming_folder;
  Alcotest.(check bool) has_label true has_functional_tag

let test_privacy_flags () =
  let collection = from_html "bookmarks_privacy.html" in
  let entities_array = entities collection in

  (* Find public example bookmark *)
  let public_entity = find_entity_by_uri entities_array "https://example.com/" in
  Alcotest.(check bool) same_shared true (Entity.shared public_entity);

  (* Find private internal company site *)
  let private_entity = find_entity_by_uri entities_array "https://internal.company.com/" in
  Alcotest.(check bool) same_shared false (Entity.shared private_entity);

  (* Find GitHub bookmark with no privacy flag (should default to public) *)
  let github_entity = find_entity_by_uri entities_array "https://github.com/" in
  Alcotest.(check bool) same_shared true (Entity.shared github_entity)

let test_feed_attributes () =
  let collection = from_html "bookmarks_feeds.html" in
  let entities_array = entities collection in

  (* Find O'Reilly Radar feed *)
  let oreilly_entity =
    find_entity_by_uri entities_array "https://feeds.feedburner.com/oreilly/radar"
  in
  Alcotest.(check bool) same_is_feed true (Entity.is_feed oreilly_entity);

  let labels = Entity.labels oreilly_entity in
  let has_tech = Label_set.mem (Label.of_string "tech") labels in
  let has_books = Label_set.mem (Label.of_string "books") labels in
  let has_rss_folder = Label_set.mem (Label.of_string "RSS Feeds") labels in
  Alcotest.(check bool) has_label true has_tech;
  Alcotest.(check bool) has_label true has_books;
  Alcotest.(check bool) has_label true has_rss_folder;

  (* Find GitHub which is not a feed *)
  let github_entity = find_entity_by_uri entities_array "https://github.com/" in
  Alcotest.(check bool) same_is_feed false (Entity.is_feed github_entity)

let test_last_visit_times () =
  let collection = from_html "bookmarks_simple.html" in
  let entities_array = entities collection in

  (* Find GitHub bookmark which has LAST_VISIT *)
  let github_entity = find_entity_by_uri entities_array "https://github.com/" in

  let entity_has_last_visit =
    match Entity.last_visited_at github_entity with
    | Some _ -> true
    | None -> false
  in
  Alcotest.(check bool) has_last_visit true entity_has_last_visit

let test_descriptions () =
  let collection = from_html "bookmarks_simple.html" in
  let entities_array = entities collection in

  (* Find example website with description *)
  let example_entity = find_entity_by_uri entities_array "https://example.com/" in

  let expected_desc = "A simple example website for testing" in
  let actual_desc =
    match Entity.extended example_entity with
    | Some desc -> Extended.to_string desc
    | None -> ""
  in
  Alcotest.(check string) same_extended expected_desc actual_desc

let test_github_labels_debug () =
  (* Focused test: check exactly what labels GitHub bookmark has *)
  let collection = from_html "bookmarks_folders.html" in
  let entities_array = entities collection in
  let github_entity = find_entity_by_uri entities_array "https://github.com/" in

  let labels = Entity.labels github_entity in
  let label_strings = Label_set.fold (fun label acc -> Label.to_string label :: acc) labels [] in
  let sorted_labels = List.sort String.compare label_strings in

  (* GitHub should have: Programming (folder), git (tag), hosting (tag) *)
  let expected_labels = [ "Programming"; "git"; "hosting" ] in
  let sorted_expected = List.sort String.compare expected_labels in

  Alcotest.(check (list string)) "github labels" sorted_expected sorted_labels

let tests =
  let open Alcotest in
  [
    ( "Netscape Parser",
      [
        test_case "simple bookmarks" `Quick test_simple_bookmarks;
        test_case "folder structure" `Quick test_folder_structure;
        test_case "nested folders" `Quick test_nested_folders;
        test_case "privacy flags" `Quick test_privacy_flags;
        test_case "feed attributes" `Quick test_feed_attributes;
        test_case "last visit times" `Quick test_last_visit_times;
        test_case "descriptions" `Quick test_descriptions;
        test_case "github labels debug" `Quick test_github_labels_debug;
      ] );
  ]

let () = Alcotest.run "Netscape" tests
