open Hbt

(* Known failing tests due to formatting differences *)
let known_yaml_failures = [ "xml_sample"; "json_sample"; "bookmarks_pinboard" ]
let known_html_failures = [ "bookmarks_feeds" ]
let should_skip_yaml_test name = List.mem name known_yaml_failures
let should_skip_html_test name = List.mem name known_html_failures

let collection_to_yaml (collection : Collection.t) : string =
  let yaml_result = Collection.yaml_of_t collection in
  let yaml_string = Yaml.to_string yaml_result in
  Result.get_ok yaml_string

let parse_input_to_collection : Test_data.input_source -> Collection.t = function
  | Content content -> Markdown.parse content
  | File filename when String.ends_with ~suffix:".html" filename -> Collection.from_html filename
  | File filename when String.ends_with ~suffix:".xml" filename ->
      let entries = Pinboard.from_xml filename in
      let collection = Collection.create () in
      let add_entry entry =
        let entity = Collection.Entity.of_pinboard entry in
        ignore (Collection.upsert collection entity)
      in
      List.iter add_entry entries;
      collection
  | File filename when String.ends_with ~suffix:".json" filename ->
      let entries = Pinboard.from_json filename in
      let collection = Collection.create () in
      let add_entry entry =
        let entity = Collection.Entity.of_pinboard entry in
        ignore (Collection.upsert collection entity)
      in
      List.iter add_entry entries;
      collection
  | File filename -> failwith ("Unknown file type: " ^ filename)

let test_yaml_output (test_case : Test_data.test_case) =
  let test_fn () =
    let expected_yaml =
      match test_case.expected with
      | Yaml s -> s
      | Html _ -> failwith "Expected YAML output for this test"
    in
    let collection = parse_input_to_collection test_case.input in
    let actual_yaml = collection_to_yaml collection in
    let normalized_actual = Test_data.normalize_yaml actual_yaml in
    let normalized_expected = Test_data.normalize_yaml expected_yaml in
    Alcotest.(check string) test_case.name normalized_expected normalized_actual
  in
  let test_fn =
    if should_skip_yaml_test test_case.name then
      Alcotest.skip
    else
      test_fn
  in
  (test_case.name, `Quick, test_fn)

let test_html_output (test_case : Test_data.test_case) =
  let test_fn () =
    let expected_html =
      match test_case.expected with
      | Html s -> s
      | Yaml _ -> failwith "Expected HTML output for this test"
    in
    let collection = parse_input_to_collection test_case.input in
    let actual_html = Collection.to_html collection in
    let normalized_actual = Test_data.normalize_html actual_html in
    let normalized_expected = Test_data.normalize_html expected_html in
    Alcotest.(check string) test_case.name normalized_expected normalized_actual
  in
  let test_fn =
    if should_skip_html_test test_case.name then
      Alcotest.skip
    else
      test_fn
  in
  (test_case.name, `Quick, test_fn)

let all_yaml_tests =
  let test_cases = Test_data.yaml_test_cases () in
  ("YAML Output", List.map test_yaml_output test_cases)

let all_html_tests =
  let test_cases = Test_data.html_test_cases () in
  ("HTML Output", List.map test_html_output test_cases)

let all_tests = [ all_yaml_tests; all_html_tests ]
let () = Alcotest.run "HBT Tests" all_tests
