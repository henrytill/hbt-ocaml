type input_source =
  | Content of string (* For parsers that accept content *)
  | File of string (* For parsers that accept filenames *)

type expected_output =
  | Yaml of string
  | Html of string

type test_case = {
  name : string;
  input : input_source;
  expected : expected_output;
}

let normalize_yaml content =
  (* Basic YAML normalization - remove trailing whitespace *)
  let lines = String.split_on_char '\n' content in
  let trimmed_lines = List.map String.trim lines in
  String.concat "\n" trimmed_lines

let normalize_html content =
  (* Basic HTML normalization - remove extra whitespace *)
  let lines = String.split_on_char '\n' content in
  let trimmed_lines = List.map String.trim lines in
  let is_non_empty line = line <> "" in
  let non_empty_lines = List.filter is_non_empty trimmed_lines in
  String.concat "\n" non_empty_lines

let discover_test_files dir extension =
  let full_dir = Filename.concat "data" dir in
  let suffix = ".input" ^ extension in
  try
    let files = Sys.readdir full_dir in
    let file_list = Array.to_list files in
    let extract_name filename =
      if String.ends_with ~suffix filename then
        let name_len = String.length filename - String.length suffix in
        let name = String.sub filename 0 name_len in
        Some name
      else
        None
    in
    let filtered_names = List.filter_map extract_name file_list in
    List.sort String.compare filtered_names
  with Sys_error _ -> []

let read_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

let load_test_case dir extension name =
  let base_path = Filename.concat "data" dir in
  let input_file = Filename.concat base_path (name ^ ".input" ^ extension) in
  let expected_yaml_file = Filename.concat base_path (name ^ ".expected.yaml") in
  let expected_html_file = Filename.concat base_path (name ^ ".expected.html") in

  let input_content = read_file input_file in

  let input =
    match extension with
    | ".md" -> Content input_content
    | _ -> File input_file
  in

  let expected =
    if Sys.file_exists expected_html_file then
      Html (read_file expected_html_file)
    else
      Yaml (read_file expected_yaml_file)
  in

  { name; input; expected }

let load_test_case_yaml dir extension name =
  let base_path = Filename.concat "data" dir in
  let input_file = Filename.concat base_path (name ^ ".input" ^ extension) in
  let expected_yaml_file = Filename.concat base_path (name ^ ".expected.yaml") in

  let input_content = read_file input_file in

  let input =
    match extension with
    | ".md" -> Content input_content
    | _ -> File input_file
  in

  { name; input; expected = Yaml (read_file expected_yaml_file) }

let yaml_test_cases () =
  let markdown_files = discover_test_files "markdown" ".md" in
  let markdown_cases = List.map (load_test_case_yaml "markdown" ".md") markdown_files in
  let xml_files = discover_test_files "pinboard" ".xml" in
  let xml_cases = List.map (load_test_case_yaml "pinboard" ".xml") xml_files in
  let json_files = discover_test_files "pinboard" ".json" in
  let json_cases = List.map (load_test_case_yaml "pinboard" ".json") json_files in
  let html_files = discover_test_files "html" ".html" in
  let html_cases = List.map (load_test_case_yaml "html" ".html") html_files in
  List.concat [ markdown_cases; xml_cases; json_cases; html_cases ]

let html_test_cases () =
  let html_files = discover_test_files "html" ".html" in
  let html_cases = List.map (load_test_case "html" ".html") html_files in
  let is_html_test tc =
    match tc.expected with
    | Html _ -> true
    | Yaml _ -> false
  in
  List.filter is_html_test html_cases
