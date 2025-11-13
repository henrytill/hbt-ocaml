module Result_syntax = struct
  let ( let* ) = Result.bind
  let ( let+ ) x f = Result.map f x

  let ( and+ ) x y =
    match (x, y) with
    | Ok a, Ok b -> Ok (a, b)
    | Error e, _ -> Error e
    | _, Error e -> Error e

  let pure = Result.ok
end

open Hbt
open Result_syntax

type 'a t = {
  name : string;
  format : 'a Data.t;
  input : string;
  expected : string;
}

let base_dir = Fpath.(normalize (v "data"))

let input_to_dir : Data.input -> Fpath.t option =
  let open Fpath in
  function
  | Json -> Some (base_dir / "pinboard" / "json")
  | Xml -> Some (base_dir / "pinboard" / "xml")
  | Markdown -> Some (base_dir / "markdown")
  | Html -> Some (base_dir / "html")
  | Yaml -> None

let output_to_dir : Data.output -> Fpath.t option =
  let open Fpath in
  function
  | Html -> Some (base_dir / "html")
  | Yaml -> None

let format_to_ext : type a. a Data.t -> string = function
  | Json -> "json"
  | Xml -> "xml"
  | Markdown -> "md"
  | Html -> "html"
  | Yaml -> "yaml"

module Test_map = Map.Make (Fpath)

let split s = List.filter (( <> ) String.empty) (String.split_on_char '.' s)

let read_exn file =
  match Bos.OS.File.read file with
  | Ok contents -> contents
  | Error (`Msg msg) -> failwith msg

let accum _k v acc = v :: acc

let discover_input (format : Data.input) =
  let f acc file =
    let stem, ext = Fpath.split_ext ~multi:true file in
    let name_thunk = lazy (Fpath.to_string stem) in
    let updater =
      match split ext with
      | [ "expected"; "yaml" ] -> begin
          let expected = read_exn file in
          function
          | None ->
              let name = Lazy.force name_thunk in
              Some { name; format; input = String.empty; expected }
          | Some entry -> Some { entry with expected }
        end
      | [ "input"; ext ] when ext = format_to_ext format -> begin
          let input = read_exn file in
          function
          | None ->
              let name = Lazy.force name_thunk in
              Some { name; format; input; expected = String.empty }
          | Some entry -> Some { entry with input }
        end
      | _ -> Fun.id
    in
    Test_map.update stem updater acc
  in
  let* dir =
    match input_to_dir format with
    | None -> Error (`Msg ("no inputs for " ^ Data.to_string format))
    | Some dir -> Ok dir
  in
  let* all_files = Bos.OS.Dir.contents dir in
  try
    let inputs = List.fold_left f Test_map.empty all_files in
    pure (Test_map.fold accum inputs [])
  with Failure msg -> Error (`Msg msg)

let discover_output (format : Data.output) =
  let f acc file =
    let stem, ext = Fpath.split_ext ~multi:true file in
    let name_thunk = lazy (Fpath.to_string stem) in
    let updater =
      match split ext with
      | [ "expected"; ext ] when ext = format_to_ext format -> begin
          let expected = read_exn file in
          function
          | None ->
              let name = Lazy.force name_thunk in
              Some { name; format; input = String.empty; expected }
          | Some entry -> Some { entry with expected }
        end
      | [ "input"; _ ] -> begin
          let input = read_exn file in
          function
          | None ->
              let name = Lazy.force name_thunk in
              Some { name; format; input; expected = String.empty }
          | Some entry -> Some { entry with input }
        end
      | _ -> Fun.id
    in
    Test_map.update stem updater acc
  in
  let* dir =
    match output_to_dir format with
    | None -> Error (`Msg ("no inputs for " ^ Data.to_string format))
    | Some dir -> Ok dir
  in
  let* all_files = Bos.OS.Dir.contents dir in
  try
    let inputs = List.fold_left f Test_map.empty all_files in
    pure (Test_map.fold accum inputs [])
  with Failure msg -> Error (`Msg msg)

let yaml = Alcotest.testable Yaml.pp Yaml.equal

let test_parser (test_case : [ `Input ] t) () =
  let parsed = Data.parse test_case.format test_case.input in
  let actual_yaml = Collection.yaml_of_t parsed in
  let expected_yaml = Yaml.of_string_exn test_case.expected in
  Alcotest.(check yaml) "same yaml" expected_yaml actual_yaml

let test_formatter ~input_format (test_case : [ `Output ] t) () =
  let parsed = Data.parse input_format test_case.input in
  let formatted = Data.format test_case.format parsed in
  let actual_reparsed = Data.parse input_format formatted in
  let expected_reparsed = Data.parse input_format test_case.expected in
  let actual_yaml = Collection.yaml_of_t actual_reparsed in
  let expected_yaml = Collection.yaml_of_t expected_reparsed in
  Alcotest.(check yaml) "same yaml" expected_yaml actual_yaml

let parser_tests cases =
  List.map (fun test_case -> Alcotest.test_case test_case.name `Quick (test_parser test_case)) cases

let formatter_tests ~input_format cases =
  List.map
    (fun test_case ->
      Alcotest.test_case test_case.name `Quick (test_formatter ~input_format test_case))
    cases

let html =
  let* parser_cases = discover_input Data.Html in
  let* formatter_cases = discover_output Data.Html in
  pure
    [
      ("HTML Parser", parser_tests parser_cases);
      ("HTML Formatter", formatter_tests ~input_format:Data.Html formatter_cases);
    ]

let markdown =
  let* test_cases = discover_input Data.Markdown in
  pure [ ("Markdown Parser", parser_tests test_cases) ]

let pinboard =
  let* json_cases = discover_input Data.Json in
  let* xml_cases = discover_input Data.Xml in
  pure
    [
      ("Pinboard JSON Parser", parser_tests json_cases);
      ("Pinboard XML Parser", parser_tests xml_cases);
    ]

let () =
  let all_tests =
    let+ html and+ markdown and+ pinboard in
    html @ markdown @ pinboard
  in
  match all_tests with
  | Ok tests -> Alcotest.run "Data" tests
  | Error (`Msg msg) -> failwith msg
