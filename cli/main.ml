open Cmdliner
open Hbt

exception Unsupported_file_format of string
exception Missing_output_specification

let version = Version.version

module Args = struct
  type t = {
    input_format : Data.input option;
    output_format : Data.output option;
    output : string option;
    info : bool;
    list_tags : bool;
    mappings_file : string option;
  }

  let make input_format output_format output info list_tags mappings_file =
    { input_format; output_format; output; info; list_tags; mappings_file }
end

let detect_input_format file =
  match Data.detect_input_format file with
  | None -> raise (Unsupported_file_format (Filename.extension file))
  | Some format -> format

let read_file file =
  let ic = open_in file in
  let finally () = close_in ic in
  Fun.protect ~finally (fun () -> really_input_string ic (in_channel_length ic))

let update (coll : Collection.t) (args : Args.t) : unit =
  match args.mappings_file with
  | None -> ()
  | Some file -> read_file file |> Data.yaml_of_string |> Collection.update_labels coll

let write (content : string) : string option -> unit = function
  | None -> print_string content
  | Some file ->
      let oc = open_out file in
      let finally () = close_out oc in
      Fun.protect ~finally (fun () -> output_string oc content)

let print (file : string) (args : Args.t) (coll : Collection.t) : unit =
  let open Collection in
  let output =
    if args.info then
      Printf.sprintf "%s: %d entities\n" file (length coll)
    else if args.list_tags then
      let open Entity in
      entities coll
      |> Array.fold_left (fun acc ent -> Label_set.union acc (labels ent)) Label_set.empty
      |> Label_set.elements
      |> List.map Label.to_string
      |> String.concat "\n"
    else
      let output_format =
        match args.output_format with
        | None -> Option.bind args.output Data.detect_output_format
        | Some _ as format -> format
      in
      match output_format with
      | None -> raise Missing_output_specification
      | Some format -> Data.format format coll
  in
  write output args.output

(* Expected failures, reported as `hbt: <msg>`. Anything not listed here is a
   bug rather than bad input, and is left to escape as an internal error. *)
let explain (file : string) : exn -> string option =
  let in_file fmt = Printf.ksprintf (fun msg -> Some (file ^ ": " ^ msg)) fmt in
  function
  | Sys_error msg -> Some msg
  | Unsupported_file_format "" ->
      in_file "cannot determine the format from the file name; pass -f FORMAT"
  | Unsupported_file_format ext -> in_file "unsupported file format %S" ext
  | Missing_output_specification ->
      Some "no output format: pass -t FORMAT, or -o FILE with a known extension"
  | Data.Malformed_yaml msg -> in_file "%s" msg
  | Data.Yaml_conversion_error msg -> in_file "could not write YAML: %s" msg
  | Collection.Invalid msg -> in_file "invalid collection: %s" msg
  | Collection.Version.Unsupported version ->
      in_file
        "collection version %s is not supported, expected %s"
        version
        (Collection.Version.to_string Collection.Version.expected)
  | Collection.Version.Malformed version -> in_file "malformed collection version %S" version
  | Entity.Missing_uri -> in_file "an entity has no uri"
  | Entity.Time.Invalid_month_name month -> in_file "unknown month name %S" month
  | Markdown.Missing_date uri -> in_file "%s appears before any date heading" uri
  | Pinboard.Post.Unexpected_xml_element name -> in_file "unexpected XML element %S" name
  | Yaml.Util.Value_error msg -> in_file "%s" msg
  | Prelude.Yaml_ext.Missing_field key -> in_file "missing field %S" key
  | Scanf.Scan_failure msg -> in_file "could not parse a date: %s" msg
  | _ -> None

let process_file (args : Args.t) (file : string) : (unit, string) result =
  (* Each stage names the file it is working on, so an error in the mappings
     file is not reported against the input file. *)
  let stage current f =
    try Ok (f ())
    with exn -> (
      match explain current exn with
      | Some msg -> Error msg
      | None -> raise exn)
  in
  let ( let* ) = Result.bind in
  let* input_format =
    stage file (fun () ->
        match args.input_format with
        | None -> detect_input_format file
        | Some format -> format)
  in
  let* content = stage file (fun () -> read_file file) in
  let updated_args = { args with input_format = Some input_format } in
  let* coll = stage file (fun () -> Data.parse input_format content) in
  let* () =
    stage (Option.value ~default:file args.mappings_file) (fun () -> update coll updated_args)
  in
  stage file (fun () -> print file updated_args coll)

let from_format =
  let open Data in
  let doc = "Input format" in
  let formats = List.map (fun fmt -> (to_string fmt, fmt)) all_input_formats in
  Arg.(value & opt (some (enum formats)) None & info [ "f"; "from" ] ~docv:"FORMAT" ~doc)

let to_format =
  let open Data in
  let doc = "Output format" in
  let formats = List.map (fun fmt -> (to_string fmt, fmt)) all_output_formats in
  Arg.(value & opt (some (enum formats)) None & info [ "t"; "to" ] ~docv:"FORMAT" ~doc)

let output_file =
  let doc = "Output file (defaults to stdout)" in
  Arg.(value & opt (some string) None & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let info_flag =
  let doc = "Show collection info (entity count)" in
  Arg.(value & flag & info [ "info" ] ~doc)

let list_tags =
  let doc = "List all tags" in
  Arg.(value & flag & info [ "list-tags" ] ~doc)

let mappings_file =
  let doc = "Read tag mappings from $(docv)" in
  Arg.(value & opt (some string) None & info [ "mappings" ] ~docv:"FILE" ~doc)

let file =
  let doc = "Input file to process" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let process_file_term =
  let ( <$> ) = Cmdliner.Term.map in
  let ( <*> ) = Cmdliner.Term.( $ ) in
  let args_term =
    Args.make
    <$> from_format
    <*> to_format
    <*> output_file
    <*> info_flag
    <*> list_tags
    <*> mappings_file
  in
  process_file <$> args_term <*> file

let cmd =
  let doc = "Process bookmark files in various formats" in
  let info = Cmd.info "hbt" ~version ~doc in
  Cmd.v info process_file_term

let () = exit (Cmd.eval_result cmd)
