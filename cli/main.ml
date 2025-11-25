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

let update (args : Args.t) : Collection.t -> Collection.t =
  match args.mappings_file with
  | None -> Fun.id
  | Some file -> read_file file |> Yaml.of_string_exn |> Collection.update_labels

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

let process_file (args : Args.t) (file : string) : unit =
  let input_format =
    match args.input_format with
    | None -> detect_input_format file
    | Some format -> format
  in
  let content = read_file file in
  let updated_args = { args with input_format = Some input_format } in
  Data.parse input_format content |> update updated_args |> print file updated_args

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

let () = exit (Cmd.eval cmd)
