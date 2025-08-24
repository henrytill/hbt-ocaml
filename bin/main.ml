open Cmdliner
open Hbt

module Args = struct
  type t = {
    input_format : [ `Html | `Json | `Xml | `Markdown ] option;
    output_format : [ `Yaml | `Html ] option;
    output : string option;
    info : bool;
    list_tags : bool;
    mappings_file : string option;
  }

  let make input_format output_format output info list_tags mappings_file =
    { input_format; output_format; output; info; list_tags; mappings_file }
end

let read_file file =
  let ic = open_in file in
  let ret = In_channel.input_all ic in
  close_in ic;
  ret

let update_collection (args : Args.t) : Collection.t -> Collection.t =
  match args.mappings_file with
  | Some file -> read_file file |> Yaml.of_string |> Result.get_ok |> Collection.update_labels
  | None -> Fun.id

let write_output (content : string) (output_file : string option) : unit =
  match output_file with
  | Some file ->
      let oc = open_out file in
      output_string oc content;
      close_out oc
  | None -> print_string content

let print_collection (file : string) (args : Args.t) (collection : Collection.t) : unit =
  let open Collection in
  let output =
    if args.info then
      Printf.sprintf "%s: %d entities\n" file (length collection)
    else if args.list_tags then
      entities collection
      |> Array.fold_left (fun acc et -> Label_set.union acc (Entity.labels et)) Label_set.empty
      |> Label_set.elements
      |> List.map Label.to_string
      |> String.concat "\n"
    else
      match args.output_format with
      | Some `Yaml -> (
          let len = 1024 * 1024 in
          let yaml = yaml_of_t collection in
          match Yaml.to_string ~len yaml with
          | Ok s -> s
          | Error (`Msg e) -> failwith e)
      | Some `Html -> Collection.to_html collection
      | None -> failwith "Must specify an output format (-t) or analysis flag (--info, --list-tags)"
  in
  write_output output args.output

let run (parse : string -> 'a) (to_collection : 'a -> Collection.t) (file : string) (args : Args.t)
    : unit =
  parse file |> to_collection |> update_collection args |> print_collection file args

let collection_of_posts (posts : Pinboard.t list) : Collection.t =
  let ret = Collection.create () in
  List.iter (fun post -> ignore Collection.(insert ret (Entity.of_pinboard post))) posts;
  ret

let detect_input_format file =
  match Filename.extension file with
  | ".md" -> `Markdown
  | ".xml" -> `Xml
  | ".html" -> `Html
  | ".json" -> `Json
  | ext -> invalid_arg (Format.sprintf "No parser for extension: %s" ext)

let process_file (args : Args.t) (file : string) : unit =
  let run_md = run (Fun.compose Markdown.parse read_file) Fun.id in
  let run_xml = run Pinboard.from_xml collection_of_posts in
  let run_html = run Collection.from_html Fun.id in
  let run_json = run Pinboard.from_json collection_of_posts in
  let input_format =
    match args.input_format with
    | Some format -> format
    | None -> detect_input_format file
  in
  match input_format with
  | `Markdown -> run_md file args
  | `Xml -> run_xml file args
  | `Html -> run_html file args
  | `Json -> run_json file args

let from_format =
  let doc = "Input format" in
  let formats = [ ("html", `Html); ("json", `Json); ("xml", `Xml); ("markdown", `Markdown) ] in
  Arg.(value & opt (some (enum formats)) None & info [ "f"; "from" ] ~docv:"FORMAT" ~doc)

let to_format =
  let doc = "Output format" in
  let formats = [ ("yaml", `Yaml); ("html", `Html) ] in
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
  let info = Cmd.info "hbt" ~doc in
  Cmd.v info process_file_term

let () = exit (Cmd.eval cmd)
