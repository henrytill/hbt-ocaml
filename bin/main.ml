open Cmdliner
open Hbt

module Args = struct
  type t = {
    dump_entities : bool;
    dump_tags : bool;
    mappings_file : string option;
  }
end

let read_file file =
  let ic = open_in file in
  let ret = In_channel.input_all ic in
  close_in ic;
  ret

let update_collection (args : Args.t) : Collection.t -> Collection.t =
  match args.mappings_file with
  | Some file -> read_file file |> Yojson.Basic.from_string |> Collection.update_labels
  | None -> Fun.id

let print_collection (file : string) (args : Args.t) (collection : Collection.t) : unit =
  let open Collection in
  if args.dump_entities then
    yojson_of_t collection |> Yojson.Safe.pretty_print Format.std_formatter
  else if args.dump_tags then
    entities collection
    |> Array.fold_left (fun acc et -> Entity.labels et |> Label_set.union acc) Label_set.empty
    |> Label_set.iter (fun l -> Label.to_string l |> Printf.printf "%s\n")
  else
    let length = length collection in
    Printf.printf "%s: %d entities\n" file length

let run (parse : string -> 'a) (to_collection : 'a -> Collection.t) (file : string) (args : Args.t)
    : unit =
  parse file |> to_collection |> update_collection args |> print_collection file args

let collection_of_posts (posts : Pinboard.t list) : Collection.t =
  let ret = Collection.make (List.length posts) in
  List.iter (fun post -> ignore Collection.(insert ret (Entity.of_pinboard post))) posts;
  ret

let process_file dump_entities dump_tags mappings_file file =
  let run_markdown = run (Fun.compose Markdown.parse read_file) Fun.id in
  let run_xml = run Pinboard.from_xml collection_of_posts in
  let run_html = run Pinboard.from_html collection_of_posts in
  let run_json = run Pinboard.from_json collection_of_posts in
  let args = Args.{ dump_entities; dump_tags; mappings_file } in
  match Filename.extension file with
  | ".md" -> run_markdown file args
  | ".xml" -> run_xml file args
  | ".html" -> run_html file args
  | ".json" -> run_json file args
  | ext -> invalid_arg (Format.sprintf "no handler for files with extension '%s'\n" ext)

let dump_entities =
  let doc = "Dump entities" in
  Arg.(value & flag & info [ "dump" ] ~doc)

let dump_tags =
  let doc = "Dump tags" in
  Arg.(value & flag & info [ "tags" ] ~doc)

let mappings_file =
  let doc = "Read tag mappings from $(docv)" in
  Arg.(value & opt (some string) None & info [ "mappings" ] ~docv:"FILE" ~doc)

let file =
  let doc = "Input file to process" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let process_file_term = Term.(const process_file $ dump_entities $ dump_tags $ mappings_file $ file)

let cmd =
  let doc = "Process bookmark files in various formats" in
  let info = Cmd.info "hbt" ~doc in
  Cmd.v info process_file_term

let () = exit (Cmd.eval cmd)
