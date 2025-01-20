open Cmdliner

module Args = struct
  type t = {
    dump_entities : bool;
    dump_tags : bool;
    mappings_file : string option;
  }
end

module type FILE_HANDLER = sig
  val run : string -> Args.t -> unit
end

module Markdown : FILE_HANDLER = struct
  type t = Hbt.Collection.t

  let read_file file =
    let ic = open_in file in
    let ret = In_channel.input_all ic in
    close_in ic;
    ret

  let update (args : Args.t) : Hbt.Collection.t -> Hbt.Collection.t =
    match args.mappings_file with
    | Some file -> read_file file |> Yojson.Basic.from_string |> Hbt.Collection.update_labels
    | None -> Fun.id

  let parse (file : string) : t = read_file file |> Hbt.Markdown.parse

  let print (file : string) (args : Args.t) (collection : t) : unit =
    let open Hbt.Collection in
    if args.dump_entities then
      yojson_of_t collection |> Yojson.Safe.pretty_print Format.std_formatter
    else if args.dump_tags then
      entities collection
      |> Array.fold_left (fun acc et -> Entity.labels et |> Label_set.union acc) Label_set.empty
      |> Label_set.iter (fun l -> Label.to_string l |> Printf.printf "%s\n")
    else
      let length = length collection in
      Printf.printf "%s: %d entities\n" file length

  let run (file : string) (args : Args.t) : unit = parse file |> update args |> print file args
end

module Pinboard_shared = struct
  type t = Hbt.Pinboard.t list

  let print (file : string) (args : Args.t) (posts : t) : unit =
    if args.dump_entities then
      List.iter (fun p -> Hbt.Pinboard.href p |> Printf.printf "%s\n") posts
    else if args.dump_tags then
      ()
    else
      let length = List.length posts in
      Printf.printf "%s: %d entities\n" file length
end

module Xml : FILE_HANDLER = struct
  include Pinboard_shared

  let parse : string -> t = Hbt.Pinboard.from_xml
  let run (file : string) (args : Args.t) : unit = parse file |> print file args
end

module Html : FILE_HANDLER = struct
  include Pinboard_shared

  let parse : string -> t = Hbt.Pinboard.from_html
  let run (file : string) (args : Args.t) : unit = parse file |> print file args
end

module Json : FILE_HANDLER = struct
  include Pinboard_shared

  let parse : string -> t = Hbt.Pinboard.from_json
  let run (file : string) (args : Args.t) : unit = parse file |> print file args
end

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

let process_file dump_entities dump_tags mappings_file file =
  let args = Args.{ dump_entities; dump_tags; mappings_file } in
  match Filename.extension file with
  | ".md" -> Markdown.run file args
  | ".xml" -> Xml.run file args
  | ".html" -> Html.run file args
  | ".json" -> Json.run file args
  | ext ->
      Printf.eprintf "Error: no handler for files with extension '%s'\n" ext;
      exit 1

let process_file_t = Term.(const process_file $ dump_entities $ dump_tags $ mappings_file $ file)

let cmd =
  let doc = "Process bookmark files in various formats" in
  let info = Cmd.info "hbt" ~doc in
  Cmd.v info process_file_t

let () = exit (Cmd.eval cmd)
