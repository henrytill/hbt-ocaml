open Cmdliner

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

let update_collection (args : Args.t) : Hbt.Collection.t -> Hbt.Collection.t =
  match args.mappings_file with
  | Some file -> read_file file |> Yojson.Basic.from_string |> Hbt.Collection.update_labels
  | None -> Fun.id

let print_collection (file : string) (args : Args.t) (collection : Hbt.Collection.t) : unit =
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

module Make_runner (Collector : sig
  type t

  val parse : string -> t
  val to_collection : t -> Hbt.Collection.t
end) =
struct
  let run (file : string) (args : Args.t) : unit =
    Collector.parse file
    |> Collector.to_collection
    |> update_collection args
    |> print_collection file args
end

module Markdown = Make_runner (struct
  type t = Hbt.Collection.t

  let parse (file : string) : t = read_file file |> Hbt.Markdown.parse
  let to_collection = Fun.id
end)

let collection_of_posts (posts : Hbt.Pinboard.t list) : Hbt.Collection.t =
  let ret = Hbt.Collection.make (List.length posts) in
  List.iter (fun post -> ignore Hbt.Collection.(insert ret (Entity.of_pinboard post))) posts;
  ret

module Xml = Make_runner (struct
  type t = Hbt.Pinboard.t list

  let parse : string -> t = Hbt.Pinboard.from_xml
  let to_collection = collection_of_posts
end)

module Html = Make_runner (struct
  type t = Hbt.Pinboard.t list

  let parse : string -> t = Hbt.Pinboard.from_html
  let to_collection = collection_of_posts
end)

module Json = Make_runner (struct
  type t = Hbt.Pinboard.t list

  let parse : string -> t = Hbt.Pinboard.from_json
  let to_collection = collection_of_posts
end)

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
