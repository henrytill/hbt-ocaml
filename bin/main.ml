module type FILE_HANDLER = sig
  type t

  val parse : string -> t
  val print : string -> bool -> t -> unit
end

module Markdown : FILE_HANDLER = struct
  type t = Hbt.Collection.t

  let read_file file =
    let ic = open_in file in
    let ret = In_channel.input_all ic in
    close_in ic;
    ret

  let parse file = read_file file |> Hbt.Markdown.parse

  let print file dump_entities collection =
    let open Hbt in
    if dump_entities then
      let entities = Collection.entities collection in
      Array.iter
        (fun e -> Collection.Entity.uri e |> Uri.to_string |> Printf.printf "%s\n")
        entities
    else
      let length = Collection.length collection in
      Printf.printf "%s: %d entities\n" file length
end

module Pinboard_shared = struct
  type t = Hbt.Pinboard.t list

  let print file dump_entities posts =
    if dump_entities then
      List.iter (fun p -> Hbt.Pinboard.href p |> Printf.printf "%s\n") posts
    else
      let length = List.length posts in
      Printf.printf "%s: %d entities\n" file length
end

module Xml : FILE_HANDLER = struct
  include Pinboard_shared

  let parse = Hbt.Pinboard.from_xml
end

module Html : FILE_HANDLER = struct
  include Pinboard_shared

  let parse = Hbt.Pinboard.from_html
end

module Json : FILE_HANDLER = struct
  include Pinboard_shared

  let parse = Hbt.Pinboard.from_json
end

module Option_syntax = struct
  let ( let* ) = Option.bind
  let[@warning "-32"] return a = Some a
end

let suffixes = [ (".md", `Markdown); (".xml", `Xml); (".html", `Html); (".json", `Json) ]

let suffix_handlers : (_ * (module FILE_HANDLER)) list =
  [
    (`Markdown, (module Markdown));
    (`Xml, (module Xml));
    (`Html, (module Html));
    (`Json, (module Json));
  ]

let select_file_handler (file : string) : (module FILE_HANDLER) option =
  let open Option_syntax in
  let extension = Filename.extension file in
  let* suffix = List.assoc_opt extension suffixes in
  List.assoc_opt suffix suffix_handlers

let () =
  let dump_entities = ref false in
  let file = ref None in
  let usage_string = "Usage: " ^ Sys.argv.(0) ^ " [OPTIONS...] <FILE>" in
  let opt_list = [ ("-dump", Arg.Set dump_entities, "dump entities") ] in
  let process_arg arg = file := Some arg in
  Arg.parse opt_list process_arg usage_string;
  let file =
    match !file with
    | Some file -> file
    | None ->
        Printf.eprintf "Error: missing file argument\n";
        Arg.usage opt_list usage_string;
        exit 1
  in
  match select_file_handler file with
  | Some (module M) ->
      M.parse file |> M.print file !dump_entities;
      exit 0
  | _ ->
      Printf.eprintf "No handlers for this file type\n";
      exit 1
