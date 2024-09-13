module Args = struct
  type t = {
    mutable dump_entities : bool;
    mutable dump_tags : bool;
  }

  let make () = { dump_entities = false; dump_tags = false }
end

module type FILE_HANDLER = sig
  type t

  val parse : string -> t
  val print : string -> Args.t -> t -> unit
end

module Markdown : FILE_HANDLER = struct
  type t = Hbt.Collection.t

  let read_file file =
    let ic = open_in file in
    let ret = In_channel.input_all ic in
    close_in ic;
    ret

  let parse file = read_file file |> Hbt.Markdown.parse

  let print (file : string) (args : Args.t) (collection : t) : unit =
    let open Hbt.Collection in
    if args.dump_entities then
      Array.iter
        (fun et -> Entity.uri et |> Uri.to_string |> Printf.printf "%s\n")
        (entities collection)
    else if args.dump_tags then
      entities collection
      |> Array.fold_left (fun acc et -> Entity.labels et |> Label_set.union acc) Label_set.empty
      |> Label_set.iter (fun l -> Label.to_string l |> Printf.printf "%s\n")
    else
      let length = length collection in
      Printf.printf "%s: %d entities\n" file length
end

module Pinboard_shared = struct
  type t = Hbt.Pinboard.t list

  let print (file : string) (args : Args.t) (posts : t) : unit =
    if Args.(args.dump_entities) then
      List.iter (fun p -> Hbt.Pinboard.href p |> Printf.printf "%s\n") posts
    else if Args.(args.dump_tags) then
      ()
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
  let args = Args.make () in
  let file = ref None in
  let usage_string = "Usage: " ^ Sys.argv.(0) ^ " [OPTIONS...] <FILE>" in
  let opt_list =
    [
      ("-dump", Arg.Unit (fun () -> args.dump_entities <- true), "dump entities");
      ("-tags", Arg.Unit (fun () -> args.dump_tags <- true), "dump tags");
    ]
  in
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
      M.parse file |> M.print file args;
      exit 0
  | _ ->
      Printf.eprintf "No handlers for this file type\n";
      exit 1
