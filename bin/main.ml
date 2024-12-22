module Args = struct
  type t = {
    mutable dump_entities : bool;
    mutable dump_tags : bool;
    mutable mappings_file : string option; [@warning "-69"]
  }

  let make () = { dump_entities = false; dump_tags = false; mappings_file = None }
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

  let json_to_map (json : Yojson.Basic.t) : Hbt.Collection.Label.t Hbt.Collection.Label_map.t =
    match json with
    | `Assoc fields ->
        let f acc (key, value) =
          match value with
          | `String s ->
              let k = Hbt.Collection.Label.of_string key in
              let v = Hbt.Collection.Label.of_string s in
              Hbt.Collection.Label_map.add k v acc
          | _ -> invalid_arg "All values must be strings"
        in
        List.fold_left f Hbt.Collection.Label_map.empty fields
    | _ -> invalid_arg "Expected a JSON object"

  let update_labels (args : Args.t) : Hbt.Collection.t -> Hbt.Collection.t =
    match args.mappings_file with
    | Some file ->
        let json_str = read_file file in
        let json = Yojson.Basic.from_string json_str in
        let mapping = json_to_map json in
        let open Hbt.Collection in
        map_labels
          (Label_set.map (fun label ->
               Label_map.find_opt label mapping |> Option.value ~default:label))
    | None -> Fun.id

  let parse (file : string) : t = read_file file |> Hbt.Markdown.parse

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

  let run (file : string) (args : Args.t) : unit =
    parse file |> update_labels args |> print file args
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

let () =
  let args = Args.make () in
  let set_dump_entities () = args.dump_entities <- true in
  let set_dump_tags () = args.dump_tags <- true in
  let set_mappings_file mappings_file = args.mappings_file <- Some mappings_file in
  let opt_list =
    [
      ("-dump", Arg.Unit set_dump_entities, "dump entities");
      ("-tags", Arg.Unit set_dump_tags, "dump tags");
      ("-mappings", Arg.String set_mappings_file, "<file> Read tag mappings from <file>");
    ]
  in
  let file = ref None in
  let process_arg arg = file := Some arg in
  let usage_string = "Usage: " ^ Sys.argv.(0) ^ " <options> <file>" in
  Arg.parse opt_list process_arg usage_string;
  let file =
    match !file with
    | Some file -> file
    | None ->
        Printf.eprintf "Error: missing file argument\n";
        Arg.usage opt_list usage_string;
        exit 1
  in
  match Filename.extension file with
  | ".md" ->
      Markdown.run file args;
      exit 0
  | ".xml" ->
      Xml.run file args;
      exit 0
  | ".html" ->
      Html.run file args;
      exit 0
  | ".json" ->
      Json.run file args;
      exit 0
  | _ ->
      Printf.eprintf "No handler for this file type\n";
      exit 1
