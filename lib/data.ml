(** Data format type system with phantom types for compile-time safety.

    This module provides type-safe format handling using GADTs with direct polymorphic variant
    annotations, preventing format direction misuse at compile time. *)

type _ t =
  | Json : [ `Input ] t
  | Xml : [ `Input ] t
  | Markdown : [ `Input ] t
  | Html : [< `Input | `Output ] t
  | Yaml : [ `Output ] t

type input = [ `Input ] t
type output = [ `Output ] t

let to_string : type a. a t -> string = function
  | Json -> "json"
  | Xml -> "xml"
  | Markdown -> "markdown"
  | Html -> "html"
  | Yaml -> "yaml"

let pp : type a. Format.formatter -> a t -> unit =
  fun fmt format -> Format.pp_print_string fmt (to_string format)

let all_input_formats : input list = [ Json; Xml; Markdown; Html ]
let all_output_formats : output list = [ Html; Yaml ]

let detect_input_format (filename : string) : input option =
  match Filename.extension filename with
  | ".md" -> Some Markdown
  | ".xml" -> Some Xml
  | ".html" -> Some Html
  | ".json" -> Some Json
  | _ -> None

let parse (format : input) (content : string) : Collection.t =
  let collection_of_posts (posts : Pinboard.t list) : Collection.t =
    let ret = Collection.create () in
    let sorted = List.sort (fun a b -> String.compare (Pinboard.time a) (Pinboard.time b)) posts in
    List.iter (fun post -> ignore Collection.(insert ret (Entity.of_pinboard post))) sorted;
    ret
  in
  match format with
  | Json -> Pinboard.from_json content |> collection_of_posts
  | Xml -> Pinboard.from_xml content |> collection_of_posts
  | Markdown -> Markdown.parse content
  | Html -> Collection.from_html content

let format (format : output) : Collection.t -> string =
  let to_yaml collection =
    let len = 1024 * 1024 in
    let yaml = Collection.yaml_of_t collection in
    match Yaml.to_string ~len yaml with
    | Ok s -> s
    | Error (`Msg e) -> failwith e
  in
  match format with
  | Html -> Collection.to_html
  | Yaml -> to_yaml
