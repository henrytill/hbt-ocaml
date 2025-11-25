type _ t =
  | Json : [ `Input ] t
  | Xml : [ `Input ] t
  | Markdown : [ `Input ] t
  | Html : [< `Input | `Output ] t
  | Yaml : [< `Input | `Output ] t

type input = [ `Input ] t
type output = [ `Output ] t

let all_input_formats : input list = [ Json; Xml; Markdown; Html; Yaml ]
let all_output_formats : output list = [ Html; Yaml ]

let to_string : type a. a t -> string = function
  | Json -> "json"
  | Xml -> "xml"
  | Markdown -> "markdown"
  | Html -> "html"
  | Yaml -> "yaml"

let pp : type a. a t Fmt.t = fun fmt format -> Fmt.string fmt (to_string format)

let detect_input_format (filename : string) : input option =
  match Filename.extension filename with
  | ".json" -> Some Json
  | ".xml" -> Some Xml
  | ".md" -> Some Markdown
  | ".html" -> Some Html
  | ".yaml" -> Some Yaml
  | _ -> None

let detect_output_format (filename : string) : output option =
  match Filename.extension filename with
  | ".html" -> Some Html
  | ".yaml" -> Some Yaml
  | _ -> None

module type PARSER = sig
  val parse : string -> Collection.t
end

module type FORMATTER = sig
  val format : Collection.t -> string
end

module Json_parser = struct
  let parse input =
    let ps = Pinboard.Post.from_json input in
    Collection.of_posts ps
end

module Xml_parser = struct
  let parse input =
    let ps = Pinboard.Post.from_xml input in
    Collection.of_posts ps
end

module Yaml_parser = struct
  let parse input =
    let yaml = Yaml.of_string_exn input in
    Collection.t_of_yaml yaml
end

let parse (format : input) : string -> Collection.t =
  let (module Parser : PARSER) =
    match format with
    | Json -> (module Json_parser)
    | Xml -> (module Xml_parser)
    | Markdown -> (module Markdown)
    | Html -> (module Html)
    | Yaml -> (module Yaml_parser)
  in
  Parser.parse

module Yaml_formatter = struct
  exception Yaml_conversion_error of string

  let format coll =
    let len = 1024 * 1024 in
    let yaml = Collection.yaml_of_t coll in
    match Yaml.to_string ~len yaml with
    | Ok s -> s
    | Error (`Msg e) -> raise (Yaml_conversion_error e)
end

let format (format : output) : Collection.t -> string =
  let (module Formatter : FORMATTER) =
    match format with
    | Html -> (module Html)
    | Yaml -> (module Yaml_formatter)
  in
  Formatter.format
