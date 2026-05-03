type flow =
  [ `Input
  | `Output
  ]

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

let parse : input -> string -> Collection.t = function
  | Json -> fun s -> Collection.of_posts (Pinboard.Post.from_json s)
  | Xml -> fun s -> Collection.of_posts (Pinboard.Post.from_xml s)
  | Markdown -> Markdown.parse
  | Html -> Html.parse
  | Yaml -> fun s -> Collection.t_of_yaml (Yaml.of_string_exn s)

exception Yaml_conversion_error of string

let format : output -> Collection.t -> string = function
  | Html -> Html.format
  | Yaml -> begin
      fun coll ->
        match Yaml.to_string ~len:(1024 * 1024) (Collection.yaml_of_t coll) with
        | Ok s -> s
        | Error (`Msg e) -> raise (Yaml_conversion_error e)
    end
