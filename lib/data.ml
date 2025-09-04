exception Unsupported_file_format of string
exception Yaml_conversion_error of string

type _ t =
  | Json : [ `Input ] t
  | Xml : [ `Input ] t
  | Markdown : [ `Input ] t
  | Html : [< `Input | `Output ] t
  | Yaml : [ `Output ] t

type input = [ `Input ] t
type output = [ `Output ] t

let all_input_formats : input list = [ Json; Xml; Markdown; Html ]
let all_output_formats : output list = [ Html; Yaml ]

let to_string : type a. a t -> string = function
  | Json -> "json"
  | Xml -> "xml"
  | Markdown -> "markdown"
  | Html -> "html"
  | Yaml -> "yaml"

let pp : type a. Format.formatter -> a t -> unit =
  fun fmt format -> Format.pp_print_string fmt (to_string format)

let detect_input_format (filename : string) : input option =
  match Filename.extension filename with
  | ".md" -> Some Markdown
  | ".xml" -> Some Xml
  | ".html" -> Some Html
  | ".json" -> Some Json
  | _ -> None

let parse (format : input) (content : string) : Collection.t =
  match format with
  | Json -> Collection.of_posts (Pinboard.from_json content)
  | Xml -> Collection.of_posts (Pinboard.from_xml content)
  | Markdown -> Markdown.parse content
  | Html -> Html.collection_of_string content

let format (format : output) : Collection.t -> string =
  let to_yaml collection =
    let len = 1024 * 1024 in
    let yaml = Collection.yaml_of_t collection in
    match Yaml.to_string ~len yaml with
    | Ok s -> s
    | Error (`Msg e) -> raise (Yaml_conversion_error e)
  in
  match format with
  | Html -> Html.collection_to_string
  | Yaml -> to_yaml
