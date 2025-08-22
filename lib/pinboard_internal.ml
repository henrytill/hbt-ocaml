type t = {
  href : string;
  time : string;
  description : string option;
  extended : string option;
  tag : string list;
  hash : string option;
  shared : bool;
  toread : bool;
}

let make ~href ~time ?(description = None) ?(extended = None) ?(tag = []) ?(hash = None)
    ?(shared = false) ?(toread = false) () =
  { href; time; description; extended; tag; hash; shared; toread }

let href p = p.href
let time p = p.time
let description p = p.description
let extended p = p.extended
let tag p = p.tag
let hash p = p.hash
let shared p = p.shared
let toread p = p.toread

let equal x y =
  String.equal x.href y.href
  && String.equal x.time y.time
  && Option.equal String.equal x.description y.description
  && Option.equal String.equal x.extended y.extended
  && List.equal String.equal x.tag y.tag
  && Option.equal String.equal x.hash y.hash
  && Bool.equal x.shared y.shared
  && Bool.equal x.toread y.toread

let pp fmt p =
  let open Format in
  let none fmt () = fprintf fmt "None" in
  let some fmt elem = fprintf fmt "%S" elem in
  let pp_string_option = pp_print_option ~none some in
  let pp_sep fmt () = fprintf fmt ";@;<1 2>" in
  let pp_elem fmt elem = fprintf fmt "%S" elem in
  let pp_tag = pp_print_list ~pp_sep pp_elem in
  fprintf fmt "@[<hv>{";
  fprintf fmt "@;<1 2>@[href =@ %S@];" p.href;
  fprintf fmt "@;<1 2>@[time =@ %S@];" p.time;
  fprintf fmt "@;<1 2>@[description =@ %a@];" pp_string_option p.description;
  fprintf fmt "@;<1 2>@[extended =@ %a@];" pp_string_option p.extended;
  fprintf fmt "@;<1 2>@[tag =@ @[<hv>[@;<0 2>%a@;<0 0>]@]@];" pp_tag p.tag;
  fprintf fmt "@;<1 2>@[hash =@ %a@];" pp_string_option p.hash;
  fprintf fmt "@;<1 2>@[shared =@ %B@];" p.shared;
  fprintf fmt "@;<1 2>@[toread =@ %B@];" p.toread;
  fprintf fmt "@;<1 0>}@]"

let to_string = Format.asprintf "%a" pp

module Tags = struct
  include Set.Make (String)

  let pp fmt tags =
    let open Format in
    let pp_sep fmt () = fprintf fmt ";@ " in
    let pp_elem fmt elem = fprintf fmt "%S" elem in
    fprintf fmt "@[<2>{@ %a@ }@]" (pp_print_list ~pp_sep pp_elem) (elements tags)

  let to_string = Format.asprintf "%a" pp
end

let tags = List.fold_left (fun acc post -> Tags.(union acc (of_list post.tag))) Tags.empty

module Attrs = Prelude.Markup_ext.Attrs

let t_of_attrs (attrs : Attrs.t) : t =
  let href = Attrs.get "href" attrs in
  let time = Attrs.get "time" attrs in
  let description = Attrs.get_opt "description" attrs in
  let extended = Attrs.get_opt "extended" attrs in
  let tag = Str.split (Str.regexp "[ \t]+") (Attrs.get "tag" attrs) in
  let hash = Attrs.get_opt "hash" attrs in
  let shared = Attrs.get "shared" attrs = "yes" in
  let toread = Attrs.get "toread" attrs = "yes" in
  { href; time; description; extended; tag; hash; shared; toread }

let from_xml file =
  let ic = open_in file in
  let channel = Markup.channel ic in
  let xml = Markup.parse_xml channel in
  let signals = Markup.signals xml in
  let continue = ref true in
  let acc = ref [] in

  while !continue do
    match Markup.next signals with
    | Some (`Start_element ((_, "post"), attrs)) -> acc := t_of_attrs attrs :: !acc
    | Some (`Start_element ((_, "posts"), _)) -> ()
    | Some (`Start_element ((_, s), _)) -> failwith ("unexpected Start_element: " ^ s)
    | Some _ -> ()
    | None -> continue := false
  done;

  close_in ic;
  !acc

let get_field fields key = List.assoc key fields
let get_optional_field fields key = try Some (List.assoc key fields) with Not_found -> None

let get_string = function
  | `String s -> s
  | _ -> invalid_arg "expected string"

let get_optional_string = function
  | `String s -> Some s
  | `Null -> None
  | _ -> invalid_arg "expected string or null"

let t_of_yaml = function
  | `A items ->
      List.map
        (function
          | `O fields ->
              let href = get_field fields "href" |> get_string in
              let time = get_field fields "time" |> get_string in
              let description = get_optional_field fields "description" |> Option.map get_string in
              let extended = get_optional_field fields "extended" |> Option.map get_string in
              let tags = get_field fields "tags" |> get_string in
              let tag = Str.split (Str.regexp "[ \t]+") tags in
              let hash = get_optional_field fields "hash" |> Option.map get_string in
              let shared = get_field fields "shared" |> get_string = "yes" in
              let toread = get_field fields "toread" |> get_string = "yes" in
              { href; time; description; extended; tag; hash; shared; toread }
          | _ -> invalid_arg "expected object in array")
        items
  | _ -> invalid_arg "expected array"

let from_json file =
  let yaml =
    Yaml.of_string (In_channel.with_open_text file In_channel.input_all) |> Result.get_ok
  in
  t_of_yaml yaml
