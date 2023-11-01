type t = {
  href : string;
  time : string;
  description : string option;
  extended : string option;
  tag : string list;
  hash : string;
  shared : bool;
  toread : bool;
}

let pp fmt p =
  let open Format in
  pp_print_string fmt "{ ";
  fprintf fmt "href = %S" p.href;
  fprintf fmt "; time = %S" p.time;
  pp_print_option
    ~none:(fun fmt _ -> pp_print_string fmt "; description = None")
    (fun fmt s -> fprintf fmt "; description = Some %S" s)
    fmt p.description;
  pp_print_option
    ~none:(fun fmt _ -> pp_print_string fmt "; extended = None")
    (fun fmt s -> fprintf fmt "; extended = Some %S" s)
    fmt p.extended;
  pp_print_string fmt "; tag = [";
  pp_print_list
    ~pp_sep:(fun fmt _ -> pp_print_string fmt "; ")
    (fun fmt s -> fprintf fmt "%S" s)
    fmt p.tag;
  pp_print_char fmt ']';
  fprintf fmt "; hash = %S" p.hash;
  pp_print_string fmt "; shared = ";
  pp_print_bool fmt p.shared;
  pp_print_string fmt "; toread = ";
  pp_print_bool fmt p.toread;
  pp_print_string fmt "; }"

let to_string p = Format.asprintf "%a" pp p

let from_xml file =
  let ic = In_channel.open_text file in
  let xml = Xmlm.make_input (`Channel ic) in
  let to_t attrs =
    let get_attr k =
      try
        let k = (String.empty, k) in
        List.assoc k attrs
      with Not_found -> String.empty
    in
    let get_attr_option k =
      let s = get_attr k in
      Option.(if String.empty = s then None else Some s)
    in
    let href = get_attr "href" in
    let time = get_attr "time" in
    let description = get_attr_option "description" in
    let extended = get_attr_option "extended" in
    let tag = Str.split (Str.regexp "[ \t]+") (get_attr "tag") in
    let hash = get_attr "hash" in
    let shared = get_attr "shared" = "yes" in
    let toread = get_attr "toread" = "yes" in
    { href; time; description; extended; tag; hash; shared; toread }
  in
  let rec go depth acc =
    match Xmlm.input xml with
    | `El_start ((_, "post"), attrs) ->
        let parsed = to_t attrs in
        go (depth + 1) (parsed :: acc)
    | `El_start ((_, "posts"), _) -> go (depth + 1) acc
    | `El_start ((_, s), _) -> failwith ("unexpected El_start: " ^ s)
    | `El_end when depth = 1 -> acc
    | `El_end -> go (depth - 1) acc
    | `Data _ -> go depth acc
    | `Dtd _ -> go depth acc
  in
  let ret = go 0 [] in
  if not (Xmlm.eoi xml) then invalid_arg "document not well-formed";
  In_channel.close ic;
  ret

let from_json file =
  let json = Yojson.Basic.from_file file in
  let open Yojson.Basic.Util in
  let to_t j =
    let href = j |> member "href" |> to_string in
    let time = j |> member "time" |> to_string in
    let description = j |> member "description" |> to_string_option in
    let extended = j |> member "extended" |> to_string_option in
    let tags = j |> member "tags" |> to_string in
    let tag = Str.split (Str.regexp "[ \t]+") tags in
    let hash = j |> member "hash" |> to_string in
    let shared = j |> member "shared" |> to_string = "yes" in
    let toread = j |> member "toread" |> to_string = "yes" in
    { href; time; description; extended; tag; hash; shared; toread }
  in
  convert_each to_t json
