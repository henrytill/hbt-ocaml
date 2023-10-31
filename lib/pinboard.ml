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
  pp_print_char fmt '{';
  fprintf fmt "href = %S" p.href;
  fprintf fmt ", time = %S" p.time;
  pp_print_option (fun fmt s -> fprintf fmt ", description = %S" s) fmt p.description;
  pp_print_option (fun fmt s -> fprintf fmt ", extended = %S" s) fmt p.extended;
  pp_print_string fmt ", tag = [";
  pp_print_list
    ~pp_sep:(fun fmt _ -> pp_print_string fmt "; ")
    (fun fmt s -> fprintf fmt "%S" s)
    fmt p.tag;
  pp_print_string fmt "]";
  fprintf fmt ", hash = %S" p.hash;
  pp_print_string fmt ", shared = ";
  pp_print_bool fmt p.shared;
  pp_print_string fmt ", toread = ";
  pp_print_bool fmt p.toread;
  pp_print_char fmt '}'

let to_string p = Format.asprintf "%a" pp p

let parse_post attrs =
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
  {
    href = get_attr "href";
    time = get_attr "time";
    description = get_attr_option "description";
    extended = get_attr_option "extended";
    tag = Str.split (Str.regexp "[ \t]+") (get_attr "tag");
    hash = get_attr "hash";
    shared = get_attr "shared" = "yes";
    toread = get_attr "toread" = "yes";
  }

let from_xml file =
  let ic = In_channel.open_text file in
  let xml = Xmlm.make_input (`Channel ic) in
  let rec go depth posts =
    match Xmlm.input xml with
    | `El_start ((_, "post"), attrs) ->
        let post = parse_post attrs in
        go (depth + 1) (post :: posts)
    | `El_start ((_, "posts"), _) -> go (depth + 1) posts
    | `El_start ((_, s), _) -> failwith ("unexpected El_start: " ^ s)
    | `El_end when depth = 1 -> posts
    | `El_end -> go (depth - 1) posts
    | `Data _ -> go depth posts
    | `Dtd _ -> go depth posts
  in
  let posts = go 0 [] in
  if not (Xmlm.eoi xml) then invalid_arg "document not well-formed";
  close_in ic;
  posts
