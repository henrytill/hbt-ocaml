type post = {
  href : string;
  time : string;
  description : string Option.t;
  extended : string Option.t;
  tag : string list;
  hash : string;
  shared : bool;
}

let parse_post attrs =
  let get_attr k =
    try
      let k = (String.empty, k) in
      List.assoc k attrs
    with Not_found -> String.empty
  in
  let get_attr_maybe k =
    let s = get_attr k in
    Option.(if String.empty = s then None else Some s)
  in
  {
    href = get_attr "href";
    time = get_attr "time";
    description = get_attr_maybe "description";
    extended = get_attr_maybe "extended";
    tag = Str.split (Str.regexp "[ \t]+") (get_attr "tag");
    hash = get_attr "hash";
    shared = get_attr "shared" = "yes";
  }

let parse_xml file =
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
