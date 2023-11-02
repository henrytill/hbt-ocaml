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
[@@deriving eq, show { with_path = false }]

let to_string = show

module Tags = struct
  include Set.Make (String)

  let pp fmt t =
    let open Format in
    let pp_sep fmt () = fprintf fmt ";@ " in
    let pp_elem fmt elem = fprintf fmt "%S" elem in
    fprintf fmt "@[{%a}@]" (pp_print_list ~pp_sep pp_elem) (elements t)
end

let tags = List.fold_left (fun acc post -> Tags.of_list post.tag |> Tags.union acc) Tags.empty

let get_attr attrs k =
  try
    let k = (String.empty, k) in
    List.assoc k attrs
  with Not_found -> String.empty

let get_attr_option attrs k =
  let s = get_attr attrs k in
  Option.(if String.empty = s then None else Some s)

let _from_xml file =
  let ic = In_channel.open_text file in
  let xml = Xmlm.make_input (`Channel ic) in
  let to_t attrs =
    let href = get_attr attrs "href" in
    let time = get_attr attrs "time" in
    let description = get_attr_option attrs "description" in
    let extended = get_attr_option attrs "extended" in
    let tag = Str.split (Str.regexp "[ \t]+") (get_attr attrs "tag") in
    let hash = get_attr attrs "hash" in
    let shared = get_attr attrs "shared" = "yes" in
    let toread = get_attr attrs "toread" = "yes" in
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

let from_xml file =
  let ic = In_channel.open_text file in
  let channel = Markup.channel ic in
  let html = Markup.parse_html channel in
  let signals = Markup.signals html in
  let to_t attrs =
    let href = get_attr attrs "href" in
    let time = get_attr attrs "time" in
    let description = get_attr_option attrs "description" in
    let extended = get_attr_option attrs "extended" in
    let tag = Str.split (Str.regexp "[ \t]+") (get_attr attrs "tag") in
    let hash = get_attr attrs "hash" in
    let shared = get_attr attrs "shared" = "yes" in
    let toread = get_attr attrs "toread" = "yes" in
    { href; time; description; extended; tag; hash; shared; toread }
  in
  let rec go depth acc =
    match Markup.next signals with
    | Some (`Start_element ((_, "post"), attrs)) ->
        let parsed = to_t attrs in
        go (depth + 1) (parsed :: acc)
    | Some (`Start_element ((_, "posts"), _)) -> go (depth + 1) acc
    | Some (`Start_element ((_, s), _)) -> failwith ("unexpected Start_element: " ^ s)
    | Some `End_element when depth = 1 -> acc
    | Some `End_element -> go (depth - 1) acc
    | Some _ -> go depth acc
    | None -> failwith "foo"
  in
  let ret = go 0 [] in
  In_channel.close ic;
  ret

let maybe_start_dd stream ~on_failure on_success =
  match Markup.peek stream with
  | Some (`Start_element ((_, "dd"), _)) ->
      ignore (Markup.next stream);
      on_success ()
  | _ -> on_failure ()

let maybe_text stream ~on_failure on_success =
  match Markup.peek stream with
  | Some (`Text xs) ->
      ignore (Markup.next stream);
      on_success xs
  | _ -> on_failure ()

let next_start_a stream ?(on_failure = fun () -> failwith "parse error") on_success =
  match Markup.next stream with
  | Some (`Start_element ((_, "a"), attrs)) -> on_success attrs
  | _ -> on_failure ()

let next_text stream ?(on_failure = fun () -> failwith "parse error") on_success =
  match Markup.next stream with
  | Some (`Text xs) -> on_success xs
  | _ -> on_failure ()

let next_end stream ?(on_failure = fun () -> failwith "parse error") on_success =
  match Markup.next stream with
  | Some `End_element -> on_success ()
  | _ -> on_failure ()

let is_newline xs =
  let s = String.concat "" xs in
  Str.string_match (Str.regexp "^[\n\r]*$") s 0

let skip_newlines stream () =
  match Markup.peek stream with
  | Some (`Text xs) when is_newline xs -> ignore (Markup.next stream)
  | _ -> ()

let from_html file =
  let ic = In_channel.open_text file in
  let channel = Markup.channel ic in
  let html = Markup.parse_html channel in
  let signals = Markup.signals html in
  let tag s = (("", s), []) in
  let skip t = if Markup.peek signals = Some t then ignore (Markup.next signals) else () in
  let parse_dd () =
    maybe_start_dd signals ~on_failure:(fun () -> None) @@ fun () ->
    next_text signals @@ fun xs ->
    next_end signals @@ fun () -> Some (String.concat String.empty xs)
  in
  let to_t () =
    skip (`Start_element (tag "p"));
    let attrs = next_start_a signals (fun x -> x) in
    let description =
      maybe_text signals
        ~on_failure:(fun () -> None)
        (fun xs -> Some (String.concat String.empty xs))
    in
    skip `End_element;
    skip_newlines signals ();
    skip `End_element;
    let href = get_attr attrs "href" in
    let time = get_attr attrs "add_date" in
    let tag = Str.split (Str.regexp "[,]+") (get_attr attrs "tags") in
    let hash = get_attr attrs "hash" in
    let shared = get_attr attrs "private" = "0" in
    let toread = get_attr attrs "toread" = "1" in
    let extended = parse_dd () in
    { href; time; description; extended; tag; hash; shared; toread }
  in
  let rec go depth acc =
    match Markup.next signals with
    | Some (`Start_element ((_, "dt"), _)) ->
        let parsed = to_t () in
        go depth (parsed :: acc)
    | Some (`Start_element ((_, _), _)) -> go (depth + 1) acc
    | Some `End_element when depth = 1 -> acc
    | Some `End_element -> go (depth - 1) acc
    | Some _ -> go depth acc
    | None -> failwith "quux"
  in
  let ret = go 0 [] in
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
