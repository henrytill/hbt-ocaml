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

let make ~href ~time ~description ~extended ~tag ~hash ~shared ~toread =
  { href; time; description; extended; tag; hash; shared; toread }

let equal a b =
  String.equal a.href b.href
  && String.equal a.time b.time
  && Option.equal String.equal a.description b.description
  && Option.equal String.equal a.extended b.extended
  && List.equal String.equal a.tag b.tag
  && Option.equal String.equal a.hash b.hash
  && Bool.equal a.shared b.shared
  && Bool.equal a.toread b.toread

let pp fmt a =
  let open Format in
  fprintf fmt "@[<2>{@ ";
  fprintf fmt "@[href =@ %S@];@ " a.href;
  fprintf fmt "@[time =@ %S@];@ " a.time;
  let none fmt () = fprintf fmt "None" in
  let some fmt elem = fprintf fmt "(Some %S)" elem in
  let pp_string_option = pp_print_option ~none some in
  fprintf fmt "@[description =@ %a@];@ " pp_string_option a.description;
  fprintf fmt "@[extended =@ %a@];@ " pp_string_option a.extended;
  let pp_sep fmt () = fprintf fmt ";@ " in
  let pp_elem fmt elem = fprintf fmt "%S" elem in
  let pp_tag = pp_print_list ~pp_sep pp_elem in
  fprintf fmt "@[tag =@ @[[@ %a@,@ ]@]@];@ " pp_tag a.tag;
  fprintf fmt "@[hash =@ %a@];@ " pp_string_option a.hash;
  fprintf fmt "@[shared =@ %B@];@ " a.shared;
  fprintf fmt "@[toread =@ %B@];@ " a.toread;
  fprintf fmt "}@]@;"

let show = Format.asprintf "%a" pp
let to_string = show

module Tags = struct
  include Set.Make (String)

  let pp fmt a =
    let open Format in
    let pp_sep fmt () = fprintf fmt ";@ " in
    let pp_elem fmt elem = fprintf fmt "%S" elem in
    fprintf fmt "@[<2>{@ %a@ }@]" (pp_print_list ~pp_sep pp_elem) (elements a)

  let show = Format.asprintf "%a" pp
  let to_string = show
end

let tags = List.fold_left (fun acc post -> Tags.of_list post.tag |> Tags.union acc) Tags.empty

let get_attr attrs k =
  try
    let k = (String.empty, k) in
    List.assoc k attrs
  with Not_found -> String.empty

let get_attr_option attrs k =
  let s = get_attr attrs k in
  if String.empty = s then None else Some s

let from_xml file =
  let ic = open_in file in
  let channel = Markup.channel ic in
  let html = Markup.parse_html channel in
  let signals = Markup.signals html in
  let to_t attrs =
    let href = get_attr attrs "href" in
    let time = get_attr attrs "time" in
    let description = get_attr_option attrs "description" in
    let extended = get_attr_option attrs "extended" in
    let tag = Str.split (Str.regexp "[ \t]+") (get_attr attrs "tag") in
    let hash = get_attr_option attrs "hash" in
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
    | None -> failwith "unexpected end of stream"
  in
  let ret = go 0 [] in
  close_in ic;
  ret

let maybe_start stream name ~on_failure on_success =
  match Markup.peek stream with
  | Some (`Start_element ((_, n), attrs)) when n = name ->
      ignore (Markup.next stream);
      on_success attrs
  | e -> on_failure e

let maybe_text stream ?(p = fun _ -> true) ~on_failure on_success =
  match Markup.peek stream with
  | Some (`Text xs) when p xs ->
      ignore (Markup.next stream);
      on_success xs
  | e -> on_failure e

let next_start stream name ?(on_failure = fun _ -> failwith "parse error") on_success =
  match Markup.next stream with
  | Some (`Start_element ((_, n), attrs)) when n = name -> on_success attrs
  | e -> on_failure e

let next_text stream ?(on_failure = fun _ -> failwith "parse error") on_success =
  match Markup.next stream with
  | Some (`Text xs) -> on_success xs
  | e -> on_failure e

let next_end stream ?(on_failure = fun _ -> failwith "parse error") on_success =
  match Markup.next stream with
  | Some `End_element -> on_success ()
  | e -> on_failure e

let is_newline xs =
  let s = String.concat String.empty xs in
  Str.string_match (Str.regexp "[\n\r]*") s 0

let skip_newlines stream () =
  let f _ = () in
  maybe_text stream ~p:is_newline ~on_failure:f f

let tag s = ((String.empty, s), [])
let skip stream t = if Markup.peek stream = Some t then ignore (Markup.next stream) else ()

let from_html file =
  let ic = open_in file in
  let channel = Markup.channel ic in
  let html = Markup.parse_html channel in
  let signals = Markup.signals html in
  let parse_dd () =
    maybe_start signals "dd" ~on_failure:(fun _ -> None) @@ fun _ ->
    next_text signals @@ fun xs ->
    next_end signals @@ fun () -> Some (String.trim (String.concat String.empty xs))
  in
  let to_t () =
    skip signals (`Start_element (tag "p"));
    let attrs = next_start signals "a" (fun x -> x) in
    let description =
      maybe_text signals
        ~on_failure:(fun _ -> None)
        (fun xs -> Some (String.trim (String.concat String.empty xs)))
    in
    skip signals `End_element (* </a> *);
    skip_newlines signals ();
    skip signals `End_element (* implicit </dt> *);
    let extended = Option.map String.trim (parse_dd ()) in
    let href = get_attr attrs "href" in
    let time = get_attr attrs "add_date" in
    let tag =
      let tags = Str.split (Str.regexp "[,]+") (get_attr attrs "tags") in
      List.filter (( <> ) "toread") tags
    in
    let hash = get_attr_option attrs "hash" in
    let shared = get_attr attrs "private" = "0" in
    let toread = get_attr attrs "toread" = "1" in
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
    | None -> failwith "unexpected end of stream"
  in
  let ret = go 0 [] in
  close_in ic;
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
    let hash = j |> member "hash" |> to_string_option in
    let shared = j |> member "shared" |> to_string = "yes" in
    let toread = j |> member "toread" |> to_string = "yes" in
    { href; time; description; extended; tag; hash; shared; toread }
  in
  convert_each to_t json
