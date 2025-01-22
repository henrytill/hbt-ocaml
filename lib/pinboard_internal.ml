open Prelude

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

let href p = p.href

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

let tags = List.fold_left (fun acc post -> Tags.of_list post.tag |> Tags.union acc) Tags.empty

module Attrs = struct
  type t = ((string * string) * string) list

  let get_opt (attrs : t) (k : string) : string option = List.assoc_opt (String.empty, k) attrs
  let get (attrs : t) (k : string) : string = Option.value ~default:String.empty (get_opt attrs k)
end

let from_xml file =
  let ic = open_in file in
  let channel = Markup.channel ic in
  let html = Markup.parse_html channel in
  let signals = Markup.signals html in
  let to_t (attrs : Attrs.t) : t =
    let href = Attrs.get attrs "href" in
    let time = Attrs.get attrs "time" in
    let description = Attrs.get_opt attrs "description" in
    let extended = Attrs.get_opt attrs "extended" in
    let tag = Str.split (Str.regexp "[ \t]+") (Attrs.get attrs "tag") in
    let hash = Attrs.get_opt attrs "hash" in
    let shared = Attrs.get attrs "shared" = "yes" in
    let toread = Attrs.get attrs "toread" = "yes" in
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

let maybe_start stream name on_failure on_success =
  match Markup.peek stream with
  | Some (`Start_element ((_, n), attrs)) when n = name ->
      ignore (Markup.next stream);
      on_success attrs
  | e -> on_failure e

let maybe_text stream p on_failure on_success =
  match Markup.peek stream with
  | Some (`Text xs) when p xs ->
      ignore (Markup.next stream);
      on_success xs
  | e -> on_failure e

let next_start stream name on_failure on_success =
  match Markup.next stream with
  | Some (`Start_element ((_, n), attrs)) when n = name -> on_success attrs
  | e -> on_failure e

let next_text stream on_failure on_success =
  match Markup.next stream with
  | Some (`Text xs) -> on_success xs
  | e -> on_failure e

let next_end stream on_failure on_success =
  match Markup.next stream with
  | Some `End_element -> on_success ()
  | e -> on_failure e

let is_newline (xs : string list) : bool =
  let s = String.concat String.empty xs in
  Str.string_match (Str.regexp "[\n\r]*") s 0

let skip_newlines stream () = maybe_text stream is_newline ignore ignore
let tag s = ((String.empty, s), [])
let skip stream t = if Markup.peek stream = Some t then ignore (Markup.next stream) else ()

let from_html file =
  let ic = open_in file in
  let channel = Markup.channel ic in
  let html = Markup.parse_html channel in
  let signals = Markup.signals html in
  let parse_dd () =
    let on_failure _ = failwith "malformed <dd>" in
    let@ _ = maybe_start signals "dd" (fun _ -> None) in
    let@ xs = next_text signals on_failure in
    let@ () = next_end signals on_failure in
    Some (String.trim (String.concat String.empty xs))
  in
  let to_t () =
    skip signals (`Start_element (tag "p"));
    let on_failure _ = failwith "expected <a>" in
    let attrs = next_start signals "a" on_failure Fun.id in
    let description =
      maybe_text
        signals
        (fun _ -> true)
        (fun _ -> None)
        (fun xs -> Some (String.trim (String.concat String.empty xs)))
    in
    skip signals `End_element (* </a> *);
    skip_newlines signals ();
    skip signals `End_element (* implicit </dt> *);
    let extended = Option.map String.trim (parse_dd ()) in
    let href = Attrs.get attrs "href" in
    let time = Attrs.get attrs "add_date" in
    let tag =
      let tags = Str.split (Str.regexp "[,]+") (Attrs.get attrs "tags") in
      List.filter (( <> ) "toread") tags
    in
    let hash = Attrs.get_opt attrs "hash" in
    let shared = Attrs.get attrs "private" = "0" in
    let toread = Attrs.get attrs "toread" = "1" in
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

let t_of_yojson json =
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

let from_json file =
  let json = Yojson.Basic.from_file file in
  t_of_yojson json
