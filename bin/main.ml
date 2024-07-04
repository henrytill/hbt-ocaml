module Config = Hbt.Config
module Pinboard = Hbt.Pinboard

let is_file path =
  match (Unix.stat path).st_kind with
  | Unix.S_REG -> true
  | _ -> false

let find_files suffix dir =
  let dir_handle = Unix.opendir dir in
  let rec go acc =
    try
      let file = Unix.readdir dir_handle in
      let full_path = Filename.concat dir file in
      if is_file full_path && Filename.check_suffix file suffix then
        go (full_path :: acc)
      else
        go acc
    with End_of_file ->
      Unix.closedir dir_handle;
      acc
  in
  go []

let mkdir_p dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then
    Sys.mkdir dir 0o700

let parse dir suffix parse_fn =
  let files = find_files suffix dir in
  let posts = parse_fn (List.hd files) in
  let tags = Pinboard.tags posts in
  (files, posts, tags)

let parse_xml dir = parse dir "xml" Pinboard.from_xml
let parse_html dir = parse dir "html" Pinboard.from_html
let parse_json dir = parse dir "json" Pinboard.from_json

let pp_list pp_val fmt xs =
  let pp_sep fmt () = Format.pp_print_string fmt ";@ " in
  Format.fprintf fmt "@[[%a]@]" (Format.pp_print_list ~pp_sep pp_val) xs

let pp_string_list fmt xs =
  let pp_string fmt = Format.fprintf fmt "%S" in
  pp_list pp_string fmt xs

let pp_posts fmt posts tags =
  let posts_len = List.length posts in
  let tags_len = Pinboard.Tags.cardinal tags in
  Format.fprintf fmt "@[parsed: %d posts, %d tags@]@;" posts_len tags_len;
  Format.fprintf fmt "@[tags: %a@]@;" Pinboard.Tags.pp tags

let () =
  let fmt = Format.std_formatter in
  (* config dirs *)
  let data_dir = Config.get_data_dir () in
  mkdir_p data_dir;
  Format.fprintf fmt "@[data_dir: %S@]@;" data_dir;
  let import_dir = Config.get_import_dir () in
  mkdir_p import_dir;
  Format.fprintf fmt "@[import_dir: %S@]@;" import_dir;
  (* xml *)
  let xml_files, xml_posts, xml_tags = parse_xml import_dir in
  Format.fprintf fmt "@[xml_files: %a@]@;" pp_string_list xml_files;
  pp_posts fmt xml_posts xml_tags;
  (* html *)
  let html_files, html_posts, html_tags = parse_html import_dir in
  Format.fprintf fmt "@[html_files: %a@]@;" pp_string_list html_files;
  pp_posts fmt html_posts html_tags;
  (* json *)
  let json_files, json_posts, json_tags = parse_json import_dir in
  Format.fprintf fmt "@[json_files: %a@]@;" pp_string_list json_files;
  pp_posts fmt json_posts json_tags;
  Format.fprintf fmt "@[sample: %a@]@;" Pinboard.pp (List.hd json_posts);
  (* tags diff *)
  let diff = Pinboard.Tags.diff html_tags json_tags in
  Format.fprintf fmt "@[diff: %a@]@;" Pinboard.Tags.pp diff;
  Format.pp_print_flush fmt ()
