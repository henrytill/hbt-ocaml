module Config = Backlogged.Config
module Pinboard = Backlogged.Pinboard
module StringSet = Set.Make (String)

let find_files suffix dir =
  let dir_handle = Unix.opendir dir in
  let is_file path =
    match (Unix.stat path).st_kind with
    | Unix.S_REG -> true
    | _ -> false
  in
  let rec go acc =
    try
      let file = Unix.readdir dir_handle in
      let full_path = Filename.concat dir file in
      if is_file full_path && Filename.check_suffix file suffix then
        go (Filename.concat dir file :: acc)
      else
        go acc
    with End_of_file ->
      Unix.closedir dir_handle;
      acc
  in
  go []

let find_xml_files dir =
  let suffix = ".xml" in
  find_files suffix dir

let find_json_files dir =
  let suffix = ".json" in
  find_files suffix dir

let pp_list pp_val fmt xs =
  let open Format in
  pp_print_char fmt '[';
  pp_print_list ~pp_sep:(fun fmt _ -> pp_print_string fmt "; ") pp_val fmt xs;
  pp_print_char fmt ']'

let pp_string_list fmt xs =
  let pp_string fmt s = Format.fprintf fmt "\"%s\"" s in
  pp_list pp_string fmt xs

let parse_xml fmt import_dir =
  let open Format in
  let xml_files = find_xml_files import_dir in
  pp_print_string fmt "xml_files: ";
  pp_string_list fmt xml_files;
  pp_print_newline fmt ();
  let posts = Pinboard.from_xml (List.hd xml_files) in
  fprintf fmt "parsed: %d posts, " (List.length posts);
  let tags = Pinboard.tags posts in
  fprintf fmt "%d tags\n" (Pinboard.Tags.cardinal tags);
  pp_print_string fmt "tags: ";
  Pinboard.Tags.pp fmt tags;
  pp_print_newline fmt ()

let parse_json fmt import_dir =
  let open Format in
  let json_files = find_json_files import_dir in
  pp_print_string fmt "json_files: ";
  pp_string_list fmt json_files;
  pp_print_newline fmt ();
  let posts = Pinboard.from_json (List.hd json_files) in
  fprintf fmt "parsed: %d posts, " (List.length posts);
  let tags = Pinboard.tags posts in
  fprintf fmt "%d tags\n" (Pinboard.Tags.cardinal tags);
  pp_print_string fmt "tags: ";
  Pinboard.Tags.pp fmt tags;
  pp_print_newline fmt ()

let () =
  let fmt = Format.std_formatter in
  let data_dir = Config.get_data_dir () in
  if not (Sys.file_exists data_dir && Sys.is_directory data_dir) then
    Sys.mkdir data_dir 0o700;
  Format.fprintf fmt "data_dir: %s\n" data_dir;
  let import_dir = Config.get_import_dir () in
  if not (Sys.file_exists import_dir && Sys.is_directory import_dir) then
    Sys.mkdir import_dir 0o700;
  (* xml *)
  parse_xml fmt import_dir;
  (* json *)
  parse_json fmt import_dir
