module Config = Backlogged.Config
module Importer = Backlogged.Importer
module StringSet = Set.Make (String)

let find_xml_files dir =
  let suffix = ".xml" in
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

let pp_list pp_val fmt xs =
  Format.pp_print_char fmt '[';
  Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.pp_print_string fmt "; ") pp_val fmt xs;
  Format.pp_print_char fmt ']'

let pp_string_list fmt xs =
  let pp_string fmt s = Format.fprintf fmt "\"%s\"" s in
  pp_list pp_string fmt xs

let () =
  let fmt = Format.std_formatter in
  let data_dir = Config.get_data_dir () in
  if not (Sys.file_exists data_dir && Sys.is_directory data_dir) then
    Sys.mkdir data_dir 0o700;
  Format.fprintf fmt "data_dir: %s\n" data_dir;
  let import_dir = Config.get_import_dir () in
  if not (Sys.file_exists import_dir && Sys.is_directory import_dir) then
    Sys.mkdir import_dir 0o700;
  let xml_files = find_xml_files import_dir in
  Format.pp_print_string fmt "xml_files: ";
  pp_string_list fmt xml_files;
  Format.pp_print_newline fmt ();
  let posts = Importer.parse_xml (List.hd xml_files) in
  Format.fprintf fmt "parsed: %d posts, " (List.length posts);
  let open Importer in
  let tags =
    List.fold_left
      (fun acc post -> StringSet.of_list post.tag |> StringSet.union acc)
      StringSet.empty posts
  in
  Format.fprintf fmt "%d tags\n" (StringSet.cardinal tags);
  Format.pp_print_string fmt "tags: ";
  pp_string_list fmt (StringSet.elements tags);
  Format.pp_print_newline fmt ()
