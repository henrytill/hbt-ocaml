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

let () =
  let data_dir = Config.get_data_dir () in
  if not (Sys.file_exists data_dir && Sys.is_directory data_dir) then
    Sys.mkdir data_dir 0o700;
  Printf.printf "data_dir: %s\n" data_dir;
  let import_dir = Config.get_import_dir () in
  let xml_files = find_xml_files import_dir in
  let joined = "\"" ^ String.concat "\"; \"" xml_files ^ "\"" in
  Printf.printf "xml_files: [%s]\n" joined;
  let posts = Importer.parse_xml (List.hd xml_files) in
  Printf.printf "parsed: %d posts, " (List.length posts);
  let open Importer in
  let tags =
    List.fold_left
      (fun acc post -> StringSet.of_list post.tag |> StringSet.union acc)
      StringSet.empty posts
  in
  Printf.printf "%d tags\n" (StringSet.cardinal tags);
  let joined = "\"" ^ String.concat "\"; \"" (StringSet.elements tags) ^ "\"" in
  Printf.printf "tags: [%s]\n" joined
