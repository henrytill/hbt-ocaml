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

let find_xml_files dir =
  let suffix = ".xml" in
  find_files suffix dir

let find_html_files dir =
  let suffix = ".html" in
  find_files suffix dir

let find_json_files dir =
  let suffix = ".json" in
  find_files suffix dir

let pp_list pp_val fmt xs =
  let open Format in
  let pp_sep fmt () = pp_print_string fmt ";@ " in
  fprintf fmt "@[[%a]@]" (pp_print_list ~pp_sep pp_val) xs

let pp_string_list fmt xs =
  let pp_string fmt = Format.fprintf fmt "%S" in
  pp_list pp_string fmt xs

let parse_xml fmt import_dir =
  let open Format in
  let xml_files = find_xml_files import_dir in
  let posts = Pinboard.from_xml (List.hd xml_files) in
  let tags = Pinboard.tags posts in
  fprintf fmt "@[xml_files: %a@]@;" pp_string_list xml_files;
  fprintf fmt "@[parsed: %d posts, %d tags@]@;" (List.length posts) (Pinboard.Tags.cardinal tags);
  fprintf fmt "@[tags: %a@]@;" Pinboard.Tags.pp tags;
  tags

let parse_html fmt import_dir =
  let open Format in
  let html_files = find_html_files import_dir in
  let posts = Pinboard.from_html (List.hd html_files) in
  let tags = Pinboard.tags posts in
  fprintf fmt "@[html_files: %a@]@;" pp_string_list html_files;
  fprintf fmt "@[parsed: %d posts, %d tags@]@;" (List.length posts) (Pinboard.Tags.cardinal tags);
  fprintf fmt "@[tags: %a@]@;" Pinboard.Tags.pp tags;
  tags

let parse_json fmt import_dir =
  let open Format in
  let json_files = find_json_files import_dir in
  let posts = Pinboard.from_json (List.hd json_files) in
  let tags = Pinboard.tags posts in
  fprintf fmt "@[json_files: %a@]@;" pp_string_list json_files;
  fprintf fmt "@[parsed: %d posts, %d tags@]@;" (List.length posts) (Pinboard.Tags.cardinal tags);
  fprintf fmt "@[tags: %a@]@;" Pinboard.Tags.pp tags;
  fprintf fmt "@[sample: %a@]@;" Pinboard.pp (List.hd posts);
  tags

let () =
  let fmt = Format.std_formatter in
  let data_dir = Config.get_data_dir () in
  if not (Sys.file_exists data_dir && Sys.is_directory data_dir) then
    Sys.mkdir data_dir 0o700;
  Format.fprintf fmt "@[data_dir: %S@]@;" data_dir;
  let import_dir = Config.get_import_dir () in
  if not (Sys.file_exists import_dir && Sys.is_directory import_dir) then
    Sys.mkdir import_dir 0o700;
  (* xml *)
  ignore (parse_xml fmt import_dir);
  (* html *)
  let html_tags = parse_html fmt import_dir in
  (* json *)
  let json_tags = parse_json fmt import_dir in
  let diff = Pinboard.Tags.diff html_tags json_tags in
  Format.fprintf fmt "@[diff: %a@]@;" Pinboard.Tags.pp diff;
  Format.pp_print_flush fmt ()
