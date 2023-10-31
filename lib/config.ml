let get_data_dir () =
  let xdg = Xdg.create ~env:Sys.getenv_opt () in
  let data_dir = Xdg.data_dir xdg in
  Filename.concat data_dir "backlogged"

let get_import_dir () =
  let data_dir = get_data_dir () in
  Filename.concat data_dir "import"
