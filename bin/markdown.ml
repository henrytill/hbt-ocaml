let read_file file =
  let ic = open_in file in
  let ret = In_channel.input_all ic in
  close_in ic;
  ret

let () =
  let dump_entities = ref false in
  let file = ref None in
  let usage_string = "Usage: " ^ Sys.argv.(0) ^ " [OPTIONS...] <FILE>" in
  let opt_list = [ ("-dump", Arg.Set dump_entities, "dump entities") ] in
  let process_arg arg = file := Some arg in
  Arg.parse opt_list process_arg usage_string;
  let file =
    match !file with
    | Some file -> file
    | None ->
        Printf.eprintf "Error: missing file argument\n";
        Arg.usage opt_list usage_string;
        exit 1
  in
  begin
    let open Hbt in
    let input = read_file file in
    let collection = Markdown.parse input in
    if !dump_entities then
      let entities = Collection.entities collection in
      Array.iter
        (fun e -> Collection.Entity.uri e |> Uri.to_string |> Printf.printf "%s\n")
        entities
    else
      let length = Collection.length collection in
      Printf.printf "%s: %d entities\n" file length
  end;
  exit 0
