let read_file file =
  let ic = open_in file in
  let ret = ref [] in
  try
    while true do
      let line = input_line ic in
      ret := line :: !ret
    done
  with
  | End_of_file ->
      close_in ic;
      String.concat "\n" (List.rev !ret)
  | e ->
      close_in ic;
      raise e

let () =
  let dump_entities = ref false in
  let file = ref String.empty in
  let usage_string = "Usage: " ^ Sys.argv.(0) ^ "[options...] <file>" in
  let opt_list = [ ("-dump", Arg.Set dump_entities, "dump entities") ] in
  let process_arg arg = file := arg in
  begin
    try Arg.parse opt_list process_arg usage_string
    with _ ->
      Arg.usage opt_list usage_string;
      exit 1
  end;
  let input = read_file !file in
  let collection = Hbt.Markdown.parse input in
  if !dump_entities then
    let entities = Hbt.Collection.entities collection in
    let () =
      Array.iter
        (fun e -> Hbt.Collection.Entity.uri e |> Uri.to_string |> Printf.printf "%s\n")
        entities
    in
    exit 0
  else
    let length = Hbt.Collection.length collection in
    let () = Printf.printf "%s: %d entities\n" !file length in
    exit 0
