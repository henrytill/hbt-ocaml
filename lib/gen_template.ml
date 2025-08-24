let () =
  let args = Sys.argv in
  let input_file = args.(1) in
  let name = Filename.remove_extension (Filename.basename input_file) in
  let ic = open_in input_file in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Printf.printf "let %s = {|%s|}\n" name content
