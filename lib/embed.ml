let () =
  let args = Sys.argv in
  let input_file = args.(1) in
  let name = Filename.(remove_extension (basename input_file)) in
  let ic = open_in input_file in
  let finally () = close_in ic in
  let content = Fun.protect ~finally (fun () -> really_input_string ic (in_channel_length ic)) in
  Printf.printf "let %s = {|%s|}\n" name content
