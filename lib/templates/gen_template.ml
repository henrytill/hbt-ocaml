let () =
  let template_file = "templates/netscape_bookmarks.jinja" in
  let ic = open_in template_file in
  let template_content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  (* Strip trailing newline if present *)
  let template_content =
    if
      String.length template_content > 0
      && template_content.[String.length template_content - 1] = '\n'
    then
      String.sub template_content 0 (String.length template_content - 1)
    else
      template_content
  in
  Printf.printf "let netscape_bookmarks = {|%s|}\n" template_content
