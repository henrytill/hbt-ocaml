(* https://dune.readthedocs.io/en/stable/howto/rule-generation.html#using-dynamic-include *)

let generate_rules base =
  Printf.printf
    {|
(rule
 (package hbt)
 (target %s_out.yaml)
 (deps (:source ../%s.html))
 (alias runtest)
 (action
  (with-stdout-to
   %%{target}
   (run %%{bin:hbt} -t yaml %%{source}))))

(rule
 (package hbt)
 (deps
  (:reference ../%s.yaml)
  (:generated %s_out.yaml))
 (alias runtest)
 (action
  (diff %%{reference} %%{generated})))
|}
    base
    base
    base
    base

let generate_html_export_rules base =
  Printf.printf
    {|
(rule
 (package hbt)
 (target %s_export_out.html)
 (deps (:source ../%s.html))
 (alias runtest)
 (action
  (with-stdout-to
   %%{target}
   (run %%{bin:hbt} -t html %%{source}))))

(rule
 (package hbt)
 (deps
  (:reference ../export/%s_export.html)
  (:generated %s_export_out.html))
 (alias runtest)
 (action
  (diff %%{reference} %%{generated})))
|}
    base
    base
    base
    base

let () =
  let html_files =
    Sys.readdir ".." |> Array.to_list |> List.filter_map (Filename.chop_suffix_opt ~suffix:".html")
  in
  List.iter generate_rules html_files;
  List.iter generate_html_export_rules html_files
