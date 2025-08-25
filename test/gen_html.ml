(* https://dune.readthedocs.io/en/stable/howto/rule-generation.html#using-dynamic-include *)

let generate_rules base =
  let is_problematic = Test_filter.is_problematic_base ~dir:"html" ~ext:".html" base in
  let enabled_if =
    if is_problematic then "\n (enabled_if (= %{profile} failing-tests))" else String.empty
  in
  Printf.printf
    {|
; Test: %s

(rule
 (package hbt)
 (target %s_out.yaml)
 (deps (:source ../%s.html))
 (action
  (with-stdout-to
   %%{target}
   (run %%{bin:hbt} -t yaml %%{source}))))

(rule
 (package hbt)
 (deps
  (:reference ../%s.yaml)
  (:generated %s_out.yaml))
 (alias runtest)%s
 (action
  (diff %%{reference} %%{generated})))
|}
    base
    base
    base
    base
    base
    enabled_if

let generate_html_export_rules base =
  let is_problematic = Test_filter.is_problematic_base ~dir:"html" ~ext:".html" base in
  let enabled_if =
    if is_problematic then "\n (enabled_if (= %{profile} failing-tests))" else String.empty
  in
  Printf.printf
    {|
; Test: %s (HTML export)

(rule
 (package hbt)
 (target %s_export_out.html)
 (deps (:source ../%s.html))
 (action
  (with-stdout-to
   %%{target}
   (run %%{bin:hbt} -t html %%{source}))))

(rule
 (package hbt)
 (deps
  (:reference ../export/%s_export.html)
  (:generated %s_export_out.html))
 (alias runtest)%s
 (action
  (diff %%{reference} %%{generated})))
|}
    base
    base
    base
    base
    base
    enabled_if

let () =
  let html_files =
    Sys.readdir ".." |> Array.to_list |> List.filter_map (Filename.chop_suffix_opt ~suffix:".html")
  in
  List.iter generate_rules html_files;
  List.iter generate_html_export_rules html_files
