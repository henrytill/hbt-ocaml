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
 (deps
  (:source ../%s.input.html)
  (:hbt %%{bin:hbt}))
 (action
  (with-stdout-to
   %%{target}
   (run %%{hbt} -t yaml %%{source}))))

(rule
 (package hbt)
 (deps
  (:reference ../%s.expected.yaml)
  (:generated %s_out.yaml))
 (alias runtest)%s
 (action
  (progn
   (echo "Test HTML→YAML: %s\n")
   (diff %%{reference} %%{generated}))))
|}
    base
    base
    base
    base
    base
    enabled_if
    base

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
 (deps
  (:source ../%s.input.html)
  (:hbt %%{bin:hbt}))
 (action
  (with-stdout-to
   %%{target}
   (run %%{hbt} -t html %%{source}))))

(rule
 (package hbt)
 (deps
  (:reference ../%s.expected.html)
  (:generated %s_export_out.html))
 (alias runtest)%s
 (action
  (progn
   (echo "Test HTML→HTML: %s\n")
   (diff %%{reference} %%{generated}))))
|}
    base
    base
    base
    base
    base
    enabled_if
    base

let () =
  let html_files =
    Sys.readdir ".."
    |> Array.to_list
    |> List.filter_map (Filename.chop_suffix_opt ~suffix:".input.html")
  in
  List.iter generate_rules html_files;
  List.iter generate_html_export_rules html_files
