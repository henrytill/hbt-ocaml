let generate_xml_rule base =
  let is_problematic = Test_filter.is_problematic_base ~dir:"pinboard" ~ext:".xml" base in
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
  (:source ../%s.input.xml)
  (:hbt %%{bin:hbt}))
 (action
  (with-stdout-to
   %%{target}
   (setenv TZ UTC (run %%{hbt} -t yaml %%{source})))))

(rule
 (package hbt)
 (deps
  (:reference ../%s.expected.yaml)
  (:generated %s_out.yaml))
 (alias runtest)%s
 (action
  (progn
   (echo "Test XML→YAML: %s\n")
   (diff %%{reference} %%{generated}))))
|}
    base
    base
    base
    base
    base
    enabled_if
    base

let generate_json_rule base =
  let is_problematic = Test_filter.is_problematic_base ~dir:"pinboard" ~ext:".json" base in
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
  (:source ../%s.input.json)
  (:hbt %%{bin:hbt}))
 (action
  (with-stdout-to
   %%{target}
   (setenv TZ UTC (run %%{hbt} -t yaml %%{source})))))

(rule
 (package hbt)
 (deps
  (:reference ../%s.expected.yaml)
  (:generated %s_out.yaml))
 (alias runtest)%s
 (action
  (progn
   (echo "Test JSON→YAML: %s\n")
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
  let xml_files =
    Sys.readdir ".."
    |> Array.to_list
    |> List.filter_map (Filename.chop_suffix_opt ~suffix:".input.xml")
  in
  let json_files =
    Sys.readdir ".."
    |> Array.to_list
    |> List.filter_map (Filename.chop_suffix_opt ~suffix:".input.json")
  in
  List.iter generate_xml_rule xml_files;
  List.iter generate_json_rule json_files
