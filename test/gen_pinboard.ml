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
 (deps (:source ../%s.xml))
 (action
  (with-stdout-to
   %%{target}
   (setenv TZ UTC (run %%{bin:hbt} -t yaml %%{source})))))

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
 (deps (:source ../%s.json))
 (action
  (with-stdout-to
   %%{target}
   (setenv TZ UTC (run %%{bin:hbt} -t yaml %%{source})))))

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

let () =
  let xml_files = [ "empty"; "xml_sample" ] in
  let json_files = [ "json_sample" ] in
  List.iter generate_xml_rule xml_files;
  List.iter generate_json_rule json_files
