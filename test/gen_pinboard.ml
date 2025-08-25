let generate_xml_rule base =
  Printf.printf
    {|
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
 (alias runtest)
 (action
  (diff %%{reference} %%{generated})))
|}
    base
    base
    base
    base

let generate_json_rule base =
  Printf.printf
    {|
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
 (alias runtest)
 (action
  (diff %%{reference} %%{generated})))
|}
    base
    base
    base
    base

let () =
  let xml_files = [ "empty"; "xml_sample" ] in
  let json_files = [ "json_sample" ] in
  List.iter generate_xml_rule xml_files;
  List.iter generate_json_rule json_files
