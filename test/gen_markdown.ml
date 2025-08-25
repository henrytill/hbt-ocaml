let generate_rule base =
  let is_problematic = Test_filter.is_problematic_base ~dir:"markdown" ~ext:".md" base in
  let enabled_if =
    if is_problematic then "\n (enabled_if (= %{profile} failing-tests))" else String.empty
  in
  Printf.printf
    {|
; Test: %s

(rule
 (package hbt)
 (target %s_out.yaml)
 (deps (:source ../%s.md))
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
  Sys.readdir ".."
  |> Array.to_list
  |> List.filter_map (Filename.chop_suffix_opt ~suffix:".md")
  |> List.iter generate_rule
