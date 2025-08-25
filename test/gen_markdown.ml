let generate_rule base =
  Printf.printf
    {|
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
 (alias runtest)
 (action
  (diff %%{reference} %%{generated})))
|}
    base
    base
    base
    base

let () =
  Sys.readdir ".."
  |> Array.to_list
  |> List.filter_map (Filename.chop_suffix_opt ~suffix:".md")
  |> List.iter generate_rule
