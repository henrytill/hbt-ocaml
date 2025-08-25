(** Test filtering utilities for handling known conformance issues. *)

let issues_file = "../../../issues"

let read_issues_file () =
  let issues = ref [] in
  let ic = open_in issues_file in
  (try
     while true do
       let line = input_line ic in
       let trimmed = String.trim line in
       if trimmed <> "" && not (String.starts_with ~prefix:"#" trimmed) then
         issues := trimmed :: !issues
     done
   with End_of_file -> close_in ic);
  List.rev !issues

(** Check if a base filename should be filtered out due to known conformance issues *)
let is_problematic_base ~dir ~ext base =
  let issues = read_issues_file () in
  let full_path = dir ^ "/" ^ base ^ ext in
  List.exists (String.equal full_path) issues
