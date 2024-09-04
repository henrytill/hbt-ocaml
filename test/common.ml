(** Utilities for testing. *)

let read_file file =
  let ic = open_in file in
  let ret = ref [] in
  try
    while true do
      let line = input_line ic in
      ret := line :: !ret
    done
  with
  | End_of_file ->
      close_in ic;
      String.concat "\n" (List.rev !ret)
  | e ->
      close_in ic;
      raise e

(** [with_temp_file contents f] creates a temporary file with the given
    contents and calls [f] with the filename. The file is deleted after [f]
    returns. *)
let with_temp_file contents f =
  let filename, oc = Filename.open_temp_file "test-" String.empty in
  try
    output_string oc contents;
    close_out oc;
    try
      let result = f filename in
      Sys.remove filename;
      result
    with e ->
      Sys.remove filename;
      raise e
  with e ->
    close_out_noerr oc;
    raise e
