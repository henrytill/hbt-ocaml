(** Utilities for testing. *)

(** [with_temp_file contents f] creates a temporary file with the given contents and calls [f] with
    the filename. The file is deleted after [f] returns. *)
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
