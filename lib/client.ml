module Body = Cohttp_lwt.Body
module Client = Cohttp_lwt_unix.Client
module Code = Cohttp.Code
module Header = Cohttp.Header
module Response = Cohttp.Response

(* For binding operators *)
open Lwt.Syntax

let return = Lwt.return

let get () =
  let headers = Header.of_list [ ("Accept", "application/json"); ("Host", "www.howsmyssl.com") ] in
  let* resp, body = Client.get ~headers (Uri.of_string "https://www.howsmyssl.com/a/check") in
  let* body = Body.to_string body in
  let code = resp |> Response.status |> Code.code_of_status in
  let headers = resp |> Response.headers |> Header.to_string in
  let body_len = String.length body in
  let _ = Lwt_io.printf "Response code: %d\n" code in
  let _ = Lwt_io.printf "Headers:\n%s\n" headers in
  let _ = Lwt_io.printf "Body of length: %d\n" body_len in
  let _ = Lwt_io.printf "Body:\n%s\n" body in
  let* () = Lwt_io.flush Lwt_io.stdout in
  return ()

let min_get () =
  let headers = Header.of_list [ ("Accept", "application/json"); ("Host", "www.howsmyssl.com") ] in
  let* _, body = Client.get ~headers (Uri.of_string "https://www.howsmyssl.com/a/check") in
  Body.to_string body
