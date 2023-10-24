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
  let* body = Cohttp_lwt.Body.to_string body in
  let code = resp |> Response.status |> Code.code_of_status in
  let headers = resp |> Response.headers |> Header.to_string in
  let body_len = String.length body in
  let _ = Lwt_io.printf "Response code: %d\n" code in
  let _ = Lwt_io.printf "Headers:\n%s\n" headers in
  let _ = Lwt_io.printf "Body of length: %d\n" body_len in
  let _ = Lwt_io.printf "Body:\n%s\n" body in
  let* () = Lwt_io.flush Lwt_io.stdout in
  return ()

let run () =
  Lwt_main.run
    begin
      let* _ = get () in
      return ()
    end

module Test = struct
  let min_get () =
    let headers =
      Header.of_list [ ("Accept", "application/json"); ("Host", "www.howsmyssl.com") ]
    in
    let* _, body = Client.get ~headers (Uri.of_string "https://www.howsmyssl.com/a/check") in
    let* body = Cohttp_lwt.Body.to_string body in
    let _ = Lwt_io.printf "%s" body in
    let* () = Lwt_io.flush Lwt_io.stdout in
    return ()

  let%expect_test "run" =
    Lwt_main.run
      begin
        let* () = min_get () in
        return ()
      end;
    [%expect
      {| {"given_cipher_suites":["TLS_AES_256_GCM_SHA384","TLS_CHACHA20_POLY1305_SHA256","TLS_AES_128_GCM_SHA256","TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384","TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384","TLS_DHE_RSA_WITH_AES_256_GCM_SHA384","TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256","TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256","TLS_DHE_RSA_WITH_CHACHA20_POLY1305_SHA256","TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256","TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256","TLS_DHE_RSA_WITH_AES_128_GCM_SHA256","TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384","TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384","TLS_DHE_RSA_WITH_AES_256_CBC_SHA256","TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256","TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256","TLS_DHE_RSA_WITH_AES_128_CBC_SHA256","TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA","TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA","TLS_DHE_RSA_WITH_AES_256_CBC_SHA","TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA","TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA","TLS_DHE_RSA_WITH_AES_128_CBC_SHA","TLS_RSA_WITH_AES_256_GCM_SHA384","TLS_RSA_WITH_AES_128_GCM_SHA256","TLS_RSA_WITH_AES_256_CBC_SHA256","TLS_RSA_WITH_AES_128_CBC_SHA256","TLS_RSA_WITH_AES_256_CBC_SHA","TLS_RSA_WITH_AES_128_CBC_SHA","TLS_EMPTY_RENEGOTIATION_INFO_SCSV"],"ephemeral_keys_supported":true,"session_ticket_supported":true,"tls_compression_supported":false,"unknown_cipher_suite_supported":false,"beast_vuln":false,"able_to_detect_n_minus_one_splitting":false,"insecure_cipher_suites":{},"tls_version":"TLS 1.3","rating":"Probably Okay"} |}]
end
