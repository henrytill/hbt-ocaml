module Client = Backlogged.Client
module Pinboard = Backlogged.Importer.Pinboard

let test_pp () =
  let post =
    Pinboard.
      {
        href = "http://goodmath.scientopia.org/2010/04/26/finger-trees-done-right-i-hope/";
        time = "2018-04-14T01:46:34Z";
        description = Some "Finger Trees Done Right (I hope) | Good Math Bad Math";
        extended =
          Some
            {|"Finger Trees: a simple general-purpose data structure", by Ralf Hinze and Ross Patterson.|};
        tag = [ "data-structures" ];
        hash = "4c5865711d6b3522c8e75dff3f44e6c3";
        shared = false;
      }
  in
  let actual =
    Pinboard.pp Format.str_formatter post;
    Format.flush_str_formatter ()
  in
  let expected =
    {|{href: "http://goodmath.scientopia.org/2010/04/26/finger-trees-done-right-i-hope/", time: "2018-04-14T01:46:34Z", description: "Finger Trees Done Right (I hope) | Good Math Bad Math", extended: "\"Finger Trees: a simple general-purpose data structure\", by Ralf Hinze and Ross Patterson.", tag: ["data-structures"], hash: "4c5865711d6b3522c8e75dff3f44e6c3", shared: false}|}
  in
  if expected <> actual then (
    Printf.printf "expected: %s\n" expected;
    Printf.printf "actual: %s\n" actual;
    failwith "test failed")

let test_client () =
  let actual = Lwt_main.run (Client.min_get ()) in
  let expected =
    {|{"given_cipher_suites":["TLS_AES_256_GCM_SHA384","TLS_CHACHA20_POLY1305_SHA256","TLS_AES_128_GCM_SHA256","TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384","TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384","TLS_DHE_RSA_WITH_AES_256_GCM_SHA384","TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256","TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256","TLS_DHE_RSA_WITH_CHACHA20_POLY1305_SHA256","TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256","TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256","TLS_DHE_RSA_WITH_AES_128_GCM_SHA256","TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384","TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384","TLS_DHE_RSA_WITH_AES_256_CBC_SHA256","TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256","TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256","TLS_DHE_RSA_WITH_AES_128_CBC_SHA256","TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA","TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA","TLS_DHE_RSA_WITH_AES_256_CBC_SHA","TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA","TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA","TLS_DHE_RSA_WITH_AES_128_CBC_SHA","TLS_RSA_WITH_AES_256_GCM_SHA384","TLS_RSA_WITH_AES_128_GCM_SHA256","TLS_RSA_WITH_AES_256_CBC_SHA256","TLS_RSA_WITH_AES_128_CBC_SHA256","TLS_RSA_WITH_AES_256_CBC_SHA","TLS_RSA_WITH_AES_128_CBC_SHA","TLS_EMPTY_RENEGOTIATION_INFO_SCSV"],"ephemeral_keys_supported":true,"session_ticket_supported":true,"tls_compression_supported":false,"unknown_cipher_suite_supported":false,"beast_vuln":false,"able_to_detect_n_minus_one_splitting":false,"insecure_cipher_suites":{},"tls_version":"TLS 1.3","rating":"Probably Okay"}|}
  in
  if expected <> actual then (
    Printf.printf "expected: %s\n" expected;
    Printf.printf "actual: %s\n" actual;
    failwith "test failed")

let () =
  test_pp ();
  test_client ();
  exit 0
