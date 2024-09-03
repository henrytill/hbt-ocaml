let () =
  let open Alcotest in
  run "Pinboard" Pinboard_test.tests
