open Attic

let test_popcount () =
  let check = Alcotest.(check int) in
  check "0" 0 (Bits.popcount 0);
  check "1" 1 (Bits.popcount 1);
  check "all 32 bits" 32 (Bits.popcount 0xFFFFFFFF);
  check "alternating 0x55555555" 16 (Bits.popcount 0x55555555);
  check "alternating 0xAAAAAAAA" 16 (Bits.popcount 0xAAAAAAAA);
  for k = 0 to 31 do
    check (Printf.sprintf "1 lsl %d" k) 1 (Bits.popcount (1 lsl k))
  done;
  check "ignores bits above 32" 32 (Bits.popcount (-1))

let tests =
  let open Alcotest in
  [ ("Bits", [ test_case "popcount" `Quick test_popcount ]) ]

let () = Alcotest.run "Bits" tests
