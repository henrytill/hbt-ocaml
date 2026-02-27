open Attic

let u = Belnap.(of_view Unknown)
let t = Belnap.(of_view True)
let f = Belnap.(of_view False)
let b = Belnap.(of_view Both)

let check =
  let belnap = Alcotest.testable Belnap.pp Belnap.equal in
  Alcotest.(check belnap)

let test_get_set () =
  let v = Belnap_vec.make 4 in
  Belnap_vec.set v 0 u;
  Belnap_vec.set v 1 t;
  Belnap_vec.set v 2 f;
  Belnap_vec.set v 3 b;
  check "get 0" u (Belnap_vec.get v 0);
  check "get 1" t (Belnap_vec.get v 1);
  check "get 2" f (Belnap_vec.get v 2);
  check "get 3" b (Belnap_vec.get v 3)

let test_auto_grow () =
  let v = Belnap_vec.make 10 in
  Belnap_vec.set v 100 b;
  Alcotest.(check int) "width" 101 (Belnap_vec.width v);
  check "get 100" b (Belnap_vec.get v 100);
  check "get 50" u (Belnap_vec.get v 50);
  Alcotest.check_raises
    "out of bounds"
    (Invalid_argument "Belnap_vec.get: index out of bounds")
    (fun () -> ignore (Belnap_vec.get v 200))

let test_bulk_and () =
  let a = Belnap_vec.all_true 64 in
  let b = Belnap_vec.all_false 64 in
  let r = Belnap_vec.( && ) a b in
  Alcotest.(check bool) "is_all_false" true (Belnap_vec.is_all_false r)

let test_bulk_or () =
  let a = Belnap_vec.all_false 64 in
  let b = Belnap_vec.all_true 64 in
  let r = Belnap_vec.( || ) a b in
  Alcotest.(check bool) "is_all_true" true (Belnap_vec.is_all_true r)

let test_bulk_not () =
  let a = Belnap_vec.all_true 100 in
  let r = Belnap_vec.not a in
  Alcotest.(check bool) "not all_true is_all_false" true (Belnap_vec.is_all_false r);
  let rr = Belnap_vec.not r in
  Alcotest.(check bool) "double-not round-trips is_all_true" true (Belnap_vec.is_all_true rr)

let test_bulk_merge () =
  let a = Belnap_vec.all_true 64 in
  let b = Belnap_vec.all_false 64 in
  let r = Belnap_vec.merge a b in
  Alcotest.(check int) "count_both" 64 (Belnap_vec.count_both r)

let test_is_consistent () =
  let v = Belnap_vec.make 4 in
  Belnap_vec.set v 0 t;
  Belnap_vec.set v 1 f;
  Belnap_vec.set v 2 u;
  Belnap_vec.set v 3 b;
  Alcotest.(check bool) "with Both is not consistent" false (Belnap_vec.is_consistent v);
  let v2 = Belnap_vec.make 4 in
  Belnap_vec.set v2 0 t;
  Belnap_vec.set v2 1 f;
  Belnap_vec.set v2 2 u;
  Belnap_vec.set v2 3 t;
  Alcotest.(check bool) "without Both is consistent" true (Belnap_vec.is_consistent v2)

let test_is_all_determined () =
  let v = Belnap_vec.make 4 in
  Belnap_vec.set v 0 t;
  Belnap_vec.set v 1 f;
  Belnap_vec.set v 2 t;
  Belnap_vec.set v 3 f;
  Alcotest.(check bool) "true/false is_all_determined" true (Belnap_vec.is_all_determined v);
  let v2 = Belnap_vec.make 4 in
  Belnap_vec.set v2 0 t;
  Belnap_vec.set v2 1 f;
  Belnap_vec.set v2 2 u;
  Belnap_vec.set v2 3 f;
  Alcotest.(check bool) "with Unknown not is_all_determined" false (Belnap_vec.is_all_determined v2);
  let v3 = Belnap_vec.make 4 in
  Belnap_vec.set v3 0 t;
  Belnap_vec.set v3 1 f;
  Belnap_vec.set v3 2 b;
  Belnap_vec.set v3 3 f;
  Alcotest.(check bool) "with Both not is_all_determined" false (Belnap_vec.is_all_determined v3)

let test_counts () =
  let v = Belnap_vec.make 8 in
  Belnap_vec.set v 0 t;
  Belnap_vec.set v 1 t;
  Belnap_vec.set v 2 f;
  Belnap_vec.set v 3 f;
  Belnap_vec.set v 4 b;
  Belnap_vec.set v 5 b;
  Belnap_vec.set v 6 u;
  Belnap_vec.set v 7 u;
  Alcotest.(check int) "count_true" 2 (Belnap_vec.count_true v);
  Alcotest.(check int) "count_false" 2 (Belnap_vec.count_false v);
  Alcotest.(check int) "count_both" 2 (Belnap_vec.count_both v);
  Alcotest.(check int) "count_unknown" 2 (Belnap_vec.count_unknown v)

let test_different_widths () =
  let a = Belnap_vec.all_true 32 in
  let b = Belnap_vec.all_false 64 in
  let r = Belnap_vec.( && ) a b in
  Alcotest.(check int) "width is max" 64 (Belnap_vec.width r);
  (* first 32 entries: True && False = False *)
  for i = 0 to 31 do
    check (Printf.sprintf "pos %d is False" i) f (Belnap_vec.get r i)
  done;
  (* next 32 entries: Unknown && False = False *)
  for i = 32 to 63 do
    check (Printf.sprintf "pos %d is False" i) f (Belnap_vec.get r i)
  done

let test_word_boundaries () =
  (* Element 63: bit 63 (sign bit) of word-pair 0 *)
  let v = Belnap_vec.make 65 in
  Belnap_vec.set v 63 b;
  check "get 63 is Both" b (Belnap_vec.get v 63);
  check "get 62 is Unknown" u (Belnap_vec.get v 62);
  check "get 64 is Unknown" u (Belnap_vec.get v 64);
  (* Element 64: bit 0 of word-pair 1 *)
  Belnap_vec.set v 64 t;
  check "get 64 is True" t (Belnap_vec.get v 64);
  check "get 63 still Both" b (Belnap_vec.get v 63)

let test_width_63 () =
  (* width=63 exercises r=63 in bv_mask_tail and tail_mask *)
  let v = Belnap_vec.all_true 63 in
  Alcotest.(check int) "width" 63 (Belnap_vec.width v);
  Alcotest.(check bool) "is_all_true" true (Belnap_vec.is_all_true v);
  Alcotest.(check bool) "is_all_determined" true (Belnap_vec.is_all_determined v);
  Alcotest.(check bool) "is_consistent" true (Belnap_vec.is_consistent v);
  check "get 62 is True" t (Belnap_vec.get v 62);
  let w = Belnap_vec.all_false 63 in
  let r = Belnap_vec.merge v w in
  Alcotest.(check int) "count_both after merge" 63 (Belnap_vec.count_both r);
  (* resize from width=63 exercises the word-boundary case in bv_blit_grow *)
  let v2 = Belnap_vec.all_true 63 in
  Belnap_vec.resize v2 127;
  check "get 62 still True after resize to 127" t (Belnap_vec.get v2 62);
  check "get 63 is Unknown after resize to 127" u (Belnap_vec.get v2 63)

let tests =
  let open Alcotest in
  [
    ( "Belnap_vec",
      [
        test_case "get/set" `Quick test_get_set;
        test_case "auto_grow" `Quick test_auto_grow;
        test_case "bulk_and" `Quick test_bulk_and;
        test_case "bulk_or" `Quick test_bulk_or;
        test_case "bulk_not" `Quick test_bulk_not;
        test_case "bulk_merge" `Quick test_bulk_merge;
        test_case "is_consistent" `Quick test_is_consistent;
        test_case "is_all_determined" `Quick test_is_all_determined;
        test_case "counts" `Quick test_counts;
        test_case "different_widths" `Quick test_different_widths;
        test_case "word_boundaries" `Quick test_word_boundaries;
        test_case "width_63" `Quick test_width_63;
      ] );
  ]

let () = Alcotest.run "Belnap_vec" tests
