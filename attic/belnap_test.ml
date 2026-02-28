open Attic

let u = Belnap.(of_view Unknown)
let t = Belnap.(of_view True)
let f = Belnap.(of_view False)
let b = Belnap.(of_view Both)

let check =
  let belnap = Alcotest.testable Belnap.pp Belnap.equal in
  Alcotest.(check belnap)

let check_bool = Alcotest.(check bool)

let test_not () =
  check "not U" u (Belnap.not u);
  check "not T" f (Belnap.not t);
  check "not F" t (Belnap.not f);
  check "not B" b (Belnap.not b)

let test_and () =
  Belnap.(
    check "U && U" u (u && u);
    check "U && T" u (u && t);
    check "U && F" f (u && f);
    check "U && B" f (u && b);
    check "T && U" u (t && u);
    check "T && T" t (t && t);
    check "T && F" f (t && f);
    check "T && B" b (t && b);
    check "F && U" f (f && u);
    check "F && T" f (f && t);
    check "F && F" f (f && f);
    check "F && B" f (f && b);
    check "B && U" f (b && u);
    check "B && T" b (b && t);
    check "B && F" f (b && f);
    check "B && B" b (b && b))

let test_or () =
  Belnap.(
    check "U || U" u (u || u);
    check "U || T" t (u || t);
    check "U || F" u (u || f);
    check "U || B" t (u || b);
    check "T || U" t (t || u);
    check "T || T" t (t || t);
    check "T || F" t (t || f);
    check "T || B" t (t || b);
    check "F || U" u (f || u);
    check "F || T" t (f || t);
    check "F || F" f (f || f);
    check "F || B" b (f || b);
    check "B || U" t (b || u);
    check "B || T" t (b || t);
    check "B || F" b (b || f);
    check "B || B" b (b || b))

let test_queries () =
  check_bool "is_known U" false (Belnap.is_known u);
  check_bool "is_known T" true (Belnap.is_known t);
  check_bool "is_known F" true (Belnap.is_known f);
  check_bool "is_known B" true (Belnap.is_known b);
  check_bool "is_determined U" false (Belnap.is_determined u);
  check_bool "is_determined T" true (Belnap.is_determined t);
  check_bool "is_determined F" true (Belnap.is_determined f);
  check_bool "is_determined B" false (Belnap.is_determined b);
  check_bool "is_contradicted U" false (Belnap.is_contradicted u);
  check_bool "is_contradicted T" false (Belnap.is_contradicted t);
  check_bool "is_contradicted F" false (Belnap.is_contradicted f);
  check_bool "is_contradicted B" true (Belnap.is_contradicted b)

let test_implies () =
  Belnap.(
    check "U implies U" u (implies u u);
    check "U implies T" t (implies u t);
    check "U implies F" u (implies u f);
    check "U implies B" t (implies u b);
    check "T implies U" u (implies t u);
    check "T implies T" t (implies t t);
    check "T implies F" f (implies t f);
    check "T implies B" b (implies t b);
    check "F implies U" t (implies f u);
    check "F implies T" t (implies f t);
    check "F implies F" t (implies f f);
    check "F implies B" t (implies f b);
    check "B implies U" t (implies b u);
    check "B implies T" t (implies b t);
    check "B implies F" b (implies b f);
    check "B implies B" b (implies b b))

let test_leq_truth () =
  (* Truth order: False < {Unknown, Both} < True; Unknown and Both incomparable *)
  let leq = Belnap.leq_truth in
  (* reflexive *)
  check_bool "U ≤_t U" true (leq u u);
  check_bool "T ≤_t T" true (leq t t);
  check_bool "F ≤_t F" true (leq f f);
  check_bool "B ≤_t B" true (leq b b);
  (* False is bottom *)
  check_bool "F ≤_t U" true (leq f u);
  check_bool "F ≤_t T" true (leq f t);
  check_bool "F ≤_t B" true (leq f b);
  (* True is top *)
  check_bool "U ≤_t T" true (leq u t);
  check_bool "B ≤_t T" true (leq b t);
  (* incomparable pairs *)
  check_bool "U ≤_t B" false (leq u b);
  check_bool "B ≤_t U" false (leq b u);
  (* strict: non-bottom not ≤ False *)
  check_bool "U ≤_t F" false (leq u f);
  check_bool "T ≤_t F" false (leq t f);
  check_bool "B ≤_t F" false (leq b f);
  (* strict: True not ≤ non-top *)
  check_bool "T ≤_t U" false (leq t u);
  check_bool "T ≤_t B" false (leq t b)

let test_leq_knowledge () =
  (* Knowledge order: Unknown < {True, False} < Both; True and False incomparable *)
  let leq = Belnap.leq_knowledge in
  (* reflexive *)
  check_bool "U ≤_k U" true (leq u u);
  check_bool "T ≤_k T" true (leq t t);
  check_bool "F ≤_k F" true (leq f f);
  check_bool "B ≤_k B" true (leq b b);
  (* Unknown is bottom *)
  check_bool "U ≤_k T" true (leq u t);
  check_bool "U ≤_k F" true (leq u f);
  check_bool "U ≤_k B" true (leq u b);
  (* Both is top *)
  check_bool "T ≤_k B" true (leq t b);
  check_bool "F ≤_k B" true (leq f b);
  (* incomparable pairs *)
  check_bool "T ≤_k F" false (leq t f);
  check_bool "F ≤_k T" false (leq f t);
  (* strict: non-bottom not ≤ Unknown *)
  check_bool "T ≤_k U" false (leq t u);
  check_bool "F ≤_k U" false (leq f u);
  check_bool "B ≤_k U" false (leq b u);
  (* strict: Both not ≤ non-top *)
  check_bool "B ≤_k T" false (leq b t);
  check_bool "B ≤_k F" false (leq b f)

let test_merge () =
  check "U merge U" u (Belnap.merge u u);
  check "U merge T" t (Belnap.merge u t);
  check "U merge F" f (Belnap.merge u f);
  check "U merge B" b (Belnap.merge u b);
  check "T merge U" t (Belnap.merge t u);
  check "T merge T" t (Belnap.merge t t);
  check "T merge F" b (Belnap.merge t f);
  check "T merge B" b (Belnap.merge t b);
  check "F merge U" f (Belnap.merge f u);
  check "F merge T" b (Belnap.merge f t);
  check "F merge F" f (Belnap.merge f f);
  check "F merge B" b (Belnap.merge f b);
  check "B merge U" b (Belnap.merge b u);
  check "B merge T" b (Belnap.merge b t);
  check "B merge F" b (Belnap.merge b f);
  check "B merge B" b (Belnap.merge b b)

let tests =
  let open Alcotest in
  [
    ( "Belnap",
      [
        test_case "not" `Quick test_not;
        test_case "and" `Quick test_and;
        test_case "or" `Quick test_or;
        test_case "implies" `Quick test_implies;
        test_case "merge" `Quick test_merge;
        test_case "queries" `Quick test_queries;
        test_case "leq_truth" `Quick test_leq_truth;
        test_case "leq_knowledge" `Quick test_leq_knowledge;
      ] );
  ]

let () = Alcotest.run "Belnap" tests
