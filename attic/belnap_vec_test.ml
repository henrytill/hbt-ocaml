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

let belnap_t = Alcotest.testable Belnap.pp Belnap.equal
let belnap_list = Alcotest.(list belnap_t)
let belnap_array = Alcotest.(array belnap_t)

let test_to_list_roundtrip () =
  Alcotest.(check belnap_list) "empty" [] (Belnap_vec.to_list (Belnap_vec.of_list []));
  let xs = [ u; t; f; b ] in
  Alcotest.(check belnap_list) "4 elems" xs (Belnap_vec.to_list (Belnap_vec.of_list xs));
  (* 64 elements exercises exactly one full word-pair *)
  let xs64 = List.init 64 (fun _ -> t) in
  Alcotest.(check belnap_list) "64 elems" xs64 (Belnap_vec.to_list (Belnap_vec.all_true 64));
  (* 65 elements: last element is in word-pair 1, bit 0 *)
  let xs65 = List.init 65 (fun i -> if i = 64 then f else t) in
  Alcotest.(check belnap_list) "65 elems" xs65 (Belnap_vec.to_list (Belnap_vec.of_list xs65))

let test_to_array_roundtrip () =
  Alcotest.(check belnap_array) "empty" [||] (Belnap_vec.to_array (Belnap_vec.of_list []));
  let xs = [| u; t; f; b |] in
  Alcotest.(check belnap_array)
    "4 elems"
    xs
    (Belnap_vec.to_array (Belnap_vec.of_list (Array.to_list xs)))

let test_of_array_roundtrip () =
  Alcotest.(check belnap_array) "empty" [||] (Belnap_vec.to_array (Belnap_vec.of_array [||]));
  let xs = [| u; t; f; b |] in
  Alcotest.(check belnap_array) "4 elems" xs (Belnap_vec.to_array (Belnap_vec.of_array xs));
  (* word boundary: 65 elements, last in word-pair 1 *)
  let xs65 = Array.init 65 (fun i -> if i = 64 then b else f) in
  Alcotest.(check belnap_array) "65 elems" xs65 (Belnap_vec.to_array (Belnap_vec.of_array xs65))

let test_find_first () =
  let chk = Alcotest.(check (option int)) in
  let v = Belnap_vec.of_list [ f; f; t; b ] in
  chk "first true" (Some 2) (Belnap_vec.find_first t v);
  chk "first false" (Some 0) (Belnap_vec.find_first f v);
  chk "first both" (Some 3) (Belnap_vec.find_first b v);
  chk "no unknown" None (Belnap_vec.find_first u v);
  chk "empty" None (Belnap_vec.find_first t (Belnap_vec.make 0));
  (* first match at word boundary (index 64, word-pair 1) *)
  let v2 = Belnap_vec.of_list (List.init 65 (fun i -> if i = 64 then t else f)) in
  chk "word boundary idx 64" (Some 64) (Belnap_vec.find_first t v2);
  (* all_true 128: first in first word-pair *)
  chk "all_true first" (Some 0) (Belnap_vec.find_first t (Belnap_vec.all_true 128))

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
        test_case "to_list_roundtrip" `Quick test_to_list_roundtrip;
        test_case "to_array_roundtrip" `Quick test_to_array_roundtrip;
        test_case "of_array_roundtrip" `Quick test_of_array_roundtrip;
        test_case "find_first" `Quick test_find_first;
      ] );
  ]

(* --- QCheck2 generators --- *)
let gen_belnap : Belnap.t QCheck2.Gen.t =
  QCheck2.Gen.oneof
    [
      QCheck2.Gen.return (Belnap.of_view Belnap.Unknown);
      QCheck2.Gen.return (Belnap.of_view Belnap.True);
      QCheck2.Gen.return (Belnap.of_view Belnap.False);
      QCheck2.Gen.return (Belnap.of_view Belnap.Both);
    ]

let gen_belnap_vec_of_width n =
  QCheck2.Gen.(map Belnap_vec.of_list (list_size (return n) gen_belnap))

let gen_single = QCheck2.Gen.(bind (int_range 0 200) gen_belnap_vec_of_width)

let gen_pair =
  QCheck2.Gen.(
    bind (int_range 0 200) (fun n ->
        let gvn = gen_belnap_vec_of_width n in
        bind gvn (fun a -> map (fun b -> (a, b)) gvn)))

let gen_triple =
  QCheck2.Gen.(
    bind (int_range 0 200) (fun n ->
        let gvn = gen_belnap_vec_of_width n in
        bind gvn (fun a -> bind gvn (fun b -> map (fun c -> (a, b, c)) gvn))))

let gen_get_set =
  QCheck2.Gen.(
    bind (int_range 1 200) (fun n ->
        bind
          (int_range 0 (n - 1))
          (fun i ->
            bind gen_belnap (fun v -> map (fun vec -> (vec, i, v)) (gen_belnap_vec_of_width n)))))

(* --- Print helpers --- *)
let print_belnap_vec v =
  let w = Belnap_vec.width v in
  if w > 20 then
    Printf.sprintf "width=%d" w
  else
    String.init (w + 2) (fun i ->
        if i = 0 then
          '['
        else if i = w + 1 then
          ']'
        else
          match Belnap.to_view (Belnap_vec.get v (i - 1)) with
          | Belnap.Unknown -> 'U'
          | Belnap.True -> 'T'
          | Belnap.False -> 'F'
          | Belnap.Both -> 'B')

let print_pair (a, b) = Printf.sprintf "(w=%d, w=%d)" (Belnap_vec.width a) (Belnap_vec.width b)

let print_triple (a, b, c) =
  Printf.sprintf "(w=%d, w=%d, w=%d)" (Belnap_vec.width a) (Belnap_vec.width b) (Belnap_vec.width c)

(* --- QCheck2 property tests --- *)
let qcheck_tests =
  [
    (* Truth-order lattice laws (|| and &&) *)
    QCheck2.Test.make ~name:"or_commutativity" ~print:print_pair gen_pair (fun (a, b) ->
        Belnap_vec.equal (Belnap_vec.( || ) a b) (Belnap_vec.( || ) b a));
    QCheck2.Test.make ~name:"or_associativity" ~print:print_triple gen_triple (fun (a, b, c) ->
        Belnap_vec.equal
          (Belnap_vec.( || ) (Belnap_vec.( || ) a b) c)
          (Belnap_vec.( || ) a (Belnap_vec.( || ) b c)));
    QCheck2.Test.make ~name:"or_idempotency" ~print:print_belnap_vec gen_single (fun a ->
        Belnap_vec.equal (Belnap_vec.( || ) a a) a);
    QCheck2.Test.make ~name:"and_commutativity" ~print:print_pair gen_pair (fun (a, b) ->
        Belnap_vec.equal (Belnap_vec.( && ) a b) (Belnap_vec.( && ) b a));
    QCheck2.Test.make ~name:"and_associativity" ~print:print_triple gen_triple (fun (a, b, c) ->
        Belnap_vec.equal
          (Belnap_vec.( && ) (Belnap_vec.( && ) a b) c)
          (Belnap_vec.( && ) a (Belnap_vec.( && ) b c)));
    QCheck2.Test.make ~name:"and_idempotency" ~print:print_belnap_vec gen_single (fun a ->
        Belnap_vec.equal (Belnap_vec.( && ) a a) a);
    QCheck2.Test.make ~name:"absorption_or_and" ~print:print_pair gen_pair (fun (a, b) ->
        Belnap_vec.equal (Belnap_vec.( || ) a (Belnap_vec.( && ) a b)) a);
    QCheck2.Test.make ~name:"absorption_and_or" ~print:print_pair gen_pair (fun (a, b) ->
        Belnap_vec.equal (Belnap_vec.( && ) a (Belnap_vec.( || ) a b)) a);
    QCheck2.Test.make ~name:"or_bottom_identity" ~print:print_belnap_vec gen_single (fun a ->
        let n = Belnap_vec.width a in
        Belnap_vec.equal (Belnap_vec.( || ) a (Belnap_vec.all_false n)) a);
    QCheck2.Test.make ~name:"and_top_identity" ~print:print_belnap_vec gen_single (fun a ->
        let n = Belnap_vec.width a in
        Belnap_vec.equal (Belnap_vec.( && ) a (Belnap_vec.all_true n)) a);
    (* Knowledge-order join semilattice laws (merge) *)
    QCheck2.Test.make ~name:"merge_commutativity" ~print:print_pair gen_pair (fun (a, b) ->
        Belnap_vec.equal (Belnap_vec.merge a b) (Belnap_vec.merge b a));
    QCheck2.Test.make ~name:"merge_associativity" ~print:print_triple gen_triple (fun (a, b, c) ->
        Belnap_vec.equal
          (Belnap_vec.merge (Belnap_vec.merge a b) c)
          (Belnap_vec.merge a (Belnap_vec.merge b c)));
    QCheck2.Test.make ~name:"merge_idempotency" ~print:print_belnap_vec gen_single (fun a ->
        Belnap_vec.equal (Belnap_vec.merge a a) a);
    QCheck2.Test.make ~name:"merge_bottom_identity" ~print:print_belnap_vec gen_single (fun a ->
        let n = Belnap_vec.width a in
        Belnap_vec.equal (Belnap_vec.merge (Belnap_vec.make n) a) a);
    (* Auxiliary consistency properties *)
    QCheck2.Test.make ~name:"count_nonneg" ~print:print_belnap_vec gen_single (fun v ->
        Belnap_vec.count_true v >= 0
        && Belnap_vec.count_false v >= 0
        && Belnap_vec.count_both v >= 0
        && Belnap_vec.count_unknown v >= 0);
    QCheck2.Test.make ~name:"not_involutive" ~print:print_belnap_vec gen_single (fun v ->
        Belnap_vec.equal (Belnap_vec.not (Belnap_vec.not v)) v);
    QCheck2.Test.make ~name:"is_consistent_iff_no_both" ~print:print_belnap_vec gen_single (fun v ->
        Belnap_vec.is_consistent v = (Belnap_vec.count_both v = 0));
    QCheck2.Test.make ~name:"is_all_determined_iff" ~print:print_belnap_vec gen_single (fun v ->
        Belnap_vec.is_all_determined v
        = (Belnap_vec.count_unknown v = 0 && Belnap_vec.count_both v = 0));
    QCheck2.Test.make ~name:"is_all_true_iff" ~print:print_belnap_vec gen_single (fun v ->
        Belnap_vec.is_all_true v = (Belnap_vec.count_true v = Belnap_vec.width v));
    QCheck2.Test.make ~name:"is_all_false_iff" ~print:print_belnap_vec gen_single (fun v ->
        Belnap_vec.is_all_false v = (Belnap_vec.count_false v = Belnap_vec.width v));
    (* Get/set roundtrip *)
    QCheck2.Test.make
      ~name:"get_set_roundtrip"
      ~print:(fun (vec, i, _) -> Printf.sprintf "w=%d i=%d" (Belnap_vec.width vec) i)
      gen_get_set
      (fun (vec, i, v) ->
        Belnap_vec.set vec i v;
        Belnap.equal (Belnap_vec.get vec i) v);
    (* to_list / to_array *)
    QCheck2.Test.make
      ~name:"of_list_to_list_roundtrip"
      ~print:(fun xs -> Printf.sprintf "len=%d" (List.length xs))
      QCheck2.Gen.(list gen_belnap)
      (fun xs -> List.equal Belnap.equal (Belnap_vec.to_list (Belnap_vec.of_list xs)) xs);
    (* TODO: Array.equal is available from OCaml 5.4; replace the Array.to_list
       conversions below with Array.equal Belnap.equal once we require >= 5.4. *)
    QCheck2.Test.make
      ~name:"of_array_to_array_roundtrip"
      ~print:(fun arr -> Printf.sprintf "len=%d" (Array.length arr))
      QCheck2.Gen.(array gen_belnap)
      (fun arr ->
        List.equal
          Belnap.equal
          (Array.to_list (Belnap_vec.to_array (Belnap_vec.of_array arr)))
          (Array.to_list arr));
    QCheck2.Test.make
      ~name:"of_array_of_list_consistent"
      ~print:(fun xs -> Printf.sprintf "len=%d" (List.length xs))
      QCheck2.Gen.(list gen_belnap)
      (fun xs -> Belnap_vec.equal (Belnap_vec.of_array (Array.of_list xs)) (Belnap_vec.of_list xs));
    QCheck2.Test.make ~name:"to_list_matches_get" ~print:print_belnap_vec gen_single (fun v ->
        let lst = Belnap_vec.to_list v in
        let expected = List.init (Belnap_vec.width v) (Belnap_vec.get v) in
        List.equal Belnap.equal lst expected);
    (* find_first correctness *)
    QCheck2.Test.make
      ~name:"find_first_returns_correct_value"
      ~print:(fun (_, v) -> print_belnap_vec v)
      QCheck2.Gen.(pair gen_belnap gen_single)
      (fun (needle, v) ->
        match Belnap_vec.find_first needle v with
        | None -> true
        | Some i -> Belnap.equal (Belnap_vec.get v i) needle);
    QCheck2.Test.make
      ~name:"find_first_is_minimum"
      ~print:(fun (_, v) -> print_belnap_vec v)
      QCheck2.Gen.(pair gen_belnap gen_single)
      (fun (needle, v) ->
        match Belnap_vec.find_first needle v with
        | None -> true
        | Some i ->
            let rec all_before j =
              j >= i || ((not (Belnap.equal (Belnap_vec.get v j) needle)) && all_before (j + 1))
            in
            all_before 0);
    QCheck2.Test.make
      ~name:"find_first_none_iff_count_zero"
      ~print:print_belnap_vec
      gen_single
      (fun v ->
        Belnap_vec.find_first t v = None = (Belnap_vec.count_true v = 0)
        && Belnap_vec.find_first f v = None = (Belnap_vec.count_false v = 0)
        && Belnap_vec.find_first b v = None = (Belnap_vec.count_both v = 0)
        && Belnap_vec.find_first u v = None = (Belnap_vec.count_unknown v = 0));
  ]

let () =
  Alcotest.run
    "Belnap_vec"
    (tests @ [ ("QCheck", List.map QCheck_alcotest.to_alcotest qcheck_tests) ])
