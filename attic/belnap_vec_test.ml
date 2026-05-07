open Attic

let u = Belnap.(of_view Unknown)
let t = Belnap.(of_view True)
let f = Belnap.(of_view False)
let b = Belnap.(of_view Both)

(* TODO: replace body with Array.equal eq a b once we require OCaml >= 5.4 *)
let array_equal eq a b =
  let n = Array.length a in
  Array.length b = n
  &&
  let rec go i = i >= n || (eq a.(i) b.(i) && go (i + 1)) in
  go 0

let belnap_t : Belnap.t Alcotest.testable = (module Belnap)
let belnap_list = Alcotest.(list belnap_t)
let belnap_array = Alcotest.(array belnap_t)
let check = Alcotest.(check belnap_t)

(* Modules used by unit tests *)
module V0 = Belnap_vec.Make (struct
  let n = 0
end)

module V4 = Belnap_vec.Make (struct
  let n = 4
end)

module V8 = Belnap_vec.Make (struct
  let n = 8
end)

module V63 = Belnap_vec.Make (struct
  let n = 63
end)

module V64 = Belnap_vec.Make (struct
  let n = 64
end)

module V65 = Belnap_vec.Make (struct
  let n = 65
end)

module V100 = Belnap_vec.Make (struct
  let n = 100
end)

module V128 = Belnap_vec.Make (struct
  let n = 128
end)

let v64_t : V64.t Alcotest.testable = (module V64)
let v100_t : V100.t Alcotest.testable = (module V100)

let test_get_set () =
  let open V4 in
  let v = make () in
  set v (index_exn 0) u;
  set v (index_exn 1) t;
  set v (index_exn 2) f;
  set v (index_exn 3) b;
  check "get 0" u (get v (index_exn 0));
  check "get 1" t (get v (index_exn 1));
  check "get 2" f (get v (index_exn 2));
  check "get 3" b (get v (index_exn 3))

let test_bulk_and () =
  Alcotest.(check v64_t)
    "all_true && all_false = all_false"
    V64.(all_false ())
    V64.(all_true () && all_false ())

let test_bulk_or () =
  Alcotest.(check v64_t)
    "all_false || all_true = all_true"
    V64.(all_true ())
    V64.(all_false () || all_true ())

let test_bulk_not () =
  let open V100 in
  let r = not (all_true ()) in
  Alcotest.(check v100_t) "not all_true = all_false" (all_false ()) r;
  Alcotest.(check v100_t) "not not all_true = all_true" (all_true ()) (not r)

let test_bulk_merge () =
  let open V64 in
  let all_both = of_list (List.init 64 (Fun.const b)) in
  Alcotest.(check v64_t)
    "merge all_true all_false = all_both"
    all_both
    (merge (all_true ()) (all_false ()))

let test_bulk_consensus () =
  let open V64 in
  Alcotest.(check v64_t)
    "consensus all_true all_false = all_unknown"
    (make ())
    (consensus (all_true ()) (all_false ()))

let test_is_consistent () =
  let open V4 in
  let v = make () in
  set v (index_exn 0) t;
  set v (index_exn 1) f;
  set v (index_exn 2) u;
  set v (index_exn 3) b;
  Alcotest.(check bool) "with Both is not consistent" false (is_consistent v);
  let v2 = make () in
  set v2 (index_exn 0) t;
  set v2 (index_exn 1) f;
  set v2 (index_exn 2) u;
  set v2 (index_exn 3) t;
  Alcotest.(check bool) "without Both is consistent" true (is_consistent v2)

let test_is_all_determined () =
  let open V4 in
  let v = make () in
  set v (index_exn 0) t;
  set v (index_exn 1) f;
  set v (index_exn 2) t;
  set v (index_exn 3) f;
  Alcotest.(check bool) "true/false is_all_determined" true (is_all_determined v);
  let v2 = make () in
  set v2 (index_exn 0) t;
  set v2 (index_exn 1) f;
  set v2 (index_exn 2) u;
  set v2 (index_exn 3) f;
  Alcotest.(check bool) "with Unknown not is_all_determined" false (is_all_determined v2);
  let v3 = make () in
  set v3 (index_exn 0) t;
  set v3 (index_exn 1) f;
  set v3 (index_exn 2) b;
  set v3 (index_exn 3) f;
  Alcotest.(check bool) "with Both not is_all_determined" false (is_all_determined v3)

let test_counts () =
  let open V8 in
  let v = make () in
  set v (index_exn 0) t;
  set v (index_exn 1) t;
  set v (index_exn 2) f;
  set v (index_exn 3) f;
  set v (index_exn 4) b;
  set v (index_exn 5) b;
  set v (index_exn 6) u;
  set v (index_exn 7) u;
  Alcotest.(check int) "count_true" 2 (count_true v);
  Alcotest.(check int) "count_false" 2 (count_false v);
  Alcotest.(check int) "count_both" 2 (count_both v);
  Alcotest.(check int) "count_unknown" 2 (count_unknown v)

let test_word_boundaries () =
  (* Element 63: bit 63 (sign bit) of word-pair 0 *)
  let open V65 in
  let v = make () in
  set v (index_exn 63) b;
  check "get 63 is Both" b (get v (index_exn 63));
  check "get 62 is Unknown" u (get v (index_exn 62));
  check "get 64 is Unknown" u (get v (index_exn 64));
  (* Element 64: bit 0 of word-pair 1 *)
  set v (index_exn 64) t;
  check "get 64 is True" t (get v (index_exn 64));
  check "get 63 still Both" b (get v (index_exn 63))

let test_width_63 () =
  (* width=63 exercises r=63 in bv_mask_tail and tail_mask *)
  let open V63 in
  let v = all_true () in
  Alcotest.(check bool) "is_all_true" true (is_all_true v);
  Alcotest.(check bool) "is_all_determined" true (is_all_determined v);
  Alcotest.(check bool) "is_consistent" true (is_consistent v);
  check "get 62 is True" t (get v (index_exn 62));
  Alcotest.(check int) "count_both after merge" 63 (count_both (merge v (all_false ())))

let test_to_list_roundtrip () =
  Alcotest.(check belnap_list) "empty" [] V0.(to_list (make ()));
  let xs = [ u; t; f; b ] in
  Alcotest.(check belnap_list) "4 elems" xs V4.(to_list (of_list xs));
  (* 64 elements exercises exactly one full word-pair *)
  Alcotest.(check belnap_list) "64 elems" (List.init 64 (Fun.const t)) V64.(to_list (all_true ()));
  (* 65 elements: last element is in word-pair 1, bit 0 *)
  let xs65 = List.init 65 (fun i -> if i = 64 then f else t) in
  Alcotest.(check belnap_list) "65 elems" xs65 V65.(to_list (of_list xs65))

let test_to_array_roundtrip () =
  Alcotest.(check belnap_array) "empty" [||] V0.(to_array (make ()));
  let xs = [| u; t; f; b |] in
  Alcotest.(check belnap_array) "4 elems" xs V4.(to_array (of_list (Array.to_list xs)))

let test_of_array_roundtrip () =
  Alcotest.(check belnap_array) "empty" [||] V0.(to_array (of_array [||]));
  let xs = [| u; t; f; b |] in
  Alcotest.(check belnap_array) "4 elems" xs V4.(to_array (of_array xs));
  (* word boundary: 65 elements, last in word-pair 1 *)
  let xs65 = Array.init 65 (fun i -> if i = 64 then b else f) in
  Alcotest.(check belnap_array) "65 elems" xs65 V65.(to_array (of_array xs65))

let test_find_first () =
  let chk = Alcotest.(check (option int)) in
  let v = V4.of_list [ f; f; t; b ] in
  chk "first true" (Some 2) V4.(find_first t v);
  chk "first false" (Some 0) V4.(find_first f v);
  chk "first both" (Some 3) V4.(find_first b v);
  chk "no unknown" None V4.(find_first u v);
  chk "empty" None V0.(find_first t (make ()));
  (* first match at word boundary (index 64, word-pair 1) *)
  let v2 = V65.of_list (List.init 65 (fun i -> if i = 64 then t else f)) in
  chk "word boundary idx 64" (Some 64) V65.(find_first t v2);
  (* all_true 128: first in first word-pair *)
  chk "all_true first" (Some 0) V128.(find_first t (all_true ()))

let tests =
  let open Alcotest in
  [
    ( "Belnap_vec",
      [
        test_case "get/set" `Quick test_get_set;
        test_case "bulk_and" `Quick test_bulk_and;
        test_case "bulk_or" `Quick test_bulk_or;
        test_case "bulk_not" `Quick test_bulk_not;
        test_case "bulk_merge" `Quick test_bulk_merge;
        test_case "bulk_consensus" `Quick test_bulk_consensus;
        test_case "is_consistent" `Quick test_is_consistent;
        test_case "is_all_determined" `Quick test_is_all_determined;
        test_case "counts" `Quick test_counts;
        test_case "word_boundaries" `Quick test_word_boundaries;
        test_case "width_63" `Quick test_width_63;
        test_case "to_list_roundtrip" `Quick test_to_list_roundtrip;
        test_case "to_array_roundtrip" `Quick test_to_array_roundtrip;
        test_case "of_array_roundtrip" `Quick test_of_array_roundtrip;
        test_case "find_first" `Quick test_find_first;
      ] );
  ]

(* --- QCheck2 generators and property tests with shrinkable width --- *)

let gen_belnap : Belnap.t QCheck2.Gen.t = QCheck2.Gen.oneof_list [ u; t; f; b ]

(* [make_size n] packages [n] as a first-class SIZE module. *)
let make_size n =
  (module struct
    let n = n
  end : Belnap_vec.SIZE)

(* Each generator yields a SIZE module alongside Belnap.t list(s) of exactly
   S.n elements, all tied to the same randomly chosen [n].  When a property
   fails QCheck can shrink [n] to find the minimum failing width. *)
let gen_n = QCheck2.Gen.int_range 0 200

let gen_s1 =
  QCheck2.Gen.(
    bind gen_n (fun n -> map (fun xs -> (make_size n, xs)) (list_size (return n) gen_belnap)))

let gen_s2 =
  QCheck2.Gen.(
    bind gen_n (fun n ->
        let g = list_size (return n) gen_belnap in
        map (fun (xs, ys) -> (make_size n, xs, ys)) (pair g g)))

let gen_s3 =
  QCheck2.Gen.(
    bind gen_n (fun n ->
        let g = list_size (return n) gen_belnap in
        map (fun (xs, ys, zs) -> (make_size n, xs, ys, zs)) (triple g g g)))

(* n >= 1 so that a valid index exists *)
let gen_s_get_set =
  QCheck2.Gen.(
    bind (int_range 1 200) (fun n ->
        map
          (fun (xs, i, v) -> (make_size n, xs, i, v))
          (triple (list_size (return n) gen_belnap) (int_range 0 (n - 1)) gen_belnap)))

(* --- Print helpers --- *)
let pp_blist xs = Format.asprintf "%a" Fmt.(brackets (list ~sep:semi Belnap.pp)) xs
let print_s1 ((module S : Belnap_vec.SIZE), xs) = Printf.sprintf "n=%d xs=%s" S.n (pp_blist xs)

let print_s2 ((module S : Belnap_vec.SIZE), xs, ys) =
  Printf.sprintf "n=%d xs=%s ys=%s" S.n (pp_blist xs) (pp_blist ys)

let print_s3 ((module S : Belnap_vec.SIZE), xs, ys, zs) =
  Printf.sprintf "n=%d xs=%s ys=%s zs=%s" S.n (pp_blist xs) (pp_blist ys) (pp_blist zs)

let print_needle_s1 (needle, s1) = Format.asprintf "needle=%a %s" Belnap.pp needle (print_s1 s1)
let gen_needle_s1 = QCheck2.Gen.(pair gen_belnap gen_s1)

(* --- QCheck2 property tests --- *)

(* Truth-order lattice laws (|| and &&) *)

let or_commutativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys in
    equal (a || b) (b || a)
  in
  QCheck2.Test.make ~name:"|| is commutative" ~print:print_s2 gen_s2 body

let or_associativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys, zs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys and c = of_list zs in
    equal ((a || b) || c) (a || b || c)
  in
  QCheck2.Test.make ~name:"|| is associative" ~print:print_s3 gen_s3 body

let or_idempotency =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs in
    equal (a || a) a
  in
  QCheck2.Test.make ~name:"|| is idempotent" ~print:print_s1 gen_s1 body

let and_commutativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys in
    equal (a && b) (b && a)
  in
  QCheck2.Test.make ~name:"&& is commutative" ~print:print_s2 gen_s2 body

let and_associativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys, zs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys and c = of_list zs in
    equal ((a && b) && c) (a && b && c)
  in
  QCheck2.Test.make ~name:"&& is associative" ~print:print_s3 gen_s3 body

let and_idempotency =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs in
    equal (a && a) a
  in
  QCheck2.Test.make ~name:"&& is idempotent" ~print:print_s1 gen_s1 body

let absorption_or_and =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys in
    equal (a || (a && b)) a
  in
  QCheck2.Test.make ~name:"a || (a && b) = a" ~print:print_s2 gen_s2 body

let absorption_and_or =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys in
    equal (a && (a || b)) a
  in
  QCheck2.Test.make ~name:"a && (a || b) = a" ~print:print_s2 gen_s2 body

let and_distributes_over_or =
  let body ((module S : Belnap_vec.SIZE), xs, ys, zs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys and c = of_list zs in
    equal (a && (b || c)) ((a && b) || (a && c))
  in
  QCheck2.Test.make ~name:"a && (b || c) = (a && b) || (a && c)" ~print:print_s3 gen_s3 body

let or_distributes_over_and =
  let body ((module S : Belnap_vec.SIZE), xs, ys, zs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys and c = of_list zs in
    equal (a || (b && c)) ((a || b) && (a || c))
  in
  QCheck2.Test.make ~name:"a || (b && c) = (a || b) && (a || c)" ~print:print_s3 gen_s3 body

let or_bottom_identity =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs in
    equal (a || all_false ()) a
  in
  QCheck2.Test.make ~name:"all_false is identity for ||" ~print:print_s1 gen_s1 body

let and_top_identity =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs in
    equal (a && all_true ()) a
  in
  QCheck2.Test.make ~name:"all_true is identity for &&" ~print:print_s1 gen_s1 body

let or_top_annihilator =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs in
    equal (a || all_true ()) (all_true ())
  in
  QCheck2.Test.make ~name:"all_true is annihilator for ||" ~print:print_s1 gen_s1 body

let and_bottom_annihilator =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs in
    equal (a && all_false ()) (all_false ())
  in
  QCheck2.Test.make ~name:"all_false is annihilator for &&" ~print:print_s1 gen_s1 body

(* Implication *)

let implies_definition =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys in
    equal (implies a b) ((not a) || b)
  in
  QCheck2.Test.make ~name:"implies a b = not a || b" ~print:print_s2 gen_s2 body

(* Knowledge-order join semilattice laws (merge) *)

let merge_commutativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys in
    equal (merge a b) (merge b a)
  in
  QCheck2.Test.make ~name:"merge is commutative" ~print:print_s2 gen_s2 body

let merge_associativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys, zs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys and c = of_list zs in
    equal (merge (merge a b) c) (merge a (merge b c))
  in
  QCheck2.Test.make ~name:"merge is associative" ~print:print_s3 gen_s3 body

let merge_idempotency =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs in
    equal (merge a a) a
  in
  QCheck2.Test.make ~name:"merge is idempotent" ~print:print_s1 gen_s1 body

let merge_bottom_identity =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs in
    equal (merge (make ()) a) a
  in
  QCheck2.Test.make ~name:"all_unknown is identity for merge" ~print:print_s1 gen_s1 body

(* Knowledge-order meet semilattice laws (consensus) *)

let consensus_commutativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys in
    equal (consensus a b) (consensus b a)
  in
  QCheck2.Test.make ~name:"consensus is commutative" ~print:print_s2 gen_s2 body

let consensus_associativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys, zs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys and c = of_list zs in
    equal (consensus (consensus a b) c) (consensus a (consensus b c))
  in
  QCheck2.Test.make ~name:"consensus is associative" ~print:print_s3 gen_s3 body

let consensus_idempotency =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs in
    equal (consensus a a) a
  in
  QCheck2.Test.make ~name:"consensus is idempotent" ~print:print_s1 gen_s1 body

let consensus_top_identity =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs in
    equal (consensus a (all_both ())) a
  in
  QCheck2.Test.make ~name:"all_both is identity for consensus" ~print:print_s1 gen_s1 body

(* Auxiliary consistency properties *)

let count_nonneg =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    M.count_true v >= 0 && M.count_false v >= 0 && M.count_both v >= 0 && M.count_unknown v >= 0
  in
  QCheck2.Test.make ~name:"counts are non-negative" ~print:print_s1 gen_s1 body

let counts_sum_to_n =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    M.count_true v + M.count_false v + M.count_both v + M.count_unknown v = S.n
  in
  QCheck2.Test.make ~name:"counts sum to n" ~print:print_s1 gen_s1 body

let not_involutive =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let v = of_list xs in
    equal (not (not v)) v
  in
  QCheck2.Test.make ~name:"not is involutive" ~print:print_s1 gen_s1 body

let de_morgan_and =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys in
    equal (not (a && b)) ((not a) || not b)
  in
  QCheck2.Test.make ~name:"not (a && b) = not a || not b" ~print:print_s2 gen_s2 body

let de_morgan_or =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let a = of_list xs and b = of_list ys in
    equal (not (a || b)) ((not a) && not b)
  in
  QCheck2.Test.make ~name:"not (a || b) = not a && not b" ~print:print_s2 gen_s2 body

let is_consistent_iff_no_both =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let v = of_list xs in
    is_consistent v = (count_both v = 0)
  in
  QCheck2.Test.make ~name:"is_consistent iff count_both = 0" ~print:print_s1 gen_s1 body

let is_all_determined_iff =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    M.is_all_determined v = (M.count_unknown v = 0 && M.count_both v = 0)
  in
  QCheck2.Test.make ~name:"is_all_determined iff no unknown or both" ~print:print_s1 gen_s1 body

let is_all_true_iff =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let v = of_list xs in
    is_all_true v = (count_true v = S.n)
  in
  QCheck2.Test.make ~name:"is_all_true iff count_true = n" ~print:print_s1 gen_s1 body

let is_all_false_iff =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let v = of_list xs in
    is_all_false v = (count_false v = S.n)
  in
  QCheck2.Test.make ~name:"is_all_false iff count_false = n" ~print:print_s1 gen_s1 body

(* to_list / to_array *)

let of_list_to_list_roundtrip =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    List.equal Belnap.equal (to_list (of_list xs)) xs
  in
  QCheck2.Test.make ~name:"of_list/to_list roundtrip" ~print:print_s1 gen_s1 body

let of_array_to_array_roundtrip =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let arr = Array.of_list xs in
    array_equal Belnap.equal (to_array (of_array arr)) arr
  in
  QCheck2.Test.make ~name:"of_array/to_array roundtrip" ~print:print_s1 gen_s1 body

let of_array_of_list_consistent =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    equal (of_list xs) (of_array (Array.of_list xs))
  in
  QCheck2.Test.make ~name:"of_array and of_list agree" ~print:print_s1 gen_s1 body

let to_list_matches_get =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let v = of_list xs in
    List.equal Belnap.equal (to_list v) (List.init S.n (fun i -> get v (index_exn i)))
  in
  QCheck2.Test.make ~name:"to_list matches sequential get" ~print:print_s1 gen_s1 body

(* find_first correctness *)

let find_first_returns_correct_value =
  let body (needle, ((module S : Belnap_vec.SIZE), xs)) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let v = of_list xs in
    match find_first needle v with
    | None -> true
    | Some i -> Belnap.equal (get v (index_exn i)) needle
  in
  QCheck2.Test.make
    ~name:"find_first returns element matching needle"
    ~print:print_needle_s1
    gen_needle_s1
    body

let no_earlier_match (type a) (module M : Belnap_vec.S with type t = a) (needle : Belnap.t) (v : a)
    (i : int) : bool =
  let rec go j = j >= i || ((not (Belnap.equal M.(get v (index_exn j)) needle)) && go (j + 1)) in
  go 0

let find_first_is_minimum =
  let body (needle, ((module S : Belnap_vec.SIZE), xs)) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    match M.(find_first needle v) with
    | None -> true
    | Some i -> no_earlier_match (module M) needle v i
  in
  QCheck2.Test.make
    ~name:"find_first returns leftmost match"
    ~print:print_needle_s1
    gen_needle_s1
    body

let find_first_none_iff_count_zero =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let v = of_list xs in
    List.for_all
      (fun (result, count) -> Option.is_none result = (count = 0))
      [
        (find_first t v, count_true v);
        (find_first f v, count_false v);
        (find_first b v, count_both v);
        (find_first u v, count_unknown v);
      ]
  in
  QCheck2.Test.make ~name:"find_first returns None iff count = 0" ~print:print_s1 gen_s1 body

(* Get/set roundtrip — n >= 1 so a valid index exists *)

let get_set_roundtrip =
  let print ((module S : Belnap_vec.SIZE), xs, i, _) =
    Printf.sprintf "n=%d i=%d xs=%s" S.n i (pp_blist xs)
  in
  let body ((module S : Belnap_vec.SIZE), xs, i, v) =
    let module M = Belnap_vec.Make (S) in
    let open M in
    let vec = of_list xs in
    set vec (index_exn i) v;
    Belnap.equal (get vec (index_exn i)) v
  in
  QCheck2.Test.make ~name:"get after set returns stored value" ~print gen_s_get_set body

let qcheck_tests =
  [
    or_commutativity;
    or_associativity;
    or_idempotency;
    and_commutativity;
    and_associativity;
    and_idempotency;
    absorption_or_and;
    absorption_and_or;
    and_distributes_over_or;
    or_distributes_over_and;
    or_bottom_identity;
    and_top_identity;
    or_top_annihilator;
    and_bottom_annihilator;
    implies_definition;
    merge_commutativity;
    merge_associativity;
    merge_idempotency;
    merge_bottom_identity;
    consensus_commutativity;
    consensus_associativity;
    consensus_idempotency;
    consensus_top_identity;
    count_nonneg;
    counts_sum_to_n;
    not_involutive;
    de_morgan_and;
    de_morgan_or;
    is_consistent_iff_no_both;
    is_all_determined_iff;
    is_all_true_iff;
    is_all_false_iff;
    of_list_to_list_roundtrip;
    of_array_to_array_roundtrip;
    of_array_of_list_consistent;
    to_list_matches_get;
    find_first_returns_correct_value;
    find_first_is_minimum;
    find_first_none_iff_count_zero;
    get_set_roundtrip;
  ]

let () =
  Alcotest.run
    "Belnap_vec"
    (tests @ [ ("QCheck", List.map QCheck_alcotest.to_alcotest qcheck_tests) ])
