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

let belnap_t = Alcotest.testable Belnap.pp Belnap.equal
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

(* Convenience shorthands for index_exn *)
let v4i = V4.index_exn
let v8i = V8.index_exn
let v63i = V63.index_exn
let v65i = V65.index_exn

(* Per-module Alcotest testable helpers *)
let v64_t = Alcotest.testable V64.pp V64.equal
let v100_t = Alcotest.testable V100.pp V100.equal

let test_get_set () =
  let v = V4.make () in
  V4.set v (v4i 0) u;
  V4.set v (v4i 1) t;
  V4.set v (v4i 2) f;
  V4.set v (v4i 3) b;
  check "get 0" u (V4.get v (v4i 0));
  check "get 1" t (V4.get v (v4i 1));
  check "get 2" f (V4.get v (v4i 2));
  check "get 3" b (V4.get v (v4i 3))

let test_bulk_and () =
  let a = V64.all_true () in
  let c = V64.all_false () in
  let r = V64.( && ) a c in
  Alcotest.(check v64_t) "all_true && all_false = all_false" (V64.all_false ()) r

let test_bulk_or () =
  let a = V64.all_false () in
  let c = V64.all_true () in
  let r = V64.( || ) a c in
  Alcotest.(check v64_t) "all_false || all_true = all_true" (V64.all_true ()) r

let test_bulk_not () =
  let a = V100.all_true () in
  let r = V100.not a in
  Alcotest.(check v100_t) "not all_true = all_false" (V100.all_false ()) r;
  let rr = V100.not r in
  Alcotest.(check v100_t) "not not all_true = all_true" (V100.all_true ()) rr

let test_bulk_merge () =
  let a = V64.all_true () in
  let c = V64.all_false () in
  let r = V64.merge a c in
  let all_both = V64.of_list (List.init 64 (fun _ -> Belnap.(of_view Both))) in
  Alcotest.(check v64_t) "merge all_true all_false = all_both" all_both r

let test_bulk_consensus () =
  let a = V64.all_true () in
  let c = V64.all_false () in
  let r = V64.consensus a c in
  let all_unknown = V64.make () in
  Alcotest.(check v64_t) "consensus all_true all_false = all_unknown" all_unknown r

let test_is_consistent () =
  let v = V4.make () in
  V4.set v (v4i 0) t;
  V4.set v (v4i 1) f;
  V4.set v (v4i 2) u;
  V4.set v (v4i 3) b;
  Alcotest.(check bool) "with Both is not consistent" false (V4.is_consistent v);
  let v2 = V4.make () in
  V4.set v2 (v4i 0) t;
  V4.set v2 (v4i 1) f;
  V4.set v2 (v4i 2) u;
  V4.set v2 (v4i 3) t;
  Alcotest.(check bool) "without Both is consistent" true (V4.is_consistent v2)

let test_is_all_determined () =
  let v = V4.make () in
  V4.set v (v4i 0) t;
  V4.set v (v4i 1) f;
  V4.set v (v4i 2) t;
  V4.set v (v4i 3) f;
  Alcotest.(check bool) "true/false is_all_determined" true (V4.is_all_determined v);
  let v2 = V4.make () in
  V4.set v2 (v4i 0) t;
  V4.set v2 (v4i 1) f;
  V4.set v2 (v4i 2) u;
  V4.set v2 (v4i 3) f;
  Alcotest.(check bool) "with Unknown not is_all_determined" false (V4.is_all_determined v2);
  let v3 = V4.make () in
  V4.set v3 (v4i 0) t;
  V4.set v3 (v4i 1) f;
  V4.set v3 (v4i 2) b;
  V4.set v3 (v4i 3) f;
  Alcotest.(check bool) "with Both not is_all_determined" false (V4.is_all_determined v3)

let test_counts () =
  let v = V8.make () in
  V8.set v (v8i 0) t;
  V8.set v (v8i 1) t;
  V8.set v (v8i 2) f;
  V8.set v (v8i 3) f;
  V8.set v (v8i 4) b;
  V8.set v (v8i 5) b;
  V8.set v (v8i 6) u;
  V8.set v (v8i 7) u;
  Alcotest.(check int) "count_true" 2 (V8.count_true v);
  Alcotest.(check int) "count_false" 2 (V8.count_false v);
  Alcotest.(check int) "count_both" 2 (V8.count_both v);
  Alcotest.(check int) "count_unknown" 2 (V8.count_unknown v)

let test_word_boundaries () =
  (* Element 63: bit 63 (sign bit) of word-pair 0 *)
  let v = V65.make () in
  V65.set v (v65i 63) b;
  check "get 63 is Both" b (V65.get v (v65i 63));
  check "get 62 is Unknown" u (V65.get v (v65i 62));
  check "get 64 is Unknown" u (V65.get v (v65i 64));
  (* Element 64: bit 0 of word-pair 1 *)
  V65.set v (v65i 64) t;
  check "get 64 is True" t (V65.get v (v65i 64));
  check "get 63 still Both" b (V65.get v (v65i 63))

let test_width_63 () =
  (* width=63 exercises r=63 in bv_mask_tail and tail_mask *)
  let v = V63.all_true () in
  Alcotest.(check bool) "is_all_true" true (V63.is_all_true v);
  Alcotest.(check bool) "is_all_determined" true (V63.is_all_determined v);
  Alcotest.(check bool) "is_consistent" true (V63.is_consistent v);
  check "get 62 is True" t (V63.get v (v63i 62));
  let w = V63.all_false () in
  let r = V63.merge v w in
  Alcotest.(check int) "count_both after merge" 63 (V63.count_both r)

let test_to_list_roundtrip () =
  Alcotest.(check belnap_list) "empty" [] (V0.to_list (V0.make ()));
  let xs = [ u; t; f; b ] in
  Alcotest.(check belnap_list) "4 elems" xs (V4.to_list (V4.of_list xs));
  (* 64 elements exercises exactly one full word-pair *)
  let xs64 = List.init 64 (fun _ -> t) in
  Alcotest.(check belnap_list) "64 elems" xs64 (V64.to_list (V64.all_true ()));
  (* 65 elements: last element is in word-pair 1, bit 0 *)
  let xs65 = List.init 65 (fun i -> if i = 64 then f else t) in
  Alcotest.(check belnap_list) "65 elems" xs65 (V65.to_list (V65.of_list xs65))

let test_to_array_roundtrip () =
  Alcotest.(check belnap_array) "empty" [||] (V0.to_array (V0.make ()));
  let xs = [| u; t; f; b |] in
  Alcotest.(check belnap_array) "4 elems" xs (V4.to_array (V4.of_list (Array.to_list xs)))

let test_of_array_roundtrip () =
  Alcotest.(check belnap_array) "empty" [||] (V0.to_array (V0.of_array [||]));
  let xs = [| u; t; f; b |] in
  Alcotest.(check belnap_array) "4 elems" xs (V4.to_array (V4.of_array xs));
  (* word boundary: 65 elements, last in word-pair 1 *)
  let xs65 = Array.init 65 (fun i -> if i = 64 then b else f) in
  Alcotest.(check belnap_array) "65 elems" xs65 (V65.to_array (V65.of_array xs65))

let test_find_first () =
  let chk = Alcotest.(check (option int)) in
  let v = V4.of_list [ f; f; t; b ] in
  chk "first true" (Some 2) (V4.find_first t v);
  chk "first false" (Some 0) (V4.find_first f v);
  chk "first both" (Some 3) (V4.find_first b v);
  chk "no unknown" None (V4.find_first u v);
  chk "empty" None (V0.find_first t (V0.make ()));
  (* first match at word boundary (index 64, word-pair 1) *)
  let v2 = V65.of_list (List.init 65 (fun i -> if i = 64 then t else f)) in
  chk "word boundary idx 64" (Some 64) (V65.find_first t v2);
  (* all_true 128: first in first word-pair *)
  chk "all_true first" (Some 0) (V128.find_first t (V128.all_true ()))

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

let gen_belnap : Belnap.t QCheck2.Gen.t =
  QCheck2.Gen.oneof
    [
      QCheck2.Gen.return (Belnap.of_view Belnap.Unknown);
      QCheck2.Gen.return (Belnap.of_view Belnap.True);
      QCheck2.Gen.return (Belnap.of_view Belnap.False);
      QCheck2.Gen.return (Belnap.of_view Belnap.Both);
    ]

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
        pair g g |> map (fun (xs, ys) -> (make_size n, xs, ys))))

let gen_s3 =
  QCheck2.Gen.(
    bind gen_n (fun n ->
        let g = list_size (return n) gen_belnap in
        triple g g g |> map (fun (xs, ys, zs) -> (make_size n, xs, ys, zs))))

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

(* --- QCheck2 property tests --- *)

(* Truth-order lattice laws (|| and &&) *)

let or_commutativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs and b = M.of_list ys in
    M.equal (M.( || ) a b) (M.( || ) b a)
  in
  QCheck2.Test.make ~name:"or_commutativity" ~print:print_s2 gen_s2 body

let or_associativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys, zs) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs and b = M.of_list ys and c = M.of_list zs in
    M.equal (M.( || ) (M.( || ) a b) c) (M.( || ) a (M.( || ) b c))
  in
  QCheck2.Test.make ~name:"or_associativity" ~print:print_s3 gen_s3 body

let or_idempotency =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs in
    M.equal (M.( || ) a a) a
  in
  QCheck2.Test.make ~name:"or_idempotency" ~print:print_s1 gen_s1 body

let and_commutativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs and b = M.of_list ys in
    M.equal (M.( && ) a b) (M.( && ) b a)
  in
  QCheck2.Test.make ~name:"and_commutativity" ~print:print_s2 gen_s2 body

let and_associativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys, zs) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs and b = M.of_list ys and c = M.of_list zs in
    M.equal (M.( && ) (M.( && ) a b) c) (M.( && ) a (M.( && ) b c))
  in
  QCheck2.Test.make ~name:"and_associativity" ~print:print_s3 gen_s3 body

let and_idempotency =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs in
    M.equal (M.( && ) a a) a
  in
  QCheck2.Test.make ~name:"and_idempotency" ~print:print_s1 gen_s1 body

let absorption_or_and =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs and b = M.of_list ys in
    M.equal (M.( || ) a (M.( && ) a b)) a
  in
  QCheck2.Test.make ~name:"absorption_or_and" ~print:print_s2 gen_s2 body

let absorption_and_or =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs and b = M.of_list ys in
    M.equal (M.( && ) a (M.( || ) a b)) a
  in
  QCheck2.Test.make ~name:"absorption_and_or" ~print:print_s2 gen_s2 body

let or_bottom_identity =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs in
    M.equal (M.( || ) a (M.all_false ())) a
  in
  QCheck2.Test.make ~name:"or_bottom_identity" ~print:print_s1 gen_s1 body

let and_top_identity =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs in
    M.equal (M.( && ) a (M.all_true ())) a
  in
  QCheck2.Test.make ~name:"and_top_identity" ~print:print_s1 gen_s1 body

(* Knowledge-order join semilattice laws (merge) *)

let merge_commutativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs and b = M.of_list ys in
    M.equal (M.merge a b) (M.merge b a)
  in
  QCheck2.Test.make ~name:"merge_commutativity" ~print:print_s2 gen_s2 body

let merge_associativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys, zs) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs and b = M.of_list ys and c = M.of_list zs in
    M.equal (M.merge (M.merge a b) c) (M.merge a (M.merge b c))
  in
  QCheck2.Test.make ~name:"merge_associativity" ~print:print_s3 gen_s3 body

let merge_idempotency =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs in
    M.equal (M.merge a a) a
  in
  QCheck2.Test.make ~name:"merge_idempotency" ~print:print_s1 gen_s1 body

let merge_bottom_identity =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs in
    M.equal (M.merge (M.make ()) a) a
  in
  QCheck2.Test.make ~name:"merge_bottom_identity" ~print:print_s1 gen_s1 body

(* Knowledge-order meet semilattice laws (consensus) *)

let consensus_commutativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs and b = M.of_list ys in
    M.equal (M.consensus a b) (M.consensus b a)
  in
  QCheck2.Test.make ~name:"consensus_commutativity" ~print:print_s2 gen_s2 body

let consensus_associativity =
  let body ((module S : Belnap_vec.SIZE), xs, ys, zs) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs and b = M.of_list ys and c = M.of_list zs in
    M.equal (M.consensus (M.consensus a b) c) (M.consensus a (M.consensus b c))
  in
  QCheck2.Test.make ~name:"consensus_associativity" ~print:print_s3 gen_s3 body

let consensus_idempotency =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs in
    M.equal (M.consensus a a) a
  in
  QCheck2.Test.make ~name:"consensus_idempotency" ~print:print_s1 gen_s1 body

let consensus_top_identity =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let a = M.of_list xs in
    M.equal (M.consensus a (M.all_both ())) a
  in
  QCheck2.Test.make ~name:"consensus_top_identity" ~print:print_s1 gen_s1 body

(* Auxiliary consistency properties *)

let count_nonneg =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    M.count_true v >= 0 && M.count_false v >= 0 && M.count_both v >= 0 && M.count_unknown v >= 0
  in
  QCheck2.Test.make ~name:"count_nonneg" ~print:print_s1 gen_s1 body

let not_involutive =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    M.equal (M.not (M.not v)) v
  in
  QCheck2.Test.make ~name:"not_involutive" ~print:print_s1 gen_s1 body

let is_consistent_iff_no_both =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    M.is_consistent v = (M.count_both v = 0)
  in
  QCheck2.Test.make ~name:"is_consistent_iff_no_both" ~print:print_s1 gen_s1 body

let is_all_determined_iff =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    M.is_all_determined v = (M.count_unknown v = 0 && M.count_both v = 0)
  in
  QCheck2.Test.make ~name:"is_all_determined_iff" ~print:print_s1 gen_s1 body

let is_all_true_iff =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    M.is_all_true v = (M.count_true v = S.n)
  in
  QCheck2.Test.make ~name:"is_all_true_iff" ~print:print_s1 gen_s1 body

let is_all_false_iff =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    M.is_all_false v = (M.count_false v = S.n)
  in
  QCheck2.Test.make ~name:"is_all_false_iff" ~print:print_s1 gen_s1 body

(* to_list / to_array *)

let of_list_to_list_roundtrip =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    List.equal Belnap.equal (M.to_list (M.of_list xs)) xs
  in
  QCheck2.Test.make ~name:"of_list_to_list_roundtrip" ~print:print_s1 gen_s1 body

let of_array_to_array_roundtrip =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let arr = Array.of_list xs in
    array_equal Belnap.equal (M.to_array (M.of_array arr)) arr
  in
  QCheck2.Test.make ~name:"of_array_to_array_roundtrip" ~print:print_s1 gen_s1 body

let of_array_of_list_consistent =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    M.equal (M.of_list xs) (M.of_array (Array.of_list xs))
  in
  QCheck2.Test.make ~name:"of_array_of_list_consistent" ~print:print_s1 gen_s1 body

let to_list_matches_get =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    List.equal Belnap.equal (M.to_list v) (List.init S.n (fun i -> M.get v (M.index_exn i)))
  in
  QCheck2.Test.make ~name:"to_list_matches_get" ~print:print_s1 gen_s1 body

(* find_first correctness *)

let find_first_returns_correct_value =
  let print (needle, s1) = Format.asprintf "needle=%a %s" Belnap.pp needle (print_s1 s1) in
  let body (needle, ((module S : Belnap_vec.SIZE), xs)) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    match M.find_first needle v with
    | None -> true
    | Some i -> Belnap.equal (M.get v (M.index_exn i)) needle
  in
  QCheck2.Test.make
    ~name:"find_first_returns_correct_value"
    ~print
    QCheck2.Gen.(pair gen_belnap gen_s1)
    body

let find_first_is_minimum =
  let print (needle, s1) = Format.asprintf "needle=%a %s" Belnap.pp needle (print_s1 s1) in
  let body (needle, ((module S : Belnap_vec.SIZE), xs)) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    match M.find_first needle v with
    | None -> true
    | Some i ->
        let rec all_before j =
          j >= i || ((not (Belnap.equal (M.get v (M.index_exn j)) needle)) && all_before (j + 1))
        in
        all_before 0
  in
  QCheck2.Test.make ~name:"find_first_is_minimum" ~print QCheck2.Gen.(pair gen_belnap gen_s1) body

let find_first_none_iff_count_zero =
  let body ((module S : Belnap_vec.SIZE), xs) =
    let module M = Belnap_vec.Make (S) in
    let v = M.of_list xs in
    M.find_first t v = None = (M.count_true v = 0)
    && M.find_first f v = None = (M.count_false v = 0)
    && M.find_first b v = None = (M.count_both v = 0)
    && M.find_first u v = None = (M.count_unknown v = 0)
  in
  QCheck2.Test.make ~name:"find_first_none_iff_count_zero" ~print:print_s1 gen_s1 body

(* Get/set roundtrip — n >= 1 so a valid index exists *)

let get_set_roundtrip =
  let print ((module S : Belnap_vec.SIZE), xs, i, _) =
    Printf.sprintf "n=%d i=%d xs=%s" S.n i (pp_blist xs)
  in
  let body ((module S : Belnap_vec.SIZE), xs, i, v) =
    let module M = Belnap_vec.Make (S) in
    let vec = M.of_list xs in
    M.set vec (M.index_exn i) v;
    Belnap.equal (M.get vec (M.index_exn i)) v
  in
  QCheck2.Test.make ~name:"get_set_roundtrip" ~print gen_s_get_set body

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
    or_bottom_identity;
    and_top_identity;
    merge_commutativity;
    merge_associativity;
    merge_idempotency;
    merge_bottom_identity;
    consensus_commutativity;
    consensus_associativity;
    consensus_idempotency;
    consensus_top_identity;
    count_nonneg;
    not_involutive;
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
