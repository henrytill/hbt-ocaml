type bv

type t = {
  mutable width : int;
  mutable data : bv;
}

let bits_log2 = 6 (* log2 of the number of bits per word, i.e. words are 64 bits *)
let bits_mask = (1 lsl bits_log2) - 1
let words_needed n = (n + bits_mask) lsr bits_log2

(* Allocating stubs *)
external bv_alloc : int -> bv = "caml_bv_alloc"
external bv_blit_grow : bv -> int -> bv = "caml_bv_blit_grow"

(* C stubs — all [@@noalloc] *)
external bv_get : bv -> int -> int = "caml_bv_get" [@@noalloc]
external bv_set : bv -> int -> int -> unit = "caml_bv_set" [@@noalloc]
external bv_mask_tail : bv -> int -> unit = "caml_bv_mask_tail" [@@noalloc]
external bv_fill : bv -> int -> int -> int -> unit = "caml_bv_fill" [@@noalloc]
external bv_not : bv -> bv -> unit = "caml_bv_not" [@@noalloc]
external bv_and : bv -> bv -> bv -> unit = "caml_bv_and" [@@noalloc]
external bv_or : bv -> bv -> bv -> unit = "caml_bv_or" [@@noalloc]
external bv_merge : bv -> bv -> bv -> unit = "caml_bv_merge" [@@noalloc]
external bv_is_consistent : bv -> int -> int = "caml_bv_is_consistent" [@@noalloc]
external bv_is_all_determined : bv -> int -> int = "caml_bv_is_all_determined" [@@noalloc]
external bv_is_all_true : bv -> int -> int = "caml_bv_is_all_true" [@@noalloc]
external bv_is_all_false : bv -> int -> int = "caml_bv_is_all_false" [@@noalloc]
external bv_count_true : bv -> int = "caml_bv_count_true" [@@noalloc]
external bv_count_false : bv -> int = "caml_bv_count_false" [@@noalloc]
external bv_count_both : bv -> int = "caml_bv_count_both" [@@noalloc]
external bv_equal : bv -> bv -> int = "caml_bv_equal" [@@noalloc]

let create () = { width = 0; data = bv_alloc 0 }

let make width =
  let nw = words_needed width in
  { width; data = bv_alloc nw }

let filled width fill =
  let raw = Belnap.to_bits fill in
  let nw = words_needed width in
  let data = bv_alloc nw in
  bv_fill data 0 nw raw;
  let vec = { width; data } in
  bv_mask_tail vec.data width;
  vec

let all_true width = filled width (Belnap.of_view Belnap.True)
let all_false width = filled width (Belnap.of_view Belnap.False)
let width vec = vec.width

let get vec i =
  if i >= vec.width then
    invalid_arg "Belnap_vec.get: index out of bounds"
  else
    Belnap.of_bits (bv_get vec.data i)

let set vec i v =
  if i >= vec.width then begin
    let new_width = i + 1 in
    let new_nw = words_needed new_width in
    let old_nw = words_needed vec.width in
    if new_nw > old_nw then
      vec.data <- bv_blit_grow vec.data new_nw;
    vec.width <- new_width
  end;
  bv_set vec.data i (Belnap.to_bits v)

let truncate vec new_width =
  if new_width < vec.width then begin
    vec.width <- new_width;
    bv_mask_tail vec.data new_width
  end

let resize vec new_width =
  if new_width <= vec.width then
    truncate vec new_width
  else begin
    let old_nw = words_needed vec.width in
    let new_nw = words_needed new_width in
    if new_nw > old_nw then
      vec.data <- bv_blit_grow vec.data new_nw;
    vec.width <- new_width
  end

let not vec =
  let nw = words_needed vec.width in
  let data = bv_alloc nw in
  bv_not vec.data data;
  { width = vec.width; data }

let ( && ) a b =
  let nwa = words_needed a.width in
  let nwb = words_needed b.width in
  let width = max a.width b.width in
  let data = bv_alloc (if nwa > nwb then nwa else nwb) in
  bv_and a.data b.data data;
  { width; data }

let ( || ) a b =
  let nwa = words_needed a.width in
  let nwb = words_needed b.width in
  let width = max a.width b.width in
  let data = bv_alloc (if nwa > nwb then nwa else nwb) in
  bv_or a.data b.data data;
  { width; data }

let merge a b =
  let nwa = words_needed a.width in
  let nwb = words_needed b.width in
  let width = max a.width b.width in
  let data = bv_alloc (if nwa > nwb then nwa else nwb) in
  bv_merge a.data b.data data;
  { width; data }

let implies a b = (not a) || b
let is_consistent vec = bv_is_consistent vec.data vec.width = 1
let is_all_determined vec = bv_is_all_determined vec.data vec.width = 1
let is_all_true vec = bv_is_all_true vec.data vec.width = 1
let is_all_false vec = bv_is_all_false vec.data vec.width = 1
let count_true vec = bv_count_true vec.data
let count_false vec = bv_count_false vec.data
let count_both vec = bv_count_both vec.data
let count_unknown vec = vec.width - count_true vec - count_false vec - count_both vec
let equal a b = Stdlib.( && ) (a.width = b.width) (bv_equal a.data b.data = 1)
