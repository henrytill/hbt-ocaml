open Bigarray

type words = (int64, int64_elt, c_layout) Array1.t
type t = { mutable width : int; mutable words : words }

let bits_log2 = 6 (* log2 of the number of bits per word, i.e. words are 64 bits *)
let bits_mask = (1 lsl bits_log2) - 1
let planes = 2 (* pos and neg bitplanes *)
let words_needed n = (n + bits_mask) lsr bits_log2

let make_words n =
  let a = Array1.create int64 c_layout n in
  Array1.fill a Int64.zero;
  a

(* C stubs — all [@@noalloc] *)
external bv_get : words -> int -> int = "caml_bv_get" [@@noalloc]
external bv_set : words -> int -> int -> unit = "caml_bv_set" [@@noalloc]
external bv_mask_tail : words -> int -> int -> unit = "caml_bv_mask_tail" [@@noalloc]
external bv_fill : words -> int -> int -> int -> unit = "caml_bv_fill" [@@noalloc]
external bv_fill_gap : words -> int -> int -> int -> unit = "caml_bv_fill_gap" [@@noalloc]
external bv_not : words -> words -> int -> unit = "caml_bv_not" [@@noalloc]
external bv_and : words -> words -> words -> int -> int -> unit = "caml_bv_and" [@@noalloc]
external bv_or : words -> words -> words -> int -> int -> unit = "caml_bv_or" [@@noalloc]
external bv_merge : words -> words -> words -> int -> int -> unit = "caml_bv_merge" [@@noalloc]
external bv_is_consistent : words -> int -> int -> int = "caml_bv_is_consistent" [@@noalloc]
external bv_is_all_determined : words -> int -> int -> int = "caml_bv_is_all_determined" [@@noalloc]
external bv_is_all_true : words -> int -> int -> int = "caml_bv_is_all_true" [@@noalloc]
external bv_is_all_false : words -> int -> int -> int = "caml_bv_is_all_false" [@@noalloc]
external bv_count_true : words -> int -> int = "caml_bv_count_true" [@@noalloc]
external bv_count_false : words -> int -> int = "caml_bv_count_false" [@@noalloc]
external bv_count_both : words -> int -> int = "caml_bv_count_both" [@@noalloc]

let create () = { width = 0; words = make_words 0 }

let make width =
  let nw = words_needed width in
  { width; words = make_words (planes * nw) }

let filled width fill =
  let raw = Belnap.to_bits fill in
  let nw = words_needed width in
  let words = make_words (planes * nw) in
  bv_fill words 0 nw raw;
  let vec = { width; words } in
  bv_mask_tail vec.words nw width;
  vec

let all_true width = filled width (Belnap.of_view Belnap.True)
let all_false width = filled width (Belnap.of_view Belnap.False)
let width vec = vec.width

let get vec i =
  if i >= vec.width then
    invalid_arg "Belnap_vec.get: index out of bounds"
  else
    Belnap.of_bits (bv_get vec.words i)

let set vec i v =
  if i >= vec.width then begin
    let new_width = i + 1 in
    let new_nw = words_needed new_width in
    let old_nw = words_needed vec.width in
    if new_nw > old_nw then begin
      let new_words = make_words (planes * new_nw) in
      Array1.blit vec.words (Array1.sub new_words 0 (Array1.dim vec.words));
      vec.words <- new_words
    end;
    vec.width <- new_width
  end;
  bv_set vec.words i (Belnap.to_bits v)

let truncate vec new_width =
  if new_width < vec.width then begin
    vec.width <- new_width;
    let nw = words_needed new_width in
    bv_mask_tail vec.words nw new_width
  end

let resize vec new_width fill =
  if new_width <= vec.width then
    truncate vec new_width
  else begin
    let raw = Belnap.to_bits fill in
    let old_nw = words_needed vec.width in
    let new_nw = words_needed new_width in
    if new_nw > old_nw then begin
      let new_words = make_words (planes * new_nw) in
      Array1.blit vec.words (Array1.sub new_words 0 (Array1.dim vec.words));
      vec.words <- new_words
    end;
    bv_fill_gap vec.words vec.width new_width raw;
    vec.width <- new_width
  end

let not vec =
  let nw = words_needed vec.width in
  let words = make_words (planes * nw) in
  bv_not vec.words words nw;
  { width = vec.width; words }

let ( && ) a b =
  let nwa = words_needed a.width in
  let nwb = words_needed b.width in
  let width = max a.width b.width in
  let words = make_words (planes * (if nwa > nwb then nwa else nwb)) in
  bv_and a.words b.words words nwa nwb;
  { width; words }

let ( || ) a b =
  let nwa = words_needed a.width in
  let nwb = words_needed b.width in
  let width = max a.width b.width in
  let words = make_words (planes * (if nwa > nwb then nwa else nwb)) in
  bv_or a.words b.words words nwa nwb;
  { width; words }

let merge a b =
  let nwa = words_needed a.width in
  let nwb = words_needed b.width in
  let width = max a.width b.width in
  let words = make_words (planes * (if nwa > nwb then nwa else nwb)) in
  bv_merge a.words b.words words nwa nwb;
  { width; words }

let implies a b = (not a) || b

let is_consistent vec =
  let nw = words_needed vec.width in
  bv_is_consistent vec.words nw vec.width = 1

let is_all_determined vec =
  let nw = words_needed vec.width in
  bv_is_all_determined vec.words nw vec.width = 1

let is_all_true vec =
  let nw = words_needed vec.width in
  bv_is_all_true vec.words nw vec.width = 1

let is_all_false vec =
  let nw = words_needed vec.width in
  bv_is_all_false vec.words nw vec.width = 1

let count_true vec =
  let nw = words_needed vec.width in
  bv_count_true vec.words nw

let count_false vec =
  let nw = words_needed vec.width in
  bv_count_false vec.words nw

let count_both vec =
  let nw = words_needed vec.width in
  bv_count_both vec.words nw

let count_unknown vec = vec.width - count_true vec - count_false vec - count_both vec
