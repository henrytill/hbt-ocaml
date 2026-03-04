type bv

let bits_log2 = 6 (* log2 of the number of bits per word, i.e. words are 64 bits *)
let bits_mask = (1 lsl bits_log2) - 1
let words_needed n = (n + bits_mask) lsr bits_log2

(* Allocating stubs *)
external bv_alloc : int -> bv = "bv_alloc"
external bv_to_array : bv -> int -> Belnap.t array = "bv_to_array"

(* Non-allocating stubs *)
external bv_init_from_list : bv -> Belnap.t list -> unit = "bv_init_from_list" [@@noalloc]
external bv_init_from_array : bv -> Belnap.t array -> unit = "bv_init_from_array" [@@noalloc]
external bv_get : bv -> int -> int = "bv_get" [@@noalloc]
external bv_set : bv -> int -> int -> unit = "bv_set" [@@noalloc]
external bv_mask_tail : bv -> int -> unit = "bv_mask_tail" [@@noalloc]
external bv_fill : bv -> int -> int -> int -> unit = "bv_fill" [@@noalloc]
external bv_not : bv -> bv -> unit = "bv_not" [@@noalloc]
external bv_and : bv -> bv -> bv -> unit = "bv_and" [@@noalloc]
external bv_or : bv -> bv -> bv -> unit = "bv_or" [@@noalloc]
external bv_merge : bv -> bv -> bv -> unit = "bv_merge" [@@noalloc]
external bv_consensus : bv -> bv -> bv -> unit = "bv_consensus" [@@noalloc]
external bv_is_consistent : bv -> int -> int = "bv_is_consistent" [@@noalloc]
external bv_is_all_determined : bv -> int -> int = "bv_is_all_determined" [@@noalloc]
external bv_is_all_true : bv -> int -> int = "bv_is_all_true" [@@noalloc]
external bv_is_all_false : bv -> int -> int = "bv_is_all_false" [@@noalloc]
external bv_count_true : bv -> int = "bv_count_true" [@@noalloc]
external bv_count_false : bv -> int = "bv_count_false" [@@noalloc]
external bv_count_both : bv -> int = "bv_count_both" [@@noalloc]
external bv_equal : bv -> bv -> int = "bv_equal" [@@noalloc]
external bv_find_first : bv -> int -> int -> int = "bv_find_first" [@@noalloc]

include Belnap_vec_intf

module Make (S : SIZE) = struct
  type t = { data : bv }
  type index = int

  let nw = words_needed S.n
  let make () = { data = bv_alloc nw }

  let filled fill =
    let raw = Belnap.to_bits fill in
    let data = bv_alloc nw in
    bv_fill data 0 nw raw;
    bv_mask_tail data S.n;
    { data }

  let all_true () = filled Belnap.(of_view True)
  let all_false () = filled Belnap.(of_view False)
  let all_both () = filled Belnap.(of_view Both)

  let of_array a =
    let len = Array.length a in
    if len <> S.n then
      invalid_arg (Printf.sprintf "Belnap_vec.of_array: expected %d elements, got %d" S.n len);
    let data = bv_alloc nw in
    bv_init_from_array data a;
    { data }

  let of_list l =
    let len = List.length l in
    if len <> S.n then
      invalid_arg (Printf.sprintf "Belnap_vec.of_list: expected %d elements, got %d" S.n len);
    let data = bv_alloc nw in
    bv_init_from_list data l;
    { data }

  let to_array vec = bv_to_array vec.data S.n
  let to_list vec = Array.to_list (to_array vec)
  let index i = if Stdlib.( && ) (i >= 0) (i < S.n) then Some i else None

  let index_exn i =
    if Stdlib.( && ) (i >= 0) (i < S.n) then
      i
    else
      invalid_arg (Printf.sprintf "Belnap_vec.index_exn: %d out of [0, %d)" i S.n)

  let get vec i = Belnap.of_bits (bv_get vec.data i)
  let set vec i v = bv_set vec.data i (Belnap.to_bits v)

  let not vec =
    let data = bv_alloc nw in
    bv_not vec.data data;
    { data }

  let ( && ) a b =
    let data = bv_alloc nw in
    bv_and a.data b.data data;
    { data }

  let ( || ) a b =
    let data = bv_alloc nw in
    bv_or a.data b.data data;
    { data }

  let merge a b =
    let data = bv_alloc nw in
    bv_merge a.data b.data data;
    { data }

  let consensus a b =
    let data = bv_alloc nw in
    bv_consensus a.data b.data data;
    { data }

  let implies a b = (not a) || b
  let is_consistent vec = bv_is_consistent vec.data S.n = 1
  let is_all_determined vec = bv_is_all_determined vec.data S.n = 1
  let is_all_true vec = bv_is_all_true vec.data S.n = 1
  let is_all_false vec = bv_is_all_false vec.data S.n = 1
  let count_true vec = bv_count_true vec.data
  let count_false vec = bv_count_false vec.data
  let count_both vec = bv_count_both vec.data
  let count_unknown vec = S.n - count_true vec - count_false vec - count_both vec

  let find_first needle vec =
    let idx = bv_find_first vec.data S.n (Belnap.to_bits needle) in
    if idx = -1 then None else Some idx

  let equal a b = bv_equal a.data b.data = 1
  let pp fmt vec = Fmt.brackets (Fmt.array ~sep:Fmt.semi Belnap.pp) fmt (to_array vec)
end
