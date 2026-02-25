type t = {
  mutable width : int;
  words : int Dynarray.t;
}

let bits_log2 = 5
let bits_mask = (1 lsl bits_log2) - 1
let words_needed n = (n + bits_mask) lsr bits_log2

let tail_mask n =
  let r = n land bits_mask in
  if r = 0 then
    -1
  else
    (1 lsl r) - 1

let mask_tail vec =
  let nw = words_needed vec.width in
  if nw > 0 then begin
    let m = tail_mask vec.width in
    let pos_idx = (nw - 1) * 2 in
    let neg_idx = pos_idx + 1 in
    Dynarray.set vec.words pos_idx (Dynarray.get vec.words pos_idx land m);
    Dynarray.set vec.words neg_idx (Dynarray.get vec.words neg_idx land m)
  end

let create () = { width = 0; words = Dynarray.create () }
let make width = { width; words = Dynarray.make (2 * words_needed width) 0 }

let filled width fill =
  let pos_word = if fill land 1 = 1 then -1 else 0 in
  let neg_word = if fill lsr 1 = 1 then -1 else 0 in
  let nw = words_needed width in
  let words = Dynarray.create () in
  for _ = 0 to nw - 1 do
    Dynarray.add_last words pos_word;
    Dynarray.add_last words neg_word
  done;
  let vec = { width; words } in
  mask_tail vec;
  vec

let all_true width = filled width 0b01
let all_false width = filled width 0b10
let width vec = vec.width

let get_unchecked vec i =
  let w = i lsr bits_log2 in
  let b = i land bits_mask in
  let pos_idx = w * 2 in
  let pos_bit = (Dynarray.get vec.words pos_idx lsr b) land 1 in
  let neg_bit = (Dynarray.get vec.words (pos_idx + 1) lsr b) land 1 in
  Belnap.of_view
    begin
      match (neg_bit lsl 1) lor pos_bit with
      | 0b00 -> Belnap.Unknown
      | 0b01 -> Belnap.True
      | 0b10 -> Belnap.False
      | _ -> Belnap.Both
    end

let get vec i =
  if i >= vec.width then
    invalid_arg "Belnap_vec.get: index out of bounds"
  else
    get_unchecked vec i

let set_unchecked vec i (v : Belnap.t) =
  let w = i lsr bits_log2 in
  let b = i land bits_mask in
  let pos_idx = w * 2 in
  let neg_idx = pos_idx + 1 in
  let mask = 1 lsl b in
  let raw = Belnap.to_bits v in
  let pos = (raw land 1) lsl b in
  let neg = (raw lsr 1) lsl b in
  Dynarray.set vec.words pos_idx (Dynarray.get vec.words pos_idx land lnot mask lor pos);
  Dynarray.set vec.words neg_idx (Dynarray.get vec.words neg_idx land lnot mask lor neg)

let set vec i v =
  if i >= vec.width then begin
    let new_width = i + 1 in
    let new_nw = words_needed new_width in
    for _ = Dynarray.length vec.words to (2 * new_nw) - 1 do
      Dynarray.add_last vec.words 0
    done;
    vec.width <- new_width
  end;
  set_unchecked vec i v

let truncate vec new_width =
  if new_width < vec.width then begin
    vec.width <- new_width;
    Dynarray.truncate vec.words (2 * words_needed new_width);
    mask_tail vec
  end

let resize vec new_width fill =
  if new_width <= vec.width then
    truncate vec new_width
  else begin
    let raw = Belnap.to_bits fill in
    let pos_word = if raw land 1 = 1 then -1 else 0 in
    let neg_word = if raw lsr 1 = 1 then -1 else 0 in
    let is_known = raw <> 0 in
    let old_nw = words_needed vec.width in
    let new_nw = words_needed new_width in
    if is_known && old_nw > 0 && vec.width land bits_mask <> 0 then begin
      let fill_mask = lnot (tail_mask vec.width) in
      let pos_idx = (old_nw - 1) * 2 in
      let neg_idx = pos_idx + 1 in
      Dynarray.set vec.words pos_idx (Dynarray.get vec.words pos_idx lor (pos_word land fill_mask));
      Dynarray.set vec.words neg_idx (Dynarray.get vec.words neg_idx lor (neg_word land fill_mask))
    end;
    for _ = 0 to new_nw - old_nw - 1 do
      Dynarray.add_last vec.words pos_word;
      Dynarray.add_last vec.words neg_word
    done;
    vec.width <- new_width;
    if is_known then mask_tail vec
  end

(* Fold over word pairs of [vec], calling [f acc m pos neg] for each pair.
   [m] is a bitmask of the live bits in the pair — bits corresponding to actual
   elements. It is [-1] (all bits live) for full words, and [tail_mask vec.width]
   (only the low bits live) for the final partial word. Callers that do not need
   to distinguish the tail may ignore [m]. *)
let fold_pairs f init vec =
  let nw = words_needed vec.width in
  let acc = ref init in
  for i = 0 to nw - 1 do
    let pos_idx = i * 2 in
    let m = if i = nw - 1 then tail_mask vec.width else -1 in
    acc := f !acc m (Dynarray.get vec.words pos_idx) (Dynarray.get vec.words (pos_idx + 1))
  done;
  !acc

(* Map over word pairs of [vec], calling [f pos neg] for each pair and
   collecting the results into a new vec of the same width. *)
let map_pairs f vec =
  let nw = words_needed vec.width in
  let words = Dynarray.create () in
  for i = 0 to nw - 1 do
    let pos_idx = i * 2 in
    let pos, neg = f (Dynarray.get vec.words pos_idx) (Dynarray.get vec.words (pos_idx + 1)) in
    Dynarray.add_last words pos;
    Dynarray.add_last words neg
  done;
  let result = { width = vec.width; words } in
  mask_tail result;
  result

(* Map over word pairs of [a] and [b] in lockstep, calling [f ap an bp bn] for
   each pair and collecting the results into a new vec. The result width is the
   maximum of the two input widths; pairs beyond the shorter vec are padded with
   zero (representing all-[Unknown] elements). *)
let map_pairs2 f a b =
  let width = max a.width b.width in
  let nw = words_needed width in
  let words = Dynarray.create () in
  let get vec idx = if idx < Dynarray.length vec.words then Dynarray.get vec.words idx else 0 in
  for i = 0 to nw - 1 do
    let pos_idx = i * 2 in
    let neg_idx = pos_idx + 1 in
    let pos, neg = f (get a pos_idx) (get a neg_idx) (get b pos_idx) (get b neg_idx) in
    Dynarray.add_last words pos;
    Dynarray.add_last words neg
  done;
  let result = { width; words } in
  mask_tail result;
  result

let not = map_pairs (fun pos neg -> (neg, pos))
let ( && ) = map_pairs2 (fun ap an bp bn -> (ap land bp, an lor bn))
let ( || ) = map_pairs2 (fun ap an bp bn -> (ap lor bp, an land bn))
let merge = map_pairs2 (fun ap an bp bn -> (ap lor bp, an lor bn))
let implies a b = (not a) || b

(* Queries *)

let is_consistent = fold_pairs (fun ok _m pos neg -> if pos land neg <> 0 then false else ok) true

let is_all_determined =
  fold_pairs (fun ok m pos neg -> if pos lxor neg land m <> m then false else ok) true

let is_all_true =
  fold_pairs
    (fun ok m pos neg -> if pos land m <> m then false else if neg land m <> 0 then false else ok)
    true

let is_all_false =
  fold_pairs
    (fun ok m pos neg -> if pos land m <> 0 then false else if neg land m <> m then false else ok)
    true

let popcount x =
  let x = x land 0xFFFFFFFF in
  let x = x - ((x lsr 1) land 0x55555555) in
  let x = (x land 0x33333333) + ((x lsr 2) land 0x33333333) in
  let x = (x + (x lsr 4)) land 0x0f0f0f0f in
  ((x * 0x01010101) lsr 24) land 0xFF

let count_true vec = fold_pairs (fun n _m pos neg -> n + popcount (pos land lnot neg)) 0 vec
let count_false vec = fold_pairs (fun n _m pos neg -> n + popcount (lnot pos land neg)) 0 vec
let count_both vec = fold_pairs (fun n _m pos neg -> n + popcount (pos land neg)) 0 vec
let count_unknown vec = vec.width - count_true vec - count_false vec - count_both vec
