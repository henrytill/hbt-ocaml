type t = int

type view =
  | Unknown
  | True
  | False
  | Both

let of_view = function
  | Unknown -> 0b00
  | True -> 0b01
  | False -> 0b10
  | Both -> 0b11

let to_view = function
  | 0b00 -> Unknown
  | 0b01 -> True
  | 0b10 -> False
  | 0b11 -> Both
  | _ -> assert false

let of_bool b = if b then 0b01 else 0b10

let to_bool a =
  match to_view a with
  | Unknown | Both -> None
  | True -> Some true
  | False -> Some false

let of_bits x = x
let to_bits x = x

let to_string a =
  match to_view a with
  | Unknown -> "Unknown"
  | True -> "True"
  | False -> "False"
  | Both -> "Both"

let pp fmt a = Fmt.string fmt (to_string a)
let equal : t -> t -> bool = Int.equal
let leq_truth a b = a land 1 <= b land 1 && a lsr 1 >= b lsr 1
let leq_knowledge a b = a land b = a
let not a = ((a land 1) lsl 1) lor (a lsr 1)

let ( && ) a b =
  let pos = a land 1 land (b land 1) in
  let neg = (a lsr 1) lor (b lsr 1) in
  (neg lsl 1) lor pos

let ( || ) a b =
  let pos = a land 1 lor (b land 1) in
  let neg = (a lsr 1) land (b lsr 1) in
  (neg lsl 1) lor pos

let implies a b = (not a) || b
let empty = 0b00
let merge a b = a lor b
let consensus a b = a land b
let is_known a = a <> 0b00
let is_determined a = a land 1 lxor (a lsr 1) <> 0
let is_contradicted a = a = 0b11
