type t

type view =
  | Unknown
  | True
  | False
  | Both

val of_view : view -> t
val to_view : t -> view
val of_bool : bool -> t
val to_bool : t -> bool option
val to_bits : t -> int
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val not : t -> t
val ( && ) : t -> t -> t
val ( || ) : t -> t -> t
val implies : t -> t -> t
val empty : t
val merge : t -> t -> t
val is_known : t -> bool
val is_determined : t -> bool
val is_contradicted : t -> bool
