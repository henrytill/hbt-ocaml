module type S = sig
  type t

  val of_bool : bool -> t
  val empty : t
  val get : t -> bool option
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val concat : t -> t -> t
end
