type t

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val to_string : t -> string

module Tags : sig
  include Set.S

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

val tags : t list -> Tags.t
val from_xml : string -> t list
val from_html : string -> t list
val from_json : string -> t list
