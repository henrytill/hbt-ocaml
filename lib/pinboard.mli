type t

module Tags : sig
  include Set.S

  val pp : Format.formatter -> t -> unit
end

val tags : t list -> Tags.t
val from_xml : string -> t list
val from_json : string -> t list
