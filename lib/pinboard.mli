type t

val make :
  href:string ->
  time:string ->
  ?description:string ->
  ?extended:string ->
  tag:string list ->
  ?hash:string ->
  shared:bool ->
  toread:bool ->
  unit ->
  t

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val to_string : t -> string

module Tags : sig
  include Set.S

  val pp : Format.formatter -> t -> unit
end

val tags : t list -> Tags.t
val from_xml : string -> t list
val from_html : string -> t list
val from_json : string -> t list
