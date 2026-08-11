type t

exception Unexpected_xml_element of string
(** Raised by {!from_xml} for an element other than [posts] or [post]. *)

val href : t -> string
val time : t -> string
val description : t -> string option
val extended : t -> string option
val tag : t -> string list
val meta : t -> string option
val hash : t -> string option
val shared : t -> bool
val toread : t -> bool
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val to_string : t -> string
val from_json : string -> t list
val from_xml : string -> t list
