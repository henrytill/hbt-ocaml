type t

val href : t -> string
val time : t -> string
val description : t -> string option
val extended : t -> string option
val tag : t -> string list
val hash : t -> string option
val shared : t -> bool
val toread : t -> bool
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val to_string : t -> string
val from_xml : string -> t list
val from_json : string -> t list
