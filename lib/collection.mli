module Id : sig
  type t

  val to_int : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module Name : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module Name_set : sig
  include Set.S with type elt = Name.t

  val pp : Format.formatter -> t -> unit
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module Label : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module Label_set : sig
  include Set.S with type elt = Label.t

  val pp : Format.formatter -> t -> unit
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module Label_map : Map.S with type key = Label.t

module Time : sig
  type t

  val of_string : string -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module Extended : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module Entity : sig
  type t

  val make : Uri.t -> Time.t -> Name.t option -> Label_set.t -> t
  val of_pinboard : Pinboard.t -> t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val update : Time.t -> Name_set.t -> Label_set.t -> t -> t
  val absorb : t -> t -> t
  val uri : t -> Uri.t
  val created_at : t -> Time.t
  val updated_at : t -> Time.t list
  val names : t -> Name_set.t
  val labels : t -> Label_set.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

type t

val create : unit -> t
val make : int -> t
val length : t -> int
val is_empty : t -> bool
val contains : t -> Uri.t -> bool
val id : t -> Uri.t -> Id.t option
val insert : t -> Entity.t -> Id.t
val upsert : t -> Entity.t -> Id.t
val add_edge : t -> Id.t -> Id.t -> unit
val add_edges : t -> Id.t -> Id.t -> unit
val entity : t -> Id.t -> Entity.t
val edges : t -> Id.t -> Id.t array
val entities : t -> Entity.t array
val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
val map_labels : (Label_set.t -> Label_set.t) -> t -> t
val update_labels : Yojson.Basic.t -> t -> t
val to_html : t -> string
