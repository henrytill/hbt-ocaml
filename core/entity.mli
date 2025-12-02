module Uri : sig
  type t

  val empty : t
  val of_string : string -> t
  val to_string : t -> string
  val canonicalize : t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val hash : t -> int
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
end

module Name : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
end

module Name_set : sig
  include Set.S with type elt = Name.t

  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
end

module Label : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
end

module Label_set : sig
  include Set.S with type elt = Label.t

  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
end

module Label_map : Map.S with type key = Label.t

module Time : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
end

module Extended : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
end

type t

val make :
  Uri.t ->
  Time.t ->
  ?updated_at:Time.t list ->
  ?maybe_name:Name.t option ->
  ?labels:Label_set.t ->
  ?extended:Extended.t list ->
  ?shared:bool ->
  ?to_read:bool ->
  ?last_visited_at:Time.t option ->
  ?is_feed:bool ->
  unit ->
  t

val empty : t
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val update : Time.t -> Name_set.t -> Label_set.t -> Extended.t list -> t -> t
val absorb : t -> t -> t
val uri : t -> Uri.t
val created_at : t -> Time.t
val updated_at : t -> Time.t list
val names : t -> Name_set.t
val labels : t -> Label_set.t
val extended : t -> Extended.t list
val shared : t -> bool
val to_read : t -> bool
val last_visited_at : t -> Time.t option
val is_feed : t -> bool
val map_labels : (Label_set.t -> Label_set.t) -> t -> t
val of_post : Pinboard.Post.t -> t
val t_of_yaml : Yaml.value -> t
val yaml_of_t : t -> Yaml.value

module Html : sig
  module Attrs = Prelude.Markup_ext.Attrs

  val entity_of_attrs : Attrs.t -> Name_set.t -> Label_set.t -> Extended.t list -> t
end
