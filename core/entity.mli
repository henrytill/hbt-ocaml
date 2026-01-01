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

module Shared : sig
  type t

  val of_bool : bool -> t
  val empty : t
  val get : t -> bool option
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val concat : t -> t -> t
end

module To_read : sig
  type t

  val of_bool : bool -> t
  val empty : t
  val get : t -> bool option
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val concat : t -> t -> t
end

module Is_feed : sig
  type t

  val of_bool : bool -> t
  val empty : t
  val get : t -> bool option
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val concat : t -> t -> t
end

module Last_visited_at : sig
  type t

  val of_time : Time.t -> t
  val empty : t
  val get : t -> Time.t option
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val concat : t -> t -> t
end

type t

val make :
  Uri.t ->
  Time.t ->
  ?updated_at:Time.t list ->
  ?maybe_name:Name.t option ->
  ?labels:Label_set.t ->
  ?extended:Extended.t list ->
  ?shared:Shared.t ->
  ?to_read:To_read.t ->
  ?last_visited_at:Last_visited_at.t ->
  ?is_feed:Is_feed.t ->
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
val shared : t -> Shared.t
val to_read : t -> To_read.t
val last_visited_at : t -> Last_visited_at.t
val is_feed : t -> Is_feed.t
val map_labels : (Label_set.t -> Label_set.t) -> t -> t
val of_post : Pinboard.Post.t -> t
val t_of_yaml : Yaml.value -> t
val yaml_of_t : t -> Yaml.value

module Html : sig
  module Attrs = Prelude.Markup_ext.Attrs

  val entity_of_attrs : Attrs.t -> Name_set.t -> Label_set.t -> Extended.t list -> t
end
