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

  exception Invalid_month_name of string

  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
end

(** Update timestamps are a set so that an instant recorded by two entities with the same URI
    appears once however many times the input carried it, as with {!Extended_set}. A set is also
    sorted by construction, which is the ordering the wire format has always had. *)
module Time_set : sig
  include Set.S with type elt = Time.t

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

(** Descriptions are a set so that merging entities unions them, as it does {!Name_set} and
    {!Label_set}: a description shared by two entities with the same URI appears once however many
    times the input carried it. *)
module Extended_set : sig
  include Set.S with type elt = Extended.t

  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
  val of_option : Extended.t option -> t
end

module Shared : Flag_intf.S
module To_read : Flag_intf.S
module Is_feed : Flag_intf.S

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
  ?updated_at:Time_set.t ->
  ?maybe_name:Name.t option ->
  ?labels:Label_set.t ->
  ?extended:Extended_set.t ->
  ?shared:Shared.t ->
  ?to_read:To_read.t ->
  ?last_visited_at:Last_visited_at.t ->
  ?is_feed:Is_feed.t ->
  unit ->
  t

val empty : t
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val update : Time.t -> Name_set.t -> Label_set.t -> Extended_set.t -> t -> t
val absorb : t -> t -> t
val uri : t -> Uri.t
val created_at : t -> Time.t
val updated_at : t -> Time_set.t
val names : t -> Name_set.t
val labels : t -> Label_set.t
val extended : t -> Extended_set.t
val shared : t -> Shared.t
val to_read : t -> To_read.t
val last_visited_at : t -> Last_visited_at.t
val is_feed : t -> Is_feed.t
val map_labels : (Label_set.t -> Label_set.t) -> t -> t
val of_post : Pinboard.Post.t -> t

exception Missing_uri
(** Raised by {!t_of_yaml} for an entity with no [uri], or an empty one. *)

val t_of_yaml : Yaml.value -> t
val yaml_of_t : t -> Yaml.value

module Html : sig
  module Attrs = Prelude.Markup_ext.Attrs

  val entity_of_attrs : Attrs.t -> Name_set.t -> Label_set.t -> Extended_set.t -> t
end
