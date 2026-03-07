module Id : sig
  type t

  val to_int : t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

type t

val create : unit -> t
val make : int -> t
val length : t -> int
val is_empty : t -> bool
val contains : t -> Entity.Uri.t -> bool
val id : t -> Entity.Uri.t -> Id.t option
val insert : t -> Entity.t -> Id.t
val upsert : t -> Entity.t -> Id.t
val add_edge : t -> Id.t -> Id.t -> unit
val add_edges : t -> Id.t -> Id.t -> unit
val entity : t -> Id.t -> Entity.t
val edges : t -> Id.t -> Id.t array
val entities : t -> Entity.t array
val t_of_yaml : Yaml.value -> t
val yaml_of_t : t -> Yaml.value
val iter_labels : (Entity.Label_set.t -> Entity.Label_set.t) -> t -> unit
val update_labels : t -> Yaml.value -> unit
val of_posts : Pinboard.Post.t list -> t
