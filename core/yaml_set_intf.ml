module type ORDERED_YAML_TYPE = sig
  include Set.OrderedType

  val pp : Format.formatter -> t -> unit
  val yaml_of_t : t -> Yaml.value

  (* Decodes one entry of the array, where [None] drops it. *)
  val entry_of_yaml : Yaml.value -> t option
end

module type S = sig
  include Set.S

  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
  val of_option : elt option -> t
end
