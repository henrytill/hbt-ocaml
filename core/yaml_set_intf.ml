module type ORDERED_YAML_TYPE = sig
  include Set.OrderedType

  val pp : Format.formatter -> t -> unit
  val yaml_of_t : t -> Yaml.value

  (* Decodes one non-null entry of the array, where [None] is an absent entry and is dropped. *)
  val option_of_yaml : Yaml.value -> t option
end

module type S = sig
  include Set.S

  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
  val of_option : elt option -> t
end
