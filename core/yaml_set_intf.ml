module type S = sig
  include Set.S

  val pp : Format.formatter -> t -> unit
  val t_of_yaml : Yaml.value -> t
  val yaml_of_t : t -> Yaml.value
end
