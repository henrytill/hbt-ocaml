include module type of Yaml_set_intf
(** @inline *)

(** The set behind each multi-valued field of an entity, encoded as a YAML array. [t_of_yaml] drops
    a null entry and decodes each other with [Elt.option_of_yaml], dropping the ones it maps to
    [None].

    Each application is its own type, so {!Entity.Label_set.t} is not {!Entity.Name_set.t}. What is
    shared is only the encoding, which used to be written out once per field until the copies began
    to drift (#52). *)
module Make (Elt : ORDERED_YAML_TYPE) : S with type elt = Elt.t
