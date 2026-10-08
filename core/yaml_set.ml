include Yaml_set_intf

module Make (Elt : ORDERED_YAML_TYPE) = struct
  include Set.Make (Elt)

  let pp = Fmt.braces (Fmt.iter ~sep:Fmt.semi iter Elt.pp)

  (* A null entry is absent in every set (henrytill/hbt-data#44), so it is
     dropped here rather than by each element; [Elt.option_of_yaml] decides
     only what its own type treats as absent besides. *)
  let entry_of_yaml = function
    | `Null -> None
    | value -> Elt.option_of_yaml value

  let t_of_yaml value = of_list (Prelude.Yaml_ext.filter_map_array_exn entry_of_yaml value)
  let yaml_of_t set = Yaml.Util.list Elt.yaml_of_t (elements set)
  let of_option = Option.fold ~none:empty ~some:singleton
end
