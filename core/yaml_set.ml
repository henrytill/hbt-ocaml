include Yaml_set_intf

module Make (Elt : ORDERED_YAML_TYPE) = struct
  include Set.Make (Elt)

  let pp = Fmt.braces (Fmt.iter ~sep:Fmt.semi iter Elt.pp)
  let t_of_yaml value = of_list (Prelude.Yaml_ext.filter_map_array_exn Elt.entry_of_yaml value)
  let yaml_of_t set = Yaml.Util.list Elt.yaml_of_t (elements set)
end
