(* The set behind each multi-valued field of an entity. Its encoding used to
   be written out once per field, and the copies had begun to drift (#52).
   Each application is still its own type, so a Label_set.t is not a
   Name_set.t. *)
module Make (Elt : Yaml_set_intf.Elt) = struct
  include Set.Make (Elt)

  let pp = Fmt.braces (Fmt.iter ~sep:Fmt.semi iter Elt.pp)
  let t_of_yaml value = of_list (Prelude.Yaml_ext.filter_map_array_exn Elt.entry_of_yaml value)
  let yaml_of_t set = Yaml.Util.list Elt.yaml_of_t (elements set)
end
