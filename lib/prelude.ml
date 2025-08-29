let ( let@ ) = ( @@ )

let option_of_string s =
  if String.length s = 0 then
    None
  else
    Some s

module List_ext = struct
  let singleton x = [ x ]

  let hd_opt = function
    | [] -> None
    | x :: _ -> Some x
end

module Markup_ext = struct
  module Attrs = struct
    type t = ((string * string) * string) list

    let get_opt (k : string) (attrs : t) : string option =
      let foo = List.assoc_opt (String.empty, k) attrs in
      match foo with
      | Some "" -> None
      | other -> other

    let get (k : string) (attrs : t) : string = Option.value ~default:String.empty (get_opt k attrs)
  end
end

module Yaml_ext = struct
  let get_field ~key value =
    match Yaml.Util.find_exn key value with
    | Some v -> v
    | None -> invalid_arg ("Missing field: " ^ key)

  let map_optional_field_exn ~key ~f value =
    let g = function
      | Some (`String "") -> None
      | Some other -> Some (f other)
      | None -> None
    in
    g (Yaml.Util.find_exn key value)

  let map_array_exn f = function
    | `A items -> List.map f items
    | _ -> raise (Yaml.Util.Value_error "Expected a value array")

  let iter_array_exn f = function
    | `A items -> List.iter f items
    | _ -> raise (Yaml.Util.Value_error "Expected a value array")

  let fold_object_exn f acc = function
    | `O assoc -> List.fold_left f acc assoc
    | _ -> raise (Yaml.Util.Value_error "Expected an object")

  let int_of_float_exn value = int_of_float (Yaml.Util.to_float_exn value)
end
