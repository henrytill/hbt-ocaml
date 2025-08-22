let ( let@ ) = ( @@ )

let option_of_string s =
  if String.length s = 0 then
    None
  else
    Some s

module List_ext = struct
  let singleton x = [ x ]

  let tail_or_nil = function
    | [] -> []
    | _ :: l -> l

  let hd_opt = function
    | [] -> None
    | x :: _ -> Some x

  let take n l =
    let[@tail_mod_cons] rec go n l =
      match (n, l) with
      | 0, _ | _, [] -> []
      | n, x :: l -> x :: go (pred n) l
    in
    if n < 0 then invalid_arg "List_ext.take";
    go n l
end

module Markup_ext = struct
  module Attrs = struct
    type t = ((string * string) * string) list

    let get_opt (k : string) (attrs : t) : string option = List.assoc_opt (String.empty, k) attrs
    let get (k : string) (attrs : t) : string = Option.value ~default:String.empty (get_opt k attrs)
  end
end

module Yaml_ext = struct
  let get_field ~key value =
    match Yaml.Util.find_exn key value with
    | Some v -> v
    | None -> invalid_arg ("missing field: " ^ key)

  let map_optional_field ~key ~f value = Option.map f (Yaml.Util.find_exn key value)

  let map_array f = function
    | `A items -> List.map f items
    | _ -> invalid_arg "expected array"

  let fold_object f acc = function
    | `O assoc -> List.fold_left f acc assoc
    | _ -> invalid_arg "expected object"

  let int_of_float_value value = int_of_float (Yaml.Util.to_float_exn value)
end
