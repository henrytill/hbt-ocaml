let ( let@ ) = ( @@ )

let option_of_string s =
  if String.length s = 0 then
    None
  else
    Some s

let unless (cond : bool) (f : unit -> 'a) : 'a =
  if not cond then
    f ()

module List_ext = struct
  let singleton x = [ x ]

  let hd_opt = function
    | [] -> None
    | x :: _ -> Some x

  let drop1 = function
    | [] -> []
    | _ :: tl -> tl

  let uncons = function
    | [] -> (None, [])
    | x :: xs -> (Some x, xs)
end

module Markup_ext = struct
  module Attrs = struct
    type elt = (string * string) * string
    type t = elt list

    let is_empty (attrs : t) = List.is_empty attrs
  end
end

module Yaml_ext = struct
  let get_field ~key value =
    match Yaml.Util.find_exn key value with
    | None -> invalid_arg ("Missing field: " ^ key)
    | Some v -> v

  let map_optional_field_exn ~key ~f value =
    let g = function
      | None -> None
      | Some (`String "") -> None
      | Some other -> Some (f other)
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
