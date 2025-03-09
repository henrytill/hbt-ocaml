let ( let@ ) = ( @@ )

module List_ext = struct
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
      | n, x :: l -> x :: go (n - 1) l
    in
    if n < 0 then invalid_arg "List_ext.take";
    go n l
end
