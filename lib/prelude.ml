let ( let@ ) = ( @@ )

module List_ext = struct
  let tail_or_nil l = try List.tl l with _ -> []

  let take n l =
    let[@tail_mod_cons] rec go n l =
      match (n, l) with
      | 0, _ | _, [] -> []
      | n, x :: l -> x :: go (n - 1) l
    in
    if n < 0 then invalid_arg "List_ext.take";
    go n l
end
