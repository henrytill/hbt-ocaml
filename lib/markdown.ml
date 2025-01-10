open Prelude
open Cmarkit

module Fold_state = struct
  type t = {
    name : Collection.Name.t option;
    time : Collection.Time.t option;
    uri : Uri.t option;
    labels : Collection.Label.t list;
    maybe_parent : Collection.Id.t option;
    parents : Collection.Id.t list;
  }

  let empty =
    { name = None; time = None; uri = None; labels = []; maybe_parent = None; parents = [] }

  let pp fmt st =
    let open Format in
    let none fmt () = fprintf fmt "None" in
    let pp_sep fmt () = fprintf fmt ";@;<1 2>" in
    let pp_print_name = pp_print_option ~none Collection.Name.pp in
    let pp_print_time = pp_print_option ~none Collection.Time.pp in
    let pp_print_uri = pp_print_option ~none Uri.pp in
    let pp_print_labels = pp_print_list ~pp_sep Collection.Label.pp in
    let pp_print_maybe_parent = pp_print_option ~none Collection.Id.pp in
    let pp_print_parents = pp_print_list ~pp_sep Collection.Id.pp in
    fprintf fmt "@[<hv>{";
    fprintf fmt "@;<1 2>@[name =@ %a@];" pp_print_name st.name;
    fprintf fmt "@;<1 2>@[time =@ %a@];" pp_print_time st.time;
    fprintf fmt "@;<1 2>@[uri =@ %a@];" pp_print_uri st.uri;
    fprintf fmt "@;<1 2>@[labels =@ @[<hv>[@;<0 2>%a@;<0 0>]@]@];" pp_print_labels st.labels;
    fprintf fmt "@;<1 2>@[maybe_parent =@ %a@];" pp_print_maybe_parent st.maybe_parent;
    fprintf fmt "@;<1 2>@[parents =@ @[<hv>[@;<0 2>%a@;<0 0>]@]@];" pp_print_parents st.parents;
    fprintf fmt "@;<1 0>}@]"

  let to_entity st =
    match (st.uri, st.time) with
    | Some uri, Some time ->
        let labels = Collection.Label_set.of_list st.labels in
        Some (Collection.Entity.make uri time st.name labels)
    | _ -> None
end

let _inspect_text text =
  Format.(
    pp_print_string std_formatter text;
    pp_print_newline std_formatter ())

let _inspect_st st =
  Format.(
    Fold_state.pp std_formatter st;
    pp_print_newline std_formatter ())

let option_of_string s =
  if Int.equal (String.length s) 0 then
    None
  else
    Some s

let get_heading_text (h : Block.Heading.t) kf ks =
  match Block.Heading.inline h with
  | Inline.Text (t, _) -> ks t
  | _ -> kf ()

let block m ((c, st) : Collection.t * Fold_state.t) = function
  | Block.Heading (heading, _) when Int.equal (Block.Heading.level heading) 1 ->
      let@ heading_text = get_heading_text heading (fun () -> Folder.default) in
      let time = Some (Collection.Time.of_string heading_text) in
      let st = { st with time; maybe_parent = None; labels = [] } in
      Folder.ret (c, st)
  | Block.Heading (heading, _) ->
      let heading_level = Block.Heading.level heading in
      let labels = List_ext.take (heading_level - 2) st.labels in
      let@ heading_text = get_heading_text heading (fun () -> Folder.default) in
      let labels = Collection.Label.of_string heading_text :: labels in
      let st = { st with labels } in
      Folder.ret (c, st)
  | Block.List (list, _) ->
      let st =
        match st.maybe_parent with
        | Some parent -> { st with parents = parent :: st.parents }
        | None -> st
      in
      let c, st =
        List.fold_left
          (fun acc (item, _) -> Folder.fold_block m acc (Block.List_item.block item))
          (c, st)
          (Block.List'.items list)
      in
      let st = { st with maybe_parent = None; parents = List_ext.tail_or_nil st.parents } in
      Folder.ret (c, st)
  | _ -> Folder.default

let save_entity c st =
  let entity = Fold_state.to_entity st |> Option.get in
  let id = Collection.upsert c entity in
  begin
    match st.parents with
    | parent :: _ -> Collection.add_edges c parent id
    | [] -> ()
  end;
  let st = { st with uri = None; name = None; maybe_parent = Some id } in
  Folder.ret (c, st)

let handle_autolink (link : Inline.Autolink.t) ((c, st) : Collection.t * Fold_state.t) =
  let link_text, _ = Inline.Autolink.link link in
  let uri = Some (Uri.of_string link_text) in
  let st = { st with uri } in
  save_entity c st

let get_def l kf ks =
  match Inline.Link.reference l with
  | `Inline (link_def, _) -> ks link_def
  | _ -> kf ()

let get_dest ld kf ks =
  match Link_definition.dest ld with
  | Some (link_dest, _) -> ks link_dest
  | _ -> kf ()

let rec extract_string (inlines : Inline.t list) : string =
  (* TODO: handle more cases, rewrite as loop *)
  let rec go acc = function
    | Inline.Code_span (s, _) :: xs ->
        let a = Printf.sprintf "`%s`" (Inline.Code_span.code s) in
        go (a :: acc) xs
    | Inline.Inlines (is, _) :: xs ->
        let a = extract_string is in
        go (a :: acc) xs (* ugh *)
    | Inline.Text (t, _) :: xs -> go (t :: acc) xs
    | _ :: xs -> go acc xs
    | [] -> String.concat String.empty (List.rev acc)
  in
  go [] inlines

let get_text (link : Inline.Link.t) : string option =
  Inline.Link.text link |> Inline.normalize |> fun x -> [ x ] |> extract_string |> option_of_string

let handle_link (link : Inline.Link.t) ((c, st) : Collection.t * Fold_state.t) =
  let kf () = Folder.default in
  let@ link_def = get_def link kf in
  let@ link_dest = get_dest link_def kf in
  let link_text = get_text link in
  let uri = Some (Uri.of_string link_dest) in
  let name = Option.map Collection.Name.of_string link_text in
  let st = { st with uri; name } in
  save_entity c st

let inline _m (acc : Collection.t * Fold_state.t) = function
  | Inline.Autolink (a, _) -> handle_autolink a acc
  | Inline.Link (l, _) -> handle_link l acc
  | _ -> Folder.default

let parse input =
  let folder = Folder.make ~block ~inline () in
  let doc = Doc.of_string input in
  let collection = Collection.create () in
  let state = Fold_state.empty in
  let ret, _state = Folder.fold_doc folder (collection, state) doc in
  assert (collection == ret);
  assert (collection = ret);
  ret
