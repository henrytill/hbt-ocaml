open Prelude
open Cmarkit

module Fold_state = struct
  type t = {
    name : Entity.Name.t option;
    time : Entity.Time.t option;
    uri : Entity.Uri.t option;
    labels : Entity.Label.t list;
    maybe_parent : Collection.Id.t option;
    parents : Collection.Id.t list;
  }

  let empty =
    { name = None; time = None; uri = None; labels = []; maybe_parent = None; parents = [] }

  let[@warning "-32"] pp fmt st =
    let open Format in
    let none fmt () = fprintf fmt "None" in
    let pp_sep fmt () = fprintf fmt ";@;<1 2>" in
    let pp_print_name = pp_print_option ~none Entity.Name.pp in
    let pp_print_time = pp_print_option ~none Entity.Time.pp in
    let pp_print_uri = pp_print_option ~none Entity.Uri.pp in
    let pp_print_labels = pp_print_list ~pp_sep Entity.Label.pp in
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
        let labels = Entity.Label_set.of_list st.labels in
        Some (Entity.make uri time ~maybe_name:st.name ~labels ())
    | _ -> None
end

let kdefault () : 'a Folder.result = Folder.default

let get_heading_text (h : Block.Heading.t) (kf : unit -> 'a Folder.result)
    (ks : string -> 'a Folder.result) : 'a Folder.result =
  match Block.Heading.inline h with
  | Inline.Text (t, _) -> ks t
  | _ -> kf ()

let block m ((c, st) : Collection.t * Fold_state.t) = function
  | Block.Heading (heading, _) when Block.Heading.level heading = 1 ->
      let@ heading_text = get_heading_text heading kdefault in
      let time = Some (Entity.Time.of_string heading_text) in
      let st = { st with time; maybe_parent = None; labels = [] } in
      Folder.ret (c, st)
  | Block.Heading (heading, _) ->
      let heading_level = Block.Heading.level heading in
      let labels = List_ext.take (heading_level - 2) st.labels in
      let@ heading_text = get_heading_text heading kdefault in
      let labels = Entity.Label.of_string heading_text :: labels in
      let st = { st with labels } in
      Folder.ret (c, st)
  | Block.List (list, _) ->
      let st =
        match st.maybe_parent with
        | None -> st
        | Some parent -> { st with parents = parent :: st.parents }
      in
      let c, st =
        List.fold_left
          (fun acc (item, _) -> Folder.fold_block m acc (Block.List_item.block item))
          (c, st)
          (Block.List'.items list)
      in
      let st = { st with maybe_parent = None; parents = List_ext.drop1 st.parents } in
      Folder.ret (c, st)
  | _ -> Folder.default

let save_entity c st =
  let entity = Option.get (Fold_state.to_entity st) in
  let id = Collection.upsert c entity in
  Option.iter (fun parent -> Collection.add_edges c parent id) (List_ext.hd_opt st.parents);
  let st = { st with uri = None; name = None; maybe_parent = Some id } in
  Folder.ret (c, st)

let handle_autolink (link : Inline.Autolink.t) ((c, st) : Collection.t * Fold_state.t) =
  let link_text, _ = Inline.Autolink.link link in
  let uri = Some (Entity.Uri.of_string link_text) in
  let st = { st with uri } in
  save_entity c st

let get_def (l : Inline.Link.t) (kf : unit -> 'a) (ks : Link_definition.t -> 'a) : 'a =
  match Inline.Link.reference l with
  | `Inline (link_def, _) -> ks link_def
  | _ -> kf ()

let get_dest (ld : Link_definition.t) (kf : unit -> 'a) (ks : string -> 'a) : 'a =
  match Link_definition.dest ld with
  | Some (link_dest, _) -> ks link_dest
  | _ -> kf ()

let rec extract_string (inlines : Inline.t list) : string =
  let rec go acc = function
    | [] -> String.concat String.empty (List.rev acc)
    | Inline.Code_span (s, _) :: xs ->
        let a = Printf.sprintf "`%s`" (Inline.Code_span.code s) in
        go (a :: acc) xs
    | Inline.Inlines (is, _) :: xs ->
        let a = extract_string is in
        go (a :: acc) xs
    | Inline.Text (t, _) :: xs -> go (t :: acc) xs
    | _ :: xs -> go acc xs
  in
  go [] inlines

let get_text (link : Inline.Link.t) : string option =
  Inline.Link.text link
  |> Inline.normalize
  |> List_ext.singleton
  |> extract_string
  |> String.trim
  |> option_of_string

let handle_link (link : Inline.Link.t) ((c, st) : Collection.t * Fold_state.t) =
  let@ link_def = get_def link kdefault in
  let@ link_dest = get_dest link_def kdefault in
  let link_text = get_text link in
  let uri = Some (Entity.Uri.of_string link_dest) in
  let name = Option.map Entity.Name.of_string link_text in
  let st = { st with uri; name } in
  save_entity c st

let inline _m (acc : Collection.t * Fold_state.t) = function
  | Inline.Autolink (a, _) -> handle_autolink a acc
  | Inline.Link (l, _) -> handle_link l acc
  | _ -> Folder.default

let parse input =
  let folder = Folder.make ~block ~inline () in
  let initial = Collection.create () in
  let state = Fold_state.empty in
  let doc = Doc.of_string input in
  let final, _state = Folder.fold_doc folder (initial, state) doc in
  assert (initial == final);
  assert (initial = final);
  final
