open Hbt

(* Characters that are significant in both attribute and text position. *)
let hostile = {|Tom & Jerry <b> "quoted" 'single'|}
let uri_string = "https://example.org/?a=1&b=2"

let collection_with ~name ~label ~description =
  let open Entity in
  let coll = Collection.create () in
  let e =
    Entity.make
      (Uri.of_string uri_string)
      (Time.of_string "2023-11-15T00:00:00Z")
      ~maybe_name:(Some (Name.of_string name))
      ~labels:(Label_set.singleton (Label.of_string label))
      ~extended:[ Extended.of_string description ]
      ()
  in
  ignore (Collection.upsert coll e);
  coll

let hostile_collection () = collection_with ~name:hostile ~label:"a&b" ~description:hostile

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i = i + n <= h && (String.equal (String.sub haystack i n) needle || go (i + 1)) in
  go 0

let check_contains output needle =
  Alcotest.(check bool) (Printf.sprintf "output contains %S" needle) true (contains output needle)

let test_escapes_attributes () =
  let output = Html.format (hostile_collection ()) in
  check_contains output {|HREF="https://example.org/?a=1&amp;b=2"|};
  check_contains output {|TAGS="a&amp;b"|}

let test_escapes_text_content () =
  let output = Html.format (hostile_collection ()) in
  (* & < > are escaped; quotes are safe in text position and pass through. *)
  check_contains output {|>Tom &amp; Jerry &lt;b&gt; "quoted" 'single'</A>|};
  check_contains output {|<DD>Tom &amp; Jerry &lt;b&gt; "quoted" 'single'|}

let test_roundtrip_preserves_markup_characters () =
  let reparsed = Html.parse (Html.format (hostile_collection ())) in
  let open Entity in
  Alcotest.(check int) "same length" 1 (Collection.length reparsed);
  let id = Option.get (Collection.id reparsed (Uri.canonicalize (Uri.of_string uri_string))) in
  let e = Collection.entity reparsed id in
  Alcotest.(check (list string))
    "name survives the round trip"
    [ hostile ]
    (List.map Name.to_string (Name_set.elements (names e)));
  Alcotest.(check (list string))
    "label survives the round trip"
    [ "a&b" ]
    (List.map Label.to_string (Label_set.elements (labels e)));
  Alcotest.(check (list string))
    "extended survives the round trip"
    [ hostile ]
    (List.map Extended.to_string (extended e))

let test_preserves_non_http_schemes () =
  let coll = Collection.create () in
  List.iter
    (fun s ->
      let e = Entity.(make (Uri.of_string s) (Time.of_string "2023-11-15T00:00:00Z") ()) in
      ignore (Collection.upsert coll e))
    [ "ftp://ftp.example.org/pub"; "gopher://gopher.example.org/" ];
  let output = Html.format coll in
  check_contains output {|HREF="ftp://ftp.example.org/pub"|};
  check_contains output {|HREF="gopher://gopher.example.org/"|}

let parse_one attrs =
  let doc =
    Printf.sprintf
      "<!DOCTYPE NETSCAPE-Bookmark-file-1>\n\
       <DL><p>\n\
       <DT><A HREF=\"https://a.org/\" ADD_DATE=\"0\" %s>title</A>\n\
       </DL><p>\n"
      attrs
  in
  let coll = Html.parse doc in
  let id =
    Option.get (Collection.id coll Entity.(Uri.canonicalize (Uri.of_string "https://a.org/")))
  in
  Collection.entity coll id

let labels_of e = List.map Entity.Label.to_string (Entity.Label_set.elements (Entity.labels e))
let to_read_of e = Entity.To_read.get (Entity.to_read e)

let test_tags_are_trimmed () =
  let e = parse_one {|TAGS="x, y ,  z"|} in
  Alcotest.(check (list string)) "tags trimmed, no empties" [ "x"; "y"; "z" ] (labels_of e)

let test_toread_tag_sets_flag () =
  let e = parse_one {|TAGS="x, toread"|} in
  Alcotest.(check (list string)) "toread is not a label" [ "x" ] (labels_of e);
  Alcotest.(check (option bool)) "toread tag sets the flag" (Some true) (to_read_of e)

let test_toread_matched_exactly () =
  let e = parse_one {|TAGS="toreading"|} in
  Alcotest.(check (list string)) "toreading is an ordinary label" [ "toreading" ] (labels_of e);
  Alcotest.(check (option bool)) "toreading does not set the flag" None (to_read_of e)

let test_explicit_toread_wins_either_order () =
  let tag_first = parse_one {|TAGS="toread" TOREAD="0"|} in
  let attr_first = parse_one {|TOREAD="0" TAGS="toread"|} in
  Alcotest.(check (option bool))
    "explicit TOREAD wins when the tag comes first"
    (Some false)
    (to_read_of tag_first);
  Alcotest.(check (option bool))
    "explicit TOREAD wins when the attribute comes first"
    (Some false)
    (to_read_of attr_first)

let tests =
  let open Alcotest in
  [
    ( "Parser",
      [
        test_case "tags are trimmed" `Quick test_tags_are_trimmed;
        test_case "toread tag sets flag" `Quick test_toread_tag_sets_flag;
        test_case "toread matched exactly" `Quick test_toread_matched_exactly;
        test_case "explicit toread wins either order" `Quick test_explicit_toread_wins_either_order;
      ] );
    ( "Formatter",
      [
        test_case "escapes attributes" `Quick test_escapes_attributes;
        test_case "escapes text content" `Quick test_escapes_text_content;
        test_case
          "roundtrip preserves markup characters"
          `Quick
          test_roundtrip_preserves_markup_characters;
        test_case "preserves non-http schemes" `Quick test_preserves_non_http_schemes;
      ] );
  ]

let () = Alcotest.run "Html" tests
