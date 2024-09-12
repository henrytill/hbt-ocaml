open Hbt.Collection

module Pretty_printing = struct
  let pp_list pp_item ppf list =
    let open Format in
    let pp_sep fmt () = fprintf fmt ";@;<1 2>" in
    fprintf ppf "@[<hv>[@;<0 2>%a@;<0 0>]@]" (pp_print_list ~pp_sep pp_item) list

  let archive_uri = Uri.of_string "https://archive.org/"
  let created_at = Time.of_string "September 2, 2024"
  let archive_maybe_name = Some (Name.of_string "The Internet Archive")

  let%expect_test "bare" =
    let entity = Entity.make archive_uri created_at None Label_set.empty in
    Entity.pp Format.std_formatter entity;
    [%expect
      {|
      {
        uri = https://archive.org/;
        created_at = "1725260400";
        updated_at = [];
        names = {};
        labels = {};
      }
      |}]

  let%expect_test "name and label" =
    let labels = Label_set.of_list [ Label.of_string "archives" ] in
    let entity = Entity.make archive_uri created_at archive_maybe_name labels in
    Entity.pp Format.std_formatter entity;
    [%expect
      {|
      {
        uri = https://archive.org/;
        created_at = "1725260400";
        updated_at = [];
        names = {"The Internet Archive"};
        labels = {"archives"};
      }
      |}]

  let%expect_test "more labels" =
    let labels = Label_set.of_list [ Label.of_string "archives"; Label.of_string "books" ] in
    let entity = Entity.make archive_uri created_at archive_maybe_name labels in
    Entity.pp Format.std_formatter entity;
    [%expect
      {|
      {
        uri = https://archive.org/;
        created_at = "1725260400";
        updated_at = [];
        names = {"The Internet Archive"};
        labels = {"archives"; "books"};
      }
      |}]

  let%expect_test "multiples" =
    let archive =
      let labels = Label_set.of_list [ Label.of_string "archives"; Label.of_string "search" ] in
      Entity.make archive_uri created_at archive_maybe_name labels
    in
    let google =
      let google_uri = Uri.of_string "https://www.google.com/" in
      let google_maybe_name = Some (Name.of_string "Google") in
      let labels = Label_set.of_list [ Label.of_string "search" ] in
      Entity.make google_uri created_at google_maybe_name labels
    in
    let entities = [ archive; google ] in
    pp_list Entity.pp Format.std_formatter entities;
    [%expect
      {|
      [
        {
          uri = https://archive.org/;
          created_at = "1725260400";
          updated_at = [];
          names = {"The Internet Archive"};
          labels = {"archives"; "search"};
        };
        {
          uri = https://www.google.com/;
          created_at = "1725260400";
          updated_at = [];
          names = {"Google"};
          labels = {"search"};
        }
      ]
      |}]
end

module To_html = struct
  let%expect_test "to_html empty" =
    let collection = make () in
    let actual = to_html collection in
    print_endline actual;
    [%expect
      {|
      <!DOCTYPE NETSCAPE-Bookmark-file-1>
      <META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
      <TITLE>Bookmarks</TITLE>

      <dl></dl>
      |}]

  let%expect_test "to_html basic" =
    let labels_foo = Label_set.(empty |> add (Label.of_string "Foo")) in
    let foo =
      Entity.make
        (Uri.of_string "https://foo.com")
        (Time.of_string "November 15, 2023")
        (Some (Name.of_string "Foo"))
        labels_foo
    in
    let labels_bar = Label_set.add (Label.of_string "Bar") labels_foo in
    let bar =
      Entity.make
        (Uri.of_string "https://bar.com")
        (Time.of_string "November 15, 2023")
        (Some (Name.of_string "Bar"))
        labels_bar
    in
    let labels_baz = Label_set.add (Label.of_string "Baz") labels_bar in
    let baz =
      Entity.make
        (Uri.of_string "https://baz.com")
        (Time.of_string "November 15, 2023")
        (Some (Name.of_string "Baz"))
        labels_baz
    in
    let collection =
      let ret = make () in
      let _id_foo = upsert ret foo in
      let _id_bar = upsert ret bar in
      let _id_baz = upsert ret baz in
      ret
    in
    let actual = to_html collection in
    print_endline actual;
    [%expect
      {|
      <!DOCTYPE NETSCAPE-Bookmark-file-1>
      <META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
      <TITLE>Bookmarks</TITLE>

      <dl>
       <dt><a href="https://foo.com/" add_date="1700035200" tags="Foo">Foo</a></dt>
       <dt><a href="https://bar.com/" add_date="1700035200" tags="Bar,Foo">Bar</a>
       </dt>
       <dt>
        <a href="https://baz.com/" add_date="1700035200" tags="Bar,Baz,Foo">Baz</a>
       </dt>
      </dl>
      |}]
end
