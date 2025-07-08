open Hbt

let testable_id = Alcotest.testable Collection.Id.pp Collection.Id.equal
let testable_entity = Alcotest.testable Collection.Entity.pp Collection.Entity.equal

let test_empty () =
  let actual = Markdown.parse String.empty in
  Alcotest.(check bool) "is_empty" true (Collection.is_empty actual)

let test_only_date () =
  let input = "# November 15, 2023\n" in
  let actual = Markdown.parse input in
  Alcotest.(check bool) "is_empty" true (Collection.is_empty actual)

let no_labels = {|# November 15, 2023

- [Foo](https://foo.com)
- [Bar](https://bar.com)
|}

let test_no_labels () =
  let open Collection in
  let actual = Markdown.parse no_labels in
  let time = Time.of_string "November 15, 2023" in
  let labels = Label_set.empty in
  let foo =
    Entity.make (Uri.of_string "https://foo.com") time (Some (Name.of_string "Foo")) labels
  in
  let bar =
    Entity.make (Uri.of_string "https://bar.com") time (Some (Name.of_string "Bar")) labels
  in
  let expected, id_foo, id_bar =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    (ret, id_foo, id_bar)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo);
  Alcotest.(check testable_entity) "same entity" (entity expected id_bar) (entity actual id_bar)

let no_url = {|# November 15, 2023

- Foo
|}

let test_no_url () =
  let actual = Markdown.parse no_url in
  Alcotest.(check bool) "is_empty" true (Collection.is_empty actual)

let no_title = {|# November 15, 2023

- <https://foo.com>
|}

let test_no_title () =
  let open Collection in
  let actual = Markdown.parse no_title in
  let foo =
    Entity.make
      (Uri.of_string "https://foo.com")
      (Time.of_string "November 15, 2023")
      None
      Label_set.empty
  in
  let expected, id_foo =
    let ret = create () in
    let id_foo = upsert ret foo in
    (ret, id_foo)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo)

let indented = {|# November 15, 2023

  - [Foo](https://foo.com)
|}

let test_indented () =
  let open Collection in
  let actual = Markdown.parse indented in
  let foo =
    Entity.make
      (Uri.of_string "https://foo.com")
      (Time.of_string "November 15, 2023")
      (Some (Name.of_string "Foo"))
      Label_set.empty
  in
  let expected, id_foo =
    let ret = create () in
    let id_foo = upsert ret foo in
    (ret, id_foo)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo)

let indented_double = {|# November 15, 2023

    - [Foo](https://foo.com)
|}

let test_indented_double () =
  let actual = Markdown.parse indented_double in
  Alcotest.(check bool) "is_empty" true (Collection.is_empty actual)

let parent = {|# November 15, 2023

- [Foo](https://foo.com)
  - [Bar](https://bar.com)
|}

let test_parent () =
  let open Collection in
  let actual = Markdown.parse parent in
  let time = Time.of_string "November 15, 2023" in
  let labels = Label_set.empty in
  let foo =
    Entity.make (Uri.of_string "https://foo.com") time (Some (Name.of_string "Foo")) labels
  in
  let bar =
    Entity.make (Uri.of_string "https://bar.com") time (Some (Name.of_string "Bar")) labels
  in
  let expected, id_foo, id_bar =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    (ret, id_foo, id_bar)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo);
  Alcotest.(check testable_entity) "same entity" (entity expected id_bar) (entity actual id_bar);
  Alcotest.(check (array testable_id)) "same edges" [| id_bar |] (edges actual id_foo);
  Alcotest.(check (array testable_id)) "same edges" [| id_foo |] (edges actual id_bar)

let parents =
  {|# November 15, 2023

- [Foo](https://foo.com)
  - [Bar](https://bar.com)
    - [Baz](https://baz.com)
|}

let test_parents () =
  let open Collection in
  let actual = Markdown.parse parents in
  let time = Time.of_string "November 15, 2023" in
  let labels = Label_set.empty in
  let foo =
    Entity.make (Uri.of_string "https://foo.com") time (Some (Name.of_string "Foo")) labels
  in
  let bar =
    Entity.make (Uri.of_string "https://bar.com") time (Some (Name.of_string "Bar")) labels
  in
  let baz =
    Entity.make (Uri.of_string "https://baz.com") time (Some (Name.of_string "Baz")) labels
  in
  let expected, id_foo, id_bar, id_baz =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    let id_baz = upsert ret baz in
    (ret, id_foo, id_bar, id_baz)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo);
  Alcotest.(check testable_entity) "same entity" (entity expected id_bar) (entity actual id_bar);
  Alcotest.(check testable_entity) "same entity" (entity expected id_baz) (entity actual id_baz);
  Alcotest.(check (array testable_id)) "same edges" [| id_bar |] (edges actual id_foo);
  Alcotest.(check (array testable_id)) "same edges" [| id_foo; id_baz |] (edges actual id_bar);
  Alcotest.(check (array testable_id)) "same edges" [| id_bar |] (edges actual id_baz)

let parents_indented =
  {|# November 15, 2023

  - [Foo](https://foo.com)
    - [Bar](https://bar.com)
      - [Baz](https://baz.com)
|}

let test_parents_indented () =
  let open Collection in
  let actual = Markdown.parse parents_indented in
  let time = Time.of_string "November 15, 2023" in
  let labels = Label_set.empty in
  let foo =
    Entity.make (Uri.of_string "https://foo.com") time (Some (Name.of_string "Foo")) labels
  in
  let bar =
    Entity.make (Uri.of_string "https://bar.com") time (Some (Name.of_string "Bar")) labels
  in
  let baz =
    Entity.make (Uri.of_string "https://baz.com") time (Some (Name.of_string "Baz")) labels
  in
  let expected, id_foo, id_bar, id_baz =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    let id_baz = upsert ret baz in
    (ret, id_foo, id_bar, id_baz)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo);
  Alcotest.(check testable_entity) "same entity" (entity expected id_bar) (entity actual id_bar);
  Alcotest.(check testable_entity) "same entity" (entity expected id_baz) (entity actual id_baz);
  Alcotest.(check (array testable_id)) "same edges" [| id_bar |] (edges actual id_foo);
  Alcotest.(check (array testable_id)) "same edges" [| id_foo; id_baz |] (edges actual id_bar);
  Alcotest.(check (array testable_id)) "same edges" [| id_bar |] (edges actual id_baz)

let single_parent =
  {|# November 15, 2023

- [Foo](https://foo.com)
  - [Bar](https://bar.com)
  - [Baz](https://baz.com)
  - [Quux](https://quux.com)
|}

let test_single_parent () =
  let open Collection in
  let actual = Markdown.parse single_parent in
  let time = Time.of_string "November 15, 2023" in
  let labels = Label_set.empty in
  let foo =
    Entity.make (Uri.of_string "https://foo.com") time (Some (Name.of_string "Foo")) labels
  in
  let bar =
    Entity.make (Uri.of_string "https://bar.com") time (Some (Name.of_string "Bar")) labels
  in
  let baz =
    Entity.make (Uri.of_string "https://baz.com") time (Some (Name.of_string "Baz")) labels
  in
  let quux =
    Entity.make (Uri.of_string "https://quux.com") time (Some (Name.of_string "Quux")) labels
  in
  let expected, id_foo, id_bar, id_baz, id_quux =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    let id_baz = upsert ret baz in
    let id_quux = upsert ret quux in
    (ret, id_foo, id_bar, id_baz, id_quux)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo);
  Alcotest.(check testable_entity) "same entity" (entity expected id_bar) (entity actual id_bar);
  Alcotest.(check testable_entity) "same entity" (entity expected id_baz) (entity actual id_baz);
  Alcotest.(check testable_entity) "same entity" (entity expected id_quux) (entity actual id_quux);
  Alcotest.(check (array testable_id))
    "same edges"
    [| id_bar; id_baz; id_quux |]
    (edges actual id_foo);
  Alcotest.(check (array testable_id)) "same edges" [| id_foo |] (edges actual id_bar);
  Alcotest.(check (array testable_id)) "same edges" [| id_foo |] (edges actual id_baz);
  Alcotest.(check (array testable_id)) "same edges" [| id_foo |] (edges actual id_quux)

let inverted_parent = {|# November 15, 2023

  - [Foo](https://foo.com)
- [Bar](https://bar.com)
|}

let test_inverted_parent () =
  let open Collection in
  let actual = Markdown.parse inverted_parent in
  let time = Time.of_string "November 15, 2023" in
  let labels = Label_set.empty in
  let foo =
    Entity.make (Uri.of_string "https://foo.com") time (Some (Name.of_string "Foo")) labels
  in
  let bar =
    Entity.make (Uri.of_string "https://bar.com") time (Some (Name.of_string "Bar")) labels
  in
  let expected, id_foo, id_bar =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    (ret, id_foo, id_bar)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo);
  Alcotest.(check testable_entity) "same entity" (entity expected id_bar) (entity actual id_bar)

let inverted_single_parent =
  {|# November 15, 2023

  - [Foo](https://foo.com)
  - [Bar](https://bar.com)
- [Baz](https://baz.com)
|}

let test_inverted_single_parent () =
  let open Collection in
  let actual = Markdown.parse inverted_single_parent in
  let time = Time.of_string "November 15, 2023" in
  let labels = Label_set.empty in
  let foo =
    Entity.make (Uri.of_string "https://foo.com") time (Some (Name.of_string "Foo")) labels
  in
  let bar =
    Entity.make (Uri.of_string "https://bar.com") time (Some (Name.of_string "Bar")) labels
  in
  let baz =
    Entity.make (Uri.of_string "https://baz.com") time (Some (Name.of_string "Baz")) labels
  in
  let expected, id_foo, id_bar, id_baz =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    let id_baz = upsert ret baz in
    (ret, id_foo, id_bar, id_baz)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo);
  Alcotest.(check testable_entity) "same entity" (entity expected id_bar) (entity actual id_bar);
  Alcotest.(check testable_entity) "same entity" (entity expected id_baz) (entity actual id_baz)

let label = {|# November 15, 2023

## Foo

- [Foo](https://foo.com)
- [Bar](https://bar.com)
|}

let test_label () =
  let open Collection in
  let actual = Markdown.parse label in
  let time = Time.of_string "November 15, 2023" in
  let labels = Label_set.singleton (Label.of_string "Foo") in
  let foo =
    Entity.make (Uri.of_string "https://foo.com") time (Some (Name.of_string "Foo")) labels
  in
  let bar =
    Entity.make (Uri.of_string "https://bar.com") time (Some (Name.of_string "Bar")) labels
  in
  let expected, id_foo, id_bar =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    (ret, id_foo, id_bar)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo);
  Alcotest.(check testable_entity) "same entity" (entity expected id_bar) (entity actual id_bar)

let labels =
  {|# November 15, 2023

## Foo

- [Foo](https://foo.com)
- [Bar](https://bar.com)

## Baz

- [Baz](https://baz.com)
- [Quux](https://quux.com)
|}

let test_labels () =
  let open Collection in
  let actual = Markdown.parse labels in
  let time = Time.of_string "November 15, 2023" in
  let labels_foo = Label_set.singleton (Label.of_string "Foo") in
  let foo =
    Entity.make (Uri.of_string "https://foo.com") time (Some (Name.of_string "Foo")) labels_foo
  in
  let bar =
    Entity.make (Uri.of_string "https://bar.com") time (Some (Name.of_string "Bar")) labels_foo
  in
  let labels_baz = Label_set.singleton (Label.of_string "Baz") in
  let baz =
    Entity.make (Uri.of_string "https://baz.com") time (Some (Name.of_string "Baz")) labels_baz
  in
  let quux =
    Entity.make (Uri.of_string "https://quux.com") time (Some (Name.of_string "Quux")) labels_baz
  in
  let expected, id_foo, id_bar, id_baz, id_quux =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    let id_baz = upsert ret baz in
    let id_quux = upsert ret quux in
    (ret, id_foo, id_bar, id_baz, id_quux)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo);
  Alcotest.(check testable_entity) "same entity" (entity expected id_bar) (entity actual id_bar);
  Alcotest.(check testable_entity) "same entity" (entity expected id_baz) (entity actual id_baz);
  Alcotest.(check testable_entity) "same entity" (entity expected id_quux) (entity actual id_quux)

let multiple_labels =
  {|# November 15, 2023

## Foo

- [Foo](https://foo.com)

### Bar

- [Bar](https://bar.com)

#### Baz

- [Baz](https://baz.com)
|}

let test_multiple_labels () =
  let open Collection in
  let actual = Markdown.parse multiple_labels in
  let time = Time.of_string "November 15, 2023" in
  let labels_foo = Label_set.singleton (Label.of_string "Foo") in
  let foo =
    Entity.make (Uri.of_string "https://foo.com") time (Some (Name.of_string "Foo")) labels_foo
  in
  let labels_bar = Label_set.add (Label.of_string "Bar") labels_foo in
  let bar =
    Entity.make (Uri.of_string "https://bar.com") time (Some (Name.of_string "Bar")) labels_bar
  in
  let labels_baz = Label_set.add (Label.of_string "Baz") labels_bar in
  let baz =
    Entity.make (Uri.of_string "https://baz.com") time (Some (Name.of_string "Baz")) labels_baz
  in
  let expected, id_foo, id_bar, id_baz =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    let id_baz = upsert ret baz in
    (ret, id_foo, id_bar, id_baz)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo);
  Alcotest.(check testable_entity) "same entity" (entity expected id_bar) (entity actual id_bar);
  Alcotest.(check testable_entity) "same entity" (entity expected id_baz) (entity actual id_baz)

let update =
  {|# December 5, 2023

## Foo

- [Foo](https://foo.com)

# December 6, 2023

## Bar

- [Bar](https://foo.com)
|}

let test_update () =
  let open Collection in
  let actual = Markdown.parse update in
  let uri = Uri.of_string "https://foo.com" in
  let foo =
    Entity.make
      uri
      (Time.of_string "December 5, 2023")
      (Some (Name.of_string "Foo"))
      (Label_set.singleton (Label.of_string "Foo"))
  in
  let bar =
    Entity.make
      uri
      (Time.of_string "December 6, 2023")
      (Some (Name.of_string "Bar"))
      (Label_set.singleton (Label.of_string "Bar"))
  in
  let expected, id_foo, id_bar =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    (ret, id_foo, id_bar)
  in
  Alcotest.(check testable_id) "same id" id_foo id_bar;
  Alcotest.(check int) "same length" (length expected) 1;
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo)

let descending_dates =
  {|# December 6, 2023

## Foo

- [Foo](https://foo.com)

# December 5, 2023

## Bar

- [Bar](https://foo.com)
|}

let test_descending_dates () =
  let open Collection in
  let actual = Markdown.parse descending_dates in
  let uri = Uri.of_string "https://foo.com" in
  let foo =
    Entity.make
      uri
      (Time.of_string "December 6, 2023")
      (Some (Name.of_string "Foo"))
      (Label_set.singleton (Label.of_string "Foo"))
  in
  let bar =
    Entity.make
      uri
      (Time.of_string "December 5, 2023")
      (Some (Name.of_string "Bar"))
      (Label_set.singleton (Label.of_string "Bar"))
  in
  let expected, id_foo, id_bar =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    (ret, id_foo, id_bar)
  in
  Alcotest.(check testable_id) "same id" id_foo id_bar;
  Alcotest.(check int) "same length" (length expected) 1;
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo)

let mixed_dates =
  {|# December 6, 2023

## Foo

- [Foo](https://foo.com)

# December 5, 2023

## Bar

- [Bar](https://foo.com)

# December 7, 2023

## Baz

- [Baz](https://foo.com)
|}

let test_mixed_dates () =
  let open Collection in
  let actual = Markdown.parse mixed_dates in
  let uri = Uri.of_string "https://foo.com" in
  let foo =
    Entity.make
      uri
      (Time.of_string "December 6, 2023")
      (Some (Name.of_string "Foo"))
      (Label_set.singleton (Label.of_string "Foo"))
  in
  let bar =
    Entity.make
      uri
      (Time.of_string "December 5, 2023")
      (Some (Name.of_string "Bar"))
      (Label_set.singleton (Label.of_string "Bar"))
  in
  let baz =
    Entity.make
      uri
      (Time.of_string "December 7, 2023")
      (Some (Name.of_string "Baz"))
      (Label_set.singleton (Label.of_string "Baz"))
  in
  let expected, id_foo, id_bar, id_baz =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    let id_baz = upsert ret baz in
    (ret, id_foo, id_bar, id_baz)
  in
  Alcotest.(check bool) "same id" true (List.for_all (fun id -> id == id_foo) [ id_bar; id_baz ]);
  Alcotest.(check int) "same length" (length expected) 1;
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo)

let basic =
  {|# November 16, 2023

## Foo

- [Foo](https://foo.com)

### Bar

- <https://bar.com>

## Misc

- [Hello, world!](https://example.com/)
|}

let test_basic () =
  let open Collection in
  let actual = Markdown.parse basic in
  let time = Time.of_string "November 16, 2023" in
  let labels_foo = Label_set.singleton (Label.of_string "Foo") in
  let foo =
    Entity.make (Uri.of_string "https://foo.com") time (Some (Name.of_string "Foo")) labels_foo
  in
  let bar =
    Entity.make
      (Uri.of_string "https://bar.com")
      time
      None
      (Label_set.add (Label.of_string "Bar") labels_foo)
  in
  let ex =
    Entity.make
      (Uri.of_string "https://example.com/")
      time
      (Some (Name.of_string "Hello, world!"))
      (Label_set.singleton (Label.of_string "Misc"))
  in
  let expected, id_foo, id_bar, id_ex =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    let id_ex = upsert ret ex in
    (ret, id_foo, id_bar, id_ex)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo);
  Alcotest.(check testable_entity) "same entity" (entity expected id_bar) (entity actual id_bar);
  Alcotest.(check testable_entity) "same entity" (entity expected id_ex) (entity actual id_ex)

let nested =
  {|# November 17, 2023

## Foo

- [Foo](https://foo.com)
  - <https://bar.com>
  - [Hello, world!](https://example.com/)
    - [Quux](https://quux.com)
  - <https://baz.com>
|}

let test_nested () =
  let open Collection in
  let actual = Markdown.parse nested in
  let time = Time.of_string "November 17, 2023" in
  let labels = Label_set.singleton (Label.of_string "Foo") in
  let foo =
    Entity.make (Uri.of_string "https://foo.com") time (Some (Name.of_string "Foo")) labels
  in
  let bar = Entity.make (Uri.of_string "https://bar.com") time None labels in
  let ex =
    Entity.make
      (Uri.of_string "https://example.com/")
      time
      (Some (Name.of_string "Hello, world!"))
      labels
  in
  let quux =
    Entity.make (Uri.of_string "https://quux.com") time (Some (Name.of_string "Quux")) labels
  in
  let baz = Entity.make (Uri.of_string "https://baz.com") time None labels in
  let expected, id_foo, id_bar, id_ex, id_quux, id_baz =
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    let () = add_edges ret id_foo id_bar in
    let id_ex = upsert ret ex in
    let () = add_edges ret id_bar id_ex in
    let id_quux = upsert ret quux in
    let () = add_edges ret id_ex id_quux in
    let id_baz = upsert ret baz in
    let () = add_edges ret id_foo id_baz in
    (ret, id_foo, id_bar, id_ex, id_quux, id_baz)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo);
  Alcotest.(check testable_entity) "same entity" (entity expected id_bar) (entity actual id_bar);
  Alcotest.(check testable_entity) "same entity" (entity expected id_ex) (entity actual id_ex);
  Alcotest.(check testable_entity) "same entity" (entity expected id_quux) (entity actual id_quux);
  Alcotest.(check testable_entity) "same entity" (entity expected id_baz) (entity actual id_baz)

let empty_link = {|# November 15, 2023

- [](https://foo.com)
|}

let test_empty_link () =
  let open Collection in
  let actual = Markdown.parse empty_link in
  let foo =
    Entity.make
      (Uri.of_string "https://foo.com")
      (Time.of_string "November 15, 2023")
      None
      Label_set.empty
  in
  let expected, id_foo =
    let ret = create () in
    let id_foo = upsert ret foo in
    (ret, id_foo)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo)

let link_text_with_backticks = {|# November 15, 2023

- [`Foo`](https://foo.com)
|}

let test_link_text_with_backticks () =
  let open Collection in
  let actual = Markdown.parse link_text_with_backticks in
  let foo =
    Entity.make
      (Uri.of_string "https://foo.com")
      (Time.of_string "November 15, 2023")
      (Some (Name.of_string "`Foo`"))
      Label_set.empty
  in
  let expected, id_foo =
    let ret = create () in
    let id_foo = upsert ret foo in
    (ret, id_foo)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo)

let mixed_link_text_with_backticks =
  {|# November 15, 2023

- [Hello `Foo`, world!](https://foo.com)
|}

let test_mixed_link_text_with_backticks () =
  let open Collection in
  let actual = Markdown.parse mixed_link_text_with_backticks in
  let foo =
    Entity.make
      (Uri.of_string "https://foo.com")
      (Time.of_string "November 15, 2023")
      (Some (Name.of_string "Hello `Foo`, world!"))
      Label_set.empty
  in
  let expected, id_foo =
    let ret = create () in
    let id_foo = upsert ret foo in
    (ret, id_foo)
  in
  Alcotest.(check int) "same length" (length expected) (length actual);
  Alcotest.(check testable_entity) "same entity" (entity expected id_foo) (entity actual id_foo)

let tests =
  let open Alcotest in
  [
    ( "parse",
      [
        test_case "test_empty" `Quick test_empty;
        test_case "test_only_date" `Quick test_only_date;
        test_case "test_no_labels" `Quick test_no_labels;
        test_case "test_no_url" `Quick test_no_url;
        test_case "test_no_title" `Quick test_no_title;
        test_case "test_indented" `Quick test_indented;
        test_case "test_indented_double" `Quick test_indented_double;
        test_case "test_parent" `Quick test_parent;
        test_case "test_parents" `Quick test_parents;
        test_case "test_parents_indented" `Quick test_parents_indented;
        test_case "test_single_parent" `Quick test_single_parent;
        test_case "test_inverted_parent" `Quick test_inverted_parent;
        test_case "test_inverted_single_parent" `Quick test_inverted_single_parent;
        test_case "test_label" `Quick test_label;
        test_case "test_labels" `Quick test_labels;
        test_case "test_multiple_labels" `Quick test_multiple_labels;
        test_case "test_update" `Quick test_update;
        test_case "test_descending_dates" `Quick test_descending_dates;
        test_case "test_mixed_dates" `Quick test_mixed_dates;
        test_case "test_basic" `Quick test_basic;
        test_case "test_nested" `Quick test_nested;
        test_case "test_empty_link" `Quick test_empty_link;
        test_case "test_link_text_with_backticks" `Quick test_link_text_with_backticks;
        test_case "test_mixed_link_text_with_backticks" `Quick test_mixed_link_text_with_backticks;
      ] );
  ]

let () = Alcotest.run "Markdown" tests
