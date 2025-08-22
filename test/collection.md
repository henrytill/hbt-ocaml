# Collection

```ocaml
# Unix.putenv "TZ" "UTC";;
- : unit = ()
# #require "hbt-core";;
# open Hbt.Collection;;
# #install_printer Hbt.Collection.Entity.pp;;
```

## Entity

Preliminaries

```ocaml
# let archive_uri = Uri.of_string "https://archive.org/";;
val archive_uri : Uri.t = <abstr>
# let created_at = Time.of_string "September 2, 2024";;
val created_at : Time.t = <abstr>
# let archive_maybe_name = Some (Name.of_string "The Internet Archive");;
val archive_maybe_name : Name_set.elt option = Some <abstr>
```

A bare entity

```ocaml
# let entity = Entity.make archive_uri created_at None Label_set.empty;;
val entity : Entity.t =
  {
    uri = https://archive.org/;
    created_at = "1725235200";
    updated_at = [];
    names = {};
    labels = {};
    extended = None;
    shared = false;
    to_read = false;
    last_visited_at = None;
    is_feed = false;
  }
```

An entity with a name and label

```ocaml
# let entity =
    let labels = Label_set.of_list [ Label.of_string "archives" ] in
    Entity.make archive_uri created_at archive_maybe_name labels;;
val entity : Entity.t =
  {
    uri = https://archive.org/;
    created_at = "1725235200";
    updated_at = [];
    names = {"The Internet Archive"};
    labels = {"archives"};
    extended = None;
    shared = false;
    to_read = false;
    last_visited_at = None;
    is_feed = false;
  }
```

An entity with more labels

```ocaml
# let entity =
    let labels = Label_set.of_list [ Label.of_string "archives"; Label.of_string "books" ] in
    Entity.make archive_uri created_at archive_maybe_name labels;;
val entity : Entity.t =
  {
    uri = https://archive.org/;
    created_at = "1725235200";
    updated_at = [];
    names = {"The Internet Archive"};
    labels = {"archives"; "books"};
    extended = None;
    shared = false;
    to_read = false;
    last_visited_at = None;
    is_feed = false;
  }
```

Multiple entities

```ocaml
# let entities =
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
    [ archive; google ];;
val entities : Entity.t list =
  [{
     uri = https://archive.org/;
     created_at = "1725235200";
     updated_at = [];
     names = {"The Internet Archive"};
     labels = {"archives"; "search"};
     extended = None;
     shared = false;
     to_read = false;
     last_visited_at = None;
     is_feed = false;
   };
   {
     uri = https://www.google.com/;
     created_at = "1725235200";
     updated_at = [];
     names = {"Google"};
     labels = {"search"};
     extended = None;
     shared = false;
     to_read = false;
     last_visited_at = None;
     is_feed = false;
   }]
```

## HTML Generation

Empty collection

```ocaml
# let collection = create () in
  let actual = to_html collection in
  print_endline actual;;
<!DOCTYPE NETSCAPE-Bookmark-file-1>
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
<TITLE>Bookmarks</TITLE>
<dl></dl>

- : unit = ()
```

Basic

```ocaml
# let collection =
    let labels_foo = Label_set.singleton (Label.of_string "Foo") in
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
    let ret = create () in
    let id_foo = upsert ret foo in
    let id_bar = upsert ret bar in
    let id_baz = upsert ret baz in
    let () = add_edges ret id_foo id_bar in
    let () = add_edges ret id_foo id_baz in
    ret;;
val collection : t = <abstr>
# to_html collection |> print_endline;;
<!DOCTYPE NETSCAPE-Bookmark-file-1>
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
<TITLE>Bookmarks</TITLE>
<dl><dt><a href="https://foo.com/" add_date="1700006400" last_modified="1700006400" tags="Foo">Foo</a></dt>
 <dt><a href="https://bar.com/" add_date="1700006400" last_modified="1700006400" tags="Bar,Foo">Bar</a></dt>
 <dt><a href="https://baz.com/" add_date="1700006400" last_modified="1700006400" tags="Bar,Baz,Foo">Baz</a></dt>
</dl>

- : unit = ()
```

### YAML

```ocaml
# yaml_of_t collection |> Yaml.to_string |> Result.get_ok |> print_endline;;
version: 0.1.0
length: 3
value:
- id: 2
  entity:
    uri: https://baz.com/
    createdAt: 1700006400
    updatedAt: []
    names:
    - Baz
    labels:
    - Bar
    - Baz
    - Foo
    shared: false
    toRead: false
    isFeed: false
  edges:
  - 0
- id: 1
  entity:
    uri: https://bar.com/
    createdAt: 1700006400
    updatedAt: []
    names:
    - Bar
    labels:
    - Bar
    - Foo
    shared: false
    toRead: false
    isFeed: false
  edges:
  - 0
- id: 0
  entity:
    uri: https://foo.com/
    createdAt: 1700006400
    updatedAt: []
    names:
    - Foo
    labels:
    - Foo
    shared: false
    toRead: false
    isFeed: false
  edges:
  - 1
  - 2

- : unit = ()
```

```ocaml
# let roundtripped = yaml_of_t collection |> Yaml.to_string |> Result.get_ok |> Yaml.of_string |> Result.get_ok |> t_of_yaml;;
val roundtripped : t = <abstr>
# yaml_of_t roundtripped |> Yaml.to_string |> Result.get_ok |> print_endline;;
version: 0.1.0
length: 3
value:
- id: 2
  entity:
    uri: https://baz.com/
    createdAt: 1700006400
    updatedAt: []
    names:
    - Baz
    labels:
    - Bar
    - Baz
    - Foo
    shared: false
    toRead: false
    isFeed: false
  edges:
  - 0
- id: 1
  entity:
    uri: https://bar.com/
    createdAt: 1700006400
    updatedAt: []
    names:
    - Bar
    labels:
    - Bar
    - Foo
    shared: false
    toRead: false
    isFeed: false
  edges:
  - 0
- id: 0
  entity:
    uri: https://foo.com/
    createdAt: 1700006400
    updatedAt: []
    names:
    - Foo
    labels:
    - Foo
    shared: false
    toRead: false
    isFeed: false
  edges:
  - 1
  - 2

- : unit = ()
```
