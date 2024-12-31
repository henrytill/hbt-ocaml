# Collection

```ocaml
# #require "hbt";;
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
    created_at = "1725260400";
    updated_at = [];
    names = {};
    labels = {};
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
    created_at = "1725260400";
    updated_at = [];
    names = {"The Internet Archive"};
    labels = {"archives"};
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
    created_at = "1725260400";
    updated_at = [];
    names = {"The Internet Archive"};
    labels = {"archives"; "books"};
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
   }]
```

## HTML Generation

Empty collection

```ocaml
# let collection = make () in
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
# let labels_foo = Label_set.(empty |> add (Label.of_string "Foo")) in
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
  print_endline actual;;
<!DOCTYPE NETSCAPE-Bookmark-file-1>
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
<TITLE>Bookmarks</TITLE>
<dl><dt><a href="https://foo.com/" add_date="1700035200" last_modified="1700035200" tags="Foo">Foo</a></dt>
 <dt><a href="https://bar.com/" add_date="1700035200" last_modified="1700035200" tags="Bar,Foo">Bar</a></dt>
 <dt><a href="https://baz.com/" add_date="1700035200" last_modified="1700035200" tags="Bar,Baz,Foo">Baz</a></dt>
</dl>

- : unit = ()
```
