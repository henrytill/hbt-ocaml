(lang dune 3.10)

(name hbt)

(generate_opam_files true)

(source
 (github henrytill/hbt-ocaml))

(authors "Henry Till <henrytill@gmail.com>")

(maintainers "Henry Till <henrytill@gmail.com>")

(license ISC)

(package
 (name hbt)
 (synopsis "Heterogeneous Bookmark Transformation")
 (description "A tool for parsing and transforming bookmarks from various sources")
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  (alcotest
   (and
    :with-test
    (>= 1.7.0)))
  (cmarkit
   (>= 0.3.0))
  (markup
   (>= 1.0.3))
  (ppx_expect
   (and
    :with-test
    (>= v0.17.0)))
  (tyxml
   (>= 4.6.0))
  (uri
   (>= 4.4.0))
  (yojson
   (>= 2.1.0))))
