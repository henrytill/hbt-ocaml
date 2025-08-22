(lang dune 3.17)

(subst disabled)

(using mdx 0.4)

(name hbt)

(version 0.1.0)

(generate_opam_files true)

(source
 (github henrytill/hbt-ocaml))

(authors "Henry Till <henrytill@gmail.com>")

(maintainers "Henry Till <henrytill@gmail.com>")

(license ISC)

(package
 (name hbt-core)
 (synopsis "Heterogeneous Bookmark Transformation")
 (description
  "A tool for parsing and transforming bookmarks from various sources")
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
  (ezjsonm
   (>= 1.3.0))
  (markup
   (>= 1.0.3))
  (mdx
   (and
    :with-test
    (>= 2.4.1)))
  (semver
   (>= 0.2.1))
  (tyxml
   (>= 4.6.0))
  (uri
   (>= 4.4.0))
  (yaml
   (>= 3.2.0))))

(package
 (name hbt)
 (synopsis "Heterogeneous Bookmark Transformation (command-line interface)")
 (description
  "A tool for parsing and transforming bookmarks from various sources (command-line interface)")
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  (hbt-core
   (= :version))
  (cmdliner
   (>= 1.3.0))))
