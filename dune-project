(lang dune 3.17)

(name hbt)

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
  (bos
   (and
    :with-test
    (>= 0.2.1)))
  (fpath
   (and
    :with-test
    (>= 0.7.3)))
  (cmarkit
   (>= 0.3.0))
  (ezjsonm
   (>= 1.3.0))
  (fmt
   (>= 0.11.0))
  (mustache
   (>= 3.3.0))
  (markup
   (>= 1.0.3))
  (semver
   (>= 0.2.1))
  (uri
   (>= 4.4.0))
  (xmlm
   (>= 1.4.0))
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
