(lang dune 3.17)

(name hbt)

(generate_opam_files true)

(source
 (github henrytill/hbt-ocaml))

(authors "Henry Till <henrytill@gmail.com>")

(maintainers "Henry Till <henrytill@gmail.com>")

(license ISC)

(implicit_transitive_deps false)

(package
 (name hbt-prelude)
 (synopsis "Heterogeneous Bookmark Transformation (Library code)")
 (description
  "A tool for parsing and transforming bookmarks from various sources")
 (depends
  (ocaml
   (>= 5.2.0))
  (yaml
   (>= 3.2.0))))

(package
 (name hbt-pinboard)
 (synopsis "Heterogeneous Bookmark Transformation (Pinboard support)")
 (description
  "A tool for parsing and transforming bookmarks from various sources")
 (depends
  (ocaml
   (>= 5.2.0))
  (hbt-prelude
   (= :version))
  (ezjsonm
   (>= 1.3.0))
  (xmlm
   (>= 1.4.0))))

(package
 (name hbt-core)
 (synopsis "Heterogeneous Bookmark Transformation")
 (description
  "A tool for parsing and transforming bookmarks from various sources")
 (depends
  (ocaml
   (>= 5.2.0))
  (hbt-prelude
   (= :version))
  (hbt-pinboard
   (= :version))
  (alcotest
   (and
    :with-test
    (>= 1.7.0)))
  (bos
   (and
    :with-test
    (>= 0.2.1)))
  (cmarkit
   (>= 0.3.0))
  (fmt
   (>= 0.11.0))
  (fpath
   (and
    :with-test
    (>= 0.7.3)))
  (markup
   (>= 1.0.3))
  (mustache
   (>= 3.3.0))
  (semver
   (>= 0.2.1))
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
  (hbt-core
   (= :version))
  (cmdliner
   (>= 1.3.0))
  (yaml
   (>= 3.2.0))))
