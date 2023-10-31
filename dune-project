(lang dune 3.10)

(name backlogged)

(generate_opam_files true)

(source
 (github henrytill/backlogged))

(authors "Henry Till <henrytill@gmail.com>")

(maintainers "Henry Till <henrytill@gmail.com>")

(license ISC)

(documentation https://url/to/documentation)

(package
 (name backlogged)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  (cohttp (>= 5.3.0))
  (cohttp-lwt (>= 5.3.0))
  (cohttp-lwt-unix (>= 5.3.0))
  (lwt (>= 5.7.0))
  (lwt_ssl (>= 1.2.0))
  (uri (>= 4.4.0))
  (xdg (>= 3.10.0))
  (xmlm (>= 1.4.0))
  (yojson (>= 2.1.0)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
