(lang dune 3.10)

(name hbt)

(generate_opam_files true)

(source
 (github henrytill/hbt-ocaml))

(authors "Henry Till <henrytill@gmail.com>")

(maintainers "Henry Till <henrytill@gmail.com>")

(license ISC)

(documentation https://url/to/documentation)

(package
 (name hbt)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  (alcotest
   (and
    :with-test
    (>= 1.7.0)))
  (cohttp
   (>= 5.3.0))
  (cohttp-lwt
   (>= 5.3.0))
  (cohttp-lwt-unix
   (>= 5.3.0))
  (lwt
   (>= 5.7.0))
  (lwt_ssl
   (>= 1.2.0))
  (markup
   (>= 1.0.3))
  (uri
   (>= 4.4.0))
  (xdg
   (>= 3.10.0))
  (yojson
   (>= 2.1.0)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
