Create a test input file:

  $ cat >input.md <<EOF
  > # November 15, 2023
  > 
  > ## programming
  > 
  > ### ocaml
  > 
  > - [OCaml Website](https://ocaml.org)
  > - <https://discuss.ocaml.org/>
  > 
  > ### tools
  > 
  > #### editor
  > 
  > - [Emacs](https://www.gnu.org/software/emacs/)
  > EOF

Test basic functionality first - counting entities:

  $ hbt input.md
  input.md: 3 entities

Test dumping entities:

  $ hbt --dump input.md
  {
    "version": "0.1.0",
    "length": 3,
    "value": [
      {
        "id": 2,
        "entity": {
          "uri": "https://www.gnu.org/software/emacs/",
          "createdAt": 1700035200.0,
          "updatedAt": [],
          "names": [ "Emacs" ],
          "labels": [ "editor", "ocaml", "tools" ]
        },
        "edges": []
      },
      {
        "id": 1,
        "entity": {
          "uri": "https://discuss.ocaml.org/",
          "createdAt": 1700035200.0,
          "updatedAt": [],
          "names": [],
          "labels": [ "ocaml", "programming" ]
        },
        "edges": []
      },
      {
        "id": 0,
        "entity": {
          "uri": "https://ocaml.org/",
          "createdAt": 1700035200.0,
          "updatedAt": [],
          "names": [ "OCaml Website" ],
          "labels": [ "ocaml", "programming" ]
        },
        "edges": []
      }
    ]
  }

Test dumping tags:

  $ hbt --tags input.md
  editor
  ocaml
  programming
  tools

Now create a mappings file to transform some tags:

  $ cat >mappings.json <<EOF
  > {
  >   "editor": "editors",
  >   "programming": "dev",
  >   "ocaml": "languages"
  > }
  > EOF

Test the tag mapping functionality:

  $ hbt --tags --mappings mappings.json input.md
  dev
  editors
  languages
  tools

Verify that entities are preserved while tags are transformed:

  $ hbt --dump --mappings mappings.json input.md
  {
    "version": "0.1.0",
    "length": 3,
    "value": [
      {
        "id": 2,
        "entity": {
          "uri": "https://www.gnu.org/software/emacs/",
          "createdAt": 1700035200.0,
          "updatedAt": [],
          "names": [ "Emacs" ],
          "labels": [ "editors", "languages", "tools" ]
        },
        "edges": []
      },
      {
        "id": 1,
        "entity": {
          "uri": "https://discuss.ocaml.org/",
          "createdAt": 1700035200.0,
          "updatedAt": [],
          "names": [],
          "labels": [ "dev", "languages" ]
        },
        "edges": []
      },
      {
        "id": 0,
        "entity": {
          "uri": "https://ocaml.org/",
          "createdAt": 1700035200.0,
          "updatedAt": [],
          "names": [ "OCaml Website" ],
          "labels": [ "dev", "languages" ]
        },
        "edges": []
      }
    ]
  }

Test with an empty mappings file:

  $ cat >empty-mappings.json <<EOF
  > {}
  > EOF
  $ hbt --tags --mappings empty-mappings.json input.md
  editor
  ocaml
  programming
  tools

Test with invalid JSON mappings file:

  $ cat >invalid.json <<EOF
  > {
  >   "editor": 123,
  >   "programming": ["foo"]
  > }
  > EOF
  $ hbt --mappings invalid.json input.md
  hbt: internal error, uncaught exception:
       Invalid_argument("Collection.json_to_map: all values must be strings")
       
  [125]

Test with missing mappings file:

  $ hbt --mappings nonexistent.json input.md
  hbt: internal error, uncaught exception:
       Sys_error("nonexistent.json: No such file or directory")
       
  [125]

# Local Variables:
# mode: prog
# tab-width: 2
# eval: (whitespace-mode 0)
# End:
