Set TZ:

  $ export TZ=UTC

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

  $ hbt --info input.md
  input.md: 3 entities

Test dumping entities:

  $ hbt -t yaml input.md
  version: 0.1.0
  length: 3
  value:
  - id: 0
    entity:
      uri: https://ocaml.org/
      createdAt: 1700006400
      updatedAt: []
      names:
      - OCaml Website
      labels:
      - ocaml
      - programming
    edges: []
  - id: 1
    entity:
      uri: https://discuss.ocaml.org/
      createdAt: 1700006400
      updatedAt: []
      names: []
      labels:
      - ocaml
      - programming
    edges: []
  - id: 2
    entity:
      uri: https://www.gnu.org/software/emacs/
      createdAt: 1700006400
      updatedAt: []
      names:
      - Emacs
      labels:
      - editor
      - ocaml
      - tools
    edges: []

Test dumping tags:

  $ hbt --list-tags input.md
  editor
  ocaml
  programming
  tools

Now create a mappings file to transform some tags:

  $ cat >mappings.yaml <<EOF
  > editor: editors
  > programming: dev
  > ocaml: languages
  > EOF

Test the tag mapping functionality:

  $ hbt --list-tags --mappings mappings.yaml input.md
  dev
  editors
  languages
  tools

Verify that entities are preserved while tags are transformed:

  $ hbt -t yaml --mappings mappings.yaml input.md
  version: 0.1.0
  length: 3
  value:
  - id: 0
    entity:
      uri: https://ocaml.org/
      createdAt: 1700006400
      updatedAt: []
      names:
      - OCaml Website
      labels:
      - dev
      - languages
    edges: []
  - id: 1
    entity:
      uri: https://discuss.ocaml.org/
      createdAt: 1700006400
      updatedAt: []
      names: []
      labels:
      - dev
      - languages
    edges: []
  - id: 2
    entity:
      uri: https://www.gnu.org/software/emacs/
      createdAt: 1700006400
      updatedAt: []
      names:
      - Emacs
      labels:
      - editors
      - languages
      - tools
    edges: []

Test with an empty mappings file:

  $ cat >empty-mappings.yaml <<EOF
  > {}
  > EOF
  $ hbt --list-tags --mappings empty-mappings.yaml input.md
  editor
  ocaml
  programming
  tools

Test with invalid JSON mappings file:

  $ cat >invalid.yaml <<EOF
  > editor: 123,
  > programming: [foo]
  > EOF
  $ hbt --info --mappings invalid.yaml input.md
  hbt: internal error, uncaught exception:
       Yaml__Util.Value_error("Expected a string value")
       
  [125]

Test with missing mappings file:

  $ hbt --info --mappings nonexistent.json input.md
  hbt: internal error, uncaught exception:
       Sys_error("nonexistent.json: No such file or directory")
       
  [125]

Test output format auto-detection from file extension:

  $ hbt input.md -o output.yaml
  $ head -10 output.yaml
  version: 0.1.0
  length: 3
  value:
  - id: 0
    entity:
      uri: https://ocaml.org/
      createdAt: 1700006400
      updatedAt: []
      names:
      - OCaml Website

  $ hbt input.md -o output.html
  $ head -5 output.html
  <!DOCTYPE NETSCAPE-Bookmark-file-1>
  <META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
  <TITLE>Bookmarks</TITLE>
  <H1>Bookmarks</H1>
  <DL><p>

Test that explicit format overrides auto-detection:

  $ hbt input.md -t yaml -o explicit.html
  $ head -5 explicit.html
  version: 0.1.0
  length: 3
  value:
  - id: 0
    entity:

Test that unrecognized extension fails without explicit format:

  $ hbt input.md -o output.txt
  hbt: internal error, uncaught exception:
       Dune__exe__Main.Missing_output_specification
       
  [125]

# Local Variables:
# mode: prog
# tab-width: 2
# eval: (whitespace-mode 0)
# End:
