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

A label mapped to the empty string is dropped, not replaced by an empty one:

  $ cat >drop-mappings.yaml <<EOF
  > programming: ""
  > EOF
  $ hbt --list-tags --mappings drop-mappings.yaml input.md
  editor
  ocaml
  tools

An empty key has nothing to map, and is refused:

  $ cat >empty-key.yaml <<EOF
  > "": x
  > EOF
  $ hbt --list-tags --mappings empty-key.yaml input.md
  hbt: empty-key.yaml: a label cannot be empty
  [123]

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
  hbt: invalid.yaml: Expected a string value
  [123]

Test with missing mappings file:

  $ hbt --info --mappings nonexistent.json input.md
  hbt: nonexistent.json: No such file or directory
  [123]

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
  hbt: no output format: pass -t FORMAT, or -o FILE with a known extension
  [123]

Test that a missing input file is reported, not raised:

  $ hbt --info nonexistent.md
  hbt: nonexistent.md: No such file or directory
  [123]

Test that a file name with no extension is reported:

  $ touch noextension
  $ hbt --info noextension
  hbt: noextension: cannot determine the format from the file name; pass -f FORMAT
  [123]

Test that malformed YAML input is reported:

  $ cat >broken.yaml <<EOF
  > not valid yaml: [[[
  > EOF
  $ hbt --info broken.yaml
  hbt: broken.yaml: error calling parser: did not find expected node content character 0 position 0 returned: 0
  [123]

Test that a structurally invalid collection is reported:

  $ cat >bad-length.yaml <<EOF
  > version: 0.1.0
  > length: 3
  > value:
  > - id: 0
  >   entity: {uri: "https://a.org/", createdAt: 0, updatedAt: [], names: [], labels: []}
  >   edges: []
  > EOF
  $ hbt --info bad-length.yaml
  hbt: bad-length.yaml: invalid collection: declared length 3 but found 1 nodes
  [123]

Test that an unsupported collection version is reported:

  $ cat >future.yaml <<EOF
  > version: 9.9.9
  > length: 0
  > value: []
  > EOF
  $ hbt --info future.yaml
  hbt: future.yaml: collection version 9.9.9 is not supported, expected 0.1.0
  [123]

Test that a link before any date heading is reported:

  $ cat >nodate.md <<EOF
  > - [Orphan](https://orphan.example/)
  > EOF
  $ hbt --info nodate.md
  hbt: nodate.md: https://orphan.example/ appears before any date heading
  [123]

Test that an unparseable date is reported:

  $ cat >baddate.md <<EOF
  > # Not A Date
  > 
  > - [x](https://a.org/)
  > EOF
  $ hbt --info baddate.md
  hbt: baddate.md: malformed date "Not A Date"
  [123]

Test that a heading which ends before the date format does is reported, not
an internal error (#86):

  $ cat >notes.md <<EOF
  > # Notes
  > 
  > - [x](https://a.org/)
  > EOF
  $ hbt --info notes.md
  hbt: notes.md: malformed date "Notes"
  [123]

Test that a malformed Pinboard time is reported:

  $ cat >badtime.json <<EOF
  > [{"href": "https://a.org/", "description": "x", "extended": "", "meta": "", "hash": "", "time": "yesterday", "shared": "no", "toread": "no", "tags": ""}]
  > EOF
  $ hbt --info badtime.json
  hbt: badtime.json: malformed date "yesterday"
  [123]

Test that a date out of range is reported, not an internal error, by every
route that reads one (#89):

  $ cat >farheading.md <<EOF
  > # September 3, 99999999999999
  > 
  > - [x](https://a.org/)
  > EOF
  $ hbt --info farheading.md
  hbt: farheading.md: date out of range "September 3, 99999999999999"
  [123]

  $ cat >fartime.json <<EOF
  > [{"href": "https://a.org/", "description": "x", "extended": "", "meta": "", "hash": "", "time": "99999999999999-01-01T00:00:00Z", "shared": "no", "toread": "no", "tags": ""}]
  > EOF
  $ hbt --info fartime.json
  hbt: fartime.json: date out of range "99999999999999-01-01T00:00:00Z"
  [123]

  $ cat >faradd.html <<EOF
  > <DL><p>
  > <DT><A HREF="https://a.org/" ADD_DATE="1e300">x</A>
  > </DL><p>
  > EOF
  $ hbt --info faradd.html
  hbt: faradd.html: date out of range "1e300"
  [123]

  $ cat >nanadd.html <<EOF
  > <DL><p>
  > <DT><A HREF="https://a.org/" LAST_MODIFIED="nan">x</A>
  > </DL><p>
  > EOF
  $ hbt --info nanadd.html
  hbt: nanadd.html: date out of range "nan"
  [123]

  $ cat >far.yaml <<EOF
  > version: 0.1.0
  > length: 1
  > value:
  > - id: 0
  >   entity:
  >     uri: https://a.org/
  >     createdAt: 1e300
  >     updatedAt: []
  >   edges: []
  > EOF
  $ hbt --info far.yaml
  hbt: far.yaml: date out of range "1e+300"
  [123]

# Local Variables:
# mode: prog
# tab-width: 2
# eval: (whitespace-mode 0)
# End:
