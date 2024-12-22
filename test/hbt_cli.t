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

  $ hbt -dump input.md
  https://ocaml.org/
  https://discuss.ocaml.org/
  https://www.gnu.org/software/emacs/

Test dumping tags:

  $ hbt -tags input.md
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

  $ hbt -tags -mappings mappings.json input.md
  dev
  editors
  languages
  tools

Verify that entities are preserved while tags are transformed:

  $ hbt -dump -mappings mappings.json input.md
  https://ocaml.org/
  https://discuss.ocaml.org/
  https://www.gnu.org/software/emacs/

Test with an empty mappings file:

  $ cat > empty-mappings.json <<EOF
  > {}
  > EOF
  $ hbt -tags -mappings empty-mappings.json input.md
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
  $ hbt -mappings invalid.json input.md
  Fatal error: exception Invalid_argument("All values must be strings")
  [2]

Test with missing mappings file:

  $ hbt -mappings nonexistent.json input.md
  Fatal error: exception Sys_error("nonexistent.json: No such file or directory")
  [2]

# Local Variables:
# mode: prog
# tab-width: 2
# eval: (whitespace-mode 0)
# End:
