# hbt-ocaml

## REMEMBER

**Use GitHub MCP for all GitHub queries** (instead of fetching webpages)

**Never work directly on `master`** - branch first, land via PR (see [Git Workflow](#git-workflow))

**Run `dune fmt` before every commit** - nothing in CI checks formatting (see [CI](#ci))

**Fixtures live in a submodule shared with three other implementations** - changing one is a cross-language decision (see [Testing](#testing))

**A local `dune runtest` is unsandboxed and will pass on stale `_build` fixtures** - fixture-dependency mistakes surface only in Nix/CI (see [Testing](#testing))

## Overview

An OCaml implementation of hbt, a bookmark and document collection tool, developed differentially alongside:

- [hbt-go](https://github.com/henrytill/hbt-go) (Go)
- [hbt-rs](https://github.com/henrytill/hbt-rs) (Rust)
- [hbt-hs](https://github.com/henrytill/hbt-hs) (Haskell)

The tool reads bookmarks from Pinboard exports (JSON/XML), Netscape bookmark HTML, Markdown, and its own YAML, merges them into a collection keyed by URI, and writes the result as YAML or HTML.

The four implementations share a wire format and a fixture corpus, so a semantic question - what merging two entities that share a description should produce, say - gets settled once and pinned in [hbt-data](https://github.com/henrytill/hbt-data), then implemented in each. Issues are filed as companions across the repos; the discussion usually lives in whichever one hit it first.

## Core Principles

- **Make illegal states unrepresentable.** Preferred over guarding at each construction site. The multi-valued fields on `Entity` are sets for this reason - see [Merge Semantics](#merge-semantics).
- **Signatures are the documentation.** Each core module except `data.ml` has an `.mli` that is narrower than its implementation; a helper without a reason to be public stays out of it. `entity.mli` and `collection.mli` are where a reader should be able to learn the model.
- **Exceptions at the boundary, results at the top.** Parsers and deserializers raise (`Missing_uri`, `Invalid`, `Missing_field`, ...); `cli/main.ml` catches them in `explain` and turns the expected ones into `hbt: <msg>`. An exception not listed in `explain` is deliberately left to escape as an internal error, because it means a bug rather than bad input. Adding a new failure mode means adding its case there too.
- **Timestamps are UTC, always.** `Entity.Time.of_string` converts through an internal `timegm`, never `Unix.mktime`, so parsing does not depend on the caller's `TZ`. Do not reintroduce a local-time conversion.

**`Collection`'s edges are deliberately not a set.** They are an `int Dynarray.t` per node whose dedupe lives in `add_edge`'s `exists` guard, which looks like an unfinished version of the `extended` cleanup. It isn't. `extended` is unordered where edges are not: the fixture corpus has committed to insertion order, and a set would silently restate that as ascending. No fixture is non-ascending today, which is what makes the reinterpretation invisible rather than safe - and the shared schema marks `names`, `labels`, and `extended` `uniqueItems` while leaving `edges` alone. Settled as henrytill/hbt-rs#55.

## Package Layout

Four packages plus a scratch one, built by Dune, ISC. Opam files are generated - edit `dune-project`, never the `*.opam` files.

| Package | Path | Role |
| --- | --- | --- |
| `hbt-prelude` | `prelude/` | Stdlib extensions (`List_ext`, `Markup_ext`, `Yaml_ext`) and the `embed` executable |
| `hbt-pinboard` | `pinboard/` | Pinboard export decoding (`Post.from_json`, `Post.from_xml`) |
| `hbt-core` | `core/` | Collection and entity model, parsers, formatters |
| `hbt-cli` | `cli/` | The `hbt` binary |
| `hbt-attic` | `attic/` | Scratch space, not depended on by anything |

The dependency flow is a chain: `hbt-prelude` → `hbt-pinboard` → `hbt-core` → `hbt-cli`. `hbt-pinboard` does not know about `hbt-core`; it produces `Post.t`, and `Entity.of_post` adapts it.

`attic/`'s `belnap.ml` and `belnap_vec.ml` are four-valued logic values and packed bitvectors, exploratory work toward representing contradictory metadata across sources. They are not part of the conversion pipeline, and `nix flake check` builds them, so a break there is not something you introduced in `core/`.

### `hbt-core`

- `entity.ml` / `.mli` - `Entity.t`, where every field is a module rather than a bare primitive, and `Flag`, the tri-state that `Shared`, `To_read`, and `Is_feed` share. Merge semantics live here; the naming rules are under [Conventions](#conventions).
- `collection.ml` / `.mli` - `Collection.t`: an `Entity.t Dynarray.t`, an edge adjacency list, and a `Uri_hashtbl` index. `upsert` is the merge entry point; `add_edges` is bidirectional (`add_edge` is one-way and idempotent). `Version` pins the wire format and refuses anything else.
  - An `Id.t` is not a bare index: it pairs the index with the owning collection, and `equal` compares the owner physically. An id minted by one collection is not equal to one from another. Keep that when adding APIs that hand out or accept ids.
  - `Uri.hash` forces the URI's lazy query field before hashing, and the uri index depends on it: do not remove the forcing as dead code. The comment at `core/entity.ml:17` has the reasoning.
- `data.ml` - the format GADT. `_ t` is indexed by `[ `Input | `Output ]`, so `Html` and `Yaml` inhabit both and `Json` only the input side, and a nonsensical combination does not typecheck. `detect_input_format` / `detect_output_format` map extensions; `parse` and `format` dispatch. Adding a format means adding a constructor here plus the module that backs it.
- `html.ml` - Netscape bookmark parsing over a `Markup.signals` stream with an explicit element stack, and formatting through a mustache template at `core/templates/netscape_bookmarks.mustache`. The template interpolates with triple mustache throughout, so escaping is explicit and context-aware (`escape_attribute` / `escape_text`) rather than left to the library's uniform escaping, which would also rewrite apostrophes.
- `markdown.ml` - `Cmarkit.Folder` over the AST, carrying a state record where heading depth builds the label hierarchy. A bookmark appearing before any date heading raises `Missing_date`.
- `templates.ml` and `version.ml` are **generated** by rules in `core/dune` and exist only under `_build/` - don't go looking for them in the tree. `templates.ml` embeds the mustache template, so editing `core/templates/netscape_bookmarks.mustache` and rebuilding is the whole workflow. `version.ml` is `version.ml.in` through `cpp` with `core/project.h`, which defines `VERSION` behind an `#ifndef` so release and Nix builds can override it.

`(data_only_dirs data templates)` is what stops Dune from trying to build the fixture submodule and the template directory; a new data directory has to be added there.

## Merge Semantics

The most-revised part of the codebase, and where the cross-implementation issues concentrate. `Entity.absorb other existing` merges an entity into one with the same URI; `Collection.upsert` calls it.

- **Identical entities short-circuit.** `absorb` returns `existing` unchanged when `equal other existing`. This is an optimization, not the thing keeping a no-op update out: `update`'s `c = 0` branch already declines to record a timestamp equal to `created_at`, so removing the guard would not reintroduce that bug.
- **The earliest timestamp wins `created_at`**, and the one it displaces becomes an update.
- **A timestamp equal to `created_at` is not recorded** - an "update" that merely repeats the creation instant carries no information. Settled as henrytill/hbt-go#57.
- **Multi-valued fields are sets.** `names`, `labels`, and `extended` are `Name_set`, `Label_set`, and `Extended_set`. This is the load-bearing decision: the equality guard covers only identical entities, so two entities that differ in *any* field bypass it, and anything appended would land once per occurrence - making the output depend on how many times the input mentioned a bookmark. `extended` became a set in #48.
- **Ordering is a consequence, not a step.** `Set.Make` keeps elements sorted by construction, so the single representative the HTML formatter needs is `min_elt_opt`, not a head-of-list pick.

**`absorb` passes only `other.created_at` into `update` and discards `other.updated_at`.** Merging two collections that each carry update history keeps only the existing side's. Anyone taking on #49 needs this: it is the same field and the same call.

**`updated_at` is still a sorted `Time.t list`, and has the duplication bug the sets above were introduced to fix.** It sits next to three fields that were converted, so it reads as an oversight the pattern above wants completed. It is #49, open, and hbt-rs settled the same question by making it a set - which is why finishing it here is a cross-language change that moves shared fixtures, not a cleanup. Leave it unless you are doing #49 deliberately.

When touching this, add a unit test in `core/collection_test.ml` *and* consider whether the case deserves a shared fixture. Use three occurrences rather than two when the bug is a duplication - two cannot distinguish "deduplicated" from "recorded once by accident".

## Testing

Three layers: unit tests beside the code in `core/`, golden tests generated from shared fixtures, and a cram test over the built binary.

**Shared fixtures.** `core/data/` is a git submodule of [hbt-data](https://github.com/henrytill/hbt-data), consumed by all four implementations. Clone with `--recurse-submodules`, or run `git submodule update --init`. Changing a fixture is a cross-language decision: it will go red in the other three until their fixes land, so a fix and its submodule bump belong in the same commit.

**Golden tests.** `core/data_test.ml` walks the fixture directories at runtime with `bos`, pairing `<stem>.input.<ext>` with `<stem>.expected.<ext>` under `html/`, `markdown/`, `pinboard/json/`, and `pinboard/xml/`. Parser tests compare parsed YAML documents rather than text - emitters disagree about when a scalar needs quoting, and those spellings mean the same thing. Because discovery is at runtime, Dune cannot infer the inputs: the `(glob_files data/...)` deps in the `data_test` stanza are what makes a fixture directory visible to the sandbox, and a directory missing from them is a hard `Failure "directory contents ...: No such file or directory"`, not a silent skip.

**The trap is that a plain `dune runtest` will not show you that.** It is unsandboxed, so a warm `_build` still has the old copies and the suite passes; the same tree under `--sandbox copy` (which is what the Nix build and CI do) fails immediately. Removing the `data/markdown` glob passes locally with 43 tests and dies in the sandbox. Check dependency changes with `dune build @core/runtest --force --sandbox copy`.

**When a golden test fails, the default assumption is that the OCaml side is wrong.** The fixtures encode decisions already settled across four implementations, so changing one is the expensive answer and needs the cross-repo case made first - in hbt-data, with companion issues, and with the other three going red until they catch up. Change the fixture only when the settled behaviour is itself what's being revised.

**Unit tests.** `core/collection_test.ml` covers the entity and collection model, including merge semantics; `core/html_test.ml` covers escaping, the round trip through the formatter, and the `TAGS`/`TOREAD` attribute handling. Alcotest testables are the set modules themselves - `(module Extended_set)` works because each provides `t`, `pp`, and `equal`.

**Cram test.** `cli/main_test.t` drives the installed binary end to end, covering the flags and the error messages from `explain`. `(deps %{bin:hbt})` in `cli/dune` is what puts `hbt` on its `PATH`. Review the diff and then `dune promote`.

**Attic tests** (`belnap_test`, `belnap_vec_test`) build in both `exe` and `js` modes, so the js_of_ocaml stubs stay in step with the C ones. They need `conf-npm` and a js_of_ocaml compiler.

## Development Commands

```sh
dune build                 # everything
dune runtest               # cram + unit + golden tests
dune fmt                   # ocamlformat, pinned; run before every commit
dune promote               # accept reviewed cram/expect diffs
dune exec -- hbt -t yaml core/data/html/bookmarks_simple.input.html
dune build --watch
```

`.ocamlformat` pins the ocamlformat version, and ocamlformat refuses to run when the installed one differs - so `dune fmt` errors out on a mismatch rather than reformatting with the wrong rules. Nothing in the flake enforces it (`ocamlformat = "*"`), so pin it yourself:

```sh
opam pin add ocamlformat $(sed -n 's/^version = //p' .ocamlformat) --yes
```

Two build profiles beyond the default, defined in the root `dune`: `--profile static` links statically (used by the musl build), and `--profile ubsan` builds the C stubs under UBSan with `ALCOTEST_VERBOSE` on.

### CLI

`hbt [OPTIONS] FILE`

| Flag | Meaning |
| --- | --- |
| `-f`, `--from` | Input format: `json`, `xml`, `markdown`, `html`, `yaml`. Inferred from the argument's extension when omitted |
| `-t`, `--to` | Output format: `html`, `yaml`. Inferred from `-o`'s extension when omitted |
| `-o`, `--output` | Output file; stdout otherwise |
| `--info` | Entity count |
| `--list-tags` | Every label, sorted |
| `--mappings <FILE>` | Label rewrites, read as YAML |

Extension detection recognizes `.json`, `.xml`, `.md`, `.html`, `.yaml` on input and `.html`, `.yaml` on output. `.yml` is not recognized and errors. The analysis flags short-circuit: `--info` wins over `--list-tags`, which wins over `-t`/`-o`. But `-o` still applies to whatever they produce - `hbt --info -t yaml -o out.yaml f.md` writes `f.md: 3 entities` *into* `out.yaml` and prints nothing, overwriting whatever was there. hbt-rs ignores `-o` for the analysis flags; this implementation does not. `--mappings` is applied before any of them. With no output format and no analysis flag, the CLI errors rather than guessing.

There is no `--schema` flag here; `core/data/collection.schema.json` is maintained in hbt-data, generated from the Rust types.

### Nix

```sh
nix flake check -L         # builds hbt-cli and hbt-attic, tests included
nix build -L .#hbt-cli
nix build -L .#hbt-cli-static   # musl static build, Linux only
nix develop                # dev shell with the pinned toolchain
```

The flake sets `self.submodules = true`, so flake builds see `core/data/`. It builds through opam-nix against `ocaml-base-compiler` 5.3.0, which is narrower than `dune-project`'s `>= 5.2.0` - a green Nix build does not prove the stated minimum still holds. `CPP_FLAGS` is where the flake overrides `VERSION`, using the flake's own rev.

## CI

`.github/workflows/ci.yml` runs on pushes and PRs to `master`, with no path filter:

- **Linux (Nix flake)** - `nix flake check -L`, then both package builds; uploads the static binary as an artifact.

It is the only required status check. The flake's `checks` are the `hbt-attic` and `hbt-cli` derivations, built with `with-test = true`, so the test suite runs inside the build. **Nothing in CI checks formatting** - ocamlformat is a dev-shell tool only, so `dune fmt` is on you.

Two other workflows: `zizmor.yml` (Actions security scan, path-filtered to `.github/**` plus a weekly cron) and `update.yml` (monthly flake lock bump). Because zizmor is path-filtered, GitHub reports nothing at all for a PR that does not touch `.github/**` - which is why it is not a required check, and must not become one.

`.github/dependabot.yml` bumps Actions on a cooldown.

## Git Workflow

**Never commit to `master`.**

```sh
git checkout -b <topic>   # branch first, before making any changes
# ... work, commit ...
git push -u origin <topic>
gh pr create
gh pr merge --rebase      # after CI is green, and only with permission
```

If changes have already been made on `master` by mistake, move them to a branch before committing. `git branch -f` on the branch you are *not* on is the clean way to rewind one without disturbing the working tree or the submodule.

`develop` no longer exists on `origin`; `master` is the only long-lived branch there.

### Branch protection on `origin/master`

The GitHub remote enforces this; direct pushes to `master` will be rejected.

| Rule | Setting |
| --- | --- |
| Pull request required | yes, 0 approvals (stale reviews dismissed) |
| Required status checks | `Linux (Nix flake)`, strict (branch must be up to date) |
| Linear history | required |
| Conversation resolution | required |
| Force pushes / deletions | blocked |
| Applies to administrators | yes (no bypass) |

### Merge settings

Rebase is the only merge method enabled; squash merges and merge commits are turned off. Merged branches are deleted automatically on GitHub, so prune locally afterwards:

```sh
git fetch --prune
git branch -D <topic>   # -d may refuse: rebase merges rewrite SHAs
```

Rebase merges always create new commit SHAs, so a local branch kept after merging will look diverged from `master`. Delete it rather than reusing it. Note that the auto-delete only fires for merges GitHub itself performs; a rebase pushed by hand leaves the remote branch behind.

### The `ivan` remote

`ivan:/srv/git/hbt-ocaml.git` is a plain bare repo with no protection. Pushing `master` there directly is fine and unaffected by the above. It carries branches `origin` does not have; don't assume one seen there exists on GitHub.

### Commit messages

`<scope>: <terse description>`, where the scope is the module or file being changed - `entity:`, `collection:`, `ci:`, `AGENTS.md:`. Use the module as the scope and don't requalify its members (`entity: make extended a set`, not `entity: Entity.extended ...`).

Wrap the body at the usual width and explain *why*, particularly which invariant was wrong and what now makes it unrepresentable. Reference the companion issues in the other repos by full `henrytill/hbt-go#65` form, since bare `#65` resolves to this repo.

Do **not** hard-wrap prose in GitHub issue bodies, PR bodies, or comments - one long line per paragraph, and let GitHub wrap it. Commit messages are the exception.

## Conventions

- **A module per field, not a bare primitive.** `Name`, `Label`, and `Extended` are all `type t = string` over the same small signature (`entity.mli` has it), and are deliberately *not* factored together: they exist so the type checker catches field mix-ups, which sharing an implementation would undo. The `Set.Make` + `pp`/`t_of_yaml`/`yaml_of_t` boilerplate around them is a known triplication, tracked in #52 and deliberately not acted on; leave it alone unless that issue is the task.
- **One module type, N aliases** where the behaviour really is shared. `Shared`, `To_read`, and `Is_feed` are three aliases of `Flag` constrained by `Flag_intf.S`, which keeps them distinct in the signature while sharing the implementation. Follow that shape rather than copying a flag module.
- **`t_of_yaml` / `yaml_of_t` on every serialized type**, named that way throughout. YAML is the interchange format and the debugging format.
- **Optional fields are omitted, not nulled.** `yaml_of_t` builds an association list and appends the optional fields only when they are non-empty; the shared wire format leaves an unset field out entirely.
- **Comments explain the bug that motivated the code.** Several carry short notes of the form "X used to happen, so now Y", sometimes with an issue reference. This is deliberate and worth continuing: it stops a later simplification from quietly reintroducing a fixed bug.
- **Add new libraries to the `dune` stanza that uses them.** `dune-project` sets `(implicit_transitive_deps false)`, so depending on `hbt-core` does not give you `yaml`. Omission is a compile error, not a silent one.

## Adding a Format

1. Add the constructor to `Data._ t`, indexed `[ `Input ]`, `[ `Output ]`, or both, and to `all_input_formats` / `all_output_formats`.
2. Extend `to_string` and the relevant `detect_*_format`.
3. Write the module implementing `parse : string -> Collection.t` and/or `format : Collection.t -> string`, and wire it into `Data.parse` / `Data.format`.
4. Add fixtures to hbt-data, map the directory in `data_test.ml`'s `input_to_dir` / `output_to_dir`, and add the `(glob_files ...)` dep in `core/dune`.
5. Extend `explain` in `cli/main.ml` with whatever the new parser raises.

## Adding a Field to `Entity`

The compiler will walk you through the record, `make`, and `empty`. It will not catch any of the following, so work the list:

1. Add the field to `equal` and `pp` - both are hand-written. Leaving it out of `equal` makes entities that differ only in the new field compare equal, so `absorb` takes its short-circuit and silently drops the incoming value.
2. Add the case to `build` (the `t_of_yaml` fold) and the emission in `yaml_of_t`, omitting it when unset rather than writing a null.
3. Decide the merge rule and implement it in `update` or `absorb`. If it is multi-valued, make it a set - see [Merge Semantics](#merge-semantics).
4. Map it from Pinboard in `of_post` and from HTML attributes in `Entity.Html.entity_of_attrs` if the wire formats carry it.
5. Add it to `Template_entity` in `core/html.ml` if it belongs in the HTML output, and to the mustache template.
6. Update `entity.mli`, and the schema in hbt-data if the wire format changed.
