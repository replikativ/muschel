# Changelog

All notable changes to **muschel** will be documented in this file.

The format follows [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).

## [0.1.x] — 2026-05-25

First public release. Patch number is git-derived (`0.1.<git-count>`)
via `build.clj`, so individual deployments carry a unique version.

### Added

- **Lexer** (`muschel.lex`) — hand-written tokenizer covering quoting,
  parameter expansion (`$VAR`, `${...}`), command substitution
  (`$(...)`, backticks), arithmetic (`$(( ... ))`), heredocs / herestrings,
  process substitution placeholders, `[[`, `((`.
- **Parser** (`muschel.parse`) — recursive-descent over the token stream,
  no external grammar dep, sci-safe. Handles pipelines, lists, control
  flow (`if/for/while/case/select`), functions, redirects, here-docs.
- **AST** (`muschel.ast`) — node ctors, predicates, walk/zip helpers.
- **Env** (`muschel.env`) — immutable shell-environment value with
  scope stack for `local` / function frames; exported / readonly flags;
  positional params.
- **Expansion** (`muschel.expand`) — POSIX expansion order: brace →
  tilde → param → cmd-subst → arith → word-split → glob → quote removal.
- **Arithmetic** (`muschel.arith`) — full bash arithmetic expression
  evaluator (precedence, ternary, all assignment ops, bitwise, etc.).
- **Permit** (`muschel.permit`) — parse-time AST walk + runtime-hook
  re-check at every `run-external`. Default ruleset shipped in
  `resources/muschel/default-permit.edn`. Pluggable prompter for
  `:prompt` outcomes.
- **Host protocol** (`muschel.host`) — abstracts I/O, fs, spawn, async.
  Three impls:
  - `muschel.host.jvm` — `babashka.process` + `java.io.*`
  - `muschel.host.node` — `child_process.spawnSync` + `fs`
  - `muschel.host.browser` — virtual fs (atom of `path → content`) +
    virtual tool registry (`name → (fn [argv stdin env])`)
- **Executor** (`muschel.exec`) — builtins (`cd/pwd/echo/printf/export/
  unset/set/shift/return/read/test/[`/`true/false/:/eval/source/.`),
  control flow, pipes (real on JVM/node, sequential in browser),
  redirects (`<`, `>`, `>>`, `<<`, `<<<`, `2>&1`, `&>`, `<&`, `>&`),
  background jobs (forkable spins via [spindel](https://github.com/replikativ/spindel)
  when present, futures on JVM, async stub in cljs).
- **Session** (`muschel.session`) — protocol for stateful exec across
  multiple `run` calls. `AtomSession` (default) + optional
  `SpindelSession` (JVM, via `:spindel` deps alias).
- **JS / TypeScript binding** (`muschel.js-api`) — npm bundle exporting
  `parse`, `check`, `run`, `session`, `nodeHost`, `browserHost`,
  `stockTools`, `setVar`, `getVar`, `cwd`. TypeScript types shipped
  in `muschel.d.ts`.
- **Core facade** (`muschel.core`) — public Clojure API
  re-exporting `parse`, `check`, `run`, `run-and-capture`, `new-env`,
  `get-var`, `set-var`, `atom-session`, `default-rules`, `jvm-host`.
- **Browser playground** (`muschel.playground`) — minimal terminal UI
  over the browser host, pre-seeded virtual fs, stock tools. Deployable
  via GitHub Pages from `docs/`.
- **Babashka integration** — `bb.edn` with `bb sh` (interactive shell),
  `bb exec '<src>'` (one-shot), and `bb translate '<src>'` tasks.
  All muschel namespaces are sci-safe; the JVM host's
  `babashka.process` deps are bb built-ins.
- **Bash → Clojure translator** (`muschel.emit`) — walks the AST and
  emits a `(fn [env opts] ...)` form that runs the program with bash
  control flow expressed as native Clojure (`if`/`reduce`/`loop`/
  `as->`) and leaf work delegated to small `muschel.exec` /
  `muschel.env` helpers. Deferred constructs (pipes, redirects, test
  brackets, …) get hoisted into a top-level `let` for readability.
  Useful for inspection, AOT, and embedding.
- **JS / TypeScript binding** (`muschel.js-api`) — npm bundle exporting
  `parse`, `check`, `run`, `session`, `nodeHost`, `browserHost`,
  `stockTools`, `setVar`, `getVar`, `cwd`. TypeScript types shipped
  in `muschel.d.ts`.
- **Test corpus** — port of a slice of [mvdan/sh](https://github.com/mvdan/sh)
  print/parse tests (BSD-3-Clause; attribution preserved in
  `LICENSE-mvdan-sh.txt`).
- **CI / CD** — CircleCI via `replikativ/clj-tools` orb: setup → build
  → format → unittest + cljstest → deploy (Clojars) → release
  (GitHub). `build.clj` covers jar / install / deploy / release /
  npm-publish.

### Test stats

- JVM: 216 tests / 555 assertions
- ClojureScript (node): 131 tests / 396 assertions
- mvdan corpus: 661 / 852 cases passing (failure budget 195)

### Known gaps

- `coproc`, `time` keyword, `select` in nested form, some advanced
  parameter expansion ops (`@:offset:length` corner cases),
  `extglob` / `globstar` flags. See `doc/grammar-study.md` for the
  full feature matrix.
- Browser host pipes are sequential (single-threaded JS); concurrent
  pipelines require the node or JVM host.
