# muschel

[![CircleCI](https://circleci.com/gh/replikativ/muschel.svg?style=shield)](https://circleci.com/gh/replikativ/muschel)
[![Clojars](https://img.shields.io/clojars/v/org.replikativ/muschel.svg)](https://clojars.org/org.replikativ/muschel)
[![Slack](https://img.shields.io/badge/slack-join_chat-brightgreen.svg)](https://clojurians.slack.com/archives/C09622F337D)

**muschel** — German for *shell* (the bivalve). A Clojure(Script) library
that parses bash/POSIX-shell source into a structured AST, checks it
against a permit (allow/deny) policy, and executes it through a
pluggable host — JVM, babashka, Node.js, or in the browser against a
virtual file system and virtual tool registry.

Built for LLM agent shells: agents emit bash naturally, and muschel lets
that flow without giving away the keys to the kingdom.

## Why?

Most LLM tool integrations either:

1. Trust the agent's bash output and pipe it to `sh -c` — easy, dangerous.
2. Whitelist commands by argv prefix — bypassable through env vars
   (`PYTHONWARNINGS`, `PAGER`), command substitution `$(...)`, etc.
3. Replace shell entirely with curated structured tools — safe, but
   strips the agent of the bash idioms it's been trained on.

muschel takes a different shape: **parse the real shell, walk the AST,
gate every effect through a permit, and execute through a host you
control — including a virtual host that runs in the browser with no
spawn capability at all.**

## Architecture

```
src/muschel/
  lex.cljc        hand-written tokenizer  (quotes, $, ((, [[, heredocs)
  errors.cljc     position-bearing errors
  ast.cljc        AST node ctors, predicates, walk/zip
  parse.cljc      recursive-descent parser over tokens
  env.cljc        immutable shell-environment value
  expand.cljc     POSIX expansion order  (brace → tilde → param → cmd-subst → arith → split → glob → quote removal)
  arith.cljc      $(( … )) evaluator
  permit.cljc     parse-time + runtime-hook allow/deny against rules
  host.cljc       Host protocol — abstracts I/O / spawn / fs
  host/jvm.clj    bb.process + java.io
  host/node.cljs  child_process + node fs
  host/browser.cljs  virtual fs + virtual tool registry (no spawn)
  exec.cljc       executor over the AST (builtins, control flow, pipes, redirects, bg jobs)
  session.cljc    forkable session protocol (AtomSession + SpindelSession)
  js_api.cljs     ^:export bindings for the npm bundle
  playground.cljs browser playground entry point
```

## Runtimes

| Runtime              | Status | How                                                 |
|----------------------|--------|-----------------------------------------------------|
| JVM (Clojure)        | ✓      | `host/jvm` — `babashka.process`, `java.io`          |
| Babashka (sci)       | ✓      | Same .cljc — no JVM-only deps in the hot path       |
| Node.js (cljs)       | ✓      | `host/node` — `child_process.spawnSync`             |
| Browser (cljs)       | ✓      | `host/browser` — virtual fs + virtual tool registry |
| npm / TypeScript     | ✓      | `dist/muschel.js` (+ `dist/muschel.d.ts`)           |

## Quickstart — Clojure

```clojure
(require '[muschel.env :as env]
         '[muschel.exec :as exec]
         '[muschel.host.jvm :as host.jvm])

(exec/run-and-capture
  (env/new-env)
  "git log --oneline | head -3"
  {:host (host.jvm/make)})
;; → {:stdout "abc1234 ...\n..." :stderr "" :exit 0 :env <env>}
```

With a permit:

```clojure
(require '[muschel.permit :as permit])

(exec/run-and-capture
  (env/new-env)
  "rm -rf /"
  {:host (host.jvm/make)
   :permit {:rulesets [permit/default-rules]
            :prompter permit/deny-all-prompter}})
;; → exits non-zero, stderr explains the denied rule
```

## Quickstart — Node.js / JavaScript

```bash
npm install muschel
```

```js
const m = require('muschel');

const host = m.nodeHost();
const r = m.run("echo 'hello' | tr a-z A-Z", { host });
console.log(r.stdout);  // → "HELLO\n"

// Stateful session: cd / var assignment / bg jobs persist
const sess = m.session();
m.run("cd /tmp; export FOO=bar", { host, session: sess });
const r2 = m.run("echo $FOO from $(pwd)", { host, session: sess });
console.log(r2.stdout);  // → "bar from /tmp\n"
```

## Quickstart — Babashka

muschel runs in [babashka](https://github.com/babashka/babashka) without
modification — every layer is `.cljc`, sci-safe, and the JVM host uses
`babashka.process` (which bb ships built-in).

From a checkout (`bb.edn` in the repo provides paths + tasks):

```bash
bb exec "echo hi | tr a-z A-Z"          # → HI
bb sh                                   # interactive shell prompt
```

From your own project, add muschel to your `bb.edn`:

```clojure
{:deps {org.replikativ/muschel {:mvn/version "0.1.x"}}}
```

```bash
bb -e "(require '[muschel.core :as m])
       (m/run-and-capture (m/new-env)
                          \"git log --oneline | head -3\"
                          {:host (m/jvm-host)})"
```

The `:spindel` session backend stays JVM-only (its deps don't load in
bb); use `m/atom-session` everywhere else.

## Quickstart — Browser (no spawn)

```js
import * as m from 'muschel';

const files = {
  '/README.md': '# hi\n',
  '/etc/issue': 'demo\n'
};

const host = m.browserHost({
  files,                          // pre-seeded virtual filesystem
  tools: {
    ...m.stockTools(),            // wc, grep, head (cat is stub)
    // Override `cat` to read the virtual fs
    cat: (argv, stdin) =>
      argv.length === 0
        ? { stdout: stdin, exit: 0 }
        : files[argv[0]]
          ? { stdout: files[argv[0]], exit: 0 }
          : { stderr: `cat: ${argv[0]}: not found\n`, exit: 1 },
    git: (argv) => ({ stdout: `[simulated git ${argv.join(' ')}]\n`, exit: 0 })
  }
});

m.run("cat /README.md | wc -l", { host });
// → { stdout: " 1 ...", stderr: "", exit: 0 }
```

**[Try it in your browser →](https://replikativ.github.io/muschel/playground/)**

The playground is one static HTML file in [`docs/playground/`](docs/playground/);
the bundle ships in the npm package and is loaded from
[unpkg](https://unpkg.com) at runtime, so `main` stays JS-free.

Run locally with `npm run watch:playground` and open
<http://localhost:8888/index.html>. The HTML auto-detects localhost
and loads the freshly-built bundle instead of unpkg.

## Translating bash to Clojure

`muschel.emit/translate` walks a parsed AST and returns a Clojure form
`(fn [env opts] …)` that runs the program — with bash control flow
expressed as **native Clojure control flow** (`if`, `loop`, `reduce`,
`as->`) and leaf work delegated to small `muschel.exec` /
`muschel.env` helpers. Useful for inspection, AOT, and embedding.

For a fully-inlineable program:

```bash
$ bb translate "X=42; for i in a b c; do echo \$i=\$X; done"
```

```clojure
(fn [env opts]
  (let [opts (merge opts (muschel.exec/expand-opts opts))]
    (as-> env env
      (muschel.env/set-var env "X" "42")
      (reduce (fn [env x]
                (let [env (muschel.env/set-var env "i" (str x))]
                  (muschel.exec/run-argv env
                    ["echo" (str (muschel.env/get-var env "i")
                                 "="
                                 (muschel.env/get-var env "X"))] opts)))
              env ["a" "b" "c"]))))
```

For constructs that need real I/O plumbing (pipes, redirects, test
brackets, command substitution, glob), the AST is hoisted into a
top-level `let` and `muschel.exec` handles it at runtime:

```bash
$ bb translate "if [ -f /etc/hostname ]; then echo found \$NAME; fi"
```

```clojure
(let [ast4739 '{:type :test-bracket :form :single
                :args [{:type :word :parts [{:type :lit :value "-f"}]}
                       {:type :word :parts [{:type :lit :value "/etc/hostname"}]}]}]
  (fn [env opts]
    (let [opts (merge opts (muschel.exec/expand-opts opts))]
      (let [env (muschel.exec/exec-cmd env ast4739 opts)]
        (if (zero? (:last-exit env))
          (muschel.exec/run-argv env
            ["echo" "found" (muschel.env/get-var env "NAME")] opts)
          env)))))
```

What inlines: `if` / `for` / `while` / `until` / `&&` / `||` / `;`,
literal calls, variable set+ref, and double-quoted strings of those.
What defers: pipes, redirects, command substitution, arithmetic
expansion, test brackets, case, subshells, function defs, background
jobs. Constructs the emitter doesn't yet handle throw
`:muschel.emit/unsupported`.

## Permits

The permit layer runs **twice**:

1. **Parse time** — walk the AST, match each `call` against the
   active rulesets, classify as `allow` / `deny` / `prompt`.
2. **Runtime hook** — at every `run-external` invocation, re-check the
   resolved command name + argv (catches dynamic dispatch via `$cmd`
   and lazy command substitution).

Default rules ship in `muschel.permit/default-rules` — POSIX-ish
allowlist of read-only file tools plus a denylist of mutating /
network commands. Compose your own:

```clojure
{:rulesets [{:allow [#"^git (status|log|diff)\b"]
             :deny  [#"^rm "]
             :prompt [#".*"]}]   ; everything else asks
 :prompter (fn [{:keys [argv]}]
             ;; return :allow / :deny / :allow-once
             :deny)}
```

## Sessions

For multi-turn shells (agents, REPLs, playgrounds), use a session so
`cd`, `export`, `set -o`, and background jobs survive across calls:

```clojure
(def sess (session/atom-session (env/new-env)))

(exec/run sess "cd /tmp; X=42")
(exec/run sess "echo $X from $(pwd)")  ; → "42 from /tmp"
```

For forkable / persistent sessions backed by
[spindel](https://github.com/replikativ/spindel), require
`muschel.session.spindel`.

## Coordinates

```clojure
;; deps.edn
org.replikativ/muschel {:git/url "https://github.com/replikativ/muschel"
                       :git/sha "<sha>"}
```

```bash
# npm
npm install muschel
```

## Development

Common workflows:

```bash
clojure -M:test          # JVM tests
npm run test             # ClojureScript node tests
clojure -M:format        # cljfmt check (CI gate)
clojure -M:ffix          # cljfmt fix in place
clojure -M:lint          # clj-kondo
clojure -M:outdated      # check for newer dep versions

npm run build:npm        # rebuild dist/muschel.js
npm run build:playground # rebuild dist/playground/playground.js
npm run watch:playground # dev server at http://localhost:8888/index.html
```

Release tasks (run from `clojure -T:build`):

```bash
clojure -T:build clean
clojure -T:build jar           # build target/*.jar
clojure -T:build install       # install to local Maven repo
clojure -T:build deploy        # push to Clojars (CLOJARS_USERNAME/PASSWORD)
clojure -T:build release       # attach jar to GitHub release (GITHUB_TOKEN)
clojure -T:build npm-publish   # bump version, build, publish to npm
```

CI runs on CircleCI via the [`replikativ/clj-tools`](https://circleci.com/orbs/registry/orb/replikativ/clj-tools)
orb — setup → build → format → unittest + cljstest → deploy + release
(on `main`). See [`.circleci/config.yml`](.circleci/config.yml).

## Comparison with other JS shell tooling

The JS ecosystem has parsers and executors, but the gap muschel fills
is **parse + permit + pluggable host in one library, with the same
semantics across Node / browser / JVM / babashka**.

| Tool                 | Parses bash | Executes | Permit gate | Browser-safe | TS types |
|----------------------|-------------|----------|-------------|--------------|----------|
| [bash-parser]        | ✓ POSIX+    | —        | —           | ✓            | —        |
| [shell-quote]        | ◯ split only| —        | —           | ✓            | ✓        |
| [shelljs]            | —           | ✓ as JS  | —           | —            | ✓        |
| [zx]                 | —           | ✓ via sh | —           | —            | ✓        |
| [bash.wasm], busybox | ✓ real bash | ✓        | —           | ✓ (~2-5MB)   | —        |
| **muschel**          | ✓ POSIX+bash| ✓        | ✓ allow/deny| ✓ (~264KB)   | ✓        |

[bash-parser]: https://www.npmjs.com/package/bash-parser
[shell-quote]: https://www.npmjs.com/package/shell-quote
[shelljs]:     https://github.com/shelljs/shelljs
[zx]:          https://github.com/google/zx
[bash.wasm]:   https://github.com/aaronpowell/webassembly-language-runtimes

- **bash-parser** gives you an AST; you write the executor + safety
  layer yourself.
- **shelljs / zx** start past the parser — they assume *your* code
  produces the argv, so LLM-emitted strings still need parsing first.
- **bash.wasm / busybox-wasm** run an actual shell binary in
  WebAssembly. Real semantics, big payload, no permit hook, no JS API
  for the host's tools.
- **muschel** is for when an LLM (or human) emits bash and you want to
  *inspect what it'll do, gate it, then run it through a host you
  control* — same code path in Node, the browser, or the JVM.

## Inspirations

- [mvdan/sh](https://github.com/mvdan/sh) — Go bash parser/formatter;
  we port a slice of its test corpus (see `LICENSE-mvdan-sh.txt` for
  attribution)
- [babashka](https://github.com/babashka/babashka) — runtime philosophy
- [Claude Code permissions](https://docs.claude.com/claude-code/security) —
  the prompt/allow/deny UX pattern
- [superficie](https://github.com/replikativ/superficie) — npm + browser
  REPL packaging shape

## License

Copyright © 2026 Christian Weilbach. Apache License 2.0.

Includes test-corpus port from
[mvdan/sh](https://github.com/mvdan/sh) under BSD-3-Clause — see
[`LICENSE-mvdan-sh.txt`](LICENSE-mvdan-sh.txt).
