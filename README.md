# muschel

[![CircleCI](https://circleci.com/gh/replikativ/muschel.svg?style=shield)](https://circleci.com/gh/replikativ/muschel)
[![Clojars](https://img.shields.io/clojars/v/org.replikativ/muschel.svg)](https://clojars.org/org.replikativ/muschel)
[![Slack](https://img.shields.io/badge/slack-join_chat-brightgreen.svg)](https://clojurians.slack.com/archives/C09622F337D)

**muschel** — German for *shell* (the bivalve). A Clojure(Script) library
that parses bash/POSIX-shell source into a structured AST, checks it
against a permit (allow/deny) policy, and executes it through a
pluggable host — JVM, babashka, Node.js, or in the browser against a
virtual file system and a virtual tool registry.

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

The security story — host layering, the FS protocol, permits, budgets,
tracing — lives in **[`doc/sandbox.md`](doc/sandbox.md)**.

## Architecture

```
src/muschel/
  lex.cljc                  hand-written tokenizer  (quotes, $, ((, [[, heredocs)
  errors.cljc               position-bearing errors
  ast.cljc                  AST node ctors, predicates, walk/zip
  parse.cljc                recursive-descent parser over tokens
  env.cljc                  immutable shell-environment value
  expand.cljc               POSIX expansion (brace → tilde → param → cmd-subst → arith → split → glob → quote removal)
  arith.cljc                $(( … )) evaluator
  permit.cljc               parse-time + runtime-hook allow/deny against rules
  budget.cljc               cooperative interrupt-fn (step, deadline, combine)
  trace.cljc                bounded ring-buffer + streaming hooks (tools, fs, denials)
  runtime.cljc              late-bound parse/run/new-env registry — breaks exec↔posix cycle for `sh -c`
  exec.cljc                 executor over the AST (builtins, control flow, pipes, redirects, bg jobs)
  session.cljc              forkable session protocol (AtomSession + SpindelSession)
  fs.cljc                   containment-aware FS protocol (resolve, read, list, stat, open-sink, mkdir, …)
  fs/virtual.cljc           in-memory VFS — no real-disk access possible
  fs/disk.clj               real-disk DiskFS pinned to a root (JVM only)
  fs/traced.cljc            wraps any FS, records every protocol op for introspection
  host.cljc                 Host protocol — abstracts buffers / pipes / spawn / fs
  host/builtin.cljc         BuiltinHost — routes commands through a builtin registry first, fallback second
  host/jvm.clj              JVM fallback — `babashka.process` + `java.io`
  host/node.cljs            Node fallback — `child_process` + node fs
  host/browser.cljs         Browser fallback — virtual buffers, virtual tool registry, no spawn
  builtins/posix.cljc       ~50 POSIX builtins (cat, ls, grep, find, sed, awk dispatch, cp, mv, tee, sh -c, …)
  builtins/awk.cljc         real awk subset (lexer + parser + interp)
  builtins/awk_compat.cljc  cross-platform shims (regex, StringBuilder, format, codepoints)
  js_api.cljs               ^:export bindings for the npm bundle
  playground.cljs           browser playground entry point
  cli.clj                   JVM CLI entry — bash-shaped invocation (-c, script.sh, -s, -n, -x, -o, --) + sandbox flags
```

## Runtimes

muschel runs in four runtimes against a single `.cljc` source. The
`bb test` task and `script/test-all` exercise the full test suite on
JVM, ClojureScript-on-Node, and babashka in one go.

| Runtime           | Status | How                                                                          |
|-------------------|:------:|------------------------------------------------------------------------------|
| JVM (Clojure)     |   ✓    | `host/jvm` — `babashka.process`, `java.io`; full `DiskFS`                    |
| Babashka (sci)    |   ✓    | Same `.cljc`; `:bb` reader-conds skip `DiskFS` (JVM-only nio); VFS works     |
| Node.js (cljs)    |   ✓    | `host/node` — `child_process.spawnSync`                                      |
| Browser (cljs)    |   ✓    | `host/browser` — virtual fs + virtual tool registry, no spawn                |
| npm / TypeScript  |   ✓    | `dist/muschel.js` (+ `dist/muschel.d.ts`)                                    |

Current counts: **JVM 447/1068**, **CLJS 417/1016**, **babashka
415/1008** — all green, 0 failures.

## Quickstart — Clojure

The contained sandbox is `builtin-host` over a fallback (`jvm-host`),
backed by either `virtual-fs` (in-memory) or `disk-fs` (real disk pinned
to a root). The agent only ever sees what the FS lets it resolve, and
only ever runs commands from the builtin set.

```clojure
(require '[muschel.core :as m])

(let [host (m/builtin-host
             {:fs (m/virtual-fs {"/work/a.txt" "alpha\nbeta\n"} {:cwd "/work"})
              :fallback-host (m/jvm-host)})]
  (m/run-and-capture (m/new-env) "grep beta a.txt" {:host host}))
;; → {:stdout "beta\n" :stderr "" :exit 0 :env …}
```

With a permit (parse-time gate + runtime hook):

```clojure
(let [host (m/builtin-host {:fs (m/virtual-fs {} {:cwd "/"})
                            :fallback-host (m/jvm-host)})]
  (m/run-and-capture (m/new-env) "rm -rf /tmp/x"
                     {:host host
                      :permit {:rulesets [m/default-rules]
                               :prompter m/deny-all-prompter}}))
;; → :exit 126, :stderr explains the denial
```

`disk-fs` pins the FS to a real directory, with symlink-aware
containment so `cat ../etc/passwd` stays inside the root:

```clojure
(let [host (m/builtin-host
             {:fs (m/disk-fs "/home/me/project" {:cwd "/home/me/project"})
              :fallback-host (m/jvm-host)
              :fallback-allowlist #{"git"}})]  ; allow git, refuse everything else
  (m/run-and-capture (m/new-env) "git status" {:host host}))
```

## Quickstart — CLI

`muschel.cli` is a JVM-side command-line entry that mirrors `bash`'s
invocation surface and adds the sandbox flags on top. From a checkout:

```bash
clojure -M:cli                                  # interactive shell
clojure -M:cli -c 'echo hi | grep h'            # one-shot
clojure -M:cli script.sh foo bar                # run a script ($1=foo $2=bar)
clojure -M:cli -n script.sh                     # validate syntax only
clojure -M:cli -o errexit -c 'false; echo no'   # -o sets shell options

# Sandboxed against the current directory:
clojure -M:cli --sandbox --root . script.sh

# Sandboxed against an in-memory VFS (optionally seeded from edn):
clojure -M:cli --sandbox --virtual ./fixtures/seed.edn -c 'cat /work/a.txt'

# Let some real-world tools through to the fallback host:
clojure -M:cli --sandbox --root . --allow git,clojure -c 'git status'

# Custom permit overlay on top of the default ruleset:
clojure -M:cli --sandbox --root . --permit ./tight.edn -c 'curl example.com'

# Analysis subcommands:
clojure -M:cli translate -f script.sh           # bash → Clojure form
clojure -M:cli check     -f script.sh --permit ./tight.edn   # permit dry-run
clojure -M:cli parse     'echo hi'              # pretty-print AST
```

Bash positional semantics are honoured: anything after `-c CMD`, after
a script-file, or after `--` becomes `$0`/`$1`/... and is NOT
re-interpreted as a flag. So `muschel script.sh -x` runs the script
with `$1="-x"` — same as bash, NOT with xtrace on.

`--sandbox` requires exactly one of `--root DIR` (DiskFS pinned to
DIR) or `--virtual [FILE]` (empty VFS, or one seeded from an edn map
`{"/path" "content", ...}`). Without `--sandbox` the host is `JvmHost`
— full disk + permissions, no permit gate — same shape as `bb sh`.

`clojure -M:cli --help` has the full flag table.

## Quickstart — Node.js / JavaScript

```bash
npm install muschel
```

```js
const m = require('muschel');

// Browser-style virtual FS, but from Node. Nothing touches real disk.
const host = m.browserHost({
  files: { '/work/a.txt': 'alpha\nbeta\n' },
});

const r = m.run("cd /work && grep beta a.txt | tr a-z A-Z", { host });
console.log(r.stdout); // "BETA\n"

// Stateful session: cd / var assignment persists across calls.
const sess = m.atomSession();
m.run("cd /work; export FOO=bar", { host, session: sess });
const r2 = m.run("echo $FOO from $(pwd)", { host, session: sess });
console.log(r2.stdout); // "bar from /work\n"
```

`m.nodeHost()` is real-disk on Node and is **unsandboxed**; wrap it in
`m.builtin-host` (cross-platform — see [`doc/sandbox.md`](doc/sandbox.md))
or just use `m.browserHost` for the in-memory path.

## Quickstart — Babashka

muschel runs in [babashka](https://github.com/babashka/babashka) without
modification: every layer is `.cljc`, sci-safe, and uses
`babashka.process` for spawn. `:bb` reader-conditionals skip the
`fs/disk.clj` namespace (bb ships no `java.nio.file.PosixFileAttributeView`);
everything else — including nested `sh -c`, the FS-protected VFS, the
permit gate, tracing, budgets — runs the same as on JVM.

From a checkout (the bundled `bb.edn` provides paths + tasks):

```bash
bb exec "echo hi | tr a-z A-Z"          # → HI
bb sh                                   # interactive shell prompt
bb test                                 # cross-platform test suite
```

From your own project, add muschel to your `bb.edn`:

```clojure
{:deps {org.replikativ/muschel {:mvn/version "0.1.x"}}}
```

```clojure
(require '[muschel.core :as m])

(let [host (m/builtin-host {:fs (m/virtual-fs {"/work/data.csv"
                                               "name,age\nalice,30\nbob,25\n"}
                                              {:cwd "/work"})
                            :fallback-host (m/jvm-host)})]
  (m/run-and-capture (m/new-env)
                     "awk -F , '{print $1}' data.csv"
                     {:host host}))
```

## Quickstart — Browser (no spawn)

```js
import * as m from 'muschel';

const host = m.browserHost({
  files: { '/README.md': '# hi\n', '/etc/issue': 'demo\n' },
  tools: {                                       // optional: virtual external commands
    ...m.stockTools(),                           // wc, grep, head (cat is a stub)
    git: (argv) => ({ stdout: `[simulated git ${argv.join(' ')}]\n`, exit: 0 }),
  },
});

m.run("cat /README.md | wc -l", { host });
// → { stdout: " 1 …", stderr: "", exit: 0 }

// Nested sh -c also works — the inner shell goes through the SAME host,
// so the sandbox holds across recursion.
m.run('sh -c "echo hello && cat /etc/issue"', { host });
```

**[Try it in your browser →](https://replikativ.github.io/muschel/playground/)**

The playground is one static HTML file in [`docs/playground/`](docs/playground/);
the bundle ships in the npm package and is loaded from
[unpkg](https://unpkg.com) at runtime, so `main` stays JS-free. Run
locally with `npm run watch:playground` and open
<http://localhost:8888/index.html>.

## Sandbox model

Three layers, composed:

1. **`fs/FS` protocol** — every read/write/list/stat goes through a
   single containment-aware handle (`VirtualFS`, `DiskFS`, or your own).
   A path that resolves outside the root returns nil; no operation
   reaches real disk unless you explicitly hand the host a `disk-fs`.
2. **`BuiltinHost`** — wraps a fallback host (`jvm-host` /
   `node-host` / `browser-host`) and overrides `-spawn` so commands
   are dispatched to a builtin registry first. Anything not in the
   registry has to be in `:fallback-allowlist` or it gets refused
   with exit 126. The agent sees the builtins, never raw `bash`.
3. **`permit/check`** — runs twice. Once at parse time over the
   whole AST, once at every `run-external` site so dynamic dispatch
   via `$cmd` and lazy command substitution `$(…)` are also caught.

On top of that, **resource budgets** (`budget/step-interrupt`,
`budget/deadline-interrupt`, `budget/combine`) interrupt runaway loops
cooperatively, and **tracing** (`:trace` opt) captures every tool call,
FS op, and permit denial in a bounded ring buffer with optional
streaming hooks.

Full walkthrough including a worked LLM-agent integration is in
**[`doc/sandbox.md`](doc/sandbox.md)**.

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
(def sess (m/atom-session (m/new-env)))

(m/run-and-capture (m/new-env) "cd /tmp; X=42"      {:host host :session sess})
(m/run-and-capture (m/new-env) "echo $X from $(pwd)" {:host host :session sess})
;; → "42 from /tmp"
```

For forkable / persistent sessions backed by
[spindel](https://github.com/replikativ/spindel), require
`muschel.session.spindel`. (JVM only — its deps don't load in bb.)

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

What inlines: `if` / `for` / `while` / `until` / `&&` / `||` / `;`,
literal calls, variable set+ref, and double-quoted strings of those.
What defers: pipes, redirects, command substitution, arithmetic
expansion, test brackets, case, subshells, function defs, background
jobs. Constructs the emitter doesn't yet handle throw
`:muschel.emit/unsupported`.

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

Cross-runtime testing — the script that gates a release:

```bash
./script/test-all       # JVM + CLJS-on-Node + babashka, fail fast
```

Or one at a time:

```bash
clojure -M:test                                  # JVM (Kaocha)
npx shadow-cljs compile ci && node out/ci-tests.js  # CLJS (Node)
bb test                                          # babashka
```

Other tasks:

```bash
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
  control* — same code path in Node, the browser, the JVM, and bb.

## Further reading

- [`doc/sandbox.md`](doc/sandbox.md) — sandbox model, FS protocol,
  permits, budgets, tracing, agent-integration walkthrough.
- [`doc/builtins.md`](doc/builtins.md) — POSIX builtin reference.
- [`doc/awk.md`](doc/awk.md) — built-in awk subset, known gaps.
- [`doc/grammar-study.md`](doc/grammar-study.md) — notes on the
  grammar we implement and the bash extensions we deliberately skip.
- [`doc/llm-corpus.md`](doc/llm-corpus.md) — what LLMs actually emit
  when asked for shell, and what muschel's permit shape pins down.

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
