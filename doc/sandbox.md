# Sandbox model

muschel is built around three composable layers — an FS protocol, a
host wrapper, and a permit gate — plus two cross-cutting observability
hooks (tracing and resource budgets). This doc walks each one and
finishes with a worked example for LLM-agent integrators.

The same model holds on **JVM**, **babashka**, **Node.js**, and the
**browser**, with one carve-out (`DiskFS` is JVM-only because babashka
doesn't ship `java.nio.file.PosixFileAttributeView` and browsers
have no real disk). The cross-platform test suite — `script/test-all` —
keeps all three runtimes in lock-step.

## Threat model

muschel is designed for the case where **the source of the bash string
is untrusted** (an LLM, a user input box, a config file edited by
someone you don't fully trust) but **the embedding application is
trusted**.

Things muschel actively defends against:

- **Path escape.** `cat ../../etc/passwd`, `<` redirects, glob walks,
  and symlinks that point outside the root are all rejected by the
  FS protocol (`-resolve` returns nil, the builtin reports "No such
  file or directory").
- **Command escape via cmd-subst / dynamic dispatch.** `echo $(rm -rf /)`
  and `cmd=rm; $cmd -rf /` are caught by the runtime permit hook
  (`run-external` re-checks the resolved argv after expansion).
- **Privilege escalation via env vars.** Builtin invocations don't
  inherit the host process environment by default; the agent gets a
  clean env unless you opt in (`:host-env? true`).
- **Resource exhaustion.** Cooperative `interrupt-fn` checks at every
  loop / spawn / awk record let you cap step count, wall-clock, or
  arbitrary other criteria. Pipelines bound by `head -n` work as
  expected because the per-stage builtins short-circuit.

Things muschel does **not** try to do:

- Stop a tool that's already on the allowlist from doing damage. If
  you put `git push` in `:fallback-allowlist`, `git push --force
  some-remote main` will run.
- Defend against compute-bound badness inside builtins themselves
  (an attacker who craftily exploits `sed` regex backtracking can
  still slow you down, modulo the interrupt-fn).
- Hide the host's *existence*. The agent can tell muschel apart from
  bash by behavioural fingerprinting (some bash extensions throw a
  parse error).

## Layer 1 — `fs.cljc` FS protocol

```
                    +----------------------------+
   bash source ───► |  exec.cljc / builtins      |
                    |  cat /work/a.txt  -─-─-─┐  |
                    +-------------------------|--+
                                              ▼
                              +---------------------+
                              |  fs/FS protocol     |
                              |  -resolve  →  path  |
                              |  -open-source       |
                              |  -read-file …       |
                              +---------------------+
                                   │       │       │
                          ┌────────┘       │       └────────┐
                          ▼                ▼                ▼
                    VirtualFS         DiskFS            (your impl)
                    in-memory map     real disk, root
                                      symlink-aware
```

Every read/write/list/stat goes through one of `fs/-resolve`,
`fs/-read-file`, `fs/-list-dir`, … on the handle. **Resolve is the
safety hinge**: a path that lands outside the root returns nil, so
the builtin sees "no such file" and exits cleanly.

```clojure
(require '[muschel.core :as m])
(require '[muschel.fs :as fs])

(def vfs (m/virtual-fs {"/work/a.txt" "alpha\n"} {:cwd "/work"}))

(fs/resolve vfs "a.txt")              ; → "/work/a.txt"
(fs/resolve vfs "/etc/passwd")        ; → nil — outside root, defense in depth
(fs/resolve vfs "../../etc/passwd")   ; → nil — traversal collapsed and refused
```

`DiskFS` adds **symlink-aware** containment: a path is canonicalised
(real-path of the parent + leaf name) before the inside-root check, so
a symlink leaf can't ferry the resolution outside the root.

## Layer 2 — `BuiltinHost`

The host abstraction (`muschel.host/Host`) exposes spawn, buffers,
pipes, files. The default host on each runtime — `host.jvm`,
`host.node`, `host.browser` — gives unrestricted access to that
runtime's capabilities (subprocesses on JVM/Node; tools on browser).

`muschel.host.builtin/make` wraps any of those into a `BuiltinHost`
that

1. routes spawn calls through a **builtin registry** (default:
   `muschel.builtins.posix/standard` — ~50 POSIX-ish commands written
   in Clojure: `cat`, `ls`, `grep`, `find`, `sed`, `awk`, `cp`, `mv`,
   `tee`, `sh -c`, …),
2. routes file I/O (`-open-file-sink`, `-open-file-source`,
   `-file-info`, `-read-file`) through the FS handle,
3. consults a **`:fallback-allowlist`** for commands not in the
   registry — anything not on either list is refused with exit 126
   and a `muschel: <cmd>: not a builtin and not in fallback-allowlist`
   error.

```clojure
(let [host (m/builtin-host
             {:fs (m/virtual-fs {"/work/a.txt" "alpha\n"} {:cwd "/work"})
              :fallback-host (m/jvm-host)
              :fallback-allowlist #{"git" "npm"}})]   ; vetted external tools
  (m/run-and-capture (m/new-env) "rm -rf /tmp/x" {:host host}))
;; → :exit 126
;;   :stderr "muschel: rm: not a builtin and not in fallback-allowlist (allowed: …)"
```

Nested `sh -c "..."` re-enters through the same host, so the sandbox
holds across recursion. That's true on JVM, Node, the browser, and
babashka — the late-bound registry (`muschel.runtime`) avoids the
exec↔posix cycle that used to need a JVM-only dynamic require.

## Layer 3 — Permits

`muschel.permit/check` runs the AST against a stack of rulesets and
classifies each call as `:allow`, `:deny`, or `:prompt`. It runs
**twice**:

1. **Parse-time** — over the whole AST, before any I/O. Catches
   statically-visible commands.
2. **Runtime hook** — at every `run-external` site, after expansion.
   Catches `$cmd`, cmd-subst `$(…)`, and `eval`-shaped dispatch that
   the AST alone can't see.

```clojure
(require '[muschel.permit :as permit])

(def rules
  [{:tool :bash
    :pattern {:kind :cmd-name :name "rm"}
    :action :deny
    :reason "destructive — denied by policy"}
   {:tool :bash
    :pattern {:kind :argv-glob :glob "git status*"}
    :action :allow}
   {:tool :bash
    :pattern {:kind :cmd-name :name "git"}
    :action :prompt}])    ; everything else asks

(m/run-and-capture
  (m/new-env)
  "echo $(rm -rf /tmp/foo)"           ; cmd-subst hides `rm` from the AST
  {:host host
   :permit {:rulesets [rules]
            :prompter (fn [_] :deny)}})
;; → outer echo succeeds (exit 0), inner rm is caught by the runtime hook
;;   and stderr contains "runtime permit denied `rm`"
```

`permit/default-rules` ships a sensible starter set: read-only POSIX
tools are allow-listed, network and mutating tools are deny-listed.

Prompter return values:

| Value           | Meaning                                                          |
|-----------------|------------------------------------------------------------------|
| `:allow-once`   | Allow this single call; don't change rules.                      |
| `:allow-always` | Allow + emit a new rule covering this argv-prefix or cmd-name.   |
| `:deny`         | Reject; surface a permit-denial event.                           |

## Resource budgets

`muschel.budget` is a tiny **cooperative** interrupt-fn API. Pass the
fn under `:interrupt-fn` (or use the `:timeout-ms` shortcut for a
deadline) and the executor checks it at every `exec-stmt`, every
pipeline stage, every awk record, every `xargs` round. When it returns
truthy, the run aborts with an `ex-info` whose `:muschel/budget` data
key identifies which budget tripped.

```clojure
(require '[muschel.budget :as bud])

;; Cap at 1000 cooperative steps.
(m/run-and-capture
  (m/new-env)
  "i=0; while [ \"$i\" -lt 1000000 ]; do i=$((i+1)); done"
  {:host host
   :interrupt-fn (bud/step-interrupt 1000)})
;; throws ex-info {:muschel/budget :steps}

;; Cap at 100 ms wall-clock.
(m/run-and-capture (m/new-env) "while true; do :; done"
                   {:host host :timeout-ms 100})
;; throws ex-info {:muschel/budget :timeout}

;; Combine.
(bud/combine (bud/step-interrupt 10000)
             (bud/deadline-interrupt 5000))
```

## Tracing & introspection

Pass `:trace` and the executor records every tool call, FS op, and
permit denial into a bounded ring buffer. The shape:

```clojure
(:trace
 (m/run-and-capture (m/new-env) "cat a.txt | grep beta"
                    {:host host :trace true}))
;; →
{:tools  [{:name "cat"  :argv ["cat" "a.txt"] :exit 0 :stdout-bytes 8 :duration-ms 0}
          {:name "grep" :argv ["grep" "beta"] :exit 0 :stdout-bytes 5 :duration-ms 0}]
 :reads  ["/work/a.txt"]
 :writes []
 :fs     [{:op :open-source :path "/work/a.txt" :ok? true} …]
 :denied []}
```

For long-running sessions where the ring would lose history, install
streaming hooks. They're unbounded — useful for persisting the full
trace into a database (e.g. training-data collection for dvergr-style
harnesses):

```clojure
(m/run-and-capture
  (m/new-env) "cat a.txt"
  {:host host
   :trace {:cap 1000                              ; ring size for the snapshot
           :on-tool  (fn [evt] (db/persist! evt))
           :on-fs    (fn [evt] (db/persist! evt))
           :on-deny  (fn [evt] (alert! evt))}})
```

`muschel.fs.traced/wrap` is what BuiltinHost uses under the hood — you
can wrap any FS impl directly if you need lower-level FS tracing
outside the builtin path.

## Cross-runtime guarantees

Identical semantics on JVM / babashka / CLJS for: AST, expansion,
control flow, pipes, redirects, the FS protocol, permits, budgets,
tracing, nested `sh -c`, all ~50 POSIX builtins, the awk subset, jq
(via `clojure.data.json` on JVM, `cheshire.core` on bb, `js/JSON` on
CLJS), and the permit runtime hook.

Caveats:

- **Background jobs (`&` + `wait`)** need a real thread on JVM. On bb
  the bash construct still parses and runs, but the BrowserHost-style
  `host.browser/-async` is synchronous, so `&` completes inline and
  `jobs` always reports `Done`. Same on Node's `host.browser`-backed
  in-process path.
- **`DiskFS`** is JVM-only. bb skips it via a `:bb` reader-conditional
  (no `java.nio.file.PosixFileAttributeView`); Node uses
  `host.browser` + `virtual-fs` instead. A node-flavored DiskFS is on
  the roadmap.
- **`curl`** uses `java.net.http` on JVM. On CLJS the builtin returns
  a "not yet supported on CLJS" stub; on bb it works.
- The **`spindel`** session backend stays JVM-only; use
  `m/atom-session` from bb / CLJS.

## Worked example — LLM agent harness

Putting it all together: an agent emits bash, we parse it, gate it,
run it sandboxed against an in-memory VFS, and capture a complete
trace.

```clojure
(require '[muschel.core :as m])
(require '[muschel.permit :as permit])
(require '[muschel.budget :as bud])

(defn run-agent-bash
  "Run a single bash string from an LLM agent. Returns a result map
   with stdout/stderr/exit AND a trace of everything that happened."
  [agent-bash project-dir]
  (let [host (m/builtin-host
               {:fs (m/disk-fs project-dir {:cwd project-dir
                                            :max-bytes (* 8 1024 1024)})
                :fallback-host (m/jvm-host)
                :fallback-allowlist #{"git"}})       ; let git through
        opts {:host host
              :permit {:rulesets [permit/default-rules
                                  [{:tool :bash
                                    :pattern {:kind :cmd-name :name "rm"}
                                    :action :deny
                                    :reason "agent must not delete"}]]
                       :prompter (fn [{:keys [argv]}]
                                   ;; Could pop a UI here.
                                   :deny)}
              :interrupt-fn (bud/combine
                              (bud/deadline-interrupt 30000)    ; 30 s
                              (bud/step-interrupt 100000))
              :trace {:cap 500}}]
    (try
      (m/run-and-capture (m/new-env) agent-bash opts)
      (catch clojure.lang.ExceptionInfo e
        (if (bud/budget-exceeded? e)
          {:exit 124 :stderr (str "muschel: budget exceeded: "
                                  (:muschel/budget (ex-data e)) "\n")
           :trace nil}
          (throw e))))))

(run-agent-bash "ls *.clj | head -3" "/home/me/project")
;; → {:exit 0
;;    :stdout "deps.edn\nproject.clj\n…"
;;    :stderr ""
;;    :trace {:tools  [{:name "ls" …} {:name "head" …}]
;;            :reads  ["/home/me/project"]
;;            :denied []
;;            …}}

(run-agent-bash "rm -rf /tmp/work" "/home/me/project")
;; → {:exit 126
;;    :stderr "rm: not a builtin and not in fallback-allowlist …"
;;    :trace {:denied [{:tool "rm" :argv ["rm" "-rf" "/tmp/work"]
;;                      :reason "agent must not delete"}]
;;            …}}
```

From the JS side, the same flow with `muschel.run(src, opts)` plus
`opts.permit.rulesets`, `opts.interruptFn`, `opts.trace.{onTool,onFs,onDeny}`.
See the [main README](../README.md#quickstart--nodejs--javascript) for
the surface.

## Further reading

- [`../README.md`](../README.md) — high-level overview.
- [`builtins.md`](builtins.md) — per-command reference.
- [`awk.md`](awk.md) — awk subset and known gaps.
- [`grammar-study.md`](grammar-study.md) — the grammar muschel
  implements and the bash extensions we deliberately skip.
- [`llm-corpus.md`](llm-corpus.md) — what LLMs actually emit when
  asked for shell, and what muschel pins down.
