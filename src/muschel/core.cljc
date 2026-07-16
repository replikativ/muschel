(ns muschel.core
  "Public Clojure API facade for muschel. Mirrors `muschel.js-api`
   (compiled to the `muschel` npm package) so future codegen from a
   single api/specification.cljc can produce both surfaces.

       (require '[muschel.core :as m])

       ;; sandboxed run on a virtual FS
       (def host (m/builtin-host {:fs (m/virtual-fs {\"/a.txt\" \"hi\"})
                                  :fallback-host (m/jvm-host)}))
       (m/run-and-capture (m/new-env) \"cat /a.txt\" {:host host})

   For finer control, require the underlying layers directly:
   `muschel.lex`, `muschel.parse`, `muschel.ast`, `muschel.env`,
   `muschel.expand`, `muschel.permit`, `muschel.exec`, `muschel.session`,
   `muschel.host`, `muschel.fs`, `muschel.budget`, `muschel.trace`."
  (:require [muschel.budget :as budget]
            [muschel.env :as env]
            [muschel.exec :as exec]
            [muschel.fs :as fs]
            [muschel.fs.mount :as fs.mount]
            [muschel.fs.virtual :as fs.virtual]
            [muschel.parse :as parse]
            [muschel.permit :as permit]
            [muschel.session :as session]
            [muschel.host.builtin :as host.builtin]
            #?(:clj  [muschel.host.jvm :as host.jvm])
            ;; `muschel.fs.disk` uses java.nio.file.* (Files,
            ;; PosixFileAttributeView, …) which babashka doesn't ship.
            ;; Stub it out on bb — bb users get the VFS path; real-disk
            ;; access is a JVM-only feature anyway.
            #?@(:bb [] :clj [[muschel.fs.disk :as fs.disk]])))

;; ============================================================================
;; Parsing
;; ============================================================================

(def parse
  "Parse a bash source string into an AST. See `muschel.parse/parse`."
  parse/parse)

;; ============================================================================
;; Env / session
;; ============================================================================

(def new-env
  "Build a fresh shell env value. By default does NOT inherit the host
   process environment — pass `:host-env? true` to opt in.
   Options: `:cwd`, `:pos-args`, `:script`, `:host-env?`, `:vars`."
  env/new-env)

(def get-var
  "Read a shell variable. Returns the string value or `\"\"` if unset."
  env/get-var)

(def set-var
  "Set a shell variable on an env value. Returns a new env."
  env/set-var)

(def atom-session
  "Create a stateful session that threads env across `run` calls."
  session/atom-session)

;; ============================================================================
;; Permit
;; ============================================================================

(def check
  "Run the parse-time permit check. See `muschel.permit/check`."
  permit/check)

(def default-rules
  "The shipped default ruleset (read-only POSIX allow + denylist)."
  permit/default-rules)

(def deny-all-prompter
  "Prompter that denies every `:prompt` decision."
  permit/deny-all-prompter)

(def allow-all-prompter
  "Prompter that allows every `:prompt` decision (useful for tests
   and playgrounds; do NOT use in production agent loops)."
  permit/allow-all-prompter)

;; ============================================================================
;; Execution
;; ============================================================================

(def run
  "Execute a bash source string. Returns
   `{:env :exit :session :permit? :trace?}`. Does NOT capture
   stdout/stderr — caller supplies streams via `:out` / `:err`.
   See `muschel.exec/run`."
  exec/run)

(def run-and-capture
  "Like `run` but captures stdout and stderr as strings. Returns
   `{:env :exit :session :stdout :stderr :permit? :trace?}`."
  exec/run-and-capture)

;; ============================================================================
;; Hosts
;; ============================================================================

#?(:clj
   (def jvm-host
     "Create an unsandboxed JVM-backed host (`babashka.process` + `java.io`).
      Pair with `builtin-host` + `disk-fs` (or `virtual-fs`) to get a
      contained sandbox."
     host.jvm/make))

(def builtin-host
  "Build a BuiltinHost — wraps a fallback host with FS-aware builtin
   dispatch. This is the muschel sandbox model. Cross-platform
   (JVM + Node + browser).

   Options:
     :fs                 — required, a muschel.fs handle
     :fallback-host      — required, e.g. `(jvm-host)`, `(node-host)`,
                            `(browser-host)`
     :builtins           — map cmd-name → fn (default: posix/standard)
     :fallback-allowlist — set of cmd-names the fallback may run
                            (default empty; agents only see builtins)"
  host.builtin/make)

;; ============================================================================
;; Filesystems
;; ============================================================================

(def virtual-fs
  "Construct a VirtualFS from `{path → content}`. Options: `:cwd`
   (default \"/\"). Pure in-memory; structurally contained."
  fs.virtual/make)

(def mount-fs
  "Compose a base filesystem with dynamically managed Geschichte or generic
   child mounts. `git init` requires this router so takeover is atomic."
  fs.mount/make)

;; bb has no PosixFileAttributeView etc., so muschel.fs.disk doesn't
;; load there — only define `disk-fs` on real JVM.
#?(:bb nil
   :clj (def disk-fs
          "Construct a DiskFS pinned to `root`. Real disk, contained via
           `inside?`-check + parent-real-path resolution.
           Options: `:cwd`, `:max-bytes`. JVM only."
          fs.disk/make))

;; FS protocol wrappers — symmetric with JS `m.fs.*`.
(def fs-read-file          fs/read-file)
(def fs-read-bytes         fs/read-bytes)
(def fs-list-dir           fs/list-dir)
(def fs-exists?            fs/exists?)
(def fs-stat               fs/stat)
(def fs-mkdir              fs/mkdir)
(def fs-delete             fs/delete)
(def fs-rename             fs/rename)
(def fs-touch              fs/touch)
(def fs-chmod              fs/chmod)
(def fs-symlink            fs/symlink)
(def fs-sandbox-relativize fs/sandbox-relativize)
(def fs-cwd                fs/cwd)
(def fs-cd!                fs/cd!)
(def fs-resolve            fs/resolve)

;; ============================================================================
;; Resource budgets
;; ============================================================================

(def deadline-interrupt
  "Make an interrupt-fn that throws once `wall-clock-ms` have elapsed."
  budget/deadline-interrupt)

(def step-interrupt
  "Make an interrupt-fn that throws after `max-steps` invocations."
  budget/step-interrupt)

(def combine-interrupts
  "Compose multiple interrupt-fns into one."
  budget/combine)

(def budget-exceeded?
  "True if `ex` is a budget-exceeded throw from this layer."
  budget/budget-exceeded?)
