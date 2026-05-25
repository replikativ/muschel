(ns muschel.core
  "Public Clojure API facade for muschel.

   Re-exports the small surface most callers need so you can:

       (require '[muschel.core :as m])

       (m/run-and-capture (m/new-env)
                          \"git log --oneline | head -3\"
                          {:host (m/jvm-host)})

   For finer control, require the underlying layers directly:
   `muschel.lex`, `muschel.parse`, `muschel.ast`, `muschel.env`,
   `muschel.expand`, `muschel.permit`, `muschel.exec`, `muschel.session`,
   `muschel.host` and the per-platform host impls.

   The cljs / TypeScript surface is in `muschel.js-api` (compiled to the
   `muschel` npm package — see `dist/npm/`)."
  (:require [muschel.env :as env]
            [muschel.exec :as exec]
            [muschel.parse :as parse]
            [muschel.permit :as permit]
            [muschel.session :as session]
            #?(:clj  [muschel.host.jvm :as host.jvm])))

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
  "Build a fresh shell env value from the host process environment.
   Options: `:cwd`, `:pos-args`, `:script`. See `muschel.env/new-env`."
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
  "Execute a bash source string against an env. Returns
   `{:env :exit :session ...}`. See `muschel.exec/run`."
  exec/run)

(def run-and-capture
  "Like `run` but also captures stdout and stderr as strings.
   Returns `{:env :exit :session :stdout :stderr ...}`."
  exec/run-and-capture)

;; ============================================================================
;; Hosts
;; ============================================================================

#?(:clj
   (def jvm-host
     "Create a JVM-backed host (uses `babashka.process` + `java.io`)."
     host.jvm/make))
