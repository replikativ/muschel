(ns muschel.env
  "Immutable shell environment for muschel.

   The env is a plain Clojure map threaded through expand and exec.
   Every mutation (set-var, cd, record-exit, push-params) returns a new
   env — no globals, no atom-wrapping, no Runner struct. Sub-scopes
   (function calls, subshells, `(...)`, command substitution) are
   created by `fork` and discarded; their mutations don't leak.

   Reference: this mirrors `mvdan.cc/sh/interp.Runner` minus the
   stdin/stdout/stderr fields (those are exec-parameters, not env
   state) and minus the mutable design.

   ## Shape

       {:vars       {\"VAR\" {:value <str> :exported? bool :readonly? bool}}
        :cwd        <abs-path-str>
        :prev-cwd   <abs-path-str>     ; \\$OLDPWD
        :last-exit  <int>              ; \\$?
        :last-bg-pid <int> or nil       ; \\$!
        :pid        <int>              ; \\$$ — host process pid
        :script     <str>              ; \\$0
        :pos-args   [<str>]            ; \\$1..\\$9, \\$@, \\$*
        :ifs        \" \\t\\n\"             ; field separator for word splitting
        :options    {:errexit bool, :nounset bool, :pipefail bool,
                     :xtrace bool, :noglob bool, :noclobber bool}
        :funcs      {<name> <function-def-stmt>}
        :bg-procs   [<process>]        ; tracked background jobs
        :umask      <int>}             ; later

   ## Special variables

   Looked up by `get-var` directly — never stored in `:vars`:
     \\$?  → :last-exit
     \\$$  → :pid
     \\$#  → (count :pos-args)
     \\$@  \\$*  → :pos-args (joined for $*, separate for $@)
     \\$0  → :script
     \\$1..\\$9 → (:pos-args env)
     \\$!  → :last-bg-pid
     \\$_  → not supported (would need previous-cmd tracking)"
  (:require [clojure.string :as str]))

;; ============================================================================
;; Construction
;; ============================================================================

(defn- host-pid []
  #?(:clj  (try (.pid (java.lang.ProcessHandle/current))
                (catch Throwable _ 0))
     :cljs (if (and (exists? js/process) (.-pid js/process))
             (.-pid js/process)
             0)))

(defn- host-env-map
  "Read the host process environment as {name → value}.
   Node: from process.env. Browser: {}."
  []
  #?(:clj (into {} (System/getenv))
     :cljs (if (and (exists? js/process) (.-env js/process))
             (let [e (.-env js/process)]
               (into {} (for [k (js-keys e)] [k (aget e k)])))
             {})))

(defn- host-cwd []
  #?(:clj (System/getProperty "user.dir")
     :cljs (if (and (exists? js/process) (.cwd js/process))
             (.cwd js/process)
             "/")))

(defn new-env
  "Build a fresh env from the host process environment. Every env var
   inherited from the host is marked `:exported? true`.

   Options:
     :cwd         override starting cwd (default: host cwd)
     :pos-args    initial positional params (default: [])
     :script      \\$0 value (default: \"muschel\")"
  [& {:keys [cwd pos-args script]
      :or {pos-args [] script "muschel"}}]
  (let [host (host-env-map)
        vars (into {} (for [[k v] host]
                        [k {:value v :exported? true :readonly? false}]))
        cwd' (or cwd (get host "PWD") (host-cwd))]
    {:vars       vars
     :cwd        cwd'
     :prev-cwd   cwd'
     :last-exit  0
     :last-bg-pid nil
     :pid        (host-pid)
     :script     script
     :pos-args   (vec pos-args)
     :ifs        " \t\n"
     :options    {:errexit  false
                  :nounset  false
                  :pipefail false
                  :xtrace   false
                  :noglob   false
                  :noclobber false}
     :funcs      {}
     :bg-procs   []
     ;; Scope stack for `local` / function calls. Each frame is
     ;; {:captured-prev {name → prior-value-map-or-:unset}}.
     ;; Empty at top level; one frame pushed per active function call.
     :scope-stack []}))

(defn empty-env
  "A fully-empty env with no inherited host variables. For tests."
  []
  (-> (new-env)
      (assoc :vars {} :cwd "/" :prev-cwd "/")))

;; ============================================================================
;; Variable access — handles special vars uniformly
;; ============================================================================

(def ^:private special-var-names
  #{"?" "$" "#" "@" "*" "!" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
    "PWD" "OLDPWD" "IFS"})

(defn- positional [env idx]
  (let [pa (:pos-args env)
        i (dec idx)]   ; 1-indexed
    (when (and (>= i 0) (< i (count pa)))
      (nth pa i))))

(declare get-var*)

(defn get-var
  "Look up a variable by name. Returns the string value, or empty
   string if unset (bash's default). Use `get-var*` if you need to
   distinguish unset from empty."
  [env name]
  (or (get-var* env name) ""))

(defn get-var*
  "Like `get-var` but returns nil for genuinely-unset variables.
   Useful for `${VAR:-default}` semantics."
  [env name]
  (case name
    "?" (str (:last-exit env))
    "$" (str (:pid env))
    "#" (str (count (:pos-args env)))
    "!" (some-> (:last-bg-pid env) str)
    "@" (str/join " " (:pos-args env))           ; same as $* outside quotes
    "*" (str/join (subs (:ifs env) 0 1) (:pos-args env))
    "0" (:script env)
    ("1" "2" "3" "4" "5" "6" "7" "8" "9")
    (positional env #?(:clj (Integer/parseInt name)
                       :cljs (js/parseInt name)))
    "PWD"    (:cwd env)
    "OLDPWD" (:prev-cwd env)
    "IFS"    (:ifs env)
    ;; Regular var
    (get-in env [:vars name :value])))

(defn declared?
  "True if the var is declared (even if its value is empty)."
  [env name]
  (or (special-var-names name)
      (contains? (:vars env) name)))

(defn exported?
  [env name]
  (boolean (get-in env [:vars name :exported?])))

(defn readonly?
  [env name]
  (boolean (get-in env [:vars name :readonly?])))

;; ============================================================================
;; Variable mutation — every fn returns a new env
;; ============================================================================

(defn set-var
  "Set a variable to `value`. Preserves the :exported? flag if the var
   already exists. Updates IFS if name is IFS. Refuses (returns env
   unchanged) if the var is readonly."
  [env name value]
  (cond
    (= name "IFS") (assoc env :ifs value)
    (readonly? env name) env
    :else
    (update env :vars
            (fn [vs]
              (let [prev (get vs name)]
                (assoc vs name
                       {:value     value
                        :exported? (boolean (:exported? prev))
                        :readonly? (boolean (:readonly? prev))}))))))

(defn export
  "Mark a variable as exported, setting it to `value` if provided.
   `export FOO=bar` and `FOO=bar; export FOO` both go through here."
  ([env name]
   (if (contains? (:vars env) name)
     (assoc-in env [:vars name :exported?] true)
     ;; export of an unset var: declare it as exported but empty
     (assoc-in env [:vars name] {:value "" :exported? true :readonly? false})))
  ([env name value]
   (-> (set-var env name value)
       (assoc-in [:vars name :exported?] true))))

(defn unset-var
  "Remove a variable. Refuses (returns env unchanged) if readonly."
  [env name]
  (if (readonly? env name)
    env
    (update env :vars dissoc name)))

(defn mark-readonly
  [env name]
  (if (contains? (:vars env) name)
    (assoc-in env [:vars name :readonly?] true)
    env))

;; ============================================================================
;; Working directory
;; ============================================================================

(defn- absolutize [^String cwd ^String path]
  #?(:clj
     (let [f (java.io.File. path)]
       (.getCanonicalPath
        (if (.isAbsolute f) f (java.io.File. cwd path))))
     :cljs
     ;; In node, use the `path` module's resolve (works like java's
     ;; getCanonicalPath for path-string purposes, without filesystem
     ;; existence requirement). In the browser, treat paths as opaque.
     (if (and (exists? js/require))
       (let [path-mod (js/require "path")]
         (.resolve path-mod cwd path))
       ;; Browser: pure-string concatenation, no `.` / `..` resolution.
       ;; Good enough for in-memory shells that don't touch a real fs.
       (cond
         (clojure.string/starts-with? path "/") path
         (= "" path) cwd
         :else (str (clojure.string/replace cwd #"/$" "") "/" path)))))

(defn cd
  "Change to `path` (absolute or relative to current cwd). Returns new
   env with :cwd updated and :prev-cwd set to the old cwd.
   `cd -` switches to OLDPWD.
   Bash also updates the env-vars PWD and OLDPWD; we expose these via
   `get-var` so they're always in sync without explicit storage."
  [env path]
  (let [old (:cwd env)
        target (cond
                 (= path "-") (:prev-cwd env)
                 (= path "")  (or (get-var* env "HOME") old)
                 :else        (absolutize old path))]
    (assoc env :cwd target :prev-cwd old)))

;; ============================================================================
;; Exit status
;; ============================================================================

(defn record-exit
  "Record exit code of the last-executed command."
  [env code]
  (assoc env :last-exit (int code)))

(defn record-bg-pid
  [env pid]
  (assoc env :last-bg-pid pid))

(defn track-bg-proc
  "Add a process reference to the bg-procs list (for `wait`,
   cleanup, etc.)."
  [env proc]
  (update env :bg-procs (fnil conj []) proc))

;; ============================================================================
;; Positional parameters (function calls)
;; ============================================================================

(defn with-pos-args
  "Return a new env with positional params replaced by `args`. Used
   when entering a function call. The caller is responsible for
   restoring the previous pos-args after the call returns."
  [env args]
  (assoc env :pos-args (vec args)))

(defn shift
  "POSIX `shift n` — drop the first n positional args."
  ([env] (shift env 1))
  ([env n]
   (update env :pos-args (fn [pa] (vec (drop n pa))))))

;; ============================================================================
;; Options (set -e, set -u, set -o pipefail, etc.)
;; ============================================================================

(defn set-option [env opt value]
  (assoc-in env [:options opt] (boolean value)))

(defn option [env opt]
  (get-in env [:options opt]))

;; ============================================================================
;; Functions
;; ============================================================================

(defn define-fn
  "Store a function definition (the body AST) under `name`."
  [env name body-stmt]
  (assoc-in env [:funcs name] body-stmt))

(defn lookup-fn
  [env name]
  (get-in env [:funcs name]))

(defn unset-fn
  [env name]
  (update env :funcs dissoc name))

;; ============================================================================
;; Scope stack (function-local variables)
;; ============================================================================

(defn push-scope
  "Push a new scope frame (called when entering a function)."
  [env]
  (update env :scope-stack (fnil conj []) {:captured-prev {}}))

(defn declare-local
  "Mark `name` as local to the current scope. Captures the prior
   value (or :unset) so it can be restored on `pop-scope`. If `value`
   is provided, also sets the var; else leaves it (still empty string
   on read until set)."
  ([env name] (declare-local env name nil))
  ([env name value]
   (let [stack (:scope-stack env)]
     (if (empty? stack)
       ;; No scope: behave like a regular assign.
       (if value (set-var env name value) env)
       (let [prev (if (contains? (:vars env) name)
                    (get-in env [:vars name])
                    :unset)
             frame (peek stack)
             frame' (assoc-in frame [:captured-prev name] prev)
             stack' (conj (pop stack) frame')
             env' (assoc env :scope-stack stack')]
         (if value
           (set-var env' name value)
           ;; declare without assignment — start fresh (empty string)
           (assoc-in env' [:vars name]
                     {:value "" :exported? false :readonly? false})))))))

(defn pop-scope
  "Pop the topmost scope frame, restoring any locally-shadowed
   variables to their prior values."
  [env]
  (let [stack (:scope-stack env)]
    (if (empty? stack)
      env
      (let [frame (peek stack)
            env' (reduce-kv
                  (fn [e name prev]
                    (if (= prev :unset)
                      (update e :vars dissoc name)
                      (assoc-in e [:vars name] prev)))
                  env
                  (:captured-prev frame))]
        (update env' :scope-stack pop)))))

;; ============================================================================
;; Scope forking
;; ============================================================================

(defn fork
  "Create a child env for subshell-like contexts (`(cmd)`, command
   substitution, backgrounded stmts). The child sees current state
   but its mutations don't leak.

   For now we just return the env unchanged — callers should treat
   the returned env as a fresh copy. If we add mutable state later
   (atoms, transient maps), fork would deep-copy them."
  [env]
  env)

;; ============================================================================
;; Process-env export — for spawning children via babashka.process
;; ============================================================================

(defn to-process-env
  "Build a string map of all exported variables, suitable for the
   `:extra-env` (additive) or `:env` (replacement) keys of
   `babashka.process/process`. Special variables (\\$? \\$$ etc.) are
   NOT exported; only declared vars with :exported? true."
  [env]
  (into {}
        (for [[name {:keys [value exported?]}] (:vars env)
              :when exported?]
          [name value])))
