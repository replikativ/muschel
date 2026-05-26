(ns muschel.js-api
  "JavaScript / TypeScript bindings for muschel. Compiled to an npm
   package (see `:npm` target in shadow-cljs.edn and `npm-package/`).

   Mirrors `muschel.core` operation-for-operation. Naming rule:
   Clojure kebab-case → JS camelCase; `?`-suffix dropped.

   ## Quick start

     const m = require('muschel');

     // Sandboxed run — virtual FS, no real disk
     const host = m.browserHost({
       files: { '/README.md': '# muschel\\n' }
     });
     const r = m.run('cat /README.md', { host, trace: true });
     // r = { exit, stdout, stderr, env, session, trace: { tools, fs, … } }

   ## Async

   Every exported function is **synchronous** (CLJS compiles to
   straight JS). No Promises. Suits the agent-control-loop use case
   where the caller orchestrates concurrency itself."
  (:require [clojure.string :as str]
            [muschel.budget :as budget]
            [muschel.env :as env]
            [muschel.exec :as exec]
            [muschel.fs :as fs]
            [muschel.fs.virtual :as fs.virtual]
            [muschel.host :as host]
            [muschel.host.browser :as host.browser]
            [muschel.host.node :as host.node]
            [muschel.parse :as parse]
            [muschel.permit :as permit]
            [muschel.session :as session]))

;; ============================================================================
;; Conversion helpers
;; ============================================================================

(defn- ->js [x] (clj->js x :keyword-fn name))

(defn- js-opts->clj
  "Coerce a JS opts object into a Clojure map with kebab-case keys.
   Recognises the camelCase keys we use everywhere."
  [js-opts]
  (when js-opts
    (let [m (js->clj js-opts :keywordize-keys true)
          renames {:posArgs           :pos-args
                   :hostEnv           :host-env?
                   :timeoutMs         :timeout-ms
                   :interruptFn       :interrupt-fn
                   :onTool            :on-tool
                   :onFs              :on-fs
                   :onDeny            :on-deny
                   :fallbackHost      :fallback-host
                   :fallbackAllowlist :fallback-allowlist
                   :maxBytes          :max-bytes}]
      (reduce-kv (fn [acc k v]
                   (let [k' (renames k k)]
                     (-> acc (dissoc k) (assoc k' v))))
                 m m))))

(defn- ->clj-tool-fn [js-fn]
  ;; JS tool fn: (argv, stdin, env) → { stdout, stderr, exit }
  (fn [argv stdin env-map]
    (let [r (js-fn (clj->js argv) stdin (clj->js env-map))]
      {:stdout (or (aget r "stdout") "")
       :stderr (or (aget r "stderr") "")
       :exit   (or (aget r "exit") 0)})))

;; ============================================================================
;; Parsing + permit
;; ============================================================================

(defn ^:export parse
  "Parse a bash source string into an AST. Returns a JS object."
  [src]
  (try (->js (parse/parse src))
       (catch :default e
         (doto #js {}
           (aset "error" (.-message e))
           (aset "data"  (clj->js (ex-data e)))))))

(defn ^:export check
  "Run the parse-time permit check.
   `opts.rulesets` overrides; `opts.prompter` defaults to deny-all."
  ([src] (check src #js {}))
  ([src opts]
   (let [ast      (parse/parse src)
         opts-clj (js-opts->clj opts)
         rulesets (or (:rulesets opts-clj) [permit/default-rules])
         prompter (or (:prompter opts-clj) permit/deny-all-prompter)
         res      (permit/check {:ast ast :rulesets rulesets :prompter prompter})]
     (->js res))))

(def ^:export defaultRules
  "The shipped default ruleset (read-only POSIX allow + denylist)."
  (clj->js permit/default-rules :keyword-fn name))

(def ^:export denyAllPrompter permit/deny-all-prompter)
(def ^:export allowAllPrompter permit/allow-all-prompter)

;; ============================================================================
;; Env + session
;; ============================================================================

(defn ^:export newEnv
  "Build a fresh shell env. Default does NOT inherit the JS process env.
   opts: { cwd, posArgs, script, hostEnv, vars }."
  ([] (newEnv #js {}))
  ([opts]
   (let [o (js-opts->clj opts)]
     (env/new-env :cwd (:cwd o)
                  :pos-args (or (:pos-args o) [])
                  :script (or (:script o) "bash")
                  :host-env? (boolean (:host-env? o))
                  :vars (or (:vars o) {})))))

(defn ^:export getVar
  "Read a shell variable. Returns the string value or '' if unset."
  [env name]
  (env/get-var env name))

(defn ^:export setVar
  "Set a shell variable. Returns a new env."
  [env name value]
  (env/set-var env name value))

(defn ^:export atomSession
  "Create a stateful session that threads env across run() calls."
  ([] (session/atom-session (env/new-env)))
  ([env] (session/atom-session env)))

(defn ^:export sessionCwd
  "Read the current working directory from a session."
  [sess] (:cwd (session/-env sess)))

;; ============================================================================
;; Hosts
;; ============================================================================

(defn ^:export nodeHost
  "Create a Node.js host. **Unsandboxed by default** — uses real
   child_process + real fs. For a contained Node run today, use
   `browserHost({ files, tools })` (it works on Node too). A real-
   disk sandboxed Node host is a follow-up (requires CLJS ports of
   DiskFS + BuiltinHost)."
  ([] (host.node/make))
  ([_opts] (host.node/make)))

(defn ^:export browserHost
  "Create a sandboxed in-memory host. Works in browser AND Node.

   opts: { tools: {name: fn}, files: {path: content},
           includeStock: bool (default true) }

   By default we merge in `stockTools()` (cat, wc, grep, head) so a
   plain `m.browserHost({ files: ... })` can `cat | wc -l` without
   the caller registering tools. Pass `includeStock: false` to opt
   out and supply only your own."
  [opts]
  (let [include-stock? (if (and opts (some? (aget opts "includeStock")))
                         (aget opts "includeStock") true)
        stock (when include-stock?
                (into {}
                      (for [[k v] (host.browser/stock-tools)]
                        [k v])))
        tools-js (when opts (aget opts "tools"))
        user-tools (when tools-js
                     (into {}
                           (for [k (js-keys tools-js)]
                             [k (->clj-tool-fn (aget tools-js k))])))
        tools (merge stock user-tools)
        files (or (some-> (and opts (aget opts "files")) js->clj) {})]
    (host.browser/make :tools tools :files files)))

(defn ^:export virtualFS
  "Construct a VirtualFS from `{path → content}`. Options: { cwd }."
  ([] (virtualFS #js {} #js {}))
  ([files] (virtualFS files #js {}))
  ([files opts]
   (let [files-clj (or (js->clj files) {})
         o (js-opts->clj opts)]
     (fs.virtual/make files-clj (select-keys o [:cwd])))))

(defn ^:export stockTools
  "Returns the stock tools (cat / wc / grep / head) as a JS object
   spreadable into `browserHost({ tools: { …m.stockTools(), … } })`."
  []
  (let [m (host.browser/stock-tools)
        out #js {}]
    (doseq [[k clj-fn] m]
      (aset out k
            (fn [argv stdin env]
              (let [argv-clj (vec (js->clj argv))
                    env-clj  (js->clj env)
                    r        (clj-fn argv-clj (str stdin) env-clj)
                    js-r     #js {}]
                (aset js-r "stdout" (or (:stdout r) ""))
                (aset js-r "stderr" (or (:stderr r) ""))
                (aset js-r "exit"   (or (:exit r) 0))
                js-r))))
    out))

;; ============================================================================
;; Filesystem (programmatic). Works against a VirtualFS handle (built
;; via `m.virtualFS({...})`). BrowserHost has its own internal vfs
;; that is NOT the muschel.fs protocol surface yet — porting it is a
;; separate refactor. For now, callers who need post-run FS access
;; build a VFS and reuse it across runs, threading new state in via
;; fresh `browserHost({files: ...})` constructions.
;; ============================================================================

(def ^:export fs
  (let [obj #js {}]
    (aset obj "readFile"          (fn [f path] (fs/read-file f path)))
    (aset obj "listDir"           (fn [f path] (->js (fs/list-dir f path))))
    (aset obj "exists"            (fn [f path] (boolean (fs/exists? f path))))
    (aset obj "stat"              (fn [f path] (->js (fs/stat f path))))
    (aset obj "mkdir"             (fn [f path] (boolean (fs/mkdir f path))))
    (aset obj "delete"            (fn [f path] (boolean (fs/delete f path))))
    (aset obj "rename"            (fn [f from to] (boolean (fs/rename f from to))))
    (aset obj "touch"             (fn [f path] (boolean (fs/touch f path))))
    (aset obj "chmod"             (fn [f path mode] (boolean (fs/chmod f path mode))))
    (aset obj "symlink"           (fn [f target link] (boolean (fs/symlink f target link))))
    (aset obj "sandboxRelativize" (fn [f p] (fs/sandbox-relativize f p)))
    (aset obj "cwd"               (fn [f] (fs/cwd f)))
    obj))

;; ============================================================================
;; Resource budgets
;; ============================================================================

(def ^:export budget
  (let [obj #js {}]
    (aset obj "deadlineInterrupt" (fn [ms] (budget/deadline-interrupt ms)))
    (aset obj "stepInterrupt"     (fn [n]  (budget/step-interrupt n)))
    (aset obj "combine"           (fn [& fns] (apply budget/combine fns)))
    (aset obj "budgetExceeded"    (fn [e]  (budget/budget-exceeded? e)))
    obj))

;; ============================================================================
;; Run
;; ============================================================================

(defn- ->trace-opt
  "Translate a JS trace value into a Clojure shape. `true` → default
   state; an object may carry { cap, onTool, onFs, onDeny }."
  [v]
  (cond
    (or (nil? v) (false? v) (undefined? v)) nil
    (true? v) true
    (object? v)
    (let [m (js-opts->clj v)]
      (cond-> {}
        (:cap     m) (assoc :cap     (:cap m))
        (:on-tool m) (assoc :on-tool (:on-tool m))
        (:on-fs   m) (assoc :on-fs   (:on-fs m))
        (:on-deny m) (assoc :on-deny (:on-deny m))))
    :else true))

(defn ^:export run
  "Execute a bash source string and capture stdout/stderr.

   opts: { host, session, env, permit, timeoutMs, interruptFn,
           trace, in }

   Returns: { exit, stdout, stderr, env, session, permit?, trace? }"
  ([src] (run src #js {}))
  ([src js-opts]
   (let [opts-clj    (js-opts->clj js-opts)
         host-obj    (or (:host opts-clj) (host.browser/make))
         sess        (:session opts-clj)
         in          (:in opts-clj)
         initial-env (or (:env opts-clj)
                         (some-> sess session/-env)
                         (env/new-env))
         opts (cond-> {:host host-obj}
                sess              (assoc :session sess)
                in                (assoc :in in)
                (:timeout-ms opts-clj)   (assoc :timeout-ms (:timeout-ms opts-clj))
                (:interrupt-fn opts-clj) (assoc :interrupt-fn (:interrupt-fn opts-clj))
                (:permit opts-clj)       (assoc :permit (:permit opts-clj))
                (some? (aget js-opts "trace")) (assoc :trace (->trace-opt (aget js-opts "trace"))))
         result (exec/run-and-capture initial-env src opts)
         out #js {}]
     (aset out "stdout"  (:stdout result))
     (aset out "stderr"  (:stderr result))
     (aset out "exit"    (:exit result))
     (aset out "env"     (->js (:env result)))
     (aset out "session" (:session result))
     (when-let [p (:permit result)] (aset out "permit" (->js p)))
     (when-let [t (:trace result)]  (aset out "trace"  (->js t)))
     out)))
