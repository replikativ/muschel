(ns muschel.js-api
  "JavaScript / TypeScript bindings for muschel. Compiled to an npm
   package (see `:npm` target in shadow-cljs.edn).

   Surface:

     const m = require('muschel');

     // Pure analysis (parse + permit decision)
     const ast    = m.parse(\"git status | head\");
     const result = m.check(ast);             // → { decision, perCall, ... }

     // Execution
     const sess = m.session();                // optional, for stateful runs
     const r = m.run(\"echo hi\",
                     { host: m.nodeHost(), session: sess });
     // r = { stdout, stderr, exit, env, session }

     // Browser context with registered virtual tools
     const host = m.browserHost({
       tools: {
         git: (argv, stdin, env) => ({ stdout: '...', exit: 0 })
       },
       files: { '/README.md': '# hi' }
     });
     m.run('cat /README.md', { host });"
  (:require [clojure.string :as str]
            [muschel.env :as env]
            [muschel.exec :as exec]
            [muschel.host :as host]
            [muschel.host.browser :as browser]
            [muschel.host.node :as node]
            [muschel.parse :as parse]
            [muschel.permit :as permit]
            [muschel.session :as session]))

;; ============================================================================
;; Conversion helpers
;; ============================================================================

(defn- ->js
  "Convert a Clojure value to a JS-friendly one. AST nodes and other
   maps keep their keyword keys converted to JS strings; we do
   recursive clj->js with :keyword-fn name."
  [x]
  (clj->js x :keyword-fn name))

(defn- ->clj-tool-fn [js-fn]
  ;; A JS tool function takes (argv, stdin, env) and returns
  ;; {stdout, stderr, exit}. We adapt it to muschel's clj fn shape.
  (fn [argv stdin env-map]
    (let [r (js-fn (clj->js argv) stdin (clj->js env-map))]
      {:stdout (or (aget r "stdout") "")
       :stderr (or (aget r "stderr") "")
       :exit   (or (aget r "exit") 0)})))

;; ============================================================================
;; Parsing + permit
;; ============================================================================

(defn ^:export parse
  "Parse a bash source string into a Clojure AST map. Returns a
   JS object."
  [src]
  (try (->js (parse/parse src))
       (catch :default e
         (doto #js {}
           (aset "error" (.-message e))
           (aset "data" (clj->js (ex-data e)))))))

(defn ^:export check
  "Run the parse-time permit check against the default ruleset. If
   `opts.rulesets` is provided, use those instead. Returns
   `{decision, perCall, newRules}`."
  ([src] (check src #js {}))
  ([src opts]
   (let [ast (parse/parse src)
         rulesets (or (some-> (aget opts "rulesets") js->clj)
                      [permit/default-rules])
         res (permit/check {:ast ast
                            :rulesets rulesets
                            :prompter permit/deny-all-prompter})]
     (->js res))))

;; ============================================================================
;; Hosts
;; ============================================================================

(defn ^:export nodeHost
  "Create a Node.js-backed host. Uses child_process for spawn and fs
   for file I/O."
  []
  (node/make))

(defn ^:export browserHost
  "Create a browser-backed host with optional tool registry and
   pre-seeded virtual fs. `opts` is `{ tools: {name: fn}, files: {path: content} }`."
  [opts]
  (let [tools-js (aget opts "tools")
        tools (when tools-js
                (into {}
                      (for [k (js-keys tools-js)]
                        [k (->clj-tool-fn (aget tools-js k))])))
        files (or (some-> (aget opts "files") js->clj) {})]
    (browser/make :tools (or tools {}) :files files)))

(defn ^:export stockTools
  "Returns the default tool map (cat/wc/grep/head) as a plain JS
   object whose values are JS-callable functions matching the
   `ToolFn` shape. Spreadable into `browserHost({ tools: ... })`."
  []
  (let [m (browser/stock-tools)
        out #js {}]
    (doseq [[k clj-fn] m]
      (aset out k
            (fn [argv stdin env]
              ;; Convert JS args back to Clojure shape, call the
              ;; underlying clj fn, then convert the result back.
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
;; Session
;; ============================================================================

(defn ^:export session
  "Create a new AtomSession to thread state across multiple `run`
   calls (so `cd`, var assignments, bg jobs persist)."
  []
  (session/atom-session (env/new-env)))

;; ============================================================================
;; Run
;; ============================================================================

(defn ^:export run
  "Execute `src` against the given host (and optional session).
   Returns `{stdout, stderr, exit, env, session}` as a JS object.

   `opts`:
     host    — required for cljs
     session — optional, for stateful exec across calls
     env     — optional starting env (default: new-env from session if set)
     pos     — optional positional params (vector of strings)"
  [src opts]
  (let [host-obj (aget opts "host")
        session-obj (aget opts "session")
        initial-env (or (some-> session-obj session/-env)
                        (env/new-env))
        opts-clj (cond-> {:host host-obj}
                   session-obj (assoc :session session-obj))
        result (exec/run-and-capture initial-env src opts-clj)
        out #js {}]
    (aset out "stdout"  (:stdout result))
    (aset out "stderr"  (:stderr result))
    (aset out "exit"    (:exit result))
    (aset out "env"     (->js (:env result)))
    (aset out "session" (:session result))
    out))

(defn ^:export setVar
  "Convenience: set a shell variable on a session."
  [sess name value]
  (session/-swap-env! sess #(env/set-var % name value))
  sess)

(defn ^:export getVar
  "Convenience: read a shell variable from a session."
  [sess name]
  (env/get-var (session/-env sess) name))

(defn ^:export cwd
  "Read the current working directory from a session."
  [sess]
  (:cwd (session/-env sess)))
