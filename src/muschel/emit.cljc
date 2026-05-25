(ns muschel.emit
  "AST → Clojure form translator.

   Walks a parsed bash AST and emits a `(fn [env opts] ...)` that, when
   evaluated, runs the same program — but with bash control flow
   (if/for/while/&&/||/sequence) expressed as **native Clojure
   control flow**, and leaf work delegated to small muschel runtime
   helpers (`muschel.exec/run-argv`, `muschel.exec/exec-cmd`,
   `muschel.env/get-var`, `muschel.env/set-var`).

   Useful for:
   - **Inspection** — show the agent / user the Clojure equivalent of
     the bash they're about to run.
   - **AOT** — emit once at parse time, then `eval` the form to run; no
     AST walk on every execution.
   - **Embedding** — drop the emitted form into a Clojure program that
     already has its own env / host plumbing.

   Things that fundamentally can't be lowered (uncommon constructs we
   haven't taught the emitter yet) throw an `:muschel.emit/unsupported`
   ex-info. Things that lower to a runtime call but can't be inlined
   (pipes, redirects, command substitution, glob, eval) emit a call to
   `muschel.exec/exec-cmd` with the AST hoisted into a top-level let
   binding — the runtime still does the work, but the body of the
   translated fn stays readable.

   Usage:

       (require '[muschel.emit :as emit])
       (emit/translate \"echo hi | tr a-z A-Z\")
       (emit/translate-str src)   ;; → pretty-printed string"
  (:require [clojure.walk :as walk]
            #?(:clj  [clojure.pprint :as pprint]
               :cljs [cljs.pprint :as pprint])
            [muschel.parse :as parse]))

;; ============================================================================
;; AST cleanup — strip position metadata so quoted forms read nicely
;; ============================================================================

(def ^:private noise-keys
  #{:line :col :offset :end-line :end-col :end-offset})

(defn- strip-pos
  "Drop :line / :col / :offset / :end-* from every map in `ast`. The
   runtime doesn't need them; without stripping, every quoted AST in
   the output is a wall of position data."
  [ast]
  (walk/postwalk
   (fn [x] (if (map? x) (apply dissoc x noise-keys) x))
   ast))

;; ============================================================================
;; Deferred-AST collector
;; ============================================================================
;;
;; Constructs we can't inline (pipes, redirects, test brackets, case,
;; subshells, …) need their AST passed to `muschel.exec` at runtime.
;; Inlining the AST at every call site bloats the output. Instead we
;; gensym a name per unique AST and hoist all of them into a top-level
;; `let` that wraps the translated `fn`.

(def ^:private ^:dynamic *deferred-asts*
  "Atom of {symbol → stripped-ast}. Bound for the duration of one
   `translate` call. nil outside that scope."
  nil)

(defn- defer-ast!
  "Register `ast` for hoisting; return the symbol the body should
   reference. Re-uses the same gensym for structurally-equal ASTs."
  [ast]
  (let [clean (strip-pos ast)
        existing (some (fn [[sym a]] (when (= a clean) sym)) @*deferred-asts*)]
    (or existing
        (let [s (gensym "ast")]
          (swap! *deferred-asts* assoc s clean)
          s))))

;; ============================================================================
;; Errors
;; ============================================================================

(defn- unsupported!
  "Throw an ex-info that names the bash construct we don't know how to
   translate. Carries the AST node for downstream tooling to surface."
  [reason node]
  (throw (ex-info (str "muschel.emit: " reason)
                  {:type ::unsupported
                   :reason reason
                   :node (strip-pos node)})))

;; ============================================================================
;; Word emit
;; ============================================================================

(defn- emit-word-part [part]
  (case (:type part)
    :lit      (:value part)
    :var-ref  `(muschel.env/get-var ~'env ~(:name part))
    nil))

(defn- can-emit-part? [part]
  (some? (emit-word-part part)))

(defn- simple-word?
  "True if every part is :lit or :var-ref (fully inlineable)."
  [w]
  (every? can-emit-part? (:parts w)))

(defn- emit-simple-word
  "Inline expression for a word whose parts are all simple."
  [w]
  (let [exprs (mapv emit-word-part (:parts w))]
    (if (= 1 (count exprs))
      (first exprs)
      `(str ~@exprs))))

;; ============================================================================
;; Statement / command emit
;; ============================================================================

(declare emit-cmd emit-stmt)

(defn- emit-stmts
  "Emit a sequence of statements as an `as->` chain that threads `env`.
   Each statement returns a new env; the chain's value is the final
   env. Single-statement sequences emit the form directly with no
   threading wrapper."
  [stmts]
  (case (count stmts)
    0 'env
    1 (emit-stmt (first stmts))
    `(as-> ~'env ~'env
       ~@(mapv emit-stmt stmts))))

(defn- emit-call
  "A :call has assigns + args."
  [cmd]
  (let [args        (:args cmd)
        assigns     (:assigns cmd)
        all-simple? (every? simple-word? args)
        simple-vals? (every? #(simple-word? (:value %)) assigns)]
    (cond
      ;; Naked assignment(s): FOO=bar — permanent var set.
      (and (seq assigns) (empty? args) simple-vals?)
      (let [steps (for [{:keys [name value]} assigns]
                    `(muschel.env/set-var ~'env ~name ~(emit-simple-word value)))]
        (if (= 1 (count steps))
          (first steps)
          `(as-> ~'env ~'env ~@steps)))

      ;; Naked assignment with complex value → defer.
      (and (seq assigns) (empty? args))
      `(muschel.exec/exec-cmd ~'env ~(defer-ast! cmd) ~'opts)

      ;; Args, all simple, no prefix-assigns → inline argv.
      (and all-simple? (empty? assigns))
      `(muschel.exec/run-argv ~'env [~@(mapv emit-simple-word args)] ~'opts)

      ;; Args + simple prefix-assigns → inline extra-env.
      (and all-simple? simple-vals?)
      (let [extra (into {} (map (fn [{:keys [name value]}]
                                  [name (emit-simple-word value)])
                                assigns))]
        `(muschel.exec/run-argv ~'env
                                [~@(mapv emit-simple-word args)]
                                ~extra
                                ~'opts))

      :else
      ;; Some word needs runtime expansion → defer to keep env-threading
      ;; semantics correct (cmd-subst can mutate env).
      `(muschel.exec/exec-cmd ~'env ~(defer-ast! cmd) ~'opts))))

(defn- emit-cmd [cmd]
  (case (:type cmd)
    :call (emit-call cmd)

    :binary
    (case (:op cmd)
      :and
      `(let [~'env ~(emit-stmt (:left cmd))]
         (if (zero? (:last-exit ~'env))
           ~(emit-stmt (:right cmd))
           ~'env))

      :or
      `(let [~'env ~(emit-stmt (:left cmd))]
         (if (zero? (:last-exit ~'env))
           ~'env
           ~(emit-stmt (:right cmd))))

      ;; Pipes need real OS-level plumbing — defer.
      (:pipe :pipe-amp)
      `(muschel.exec/exec-cmd ~'env ~(defer-ast! cmd) ~'opts))

    :if
    `(let [~'env ~(emit-stmts (:cond cmd))]
       (if (zero? (:last-exit ~'env))
         ~(emit-stmts (:then cmd))
         ~(if (:else cmd) (emit-stmts (:else cmd)) 'env)))

    :for
    (let [words (:words cmd)
          var   (:var cmd)]
      (if (every? simple-word? words)
        `(reduce (fn [~'env x#]
                   (let [~'env (muschel.env/set-var ~'env ~var (str x#))]
                     ~(emit-stmts (:body cmd))))
                 ~'env
                 [~@(mapv emit-simple-word words)])
        `(muschel.exec/exec-cmd ~'env ~(defer-ast! cmd) ~'opts)))

    :while
    `(loop [~'env ~'env]
       (let [~'env ~(emit-stmts (:cond cmd))]
         (if (zero? (:last-exit ~'env))
           (recur ~(emit-stmts (:body cmd)))
           ~'env)))

    :until
    `(loop [~'env ~'env]
       (let [~'env ~(emit-stmts (:cond cmd))]
         (if (zero? (:last-exit ~'env))
           ~'env
           (recur ~(emit-stmts (:body cmd))))))

    (:test-bracket :test-double-bracket :arith-cmd :case :subshell
                   :brace-group :func-def :coproc)
    `(muschel.exec/exec-cmd ~'env ~(defer-ast! cmd) ~'opts)

    (unsupported! (str "cmd type " (:type cmd) " not handled") cmd)))

(defn- emit-stmt
  "Translate a single :stmt. If the stmt carries redirs or runs in
   background, defer the whole thing to exec — those need real
   file/process plumbing through the host."
  [stmt]
  (cond
    (or (:bg? stmt) (seq (:redirs stmt)))
    `(muschel.exec/exec-stmt ~'env ~(defer-ast! stmt) ~'opts)

    (:neg? stmt)
    `(let [~'env ~(emit-cmd (:cmd stmt))]
       (assoc ~'env :last-exit (if (zero? (:last-exit ~'env)) 1 0)))

    :else
    (emit-cmd (:cmd stmt))))

;; ============================================================================
;; Public surface
;; ============================================================================

(defn translate
  "Translate a bash source string (or pre-parsed AST) into a Clojure
   form that, when eval'd to a function `(fn [env opts] ...)` and
   called, runs the program.

   The returned fn installs muschel's cmd-subst / arith expansion
   handlers into `opts` before running, so deferred calls into
   `muschel.exec` can expand inner commands correctly.

   ASTs that can't be inlined (pipes, redirects, test brackets, etc.)
   are hoisted into a top-level `let` binding so the body of the fn
   stays readable.

   Throws `:muschel.emit/unsupported` for AST shapes we don't handle."
  [src-or-ast]
  (binding [*deferred-asts* (atom {})]
    (let [{:keys [stmts]} (if (string? src-or-ast)
                            (parse/parse src-or-ast)
                            src-or-ast)
          body            (emit-stmts stmts)
          fn-form         `(fn [~'env ~'opts]
                             (let [~'opts (merge ~'opts
                                                 (muschel.exec/expand-opts ~'opts))]
                               ~body))
          deferred        @*deferred-asts*]
      (if (seq deferred)
        `(let [~@(mapcat (fn [[sym ast]] [sym `(quote ~ast)])
                         (sort-by key deferred))]
           ~fn-form)
        fn-form))))

(defn translate-str
  "Like `translate` but returns a pretty-printed string."
  [src-or-ast]
  (with-out-str (pprint/pprint (translate src-or-ast))))
