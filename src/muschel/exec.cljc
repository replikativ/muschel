(ns muschel.exec
  "Execute a muschel AST against an env.

   The executor is the moral counterpart of `mvdan/sh/interp.Runner`.
   Where mvdan keeps streams + env mutable on the Runner struct, we
   thread the env value explicitly and pass stdin/stdout/stderr as
   exec-options. External commands run through `babashka.process`.

   ## API

       (run env program)
         → {:env env' :exit int}
       (exec-stmt env stmt opts)
         → env'   ; with :last-exit updated
       (run-and-capture env program)
         → {:env env' :exit int :stdout str :stderr str}

   `opts` may include:
     :in   — InputStream  (default: System/in)
     :out  — OutputStream (default: System/out)
     :err  — OutputStream (default: System/err)

   ## Layering

   `exec` requires `expand`, but `expand` only calls back into `exec`
   via the `:cmd-subst` and `:arith` config functions threaded through
   `expand-opts`. This breaks the recursive dependency cleanly."
  (:require [clojure.string :as str]
            #?(:cljs [goog.string])
            #?(:cljs [goog.string.format])
            [muschel.arith :as arith]
            [muschel.ast :as ast]
            [muschel.budget :as budget]
            [muschel.env :as env]
            [muschel.errors :as err]
            [muschel.expand :as expand]
            [muschel.fs :as mfs]
            [muschel.fs.traced :as fs.traced]
            [muschel.host :as host]
            #?(:clj [muschel.host.jvm :as host.jvm])
            [muschel.parse :as parse]
            [muschel.permit :as permit]
            [muschel.runtime :as rt]
            [muschel.session :as session]
            [muschel.trace :as trace]))

(defn- fmt
  "Portable format: cljs uses goog.string/format."
  [fmt-str & args]
  #?(:clj  (apply format fmt-str args)
     :cljs (apply goog.string/format fmt-str args)))

(defn- default-host []
  #?(:clj  (host.jvm/make)
     :cljs (throw (ex-info "muschel.exec on cljs requires :host in opts"
                           {:type ::no-host}))))

(defn- parse-int*
  ([s] (parse-int* s 10))
  ([s base]
   #?(:clj  (Long/parseLong (str s) (long base))
      :cljs (let [n (js/parseInt s base)]
              (if (js/isNaN n)
                (throw (ex-info (str "not a number: " s) {}))
                n)))))

(defn- try-parse-int [s]
  (try (parse-int* s)
       (catch #?(:clj Exception :cljs :default) _ nil)))

;; ============================================================================
;; Forward declarations
;; ============================================================================

(declare exec-stmt exec-cmd run-call run-pipeline run-and-or
         exec-stmts run-builtin)

;; ============================================================================
;; Expansion config — closes the circular dep with expand.cljc
;; ============================================================================

(defn expand-opts
  "Build the option map passed to `muschel.expand` so it can call back
   into us for command substitution and arithmetic.

   IMPORTANT: cmd-subst recursion threads `:permit` through to the
   nested exec so the runtime permit hook fires for inner commands
   too. Without this, `echo $(rm -rf /)` would bypass permit because
   the outer `echo` is the only command visible to the static AST.

   Public so emitted Clojure (`muschel.emit/translate`) can install
   the same expansion handlers before invoking the translated form."
  [base-opts]
  (let [h (:host base-opts)]
    {:cmd-subst
     (fn [env body]
       (let [;; bash quirk: $(< file) reads the file directly (no
             ;; fork). Only matches when the WHOLE body is `< file`
             ;; with no trailing command — anything more complex
             ;; falls through to normal exec.
             trimmed (str/trim (or body ""))
             file-read-match (when (str/starts-with? trimmed "<")
                               (let [rest (str/trim (subs trimmed 1))]
                                 (when (and (not (empty? rest))
                                            (not (re-find #"[\s;&|()\n]" rest)))
                                   rest)))]
         (cond
           file-read-match
           (let [path file-read-match
                 abs (if (str/starts-with? path "/")
                       path
                       (str (:cwd env) "/" path))]
             (try
               (let [content (host/read-file h abs)]
                 [env (str/replace content #"\n+$" "")])
               (catch #?(:clj Throwable :cljs :default) _
                 [(env/record-exit env 1) ""])))

           :else
           ;; bash isolates cmd-subst into a subshell: env mutations
           ;; (cd, var assigns, etc.) don't leak back to the parent.
           ;; Only :last-exit propagates.
           (let [ast (parse/parse body)
                 sb-out (host/string-sink h)
                 nested-opts (assoc base-opts :out sb-out)
                 child (env/fork env)
                 env' (exec-stmts child (:stmts ast) nested-opts)]
             [(env/record-exit env (:last-exit env'))
              (host/sink->string h sb-out)]))))
     :arith
     (fn [env expr]
       ;; Arith errors (div by zero, bad parse) surface as bash
       ;; runtime errors — print to stderr, return 0, mark exit 1.
       (try
         (let [[env' v] (arith/evaluate env expr)]
           [env' v])
         (catch #?(:clj Throwable :cljs :default) e
           (host/write-string! h (:err base-opts)
                               (str (or (.getMessage e) (str e)) "\n"))
           [(env/record-exit env 1) 0])))}))

(defn- expand-words [env words opts]
  (expand/expand-words env words
                       :cmd-subst (:cmd-subst opts)
                       :arith (:arith opts)))

(defn- expand-assign [env word opts]
  (expand/expand-assign-value env word
                              :cmd-subst (:cmd-subst opts)
                              :arith (:arith opts)))

(defn- expand-target [env word opts]
  ;; Redirect targets: bash treats these as a single field even if
  ;; multiple would be produced (no field splitting); but we apply
  ;; tilde + var + cmd-subst expansion.
  (let [[env' v] (expand-assign env word opts)]
    [env' v]))

;; ============================================================================
;; Redirections: open files / wire heredocs into the opts map
;; ============================================================================

(defn- resolve-path
  "Resolve `path` against env's :cwd if relative. Pure-string op."
  [env ^String path]
  (host/resolve-path (:cwd env) path))

(defn- open-output [h env path append?]
  (let [resolved (resolve-path env path)
        sink     (try (host/open-file-sink h resolved append?)
                      (catch #?(:clj Throwable :cljs :default) t
                        (trace/record-fs! (:trace env)
                                          {:type :fs :op :open-sink
                                           :path resolved :ok? false})
                        (throw t)))]
    (trace/record-fs! (:trace env)
                      {:type :fs :op :open-sink :path resolved :ok? (some? sink)})
    sink))

(defn- open-input [h env path]
  (let [resolved (resolve-path env path)
        source   (try (host/open-file-source h resolved)
                      (catch #?(:clj Throwable :cljs :default) t
                        (trace/record-fs! (:trace env)
                                          {:type :fs :op :open-source
                                           :path resolved :ok? false})
                        (throw t)))]
    (trace/record-fs! (:trace env)
                      {:type :fs :op :open-source :path resolved :ok? (some? source)})
    source))

(defn- apply-redir
  "Returns [env' new-opts closer-fn]. The closer-fn closes whatever we
   opened so the caller can defer-clean. All platform-specific
   stream creation goes via host."
  [env redir opts]
  (let [h (:host opts)
        opener (fn [path append?] (open-output h env path append?))
        ;; Helper: assoc into opts AND register a closer for the
        ;; allocated stream.
        ]
    (case (:type redir)
      :heredoc
      ;; The heredoc body is already in :body. Wrap as a host source
      ;; and supply as stdin.
      (let [stream (host/string-source h (str (:body redir)))]
        [env (assoc opts :in stream) #(host/close! h stream)])

      :redir
      (let [{:keys [op fd target]} redir
            close-fn (fn [s] #(host/close! h s))]
        (case op
          :out
          (let [[env' path] (expand-target env target opts)
                s (opener path false)]
            [env' (assoc opts (if (= fd 2) :err :out) s) (close-fn s)])

          :append
          (let [[env' path] (expand-target env target opts)
                s (opener path true)]
            [env' (assoc opts (if (= fd 2) :err :out) s) (close-fn s)])

          :err
          (let [[env' path] (expand-target env target opts)
                s (opener path false)]
            [env' (assoc opts :err s) (close-fn s)])

          :err-append
          (let [[env' path] (expand-target env target opts)
                s (opener path true)]
            [env' (assoc opts :err s) (close-fn s)])

          :in
          (let [[env' path] (expand-target env target opts)
                s (open-input h env path)]
            [env' (assoc opts :in s) (close-fn s)])

          :all
          (let [[env' path] (expand-target env target opts)
                s (opener path false)]
            [env' (assoc opts :out s :err s) (close-fn s)])

          :all-append
          (let [[env' path] (expand-target env target opts)
                s (opener path true)]
            [env' (assoc opts :out s :err s) (close-fn s)])

          :dup-out
          (let [[env' n] (expand-target env target opts)
                src-stream (case n
                             "1" (:out opts)
                             "2" (:err opts)
                             nil)
                dst-key (if (= fd 2) :err :out)]
            [env' (if src-stream (assoc opts dst-key src-stream) opts) (fn [])])

          :dup-in
          (let [[env' n] (expand-target env target opts)]
            (if (= n "-")
              [env' (assoc opts :in nil) (fn [])]
              [env' opts (fn [])]))

          :here-string
          (let [[env' s] (expand-target env target opts)
                stream (host/string-source h (str s "\n"))]
            [env' (assoc opts :in stream) #(host/close! h stream)])

          [env opts (fn [])])))))

(defn- apply-redirs
  "Apply a sequence of redirections, returning [env' opts' close!-fn]
   where close!-fn closes everything in reverse order.

   Any redirect that fails (target outside FS root, unreadable file, …)
   writes a bash-style \"line N: cmd: file: msg\" line to :err on
   `opts`, marks env as `:redir-failed?` so the caller can short-circuit
   the inner command and exit 1, and returns the original env / opts."
  [env redirs opts]
  (let [closers (volatile! [])
        failure (volatile! nil)
        [env' opts']
        (reduce (fn [[env opts] r]
                  (if @failure
                    [env opts]
                    (try
                      (let [[env' opts' c] (apply-redir env r opts)]
                        (vswap! closers conj c)
                        [env' opts'])
                      (catch #?(:clj Throwable :cljs :default) e
                        (vreset! failure e)
                        (host/write-string! (:host opts) (:err opts)
                                            (str "muschel: redirect: "
                                                 #?(:clj (.getMessage ^Throwable e)
                                                    :cljs (.-message e))
                                                 "\n"))
                        [env opts]))))
                [env opts]
                redirs)
        env' (cond-> env' @failure (assoc :redir-failed? true))]
    [env' opts' (fn [] (doseq [c (reverse @closers)]
                         (try (c) (catch #?(:clj Throwable :cljs :default) _ nil))))]))

;; ============================================================================
;; Builtins
;; ============================================================================

(defn- write-line
  "Write `s` to opts's `which` sink (`:out` or `:err`) via the host.
   No-op if the sink is missing. The name is historic — there's no
   trailing newline added; callers include it."
  [opts which s]
  (host/write-string! (:host opts) (get opts which) s))

(defn- builtin-cd
  [env args opts]
  (cond
    (> (count args) 1)
    (do (write-line opts :err "usage: cd [dir]\n")
        (env/record-exit env 2))

    ;; bash: `cd ''` errors out explicitly (not the same as bare `cd`,
    ;; which goes to HOME). The arg was explicitly empty.
    (and (seq args) (= "" (first args)))
    (do (write-line opts :err "cd: empty directory path\n")
        (env/record-exit env 1))

    :else
    (let [path (first args)]
      (try
        (let [target (if (nil? path)
                       (env/get-var env "HOME")
                       path)]
          (cond
            (or (nil? target) (= "" target))
            (do (write-line opts :err "cd: HOME not set\n")
                (env/record-exit env 1))

            :else
            (let [env' (env/cd env target)
                  fi (host/file-info (:host opts) (:cwd env'))]
              (cond
                ;; mvdan/sh's cd uses "no such file or directory" for
                ;; BOTH missing paths and non-directory paths (matches
                ;; what bash on Linux often prints via the chdir(2)
                ;; ENOENT/ENOTDIR -> "no such file or directory" line).
                (or (not (:exists? fi)) (not (:dir? fi)))
                (do (write-line opts :err
                                (str "cd: no such file or directory: \""
                                     target "\"\n"))
                    (env/record-exit env 1))
                :else (env/record-exit env' 0)))))
        (catch #?(:clj Throwable :cljs :default) e
          (write-line opts :err (str "cd: " (.getMessage e) "\n"))
          (env/record-exit env 1))))))

(defn- builtin-pwd
  "POSIX `pwd [-L|-P]`. We don't distinguish logical vs physical paths
   (no symlink resolution), but we still reject unknown flags. The
   printed cwd is sandbox-relative when the host carries a sandbox-
   aware FS so the host mount prefix doesn't leak."
  [env args opts]
  (if-let [bad (some #(when (and (str/starts-with? % "-")
                                 (not (re-matches #"-[LP]+" %))) %)
                     args)]
    (do (write-line opts :err (str "invalid option: \"" bad "\"\n"))
        (env/record-exit env 2))
    (let [raw (:cwd env)
          shown (if-let [fs (:fs env)]
                  (mfs/sandbox-relativize fs raw)
                  raw)]
      (write-line opts :out (str shown "\n"))
      (env/record-exit env 0))))

(defn- builtin-echo
  "bash `echo [-neE] [arg ...]`. Flags only count if they appear at
   the start AND match `^-[neE]+$` (a single non-flag-arg locks the
   rest as words, matching bash behavior)."
  [env args opts]
  (let [flag-re #"^-[neE]+$"
        ;; Consume leading flag args.
        [flag-args rest-args]
        (loop [acc [] a args]
          (if (and (seq a) (re-matches flag-re (first a)))
            (recur (conj acc (first a)) (rest a))
            [acc a]))
        flags (str/join "" (map #(subs % 1) flag-args))
        no-newline? (str/includes? flags "n")
        interpret-escapes? (str/includes? flags "e")
        s (str/join " " rest-args)
        s (if interpret-escapes? (expand/decode-ansi-c s) s)]
    (write-line opts :out (if no-newline? s (str s "\n")))
    (env/record-exit env 0)))

(defn- builtin-true  [env _ _]  (env/record-exit env 0))
(defn- builtin-false [env _ _]  (env/record-exit env 1))
(defn- builtin-colon [env _ _]  (env/record-exit env 0))

(defn- builtin-export
  [env args _opts]
  (reduce (fn [env arg]
            (let [eq (.indexOf ^String arg "=")]
              (if (neg? eq)
                (env/export env arg)
                (env/export env (subs arg 0 eq) (subs arg (inc eq))))))
          (env/record-exit env 0)
          args))

(defn- builtin-unset
  "`unset [-f] [-v] NAME ...` — remove variables (-v, default) or
   functions (-f)."
  [env args _opts]
  (let [[flags names] (split-with #(str/starts-with? % "-") args)
        flag-str (apply str (map #(subs % 1) flags))
        fn?  (str/includes? flag-str "f")]
    (env/record-exit
     (reduce (fn [e n]
               (if fn?
                 (update e :funcs dissoc n)
                 (env/unset-var e n)))
             env names)
     0)))

(def ^:private set-short-flags #{\e \u \x \f \n \a \v \m \E \B \H \C \P \T})
(def ^:private set-long-options
  #{"errexit" "nounset" "xtrace" "noglob" "noexec" "allexport"
    "pipefail" "verbose" "history" "monitor" "physical"})

(defn- builtin-set
  [env args opts]
  ;; `set -e/-u/-x/+e/+u/+x` toggle options. A bare `--` ends option
  ;; parsing and causes everything after it (which may be empty) to
  ;; REPLACE the positional params — `set --` clears them.
  ;; Unknown options error out.
  (let [arg-vec (vec args)
        sep-ix  (some (fn [[i a]] (when (= "--" a) i))
                      (map-indexed vector arg-vec))
        ;; Build the opt-arg slice. Without `--`, take leading
        ;; `-`/`+`-prefixed args AND consume one extra arg after any
        ;; `-o`/`+o` even if it doesn't start with -/+ (it's the
        ;; long-option name like `pipefail`).
        opt-args (if sep-ix
                   (subvec arg-vec 0 sep-ix)
                   (loop [i 0 acc []]
                     (let [a (when (< i (count arg-vec)) (nth arg-vec i))]
                       (cond
                         (nil? a) acc
                         (or (str/starts-with? a "-") (str/starts-with? a "+"))
                         (if (and (> (count a) 1)
                                  (some #(= \o %) (rest a))
                                  (< (inc i) (count arg-vec)))
                           (recur (+ i 2) (conj acc a (nth arg-vec (inc i))))
                           (recur (inc i) (conj acc a)))
                         :else acc))))
        pos-args (when (or sep-ix (< (count opt-args) (count arg-vec)))
                   (if sep-ix
                     (subvec arg-vec (inc sep-ix))
                     (subvec arg-vec (count opt-args))))
        ;; Walk opt-args once: split combined flags into ["-abc"] →
        ;; chars [a b c]; when we see `o`, consume the next opt-arg
        ;; as the long-option name. Track bad chars + bad long opts.
        {bad-flag :bad-flag bad-long :bad-long opt-pairs :opt-pairs
         leftover :leftover}
        (loop [in opt-args
               st {:bad-flag nil :bad-long nil :opt-pairs [] :leftover []}]
          (cond
            (or (:bad-flag st) (:bad-long st)) st
            (empty? in) st
            :else
            (let [a (first in)
                  sign (.charAt a 0)
                  ;; chars after the sign
                  cs (vec (rest a))]
              (if (some #(= \o %) cs)
                ;; Has `o`. Other chars (non-o) go to short flags;
                ;; `o` consumes the NEXT arg as long-opt name.
                (let [non-o (filter #(not= \o %) cs)
                      next-arg (second in)
                      bad-c (some #(when-not (set-short-flags %) %) non-o)]
                  (cond
                    bad-c (assoc st :bad-flag bad-c)
                    (or (nil? next-arg) (str/starts-with? next-arg "-"))
                    (assoc st :bad-long "")
                    (not (set-long-options next-arg))
                    (assoc st :bad-long next-arg)
                    :else
                    (recur (drop 2 in)
                           (-> st
                               (update :leftover into
                                       (when (seq non-o)
                                         [(apply str sign non-o)]))
                               (update :opt-pairs conj
                                       [(str sign) next-arg])))))
                ;; No `o` — just validate short flags.
                (if-let [bad (some #(when-not (set-short-flags %) %) cs)]
                  (assoc st :bad-flag bad)
                  (recur (rest in) (update st :leftover conj a)))))))]
    (cond
      bad-flag
      (do (write-line opts :err (str "set: invalid option: \"-" (str bad-flag) "\"\n"))
          (env/record-exit env 2))

      bad-long
      (do (write-line opts :err (str "set: invalid option: \"" bad-long "\"\n"))
          (env/record-exit env 2))

      :else
      (let [env' (reduce
                  (fn [e a]
                    (let [on? (str/starts-with? a "-")
                          flags (subs a 1)]
                      (reduce
                       (fn [e c]
                         (case c
                           \e (env/set-option e :errexit on?)
                           \u (env/set-option e :nounset on?)
                           \x (env/set-option e :xtrace on?)
                           \f (env/set-option e :noglob on?)
                           \a (env/set-option e :allexport on?)
                           e))
                       e flags)))
                  env leftover)
            env' (reduce (fn [e [op long]]
                           (let [on? (= op "-")
                                 k (case long
                                     "errexit"   :errexit
                                     "nounset"   :nounset
                                     "xtrace"    :xtrace
                                     "noglob"    :noglob
                                     "pipefail"  :pipefail
                                     "allexport" :allexport
                                     "noexec"    :noexec
                                     "verbose"   :verbose
                                     nil)]
                             (if k (env/set-option e k on?) e)))
                         env' opt-pairs)
            env'' (if (some? pos-args) (env/with-pos-args env' pos-args) env')]
        (env/record-exit env'' 0)))))

(defn- builtin-shift
  [env args opts]
  (cond
    (> (count args) 1)
    (do (write-line opts :err "usage: shift [n]\n")
        (env/record-exit env 2))
    (and (seq args)
         (nil? (try (parse-int* (first args)) (catch #?(:clj Exception :cljs :default) _ nil))))
    (do (write-line opts :err "usage: shift [n]\n")
        (env/record-exit env 2))
    :else
    (let [n (if (seq args) (parse-int* (first args)) 1)]
      (env/record-exit (env/shift env n) 0))))

(defn- builtin-exit
  [env args opts]
  ;; `exit [N]` — N defaults to $? (last exit); negatives + values > 255
  ;; are masked to (N & 0xff) per POSIX.
  (cond
    (> (count args) 1)
    (do (write-line opts :err "exit cannot take multiple arguments\n")
        (env/record-exit env 1))

    (and (seq args)
         (nil? (try (parse-int* (first args)) (catch #?(:clj Exception :cljs :default) _ nil))))
    (do (write-line opts :err (str "invalid exit status code: \"" (first args) "\"\n"))
        (-> env (env/record-exit 2) (assoc :exiting? true)))

    :else
    (let [n (if (seq args)
              (parse-int* (first args))
              (:last-exit env))
          masked (bit-and (long n) 0xff)]
      (-> env (env/record-exit masked) (assoc :exiting? true)))))

(defn- builtin-break-or-continue
  "`break [N]` / `continue [N]` — break out of N enclosing loops
   (default 1). We set a counter on env; the loop wrappers decrement
   and propagate. Outside a loop, both are errors."
  [name env args opts]
  (let [n (if (seq args)
            (try (parse-int* (first args))
                 (catch #?(:clj Exception :cljs :default) _ -1))
            1)]
    (cond
      (not (:in-loop? env))
      (do (write-line opts :err (str name " is only useful in a loop\n"))
          (env/record-exit env 0))

      (or (nil? n) (neg? n) (= n -1))
      (do (write-line opts :err (str "usage: " name " [n]\n"))
          (env/record-exit env 2))

      :else
      ;; The builtin itself succeeded — set :last-exit 0 so the loop
      ;; that catches the break/continue signal sees a clean exit. We
      ;; track the depth on the side-channel keys.
      (-> env
          (assoc (if (= name "break") :break-enclosing :continue-enclosing) n)
          (env/record-exit 0)))))

(defn- builtin-break    [env args opts] (builtin-break-or-continue "break"    env args opts))
(defn- builtin-continue [env args opts] (builtin-break-or-continue "continue" env args opts))

(defn- builtin-return
  "`return [N]` — exit a function with status N (default $?). Outside
   a function (and outside a sourced file), bash errors with
   \"return: can only be done from a func or sourced script\"."
  [env args opts]
  (cond
    (and (empty? (:scope-stack env)) (not (:sourcing? env)))
    (do (write-line opts :err
                    "return: can only be done from a func or sourced script\n")
        (env/record-exit env 1))

    :else
    (let [n (if (seq args)
              (try (parse-int* (first args))
                   (catch #?(:clj Exception :cljs :default) _ nil))
              (:last-exit env))]
      (if (nil? n)
        (do (write-line opts :err (str "invalid return status code: \""
                                       (first args) "\"\n"))
            (env/record-exit env 2))
        (-> env
            (env/record-exit (bit-and (long n) 0xff))
            (assoc :returning? true))))))

(defn- parse-printf-int
  "bash printf accepts decimal `42`, octal `010` → 8, hex `0x10` → 16,
   and `'c` → ASCII code of c. Returns 0 on parse error."
  [s]
  (let [s (str/trim (str s))]
    (cond
      (empty? s) 0
      (and (>= (count s) 2) (= \' (.charAt s 0)))
      (long (.charAt s 1))
      (and (>= (count s) 2) (= \0 (.charAt s 0))
           (or (= \x (.charAt s 1)) (= \X (.charAt s 1))))
      (try (parse-int* (subs s 2) 16)
           (catch #?(:clj Exception :cljs :default) _ 0))
      ;; Leading 0 (with at least one more digit) → octal.
      (and (> (count s) 1) (= \0 (.charAt s 0))
           (re-matches #"0[0-7]+" s))
      (try (parse-int* (subs s 1) 8)
           (catch #?(:clj Exception :cljs :default) _ 0))
      :else
      (try (parse-int* s)
           (catch #?(:clj Exception :cljs :default) _ 0)))))

(defn- printf-format
  "Format args according to `fmt-str` (a single bash printf format
   string). Returns [output-string args-remaining error?]. The
   args-remaining slice lets the caller loop and reuse the format
   until args are exhausted (bash printf semantics).

   Supports: %s %d %i %u %c %x %X %o %b %q %% with `-`/`+`/` `/`0`/`#`
   flags, width and precision modifiers. Integer args parse `010`
   as octal and `0x10` as hex per bash. Escapes (\\n \\t ...) in the
   format string are decoded via the ANSI-C table."
  [^String fmt-str args]
  (let [n (count fmt-str)
        out (volatile! [])
        push! (fn [s] (vswap! out conj (str s)))
        decode-escape (fn [s] (expand/decode-ansi-c s))
        consume-non-percent (fn [i]
                              (loop [j i acc []]
                                (cond
                                  (>= j n) [(apply str acc) j]
                                  (= \% (.charAt fmt-str j))
                                  [(apply str acc) j]
                                  :else
                                  (recur (inc j) (conj acc (str (.charAt fmt-str j)))))))
        format-int (fn [^String spec ^long x]
                     (try (fmt spec x)
                          (catch #?(:clj Throwable :cljs :default) _ (str x))))
        format-str (fn [^String spec ^String s]
                     (try (fmt spec s)
                          (catch #?(:clj Throwable :cljs :default) _ s)))]
    (loop [i 0 args args]
      (cond
        (>= i n) [(apply str @out) args nil]

        (not= \% (.charAt fmt-str i))
        (let [[chunk j] (consume-non-percent i)]
          (push! (decode-escape chunk))
          (recur j args))

        :else
        ;; At `%`. Parse flags + width + precision + conversion char.
        (let [j (inc i)
              [j flags] (loop [k j fs ""]
                          (let [c (when (< k n) (.charAt fmt-str k))]
                            ;; Past EOF or a non-flag char ends the
                            ;; flag scan. The `(some? c)` guard is
                            ;; load-bearing: \space is in the flag
                            ;; set, so without it `printf %` spins.
                            (if (and (some? c) (#{\- \+ \space \0 \# \'} c))
                              (recur (inc k) (str fs c))
                              [k fs])))
              [j width] (loop [k j ws ""]
                          (let [c (when (< k n) (.charAt fmt-str k))]
                            (if (and c (re-find #"[0-9]" (str c)))
                              (recur (inc k) (str ws c))
                              [k ws])))
              [j prec] (if (and (< j n) (= \. (.charAt fmt-str j)))
                         (loop [k (inc j) ps ""]
                           (let [c (when (< k n) (.charAt fmt-str k))]
                             (if (and c (re-find #"[0-9]" (str c)))
                               (recur (inc k) (str ps c))
                               [k ps])))
                         [j nil])
              conv (when (< j n) (.charAt fmt-str j))
              ;; Java's Formatter uses %d not %i; map and drop the `'`
              ;; (grouping) flag which Java doesn't accept for ints.
              java-conv (case conv \i \d conv)
              java-flags (str/replace (str flags) "'" "")
              spec (str "%" java-flags width (when prec (str "." prec))
                        (when java-conv (str java-conv)))]
          (case conv
            \% (do (push! "%") (recur (inc j) args))
            nil [(apply str @out) args "missing format char"]
            \s (let [a (or (first args) "")]
                 (push! (format-str spec a))
                 (recur (inc j) (rest args)))
            \c (let [a (or (first args) "")]
                 (push! (if (empty? a) "" (subs a 0 1)))
                 (recur (inc j) (rest args)))
            \b (let [a (or (first args) "")]
                 (push! (decode-escape a))
                 (recur (inc j) (rest args)))
            \q (let [a (or (first args) "")]
                 (push! (str "'" (str/replace a "'" "'\\''") "'"))
                 (recur (inc j) (rest args)))
            (\d \i \u \o \x \X)
            (let [a (or (first args) "0")
                  num (parse-printf-int a)]
              (push! (format-int spec num))
              (recur (inc j) (rest args)))
            [(apply str @out) args (str "invalid format char: " conv)]))))))

(defn- parse-name=value
  "Parse a `name=value` arg into [name value], or [arg nil] if no `=`."
  [^String arg]
  (let [i (.indexOf arg "=")]
    (if (neg? i)
      [arg nil]
      [(subs arg 0 i) (subs arg (inc i))])))

(defn- builtin-local
  "`local [name[=value] ...]` — declare each name as local to the
   current function scope. Outside a function, errors per bash."
  [env args opts]
  (if (empty? (:scope-stack env))
    (do (write-line opts :err "local: can only be used in a function\n")
        (env/record-exit env 1))
    (env/record-exit
     (reduce (fn [e arg]
               (let [[name value] (parse-name=value arg)]
                 (env/declare-local e name value)))
             env args)
     0)))

(defn- builtin-declare
  "Minimal `declare [-r] [-x] [name[=value] ...]`. We honour:
     -r make readonly
     -x mark exported
   Other flags (-a, -A, -i, -l, -u, -p) we accept but don't
   distinguish — declare on undeclared names sets them to empty."
  [env args opts]
  (let [[flag-args names] (split-with #(str/starts-with? % "-") args)
        flags (str/join "" (map #(subs % 1) flag-args))
        readonly? (str/includes? flags "r")
        export? (str/includes? flags "x")]
    (env/record-exit
     (reduce (fn [e arg]
               (let [[name value] (parse-name=value arg)
                     ;; bash: `declare =foo` is an invalid name error.
                     ;; We surface that here (rather than letting the
                     ;; empty name silently leak into the var table).
                     e (cond
                         (empty? name)
                         (do (write-line opts :err
                                         (str "declare: invalid name \""
                                              name "\"\n"))
                             (env/record-exit e 1))

                         ;; `declare NAME=value` — set new value
                         value (env/set-var e name value)
                         ;; `declare NAME` — preserve existing value
                         ;; if any (bash quirk); otherwise mark
                         ;; declared-but-no-value so `[[ -v NAME ]]`
                         ;; and `${NAME-default}` see it as unset.
                         (env/declared? e name) e
                         :else (assoc-in e [:vars name]
                                         {:value "" :exported? false
                                          :readonly? false :no-value? true}))
                     e (if export? (env/export e name) e)
                     e (if readonly? (env/mark-readonly e name) e)]
                 e))
             env names)
     0)))

(defn- builtin-eval
  "`eval [arg ...]` — concatenate args with spaces, parse and execute
   in the CURRENT env. Result-env's mutations leak back to caller.
   Note: bypasses parse-time permit by design (it's a runtime
   construct); the runtime hook at each external spawn still applies."
  [env args opts]
  (let [src (str/join " " args)]
    (if (empty? src)
      (env/record-exit env 0)
      (try
        (let [ast (parse/parse src)
              env' (exec-stmts env (:stmts ast) opts)]
          env')
        (catch #?(:clj Throwable :cljs :default) e
          (write-line opts :err (str "eval: " (.getMessage e) "\n"))
          (env/record-exit env 1))))))

(defn- builtin-source
  "`source FILE [arg ...]` — read FILE, parse + exec in CURRENT env.
   Mutations (cd, var assigns) persist. Positional params replaced by
   the trailing args (or kept if none)."
  [env args opts]
  (let [[path & call-args] args]
    (cond
      (nil? path)
      (do (write-line opts :err "source: filename argument required\n")
          (env/record-exit env 2))

      :else
      (let [h (:host opts)
            resolved (resolve-path env path)]
        (if (not (host/file-exists? h resolved))
          (do (write-line opts :err (str "source: " path ": file not found\n"))
              (env/record-exit env 1))
          (try
            (let [src (host/read-file h resolved)
                  ast (parse/parse src)
                  old-args (:pos-args env)
                  ;; Mark sourcing so `return` is permitted inside.
                  env' (cond-> (assoc env :sourcing? true)
                         (seq call-args) (env/with-pos-args (vec call-args)))
                  env'' (exec-stmts env' (:stmts ast) opts)
                  env''' (-> env''
                             (dissoc :returning?)
                             (dissoc :sourcing?))]
              (if (seq call-args)
                (assoc env''' :pos-args old-args)
                env'''))
            (catch #?(:clj Throwable :cljs :default) e
              (write-line opts :err (str "source: " (.getMessage e) "\n"))
              (env/record-exit env 1))))))))

(declare ^:private builtins)

(def ^:private shell-keywords
  #{"if" "then" "elif" "else" "fi"
    "for" "while" "until" "do" "done"
    "case" "esac" "in"
    "function" "select" "time" "coproc"
    "!" "{" "}" "[[" "]]"})

(defn- type-of-name
  "Returns one of :keyword :builtin :function :file :not-found for
   how bash would resolve `name`. Used by builtin-type for both the
   long-form output and the `-t` short-form."
  [env name h]
  (cond
    (shell-keywords name) :keyword
    ;; bash: a function shadows a builtin (mvdan test: `echo() { :; };
    ;; type echo` → "echo is a function").
    (env/lookup-fn env name) :function
    (contains? builtins name) :builtin
    :else
    (let [paths (str/split (or (env/get-var env "PATH") "") #":")]
      (if (some (fn [p]
                  (let [c (str p "/" name)]
                    (and (host/file-exists? h c) (host/file-executable? h c))))
                paths)
        :file
        :not-found))))

(defn- builtin-type
  "`type [-t|-p|-P] CMD ...` — report how each CMD would be resolved.
   With `-t`: short form (`alias|keyword|function|builtin|file`).
   With `-p` / `-P`: print PATH match (empty for builtins/funcs)."
  [env args opts]
  (if (empty? args)
    (env/record-exit env 0)
    (let [any-bad? (volatile! false)
          [flags names] (split-with #(str/starts-with? % "-") args)
          flag-str (apply str (map #(subs % 1) flags))
          short? (str/includes? flag-str "t")
          path?  (or (str/includes? flag-str "p") (str/includes? flag-str "P"))
          h (:host opts)]
      (doseq [name names]
        (let [k (type-of-name env name h)]
          (cond
            short?
            (case k
              :keyword  (write-line opts :out "keyword\n")
              :builtin  (write-line opts :out "builtin\n")
              :function (write-line opts :out "function\n")
              :file     (write-line opts :out "file\n")
              :not-found (vreset! any-bad? true))

            path?
            (case k
              :file (let [paths (str/split (or (env/get-var env "PATH") "") #":")
                          found (some (fn [p]
                                        (let [c (str p "/" name)]
                                          (when (and (host/file-exists? h c)
                                                     (host/file-executable? h c))
                                            c)))
                                      paths)]
                      (when found (write-line opts :out (str found "\n"))))
              ;; -p prints nothing for non-file resolutions; -P keeps
              ;; trying path even when there's a builtin/function. We
              ;; treat both the same (POSIX `-p` for now).
              (do nil (vreset! any-bad? true)))

            :else
            (case k
              :keyword  (write-line opts :out (str name " is a shell keyword\n"))
              :function (write-line opts :out (str name " is a function\n"))
              :builtin  (write-line opts :out (str name " is a shell builtin\n"))
              :file (let [paths (str/split (or (env/get-var env "PATH") "") #":")
                          found (some (fn [p]
                                        (let [c (str p "/" name)]
                                          (when (and (host/file-exists? h c)
                                                     (host/file-executable? h c))
                                            c)))
                                      paths)]
                      (write-line opts :out (str name " is " found "\n")))
              :not-found (do (write-line opts :err (str "type: " name ": not found\n"))
                             (vreset! any-bad? true))))))
      (env/record-exit env (if @any-bad? 1 0)))))

(declare run-argv run-external builtins)

(defn- builtin-command
  "`command [-v|-V] [-p] NAME [arg ...]` — run NAME, bypassing function
   lookup. With `-v` print the command resolution (path / `NAME is a
   shell builtin` / function name) — exit 0 on found, 1 on not found.

   Bash quirks we honour:
   - `-v foo` prints `foo` for builtin/function, the path for an
     external. Multiple names with -v: only print the FIRST that
     resolves; if any name doesn't resolve, exit 1 at the end.
   - Unknown flag → 'command: invalid option' + exit 2."
  [env args opts]
  (let [;; Parse leading -v/-V/-p flags (combined like -vp accepted).
        [flag-args rest-args]
        (split-with (fn [a]
                      (and (str/starts-with? a "-") (not= a "--")))
                    args)
        rest-args (vec (if (= "--" (first rest-args)) (rest rest-args) rest-args))
        flags (apply str (map #(subs % 1) flag-args))
        bad-flag (some #(when-not (#{\v \V \p} %) %) flags)]
    (cond
      bad-flag
      (do (write-line opts :err (str "command: invalid option \"-"
                                     (str bad-flag) "\"\n"))
          (env/record-exit env 2))

      (str/includes? flags "v")
      ;; -v: print how each name would resolve, return 0 if any
      ;; resolved (bash actually exits 1 if NONE resolved; with multiple
      ;; names it exits 0 as soon as one resolves but keeps printing).
      (let [any? (volatile! false)
            h (:host opts)
            path-env (env/get-var env "PATH")
            paths (str/split (or path-env "") #":")]
        (doseq [name rest-args]
          (cond
            (env/lookup-fn env name)
            (do (write-line opts :out (str name "\n")) (vreset! any? true))

            (contains? builtins name)
            (do (write-line opts :out (str name "\n")) (vreset! any? true))

            :else
            (when-let [found (some (fn [p]
                                     (let [c (str p "/" name)]
                                       (when (and (host/file-exists? h c)
                                                  (host/file-executable? h c))
                                         c)))
                                   paths)]
              (write-line opts :out (str found "\n"))
              (vreset! any? true))))
        (env/record-exit env (if @any? 0 1)))

      (empty? rest-args)
      (env/record-exit env 0)

      :else
      ;; Plain `command NAME args` — run NAME, skipping function table.
      (let [name (first rest-args)
            args' (vec (rest rest-args))]
        (cond
          (contains? builtins name)
          ((get builtins name) env args' opts)
          :else
          (run-external env name args' {} opts))))))

(defn- print-dir-stack
  "bash's pushd/popd/dirs print the stack with $HOME → ~ substituted."
  [env opts]
  (let [home (env/get-var env "HOME")
        tilde (fn [p]
                (if (and home (not= "" home) (str/starts-with? p home))
                  (str "~" (subs p (count home)))
                  p))
        stack (cons (:cwd env) (or (:dir-stack env) []))]
    (write-line opts :out (str (str/join " " (map tilde stack)) "\n"))))

(defn- builtin-pushd
  "POSIX/bash `pushd [-n] [DIR]` — push current dir + cd to DIR. With
   no args, swap top of stack. `-n` is parsed but only suppresses the
   cd (push happens). Prints the new stack on success."
  [env args opts]
  (let [{flag-args true non-flag-args false}
        (group-by #(str/starts-with? % "-") args)
        no-cd? (some #(= "-n" %) flag-args)
        non-flag-args (vec non-flag-args)
        target (first non-flag-args)
        stack (or (:dir-stack env) [])
        old-cwd (:cwd env)]
    (cond
      (> (count non-flag-args) 1)
      (do (write-line opts :err "pushd: too many arguments\n")
          (env/record-exit env 2))

      (and (empty? target) (empty? stack))
      (do (write-line opts :err "pushd: no other directory\n")
          (env/record-exit env 1))

      ;; pushd (no args): swap top of stack with cwd.
      (empty? target)
      (let [new-cwd (first stack)
            new-stack (vec (cons old-cwd (rest stack)))
            env' (assoc env :cwd new-cwd :dir-stack new-stack)]
        (print-dir-stack env' opts)
        (env/record-exit env' 0))

      :else
      (let [resolved (resolve-path env target)
            fi (host/file-info (:host opts) resolved)]
        (cond
          (not (:exists? fi))
          (do (write-line opts :err
                          (str "pushd: no such file or directory: \""
                               target "\"\n"))
              (env/record-exit env 1))

          (not (:dir? fi))
          (do (write-line opts :err (str "pushd: not a directory: \"" target "\"\n"))
              (env/record-exit env 1))

          :else
          (let [env' (cond-> env
                       (not no-cd?) (env/cd target)
                       :always (assoc :dir-stack (vec (cons old-cwd stack))))]
            (print-dir-stack env' opts)
            (env/record-exit env' 0)))))))

(defn- builtin-popd
  "POSIX/bash `popd` — pop the dir stack, cd to the popped dir."
  [env _args opts]
  (let [stack (or (:dir-stack env) [])]
    (if (empty? stack)
      (do (write-line opts :err "popd: directory stack empty\n")
          (env/record-exit env 1))
      (let [target (first stack)
            env' (env/cd env target)
            env' (assoc env' :dir-stack (vec (rest stack)))]
        (print-dir-stack env' opts)
        (env/record-exit env' 0)))))

(defn- builtin-getopts
  "POSIX `getopts OPTSTRING VARNAME [args...]` — parse one option per
   invocation. Reads/writes `OPTIND` (1-based arg index) and `OPTARG`.
   Returns 0 while options remain, 1 at end, 2 on bad usage.

   With a leading `:` in OPTSTRING, errors are silent and we emit
   `?`/`:` in VARNAME instead of stderr messages."
  [env args opts]
  (cond
    (empty? args)
    (do (write-line opts :err "getopts: usage: getopts optstring name [arg ...]\n")
        (env/record-exit env 2))

    (< (count args) 2)
    (do (write-line opts :err
                    (str "getopts: usage: getopts optstring name [arg ...]\n"))
        (env/record-exit env 2))

    (not (re-matches #"[A-Za-z_][A-Za-z_0-9]*" (second args)))
    (do (write-line opts :err
                    (str "getopts: invalid identifier: \""
                         (second args) "\"\n"))
        (env/record-exit env 2))

    :else
    (let [optstring (first args)
          varname (second args)
          silent? (str/starts-with? optstring ":")
          opt-args (if (> (count args) 2)
                     (vec (drop 2 args))
                     (vec (:pos-args env)))
          ;; OPTIND: 1-based index into opt-args.
          optind-str (or (env/get-var env "OPTIND") "1")
          optind (try (parse-int* optind-str)
                      (catch #?(:clj Exception :cljs :default) _ 1))
          ;; :getopts-pos: 1-based char position within the current arg
          ;; (skipping the leading `-`). Reset on each OPTIND advance.
          inner-pos (or (:getopts-pos env) 1)
          ;; Bash quirk: setting OPTIND from outside resets inner state.
          inner-pos (if (and (some? (:getopts-prev-optind env))
                             (not= optind (:getopts-prev-optind env)))
                      1
                      inner-pos)]
      (cond
        ;; OPTIND past the end → done.
        (> optind (count opt-args))
        (-> env
            (env/set-var varname "?")
            (env/record-exit 1))

        :else
        (let [cur (nth opt-args (dec optind) "")
              ;; Not an option arg → done.
              not-option? (or (= "" cur)
                              (not (str/starts-with? cur "-"))
                              (= "-" cur))]
          (cond
            not-option?
            (-> env
                (env/set-var varname "?")
                (env/record-exit 1))

            ;; `--` marks end of options.
            (= "--" cur)
            (-> env
                (env/set-var "OPTIND" (str (inc optind)))
                (env/set-var varname "?")
                (env/record-exit 1))

            :else
            (let [flag-char (.charAt ^String cur inner-pos)
                  flag-str (str flag-char)
                  ix (.indexOf ^String optstring flag-str)
                  takes-arg? (and (>= ix 0)
                                  (< (inc ix) (count optstring))
                                  (= \: (.charAt ^String optstring (inc ix))))
                  more-in-arg? (< (inc inner-pos) (count cur))]
              (cond
                ;; Unknown option.
                (neg? ix)
                (let [env' (if silent?
                             (-> env (env/set-var "OPTARG" flag-str))
                             (do (write-line opts :err
                                             (str "getopts: illegal option -- \""
                                                  flag-str "\"\n"))
                                 (env/set-var env "OPTARG" "")))
                      ;; Advance: next char or next arg.
                      [next-optind next-inner]
                      (if more-in-arg?
                        [optind (inc inner-pos)]
                        [(inc optind) 1])]
                  (-> env'
                      (env/set-var varname "?")
                      (env/set-var "OPTIND" (str next-optind))
                      (assoc :getopts-pos next-inner
                             :getopts-prev-optind next-optind)
                      (env/record-exit 0)))

                ;; Option takes an arg.
                takes-arg?
                (let [;; Arg is either the rest of cur OR the next arg.
                      [arg-val next-optind]
                      (cond
                        more-in-arg?
                        [(subs cur (inc inner-pos)) (inc optind)]
                        (< (inc optind) (inc (count opt-args)))
                        ;; arg is next opt-arg
                        (if (>= optind (count opt-args))
                          [nil (inc optind)]
                          [(nth opt-args optind "") (+ optind 2)])
                        :else
                        [nil (inc optind)])]
                  (cond
                    (nil? arg-val)
                    (let [env' (if silent?
                                 (-> env
                                     (env/set-var "OPTARG" flag-str)
                                     (env/set-var varname ":"))
                                 (do (write-line opts :err
                                                 (str "getopts: option requires an argument -- \""
                                                      flag-str "\"\n"))
                                     (-> env
                                         (env/set-var "OPTARG" "")
                                         (env/set-var varname "?"))))]
                      (-> env'
                          (env/set-var "OPTIND" (str next-optind))
                          (assoc :getopts-pos 1
                                 :getopts-prev-optind next-optind)
                          (env/record-exit 0)))

                    :else
                    (-> env
                        (env/set-var varname flag-str)
                        (env/set-var "OPTARG" arg-val)
                        (env/set-var "OPTIND" (str next-optind))
                        (assoc :getopts-pos 1
                               :getopts-prev-optind next-optind)
                        (env/record-exit 0))))

                ;; Plain boolean option — advance within arg or to next.
                :else
                (let [[next-optind next-inner]
                      (if more-in-arg?
                        [optind (inc inner-pos)]
                        [(inc optind) 1])]
                  (-> env
                      (env/set-var varname flag-str)
                      (env/set-var "OPTARG" "")
                      (env/set-var "OPTIND" (str next-optind))
                      (assoc :getopts-pos next-inner
                             :getopts-prev-optind next-optind)
                      (env/record-exit 0)))))))))))

(defn- builtin-hash
  "Bash `hash` builtin — we don't track command-lookup cache, so it's
   a no-op exit 0."
  [env _args _opts]
  (env/record-exit env 0))

(defn- builtin-exec
  "Bash `exec [cmd args...]` — replace shell process. We approximate:
   without args, just apply any redirects (currently handled by
   stmt-redirs already) and exit 0. With args, just run the command.
   (Full exec semantics need OS-level process replacement which we
   can't really model.)"
  [env args opts]
  (cond
    (empty? args) (env/record-exit env 0)
    :else (run-argv env (vec args) opts)))

(defn- builtin-alias
  "Bash `alias [-p] [NAME[=VALUE] ...]` — we don't expand aliases in
   the parser, but we track the table so `alias foo` queries work."
  [env args opts]
  (let [aliases (or (:aliases env) {})]
    (cond
      ;; No args (or only -p): print all aliases.
      (or (empty? args) (every? #(= "-p" %) args))
      (do (doseq [[k v] (sort aliases)]
            (write-line opts :out (str "alias " k "='" v "'\n")))
          (env/record-exit env 0))

      :else
      (let [{:keys [env' err?]}
            (reduce (fn [{:keys [env' err? aliases]} arg]
                      (if (str/includes? arg "=")
                        (let [i (.indexOf ^String arg "=")
                              name (subs arg 0 i)
                              value (subs arg (inc i))]
                          {:env' (assoc env' :aliases (assoc aliases name value))
                           :err? err?
                           :aliases (assoc aliases name value)})
                        ;; Query
                        (if-let [v (get aliases arg)]
                          (do (write-line opts :out (str "alias " arg "='" v "'\n"))
                              {:env' env' :err? err? :aliases aliases})
                          (do (write-line opts :err (str "alias: \"" arg "\" not found\n"))
                              {:env' env' :err? true :aliases aliases}))))
                    {:env' env :err? false :aliases aliases}
                    args)]
        (env/record-exit env' (if err? 1 0))))))

(defn- builtin-builtin
  "Bash `builtin NAME args...` — run NAME as a builtin, bypassing
   functions and external lookup."
  [env args opts]
  (cond
    (empty? args) (env/record-exit env 0)
    (contains? builtins (first args))
    ((get builtins (first args)) env (vec (rest args)) opts)
    :else (env/record-exit env 1)))

(defn- builtin-dirs
  "POSIX/bash `dirs [-c]` — print the dir stack."
  [env args opts]
  (cond
    (some #(= "-c" %) args)
    (env/record-exit (assoc env :dir-stack []) 0)
    :else
    (do (print-dir-stack env opts)
        (env/record-exit env 0))))

(defn- read-one-line
  "Drain the next line from `source`. Strips the trailing `\\n`. nil
   if EOF. Handles both string sources (via host) and plain strings."
  [host source]
  (when source
    (let [all (host/read-all-string host source)]
      (cond
        (or (nil? all) (= "" all)) nil
        :else
        (let [nl (.indexOf ^String all "\n")]
          (if (neg? nl)
            all
            (subs all 0 nl)))))))

(defn- builtin-read
  "POSIX `read [-r] [-d delim] [-p prompt] var ...` — minimal impl.

   Reads one line from stdin (the host's stdin via opts), splits it
   into fields by IFS, and assigns to the given vars (last var gets
   the remainder). With no vars, stores in REPLY. Exit 0 on read,
   1 on EOF.

   Not yet implemented: `-n` (n chars), `-t` (timeout), `-s` (silent),
   `-a` (array), `-N` (exact bytes)."
  [env args opts]
  (let [;; Strip flags (we accept but ignore most for now).
        [flags rest]
        (loop [args args flags #{}]
          (let [a (first args)]
            (cond
              (= a "-r") (recur (rest args) (conj flags :raw))
              (or (= a "-d") (= a "-p") (= a "-n") (= a "-t")
                  (= a "-N") (= a "-u"))
              ;; flags that take a value — skip the value too
              (recur (drop 2 args) (conj flags a))
              :else [flags args])))
        vars (if (seq rest) (vec rest) ["REPLY"])
        h (:host opts)
        line (read-one-line h (:in opts))]
    (if (nil? line)
      (env/record-exit env 1)
      (let [ifs (:ifs env)
            ;; bash splits the line by IFS — first N-1 vars get one
            ;; field each; the last var gets the remainder.
            n (count vars)
            line (if (:raw flags) line
                     ;; `read` without `-r` interprets `\` escapes.
                     (str/replace line #"\\(.)" "$1"))
            fields (if (= 1 n)
                     [line]
                     (let [parts (str/split line (re-pattern (str "[" (str/escape (or ifs " \t")
                                                                                  {\\ "\\\\"}) "]+"))
                                            n)]
                       (vec parts)))
            env' (reduce (fn [e [v val]]
                           (env/set-var e v (or val "")))
                         env (map vector vars fields))]
        (env/record-exit env' 0)))))

(defn- builtin-printf
  [env args opts]
  (cond
    (empty? args)
    (do (write-line opts :err "usage: printf format [arguments]\n")
        (env/record-exit env 2))

    :else
    (let [[fmt & fmt-args] args]
      (loop [args (vec fmt-args)]
        (let [[s rest-args err] (printf-format fmt args)]
          (write-line opts :out s)
          (cond
            err
            (do (write-line opts :err (str err "\n"))
                (env/record-exit env 1))

            (and (seq rest-args) (not= (count rest-args) (count args)))
            (recur (vec rest-args))

            :else
            (env/record-exit env 0)))))))

(defn- builtin-let
  [env args opts]
  ;; `let expr [expr...]` — evaluate each arithmetic expression.
  ;; Exit status is 0 iff the LAST expression's value is non-zero.
  ;; Arith errors (divide-by-zero, parse failures) print to stderr
  ;; and continue with the remaining expressions.
  (let [[env' last-v err?]
        (reduce (fn [[env _last err?] expr]
                  (try
                    (let [[env' v] (arith/evaluate env expr)]
                      [env' v err?])
                    (catch #?(:clj Throwable :cljs :default) e
                      (write-line opts :err (str (or (.getMessage e)
                                                     (str e)) "\n"))
                      [env 0 true])))
                [env 0 false]
                args)]
    (env/record-exit env' (cond
                            err? 1
                            (zero? last-v) 1
                            :else 0))))

(declare eval-test-expr)

(defn- test-primary-1
  "Single-arg form: `test STR` → true iff STR is non-empty."
  [_env [s] _opts]
  (not= "" s))

(defn- test-primary-2
  "Two-arg form: unary op + value. Returns true/false, or :unknown
   if the op isn't recognised (caller treats as non-test args).
   File tests on an EMPTY path return false (bash quirk)."
  [env [op v] opts]
  (let [h (:host opts)
        info (fn [p] (host/file-info h (resolve-path env p)))
        file-op? (#{"-e" "-f" "-d" "-r" "-w" "-x" "-s" "-L" "-h"} op)]
    (cond
      (and file-op? (or (nil? v) (= "" v))) false
      :else
      (case op
        "-e" (boolean (:exists?  (info v)))
        "-f" (boolean (:file?    (info v)))
        "-d" (boolean (:dir?     (info v)))
        "-r" (boolean (:readable? (info v)))
        "-w" (boolean (:writable? (info v)))
        "-x" (boolean (:executable? (info v)))
        "-s" (let [i (info v)] (boolean (and (:exists? i) (pos? (or (:size i) 0)))))
        ("-L" "-h") (boolean (:symlink? (info v)))
        "-z" (= "" v)
        "-n" (not= "" v)
        "-v" (env/has-value? env v)
        "-o" (boolean (env/option env (keyword v)))
        ("-b" "-c" "-g" "-k" "-p" "-S" "-u" "-N" "-O" "-G" "-t") false
        :unknown))))

(defn- test-primary-3
  "Three-arg form: a OP b. Returns true/false, or :unknown."
  [env [a op b] opts]
  (let [h (:host opts)
        file-age (fn [p] (or (host/file-mtime-ms h (resolve-path env p)) 0))
        same-file? (fn [x y]
                     (try (= (host/resolve-path (:cwd env) x)
                             (host/resolve-path (:cwd env) y))
                          (catch #?(:clj Throwable :cljs :default) _ false)))
        parse-int-or (fn [s fallback]
                       (try (parse-int* (str/trim (str s)))
                            (catch #?(:clj Exception :cljs :default) _ fallback)))]
    (case op
      ("=" "==") (= a b)
      "!=" (not= a b)
      "<" (neg? (compare a b))
      ">" (pos? (compare a b))
      "-eq" (= (parse-int-or a 0) (parse-int-or b 0))
      "-ne" (not= (parse-int-or a 0) (parse-int-or b 0))
      "-lt" (< (parse-int-or a 0) (parse-int-or b 0))
      "-le" (<= (parse-int-or a 0) (parse-int-or b 0))
      "-gt" (> (parse-int-or a 0) (parse-int-or b 0))
      "-ge" (>= (parse-int-or a 0) (parse-int-or b 0))
      "-ef" (same-file? a b)
      "-nt" (> (file-age a) (file-age b))
      "-ot" (< (file-age a) (file-age b))
      :unknown)))

(defn- eval-primary
  "Try test-primary-3 then -2 then -1 on the leading args. Returns
   [bool consumed-count] on success, or `:unknown` if no primary fits."
  [env args opts]
  (let [n (count args)]
    (cond
      (and (>= n 3) (not= :unknown (test-primary-3 env (take 3 args) opts)))
      [(test-primary-3 env (take 3 args) opts) 3]
      (and (>= n 2) (not= :unknown (test-primary-2 env (take 2 args) opts)))
      [(test-primary-2 env (take 2 args) opts) 2]
      (>= n 1)
      [(test-primary-1 env (take 1 args) opts) 1]
      :else
      :unknown)))

(defn- eval-test-expr
  "Parse + evaluate a test expression with `-a`, `-o`, `!`, and
   escaped-paren grouping (`(` and `)` as separate args). Returns
   [bool rest-of-args]; rest-of-args is empty on a clean parse.

   POSIX precedence: `!` > `-a` > `-o`."
  [env args opts]
  (letfn [(p-or [args]
            (let [[v r] (p-and args)]
              (loop [v v r r]
                (if (= (first r) "-o")
                  (let [[v2 r2] (p-and (vec (next r)))]
                    (recur (or v v2) r2))
                  [v r]))))
          (p-and [args]
            (let [[v r] (p-not args)]
              (loop [v v r r]
                (if (= (first r) "-a")
                  (let [[v2 r2] (p-not (vec (next r)))]
                    (recur (and v v2) r2))
                  [v r]))))
          (p-not [args]
            (cond
              (= (first args) "!")
              (let [[v r] (p-not (vec (next args)))]
                [(not v) r])
              (= (first args) "(")
              (let [[v r] (p-or (vec (next args)))]
                (if (= (first r) ")")
                  [v (vec (next r))]
                  [v r]))
              :else
              (let [r (eval-primary env args opts)]
                (if (= r :unknown)
                  [false (vec (next args))]
                  [(first r) (vec (drop (second r) args))]))))]
    (p-or args)))

(defn- builtin-test
  "POSIX `test` / `[`. Supports `-a` (and), `-o` (or), `!` (not), and
   `(` `)` for grouping."
  [env args opts]
  (let [args (vec args)
        ;; Strip trailing `]` if invoked as `[`.
        args (if (= (last args) "]") (vec (butlast args)) args)
        true!  #(env/record-exit env 0)
        false! #(env/record-exit env 1)]
    (cond
      (empty? args) (false!)

      ;; Common short-circuit paths kept for clarity.
      (= 1 (count args))
      (if (test-primary-1 env args opts) (true!) (false!))

      (= 2 (count args))
      (case (first args)
        "!" (if (test-primary-1 env (rest args) opts) (false!) (true!))
        (let [r (test-primary-2 env args opts)]
          (if (= r :unknown)
            (do (write-line opts :err
                            (str "test: unknown unary operator: "
                                 (first args) "\n"))
                (env/record-exit env 2))
            (if r (true!) (false!)))))

      (= 3 (count args))
      (let [r (test-primary-3 env args opts)]
        (if (= r :unknown)
          (case (first args)
            "!" (if (let [r2 (test-primary-2 env (rest args) opts)]
                      (if (= r2 :unknown) false r2))
                  (false!) (true!))
            (do (write-line opts :err
                            (str "test: unknown binary operator: "
                                 (second args) "\n"))
                (env/record-exit env 2)))
          (if r (true!) (false!))))

      :else
      ;; Compound expression with -a/-o/!/().
      (let [[v rest] (eval-test-expr env args opts)]
        (if (seq rest)
          (do (write-line opts :err
                          (str "test: too many arguments\n"))
              (env/record-exit env 2))
          (if v (true!) (false!)))))))

(defn- find-job
  "Resolve a wait/kill argument to a JobHandle from the session.
   Accepts `<pid>` or `%<job-id>`."
  [session arg]
  (let [jobs (session/-jobs session)]
    (cond
      (str/starts-with? arg "%")
      (let [id (parse-int* (subs arg 1))]
        (some #(when (= id (:id %)) %) jobs))
      :else
      (let [pid (try (parse-int* arg) (catch #?(:clj Exception :cljs :default) _ nil))]
        (when pid
          (some #(when (= pid (:pid %)) %) jobs))))))

(defn- builtin-wait
  "POSIX `wait [pid|%jobid ...]`. With no args, waits for ALL bg jobs
   and exits 0 — bash semantics. Waiting for a specific pid returns
   that job's exit (127 if not a child)."
  [env args opts]
  (cond
    (not (:session opts))
    (env/record-exit env 0)

    ;; No args → wait for all, always return 0
    (empty? args)
    (let [s (:session opts)]
      (doseq [j (session/-jobs s)] (session/await-job j))
      (env/record-exit env 0))

    :else
    (let [s (:session opts)
          {found-jobs true bad-args false}
          (group-by #(some? (find-job s %)) args)
          last-exit (reduce (fn [_ a] (session/await-job (find-job s a)))
                            0 found-jobs)]
      (doseq [bad bad-args]
        (write-line opts :err
                    (str "wait: pid " bad " is not a child of this shell\n")))
      (env/record-exit env (cond
                             (seq bad-args) 1
                             :else last-exit)))))

(defn- builtin-jobs
  [env _args opts]
  (when-let [s (:session opts)]
    (doseq [j (session/-jobs s)]
      (let [status (if (session/job-running? j)
                     "Running"
                     (str "Done(" (session/job-exit j) ")"))]
        (write-line opts :out (fmt "[%d] %s %s\n"
                                   (:id j) status (:pid j))))))
  (env/record-exit env 0))

(defn- builtin-kill
  [env args opts]
  ;; `kill <pid|%n>` — send TERM to a tracked job.
  ;; JVM hosts pass a Process handle in the JobHandle; for cljs hosts
  ;; this is best-effort (no kernel access from the browser).
  (when-let [s (:session opts)]
    (doseq [arg args]
      (when-let [j (find-job s arg)]
        #?(:clj
           (try (.destroy ^java.lang.Process (-> j :proc :proc))
                (catch #?(:clj Throwable :cljs :default) e
                  (write-line opts :err
                              (str "kill: " arg ": " (.getMessage e) "\n"))))
           :cljs
           nil))))
  (env/record-exit env 0))

(defn- builtin-disown
  [env args opts]
  ;; `disown [pid|%n ...]` — drop jobs from the table without killing
  (when-let [s (:session opts)]
    (let [match? (if (empty? args)
                   (constantly true)
                   (let [ids (set (keep #(:id (find-job s %)) args))]
                     #(ids (:id %))))]
      (session/-swap-env! s identity)                    ; no-op to flush
      ;; AtomSession exposes -purge-exited; for disown we just drop matches.
      ;; (Right now AtomSession doesn't expose direct-remove; piggyback on
      ;; purge by waiting briefly — pragmatic for V1.)
      ))
  (env/record-exit env 0))

(def ^:private builtins
  {"cd"     builtin-cd
   "pwd"    builtin-pwd
   "echo"   builtin-echo
   "true"   builtin-true
   "false"  builtin-false
   ":"      builtin-colon
   "export" builtin-export
   "unset"  builtin-unset
   "set"    builtin-set
   "shift"  builtin-shift
   "exit"   builtin-exit
   "let"    builtin-let
   "wait"   builtin-wait
   "jobs"   builtin-jobs
   "kill"   builtin-kill
   "disown" builtin-disown
   "test"   builtin-test
   "["      builtin-test
   "break"    builtin-break
   "continue" builtin-continue
   "return"   builtin-return
   "printf"   builtin-printf
   "eval"     builtin-eval
   "source"   builtin-source
   "."        builtin-source
   "type"     builtin-type
   "command"  builtin-command
   "read"     builtin-read
   "pushd"    builtin-pushd
   "popd"     builtin-popd
   "dirs"     builtin-dirs
   "getopts"  builtin-getopts
   "hash"     builtin-hash
   "exec"     builtin-exec
   "alias"    builtin-alias
   "builtin"  builtin-builtin
   "local"    builtin-local
   "declare"  builtin-declare
   "typeset"  builtin-declare
   "readonly" (fn [env args opts] (builtin-declare env (into ["-r"] args) opts))})

(defn- builtin? [name] (contains? builtins name))

;; ============================================================================
;; Call (the leaf — runs builtins or spawns externals)
;; ============================================================================

(defn- lit-word [^String s]
  {:type :word :parts [{:type :lit :value s}]})

(defn- runtime-permit-check
  "If `:permit` is in opts, synthesize a `:call` AST from the
   already-expanded `(name & args)` argv and re-check it against the
   same rulesets. Catches commands whose name was dynamic at parse
   time (e.g. `$cmd /tmp` where $cmd resolved to `rm` at runtime).

   Returns nil if allowed (or no permit configured); returns the
   updated env (with :last-exit 126) if denied — the caller short-
   circuits on a non-nil return."
  [env name args opts]
  (when-let [permit-cfg (:permit opts)]
    (let [synth-call {:type :call :assigns []
                      :args  (mapv lit-word (cons name args))}
          synth-stmt {:type :stmt :cmd synth-call :redirs []
                      :bg? false :neg? false}
          synth-ast  {:type :program :stmts [synth-stmt]}
          result (permit/check (assoc permit-cfg :ast synth-ast))]
      (when (= :deny (:decision result))
        (let [denied (some (fn [pc] (when (= :deny (:decision pc)) pc))
                           (:per-call result))]
          (trace/record-denied! (:trace env)
                                {:type :denied
                                 :tool name
                                 :argv (vec (cons name args))
                                 :reason (:reason denied "?")
                                 :rule-id (:rule-id denied)})
          (write-line opts :err (str "muschel: runtime permit denied `" name
                                     "`: " (:reason denied "?") "\n"))
          (env/record-exit env 126))))))

(defn- run-external
  "Spawn an external command via the host. The host decides how to
   spawn — `babashka.process` on JVM, `child_process` on node, a
   virtual-tool lookup in the browser.

   Before spawning we run `runtime-permit-check` so commands whose
   name was statically unknown (dynamic via `$cmd`) get gated at the
   effect boundary."
  [env name args extra-env opts]
  (or (runtime-permit-check env name args opts)
      (let [h (:host opts)
            spawn-opts {:cmd  name
                        :args (vec args)
                        :dir  (:cwd env)
                        :extra-env (merge (env/to-process-env env) extra-env)
                        :in   (:in opts)
                        :out  (:out opts)
                        :err  (:err opts)
                        :interrupt-fn (:interrupt-fn env)
                        :trace (:trace env)}
            result (try (host/spawn h spawn-opts)
                        (catch #?(:clj Throwable :cljs :default) e
                          ;; Budget-exceeded interrupts must propagate
                          ;; up to run-and-capture's boundary, not be
                          ;; logged-and-swallowed.
                          (when (budget/budget-exceeded? e) (throw e))
                          (write-line opts :err
                                      (str name ": "
                                           #?(:clj (.getMessage ^Throwable e)
                                              :cljs (.-message e)) "\n"))
                          nil))]
        (if result
          (let [exit ((:wait result))]
            (env/record-exit env (or exit 0)))
          (env/record-exit env 127)))))                      ; command not found

(defn- run-call
  "Execute a :call AST (assigns + args). If args resolves to no
   command, naked assignments mutate env permanently."
  [env call opts]
  (let [;; Track whether expansion runs a cmd-subst; if it does,
        ;; env1's :last-exit reflects that AND we should propagate
        ;; on `$(false)`-style empty-call lines. Otherwise the call's
        ;; exit is 0 (or, for a naked assignment, 0).
        prev-exit (:last-exit env)
        [env1 fields] (expand-words env (:args call) opts)
        cmd-subst-exit (when (not= prev-exit (:last-exit env1))
                         (:last-exit env1))
        fields (if (and (= 1 (count fields)) (= "" (first fields)))
                 []
                 fields)]
    (cond
      ;; Naked assignments: `FOO=bar BAZ=qux` (no cmd) — permanent.
      ;; `FOO+=bar` appends to the existing value. Readonly violations
      ;; print to stderr and exit 1, matching bash. If the call had
      ;; args that all expanded away (e.g. `$(false)`), bash propagates
      ;; the LAST cmd-subst's exit; otherwise we record 0.
      (empty? fields)
      (let [[env' readonly-err? last-cs-exit]
            (reduce
             (fn [[env err? cs-exit] as]
               (cond
                 err? [env err? cs-exit]
                 (env/readonly? env (:name as))
                 (do (write-line opts :err (str (:name as) ": readonly variable\n"))
                     [env true cs-exit])
                 :else
                 (let [prev (:last-exit env)
                       [env' val] (expand-assign env (:value as) opts)
                       final     (if (:append? as)
                                   (str (env/get-var env' (:name as)) val)
                                   val)
                       ;; Track cmd-subst exit ONLY when expansion
                       ;; actually changed last-exit (cmd-subst ran).
                       new-cs (when (not= prev (:last-exit env'))
                                (:last-exit env'))]
                   [(env/set-var env' (:name as) final) false (or new-cs cs-exit)])))
             [env1 false cmd-subst-exit] (:assigns call))
            had-args? (seq (:args call))]
        (env/record-exit env' (cond readonly-err? 1
                                    had-args?    (or cmd-subst-exit 0)
                                    :else        (or last-cs-exit 0))))

      :else
      (let [name (first fields)
            args (rest fields)
            ;; Per-command env from assignments (visible to external
            ;; cmd only, not persisted in env). `+=` also appends here,
            ;; reading from the caller's env (not from previous extras).
            extra-env
            (reduce (fn [m as]
                      (let [[_ val] (expand-assign env1 (:value as) opts)
                            final   (if (:append? as)
                                      (str (env/get-var env1 (:name as)) val)
                                      val)]
                        (assoc m (:name as) final)))
                    {}
                    (:assigns call))
            ;; Function lookup
            f-body (env/lookup-fn env1 name)]
        (cond
          f-body
          ;; Function call:
          ;;  - push a fresh scope (for `local`)
          ;;  - swap pos-args to the call args
          ;;  - run the body (a cmd, not a stmt)
          ;;  - restore pos-args
          ;;  - pop scope (restores any locally-shadowed vars)
          ;;  - clear :returning? (return only exits the function)
          ;;
          ;; Prefix assignments (FOO=bar f) propagate INTO the function
          ;; body so `local`/non-local vars inside f see them; bash
          ;; sees these as exported-for-the-function. We set them in
          ;; the scoped env so they roll back on scope pop.
          (let [old (:pos-args env1)
                env2 (-> env1 env/push-scope (env/with-pos-args args))
                env2 (reduce-kv
                      (fn [e k v] (env/declare-local e k v))
                      env2 extra-env)
                env3 (exec-cmd env2 f-body opts)]
            (-> env3
                env/pop-scope
                (assoc :pos-args old)
                (dissoc :returning?)))

          (builtin? name)
          (let [started-at #?(:clj (System/currentTimeMillis) :cljs 0)
                env'       ((get builtins name) env1 args opts)
                exit       (or (:last-exit env') 0)]
            (trace/record-tool! (:trace env)
                                {:type :tool
                                 :name name
                                 :argv (vec (cons name args))
                                 :exit exit
                                 :duration-ms (- #?(:clj (System/currentTimeMillis) :cljs 0)
                                                 started-at)})
            env')

          :else
          (run-external env1 name args extra-env opts))))))

(defn run-argv
  "Dispatch a resolved argv (already expanded into a vector of strings)
   against the function table → builtins → external command. Public so
   emitted Clojure can call it without re-walking AST. `extra-env` is
   optional per-call exported vars (as from `FOO=bar cmd`)."
  ([env argv opts]            (run-argv env argv {} opts))
  ([env argv extra-env opts]
   (if (empty? argv)
     (env/record-exit env 0)
     (let [name (first argv)
           args (vec (rest argv))
           f-body (env/lookup-fn env name)]
       (cond
         f-body
         (let [old (:pos-args env)
               env1 (-> env env/push-scope (env/with-pos-args args))
               env1 (reduce-kv
                     (fn [e k v] (env/declare-local e k v))
                     env1 extra-env)
               env2 (exec-cmd env1 f-body opts)]
           (-> env2 env/pop-scope (assoc :pos-args old) (dissoc :returning?)))

         (builtin? name)
         ((get builtins name) env args opts)

         :else
         (run-external env name args extra-env opts))))))

;; ============================================================================
;; Binary: pipe / and / or
;; ============================================================================

(defn- run-binary [env binary opts]
  (case (:op binary)
    ;; bash: non-final operands of `&&`/`||` are errexit-protected.
    ;; The chain as a whole only trips errexit if the FINAL operand
    ;; ran AND was unprotected (i.e. parent didn't protect us). If
    ;; the chain short-circuits before reaching the final, no trip.
    ;;
    ;; We mark the binary stmt as :errexit-protected? so exec-stmt-body
    ;; doesn't arm post-hoc; whether the final operand armed is its
    ;; own concern (and stays in env).
    :and
    (let [env' (-> (exec-stmt env (:left binary)
                              (assoc opts :errexit-protected? true))
                   (dissoc :errexit-armed?))]
      (if (zero? (:last-exit env'))
        (exec-stmt env' (:right binary) opts)
        env'))

    :or
    (let [env' (-> (exec-stmt env (:left binary)
                              (assoc opts :errexit-protected? true))
                   (dissoc :errexit-armed?))]
      (if (zero? (:last-exit env'))
        env'
        (exec-stmt env' (:right binary) opts)))

    (:pipe :pipe-amp)
    (run-pipeline env binary opts)))

(defn- collect-pipeline-stmts
  "A right-folded `:binary :pipe` tree → flat sequence of stmts.
   Stops at any non-pipe op."
  [stmt]
  (let [c (:cmd stmt)]
    (if (and (= :binary (:type c))
             (#{:pipe :pipe-amp} (:op c)))
      (concat (collect-pipeline-stmts (:left c))
              (collect-pipeline-stmts (:right c)))
      [stmt])))

(defn- run-pipeline [env binary opts]
  ;; Collect the full chain (a | b | c | d), launch each as an async
  ;; task, chain stdouts to next stdin via host pipes. The host
  ;; decides concurrency: JVM uses futures + Java piped streams,
  ;; cljs may collect sequentially (output of N captured before
  ;; running N+1).
  (let [h (:host opts)
        stmts (collect-pipeline-stmts {:cmd binary})
        n (count stmts)
        pipe-pairs (vec (repeatedly (dec n) #(host/make-pipe h)))
        sources (mapv first pipe-pairs)    ; read-end of each pipe
        sinks   (mapv second pipe-pairs)   ; write-end of each pipe
        last-idx (dec n)
        tasks
        (vec
         (map-indexed
          (fn [i st]
            (let [in  (if (zero? i) (:in opts) (nth sources (dec i)))
                  out (if (= i last-idx) (:out opts) (nth sinks i))
                  err (if (and (= (:op binary) :pipe-amp) (not= i last-idx))
                        (nth sinks i)
                        (:err opts))
                  sub-opts (assoc opts :in in :out out :err err)]
              (host/async h
                          (fn []
                            (try
                              (let [env' (exec-stmt env st sub-opts)]
                                (when-not (= i last-idx)
                                  (host/close! h (nth sinks i)))
                                env')
                              (catch #?(:clj Throwable :cljs :default) t
                                (when-not (= i last-idx)
                                  (host/close! h (nth sinks i)))
                                (if (budget/budget-exceeded? t)
                                  (throw t)
                                  (env/record-exit env 1))))))))
          stmts))
        results (try (mapv #(host/await-async h %) tasks)
                     (catch #?(:clj Throwable :cljs :default) t
                       ;; Java futures wrap the worker's throw in
                       ;; ExecutionException. Unwrap so budget-exceeded
                       ;; comes through cleanly.
                       (let [cause #?(:clj (or (.getCause ^Throwable t) t)
                                      :cljs t)]
                         (if (budget/budget-exceeded? cause)
                           (throw cause)
                           (throw t)))))
        ;; Per POSIX (and default bash), pipeline exit = last cmd's exit.
        ;; pipefail uses the RIGHTMOST non-zero (bash semantics).
        last-exit (or (:last-exit (last results)) 0)
        final-exit (if (env/option env :pipefail)
                     (or (last (filter #(and % (not (zero? %)))
                                       (map :last-exit results)))
                         last-exit)
                     last-exit)]
    (env/record-exit env final-exit)))

;; ============================================================================
;; Compound commands
;; ============================================================================

(defn- run-if [env cmd opts]
  ;; POSIX: when an if has no else branch and the cond is false (or no
  ;; elif matches), the exit status is 0 — not the failed cond's
  ;; status. Only the body that ACTUALLY ran determines exit.
  ;; Conditions of `if`/`elif` are errexit-protected.
  (let [cond-opts (assoc opts :errexit-protected? true)
        env-cond (exec-stmts env (:cond cmd) cond-opts)]
    (cond
      (zero? (:last-exit env-cond))
      (exec-stmts env-cond (:then cmd) opts)

      (seq (:elifs cmd))
      (loop [env env-cond elifs (:elifs cmd)]
        (if (empty? elifs)
          (if (:else cmd)
            (exec-stmts env (:else cmd) opts)
            (env/record-exit env 0))
          (let [{:keys [cond then]} (first elifs)
                env-c (exec-stmts env cond cond-opts)]
            (if (zero? (:last-exit env-c))
              (exec-stmts env-c then opts)
              (recur env-c (rest elifs))))))

      (:else cmd)
      (exec-stmts env-cond (:else cmd) opts)

      :else
      (env/record-exit env-cond 0))))

(defn- handle-loop-iteration
  "After a loop body executes, inspect break/continue counters.
   Returns one of:
     [:next env]    — body finished, do next iteration (counter
                      decremented if continue=1 hit)
     [:done env]    — exit the loop (with counter possibly still >0
                      to break the outer loop too)
     [:abort env]   — propagate exit / return; caller exits loop too"
  [env-body]
  (cond
    (:exiting? env-body) [:abort env-body]
    (:returning? env-body) [:abort env-body]

    (pos? (or (:break-enclosing env-body) 0))
    [:done (update env-body :break-enclosing dec)]

    (pos? (or (:continue-enclosing env-body) 0))
    (let [env' (update env-body :continue-enclosing dec)]
      (if (pos? (:continue-enclosing env'))
        [:done env']     ; outer loop continues to break
        [:next env']))

    :else [:next env-body]))

(defn- with-in-loop [env body]
  (let [outer? (:in-loop? env)
        env (assoc env :in-loop? true)
        result (body env)]
    (assoc result :in-loop? outer?)))

(defn- run-for [env cmd opts]
  (let [vname (:var cmd)
        [env' words] (if (:iterate-positional? cmd)
                       [env (vec (:pos-args env))]    ; `for x; do ...` → $@
                       (expand-words env (:words cmd) opts))]
    (with-in-loop env'
      (fn [env-in-loop]
        (loop [env env-in-loop words words last-body-exit 0]
          (if (empty? words)
            (env/record-exit env last-body-exit)
            (let [env-iter (env/set-var env vname (first words))
                  env-body (exec-stmts env-iter (:body cmd) opts)
                  [k env-after] (handle-loop-iteration env-body)]
              (case k
                :abort env-after
                :done  env-after
                :next  (recur env-after (rest words) (:last-exit env-body))))))))))

(defn- run-while [env cmd opts]
  ;; POSIX: loop exit is the last body's exit, or 0 if body never ran.
  ;; While/until conditions are errexit-protected (matching bash).
  (let [cond-opts (assoc opts :errexit-protected? true)]
    (with-in-loop env
      (fn [env-in-loop]
        (loop [env env-in-loop last-body-exit 0 ran-body? false]
          (let [env-c (exec-stmts env (:cond cmd) cond-opts)]
            (if (zero? (:last-exit env-c))
              (let [env-b (exec-stmts env-c (:body cmd) opts)
                    [k env-after] (handle-loop-iteration env-b)]
                (case k
                  :abort env-after
                  :done  env-after
                  :next  (recur env-after (:last-exit env-b) true)))
              (env/record-exit env-c (if ran-body? last-body-exit 0)))))))))

(defn- run-until [env cmd opts]
  (let [cond-opts (assoc opts :errexit-protected? true)]
    (with-in-loop env
      (fn [env-in-loop]
        (loop [env env-in-loop last-body-exit 0 ran-body? false]
          (let [env-c (exec-stmts env (:cond cmd) cond-opts)]
            (if (not (zero? (:last-exit env-c)))
              (let [env-b (exec-stmts env-c (:body cmd) opts)
                    [k env-after] (handle-loop-iteration env-b)]
                (case k
                  :abort env-after
                  :done  env-after
                  :next  (recur env-after (:last-exit env-b) true)))
              (env/record-exit env-c (if ran-body? last-body-exit 0)))))))))

(defn- word-has-quoted-part?
  "True if `word` (AST) contains any :squoted, :dquoted, :escape part —
   meaning bash treats the corresponding text as LITERAL, not glob,
   when used as a pattern on the RHS of `==` / `!=` / `case`."
  [word]
  (boolean
   (some (fn [p] (#{:squoted :dquoted :escape :ansi-c-quoted} (:type p)))
         (:parts word))))

(defn- test-primary
  "Evaluate one 'primary' test expression — 1, 2, or 3 string args
   like `-f file`, `a == b`. Returns true/false bool. For `==`/`!=`
   inside `[[ ]]`, the right side is treated as a GLOB pattern (bash
   semantics) UNLESS the RHS AST word had any quoted parts, in which
   case it's a literal match."
  [env words double? rhs-quoted? opts]
  (let [h (:host opts)
        tmp-env (env/record-exit env 0)
        r (builtin-test tmp-env (vec words)
                        (assoc opts
                               :err (host/string-sink h)
                               :out (host/string-sink h)))]
    (cond
      (and double? (= 3 (count words)) (#{"==" "!="} (second words)))
      (let [[a op b] words
            match? (if rhs-quoted?
                     (= a b)
                     (let [pat (expand/glob->regex b)
                           ;; (?s) = DOTALL: `*` (→ `.*`) matches `\n`
                           ;; too, so `[[ "a\nb" == *b ]]` works.
                           rx  (re-pattern (str "(?s)^" pat "$"))]
                       (boolean (re-find rx a))))]
        (if (= op "==") match? (not match?)))

      :else
      (zero? (:last-exit r)))))

(defn- split-on [pred xs]
  "Split xs into groups separated by elements matching pred."
  (let [groups (volatile! [[]])]
    (doseq [x xs]
      (if (pred x)
        (vswap! groups conj [])
        (vswap! groups update (dec (count @groups)) conj x)))
    @groups))

(defn- eval-compound-test
  "Evaluate the inner expression of `[[ ... ]]`.

   `strs` is a vector of expanded strings (one per AST arg);
   `quoteds?` is the parallel vector of quoted-flag booleans so that
   `==` / `!=` know when to treat their RHS as literal."
  [env strs quoteds? double? opts]
  (let [or-groups (split-on #(= "||" %) strs)
        ;; Track grouping by index so we can read quoteds? alongside.
        idxs (vec (range (count strs)))
        or-idx-groups (split-on #(= "||" (nth strs %)) idxs)]
    (some
     (fn [[or-group or-idx-group]]
       (let [and-groups (split-on #(= "&&" %) or-group)
             and-idx-groups (split-on #(= "&&" (nth strs %)) or-idx-group)]
         (every?
          (fn [[and-group and-idx-group]]
            (let [[negated? body body-idxs]
                  (loop [n? false g and-group ig and-idx-group]
                    (if (= (first g) "!")
                      (recur (not n?) (rest g) (rest ig))
                      [n? (vec g) (vec ig)]))]
              (if (empty? body)
                false
                ;; For `a OP b` patterns, the RHS's quoted? is at the
                ;; last index of body-idxs.
                (let [rhs-q? (boolean (and (= 3 (count body))
                                           (nth quoteds? (last body-idxs))))
                      r (test-primary env body double? rhs-q? opts)]
                  (if negated? (not r) r)))))
          (map vector and-groups and-idx-groups))))
     (map vector or-groups or-idx-groups))))

(defn- expand-test-args
  "Expand each `[[ ]]` arg word to a single string (NO field-split, NO
   glob — bash semantics inside `[[ ]]`). Returns [env strs quoteds?]
   where quoteds? is the parallel vector of word-has-quoted-part?."
  [env words opts]
  (reduce (fn [[env strs qs] w]
            (let [[env' v] (expand/expand-assign-value env w
                                                       :cmd-subst (:cmd-subst opts)
                                                       :arith (:arith opts))]
              [env' (conj strs v) (conj qs (word-has-quoted-part? w))]))
          [env [] []]
          words))

(defn- run-test-bracket
  [env cmd opts]
  (case (:form cmd)
    :single (let [[env' args] (expand-words env (:args cmd) opts)]
              (builtin-test env' args opts))
    :double (let [[env' args quoteds?] (expand-test-args env (:args cmd) opts)
                  pass? (eval-compound-test env' (vec args) quoteds? true opts)]
              (env/record-exit env' (if pass? 0 1)))))

;; ============================================================================
;; Stmt dispatch
;; ============================================================================

(defn exec-cmd [env cmd opts]
  (case (:type cmd)
    :call         (run-call env cmd opts)
    :binary       (run-binary env cmd opts)
    :if           (run-if env cmd opts)
    :for          (run-for env cmd opts)
    :while        (run-while env cmd opts)
    :until        (run-until env cmd opts)
    :brace-group  (exec-stmts env (:body cmd) opts)
    :subshell     (let [child (env/fork env)
                        env' (exec-stmts child (:body cmd) opts)]
                    ;; Subshell mutations don't leak; only :last-exit
                    ;; propagates back.
                    (env/record-exit env (:last-exit env')))
    :function-def (env/define-fn (env/record-exit env 0) (:name cmd) (:body cmd))
    :test-bracket (run-test-bracket env cmd opts)
    :arith-cmd
    ;; ((expr)) — evaluate; exit status is 0 if result is nonzero, 1
    ;; if zero. Runtime arith errors (`((3/0))`) print to stderr and
    ;; exit 1, matching bash. Catch both ExceptionInfo (our raised)
    ;; and ArithmeticException (raw JVM div-by-zero).
    (try
      (let [[env' v] (arith/evaluate env (:expr cmd))]
        (env/record-exit env' (if (zero? v) 1 0)))
      (catch #?(:clj Throwable :cljs :default) e
        (write-line opts :err (str (or (.getMessage e) (str e)) "\n"))
        (env/record-exit env 1)))

    :c-for
    ;; for ((init; cond; update)); do body; done
    (let [{:keys [init cond update body]} cmd
          [env-i _] (if (str/blank? init) [env 0] (arith/evaluate env init))]
      (with-in-loop env-i
        (fn [env-in-loop]
          (loop [env env-in-loop last-body-exit 0 ran? false]
            (let [[env-c c] (if (str/blank? cond)
                              [env 1]
                              (arith/evaluate env cond))]
              (if (zero? c)
                (env/record-exit env-c (if ran? last-body-exit 0))
                (let [env-b (exec-stmts env-c body opts)
                      [k env-after] (handle-loop-iteration env-b)]
                  (case k
                    :abort env-after
                    :done  env-after
                    :next  (let [[env-u _] (if (str/blank? update)
                                             [env-after 0]
                                             (arith/evaluate env-after update))]
                             (recur env-u (:last-exit env-b) true))))))))))
    :case         (let [[env' word-val] (expand/expand-assign-value
                                         env (:word cmd)
                                         :cmd-subst (:cmd-subst opts)
                                         :arith (:arith opts))]
                    (loop [env env'
                           clauses (:clauses cmd)
                           force-run? false]
                      (if (empty? clauses)
                        (env/record-exit env 0)
                        (let [clause (first clauses)
                              ;; Expand each pattern (no field-split; first
                              ;; field if multiple). Skip patterns containing
                              ;; cmd-subst-side-effects — handled by expand.
                              ;; Each pattern carries its AST so we can
                              ;; check if it was quoted (then it's a
                              ;; literal match, not glob).
                              [env-p pats]
                              (reduce
                               (fn [[env acc] w]
                                 (let [[env' p] (expand/expand-assign-value
                                                 env w
                                                 :cmd-subst (:cmd-subst opts)
                                                 :arith (:arith opts))]
                                   [env' (conj acc {:s p
                                                    :quoted? (word-has-quoted-part? w)})]))
                               [env []]
                               (:patterns clause))
                              matches? (some (fn [{:keys [s quoted?]}]
                                               (if quoted?
                                                 (= s word-val)
                                                 (let [rx (re-pattern
                                                           (str "(?s)^"
                                                                (expand/glob->regex s)
                                                                "$"))]
                                                   (re-find rx word-val))))
                                             pats)]
                          (if (or matches? force-run?)
                            (let [env-b (exec-stmts env-p (:body clause) opts)]
                              (case (:terminator clause)
                                :semi-amp      (recur env-b (rest clauses) true)
                                :semi-semi-amp (recur env-b (rest clauses) false)
                                ;; default `;;` (or nil for last clause) → stop
                                env-b))
                            (recur env-p (rest clauses) false))))))
    (err/error! (str "exec: unknown cmd type " (:type cmd))
                {:type ::not-implemented})))

(defn- exec-stmt-body
  "Run the stmt's redirs + cmd synchronously. Returns updated env.
   Pulled out of `exec-stmt` so the background path can call it from
   inside a future without duplicating the redir-handling machinery.

   Catches ::expand/param-error (from `${VAR:?msg}`) as a clean exit
   with the error message on stderr — matches bash semantics."
  [env stmt opts]
  (let [stmt-redirs (:redirs stmt)
        cmd (:cmd stmt)
        cmd-redirs (:redirs cmd)
        all-redirs (vec (concat stmt-redirs cmd-redirs))
        ;; A `!`-negated stmt is errexit-protected — the user is
        ;; explicitly testing the inner cmd's status.
        opts (cond-> opts (:neg? stmt) (assoc :errexit-protected? true))
        [env-r opts-r close!] (apply-redirs env all-redirs opts)
        cmd-without-redirs (dissoc cmd :redirs)]
    (try
      (let [env' (cond
                   ;; A redirect raised (file not found, FS escape, …).
                   ;; apply-redirs already wrote the error to :err.
                   ;; Skip the command and exit 1.
                   (:redir-failed? env-r)
                   (-> env-r (dissoc :redir-failed?) (env/record-exit 1))

                   :else
                   (try
                     (exec-cmd env-r cmd-without-redirs opts-r)
                     (catch #?(:clj clojure.lang.ExceptionInfo
                               :cljs ExceptionInfo) e
                       (if (= :muschel.expand/param-error
                              (:type (ex-data e)))
                         (do (write-line opts-r :err (str (:msg (ex-data e)) "\n"))
                             (-> env-r (env/record-exit 1) (assoc :exiting? true)))
                         (throw e)))))]
        (let [env' (if (:neg? stmt)
                     (env/record-exit env' (if (zero? (:last-exit env')) 1 0))
                     env')
              ;; `&&`/`||` chains handle their own arming through
              ;; recursive exec-stmt of the final operand — auto-
              ;; arming at the binary stmt level would double-count
              ;; (and arm short-circuit cases that shouldn't trip).
              cmd-type (:type cmd)
              binary-and-or? (and (= :binary cmd-type)
                                  (#{:and :or} (:op cmd)))]
          (cond-> env'
            (and (env/option env' :errexit)
                 (not (zero? (or (:last-exit env') 0)))
                 (not (:errexit-protected? opts))
                 (not (:neg? stmt))
                 (not binary-and-or?))
            (assoc :errexit-armed? true))))
      (finally
        (close!)))))

(defn exec-stmt
  "Execute a :stmt AST. Returns updated env.
   For bg (`stmt & `), spawns the stmt's body in a future tracked in
   the session, then returns immediately with env's `:last-bg-pid`
   updated for `$!`."
  [env stmt opts]
  (if (:bg? stmt)
    (let [s (or (:session opts)
                (err/error! "`&` requires a session in :opts"
                            {:type ::no-session}))
          id (session/next-job-id s)
          ;; Bg job: on JVM, use a real Clojure future (so the
          ;; session helpers can use `realized?`/`deref`). On cljs,
          ;; use a delay-like wrapper since browsers are single-
          ;; threaded.
          exit-future
          #?(:clj
             (future
               (try (:last-exit (exec-stmt-body env stmt opts))
                    (catch Throwable _ 1)))
             :cljs
             (let [done? (volatile! false)
                   v     (volatile! nil)]
               (reify
                 IDeref
                 (-deref [_]
                   (when-not @done?
                     (vreset! v
                              (try (:last-exit (exec-stmt-body env stmt opts))
                                   (catch :default _ 1)))
                     (vreset! done? true))
                   @v)
                 IPending
                 (-realized? [_] @done?))))
          synth-pid (#?(:clj Math/abs :cljs js/Math.abs)
                     (long (hash exit-future)))
          handle (session/->JobHandle id synth-pid {:future exit-future}
                                      exit-future)]
      (session/-track-job! s handle)
      ;; Bg launch resets $? to 0 — bash convention is the launch
      ;; itself succeeded regardless of what the bg job will exit with.
      (-> env (env/record-bg-pid synth-pid) (env/record-exit 0)))
    (exec-stmt-body env stmt opts)))

(defn- stop-propagating?
  "True when one of `exit` / `return` / `break` / `continue` is active
   and exec-stmts should short-circuit, letting the enclosing
   construct (script / function / loop) handle it."
  [env]
  (or (:exiting? env)
      (:returning? env)
      (pos? (or (:break-enclosing env) 0))
      (pos? (or (:continue-enclosing env) 0))))

(defn exec-stmts
  "Run statements in sequence, threading env through.

   `set -e` (errexit) handling: a stmt that ARMS errexit (an
   unprotected non-zero exit) makes the loop bail out. Protected
   contexts (`if` / `while` / `until` cond, `&&` / `||` non-final
   operand, `!`-negated, opts `:errexit-protected? true`) suppress
   the arming via the env's `:errexit-armed?` flag, which is set by
   exec-stmt-body and read+cleared here."
  [env stmts opts]
  (reduce (fn [env st]
            (budget/check-interrupt! env)
            (cond
              (stop-propagating? env) (reduced env)
              :else
              (let [env' (exec-stmt env st opts)]
                (if (and (env/option env' :errexit)
                         (:errexit-armed? env'))
                  (reduced (-> env' (dissoc :errexit-armed?)
                               (assoc :exiting? true)))
                  (dissoc env' :errexit-armed?)))))
          env
          stmts))

;; ============================================================================
;; Public API
;; ============================================================================

(defn run
  "Execute `src` (a bash string) or a parsed program AST against `env`.

   Returns `{:env new-env :exit int :session sess}`. Streams default
   to the host process stdin/stdout/stderr.

   Session handling:
   - If `:session` is in opts, that session owns the env. It is
     SNAPSHOTTED at call time; the result-env is written back when
     `run` completes. Bg jobs from this call are tracked there too.
     This makes the session the time-travelable state-owner.
   - If no session is provided, an ephemeral AtomSession is created
     for the call. Bg jobs survive as long as some caller holds the
     `:session` from the result.

   `env` is still required as the explicit starting point (for
   one-shot library use without a session). When a session is also
   provided, `env` overrides the session's current value at start —
   pass `(session/-env sess)` to thread without override."
  ([env src-or-ast] (run env src-or-ast {}))
  ([env src-or-ast {:keys [in out err session permit
                           interrupt-fn timeout-ms trace] :as opts}]
   (let [ast (if (string? src-or-ast) (parse/parse src-or-ast) src-or-ast)
         sess (or session (session/atom-session env))
         ;; Resource-budget interrupt (cooperative). The caller can
         ;; pass `interrupt-fn` directly OR a `timeout-ms` (we
         ;; synthesise a deadline-based interrupt for them); when both
         ;; are present we combine.
         ifn (budget/combine interrupt-fn
                             (when timeout-ms (budget/deadline-interrupt timeout-ms)))
         ;; Optional introspection state. When opted in, every tool
         ;; call, FS op, and permit denial is recorded.
         trace-state (trace/coerce-options trace)
         ;; Optional permit check before any exec.
         permit-result (when permit (permit/check (assoc permit :ast ast)))]
     (if (and permit-result (= :deny (:decision permit-result)))
       (let [denied (first (filter #(= :deny (:decision %))
                                   (:per-call permit-result)))]
         ;; Even though we never exec'd, surface the deny via trace so
         ;; the caller sees WHY the run was refused.
         (when trace-state
           (doseq [pc (filter #(= :deny (:decision %)) (:per-call permit-result))]
             (trace/record-denied! trace-state
                                   {:type :denied
                                    :tool (or (:tool pc) (some-> pc :call :args first :parts first :value))
                                    :argv (mapv #(some-> % :parts first :value) (some-> pc :call :args))
                                    :reason (:reason pc "?")
                                    :rule-id (:rule-id pc)})))
         (cond-> {:env env
                  :exit 126                              ; convention: permission denied
                  :session sess
                  :permit permit-result
                  :denied-reason (:reason denied)}
           trace-state (assoc :trace (trace/snapshot trace-state))))
       (let [h (or (:host opts) (default-host))
             ;; A sandbox-aware host carries `:fs` (muschel.fs handle).
             ;; Thread it into env so expand's pathname expansion walks
             ;; the FS tree instead of leaking to babashka.fs against
             ;; real disk. Also align env's :cwd with the FS's notion
             ;; of cwd so relative-path resolution stays consistent.
             host-fs (when h (try (:fs h) (catch #?(:clj Throwable :cljs :default) _ nil)))
             ;; If tracing is opted in, wrap the host's FS so every
             ;; protocol op records an event. The inner FS stays
             ;; unchanged — containment isn't affected.
             traced-fs (when (and trace-state host-fs)
                         (fs.traced/wrap host-fs trace-state))
             effective-fs (or traced-fs host-fs)
             env (cond-> env
                   effective-fs                                (assoc :fs effective-fs)
                   (and effective-fs (not= (:cwd env) (mfs/cwd effective-fs)))
                   (assoc :cwd (mfs/cwd effective-fs)
                          :prev-cwd (mfs/cwd effective-fs))
                   ifn                                         (assoc :interrupt-fn ifn)
                   trace-state                                 (assoc :trace trace-state))
             opts' (cond-> {:in  (or in #?(:clj System/in :cljs nil))
                            :out (or out #?(:clj System/out :cljs (host/string-sink h)))
                            :err (or err #?(:clj System/err :cljs (host/string-sink h)))
                            :session sess
                            :host h}
                     permit (assoc :permit permit))
             opts'' (merge opts' (expand-opts opts'))
             env' (exec-stmts env (:stmts ast) opts'')]
         ;; If a session was passed in by the caller, write the result back
         ;; so the session reflects the new state (time-travel pivot).
         (when session
           (session/-swap-env! session (constantly env')))
         (cond-> {:env env' :exit (:last-exit env') :session sess}
           permit-result (assoc :permit permit-result)
           trace-state   (assoc :trace (trace/snapshot trace-state))))))))

(defn run-and-capture
  "Like `run` but captures stdout and stderr as strings via the host."
  ([env src-or-ast] (run-and-capture env src-or-ast {}))
  ([env src-or-ast opts]
   (let [h (or (:host opts) (default-host))
         out-buf (host/string-sink h)
         err-buf (host/string-sink h)
         {:keys [env exit session permit denied-reason trace]}
         (run env src-or-ast
              (merge opts {:host h :out out-buf :err err-buf}))]
     (cond-> {:env env
              :exit exit
              :session session
              :stdout (host/sink->string h out-buf)
              :stderr (host/sink->string h err-buf)}
       permit        (assoc :permit permit)
       denied-reason (assoc :denied-reason denied-reason)
       trace         (assoc :trace trace)))))

;; Late-bound registration: lets the `sh` builtin call back into the
;; executor without a static require of `muschel.exec` from
;; `muschel.builtins.posix` (which would form a cycle), and without
;; the JVM-only `(require …) + (resolve …)` trick (which CLJS forbids).
(rt/register! {:parse   parse/parse
               :run     run-and-capture
               :new-env env/new-env})
