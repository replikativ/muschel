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
            [muschel.env :as env]
            [muschel.errors :as err]
            [muschel.expand :as expand]
            [muschel.host :as host]
            #?(:clj [muschel.host.jvm :as host.jvm])
            [muschel.parse :as parse]
            [muschel.permit :as permit]
            [muschel.session :as session]))

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
       (let [ast (parse/parse body)
             sb-out (host/string-sink h)
             nested-opts (assoc base-opts :out sb-out)
             env' (exec-stmts env (:stmts ast) nested-opts)]
         [env' (host/sink->string h sb-out)]))
     :arith
     (fn [env expr]
       (let [[env' v] (arith/evaluate env expr)]
         [env' v]))}))

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
  (host/open-file-sink h (resolve-path env path) append?))

(defn- open-input [h env path]
  (host/open-file-source h (resolve-path env path)))

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
   where close!-fn closes everything in reverse order."
  [env redirs opts]
  (let [closers (volatile! [])
        [env' opts']
        (reduce (fn [[env opts] r]
                  (let [[env' opts' c] (apply-redir env r opts)]
                    (vswap! closers conj c)
                    [env' opts']))
                [env opts]
                redirs)]
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
    :else
    (let [path (first args)]
      (try
        (let [target (if (or (nil? path) (= "" path))
                       (env/get-var env "HOME")
                       path)]
          (when (or (nil? target) (= "" target))
            (write-line opts :err "cd: HOME not set\n")
            (env/record-exit env 1))
          (let [env' (env/cd env target)]
            (env/record-exit env' 0)))
        (catch #?(:clj Throwable :cljs :default) e
          (write-line opts :err (str "cd: " (.getMessage e) "\n"))
          (env/record-exit env 1))))))

(defn- builtin-pwd
  [env _args opts]
  (write-line opts :out (str (:cwd env) "\n"))
  (env/record-exit env 0))

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
  [env args _opts]
  (env/record-exit (reduce env/unset-var env args) 0))

(defn- builtin-set
  [env args _opts]
  ;; Minimal: `set -e/-u/-x/+e/+u/+x` toggle options; rest replaces
  ;; positional params.
  (let [[opt-args rest-args] (split-with #(or (str/starts-with? % "-")
                                              (str/starts-with? % "+")) args)
        env' (reduce
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
                       e))
                   e flags)))
              env opt-args)
        env'' (if (seq rest-args) (env/with-pos-args env' rest-args) env')]
    (env/record-exit env'' 0)))

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
      (assoc env (if (= name "break") :break-enclosing :continue-enclosing) n))))

(defn- builtin-break    [env args opts] (builtin-break-or-continue "break"    env args opts))
(defn- builtin-continue [env args opts] (builtin-break-or-continue "continue" env args opts))

(defn- builtin-return
  "`return [N]` — exit a function with status N (default $?).
   Outside a function this would normally error, but bash allows it
   inside a sourced file. For simplicity we set :returning? regardless;
   the function-call wrapper clears it."
  [env args opts]
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
          (assoc :returning? true)))))

(defn- printf-format
  "Format args according to `fmt` (a single bash printf format string).
   Returns [output-string args-remaining error?]. The args-remaining
   slice lets the caller loop and reuse the format until args are
   exhausted (bash printf semantics).

   Supports: %s %d %i %u %c %x %X %o %b %q %% with width / precision
   modifiers and the `-` left-align flag. Escapes (\\n \\t ...) in the
   format string are decoded via the ANSI-C table."
  [^String fmt args]
  (let [n (count fmt)
        out (volatile! [])
        push! (fn [s] (vswap! out conj (str s)))
        decode-escape (fn [s] (expand/decode-ansi-c s))
        consume-non-percent (fn [i]
                              (loop [j i acc []]
                                (cond
                                  (>= j n) [(apply str acc) j]
                                  (= \% (.charAt fmt j))
                                  [(apply str acc) j]
                                  :else
                                  (recur (inc j) (conj acc (str (.charAt fmt j)))))))
        format-int (fn [^String fmt-spec ^long n]
                     ;; Java's String/format works for %d %x %X %o etc.
                     (try (fmt fmt-spec n) (catch #?(:clj Throwable :cljs :default) _ (str n))))
        format-str (fn [^String fmt-spec ^String s]
                     (try (fmt fmt-spec s) (catch #?(:clj Throwable :cljs :default) _ s)))
        parse-int-or (fn [s] (try (parse-int* (str/trim s))
                                  (catch #?(:clj Exception :cljs :default) _ 0)))]
    (loop [i 0 args args]
      (cond
        (>= i n) [(apply str @out) args nil]

        (not= \% (.charAt fmt i))
        (let [[chunk j] (consume-non-percent i)]
          (push! (decode-escape chunk))
          (recur j args))

        :else
        ;; At `%`. Parse flags + width + precision + conversion char.
        (let [j (inc i)
              ;; consume flags
              [j flags] (loop [k j fs ""]
                          (let [c (when (< k n) (.charAt fmt k))]
                            (if (#{\- \+ \space \0 \# \'} (or c \space))
                              (recur (inc k) (str fs c))
                              [k fs])))
              ;; consume width
              [j width] (loop [k j ws ""]
                          (let [c (when (< k n) (.charAt fmt k))]
                            (if (and c (and c (re-find #"[0-9]" (str c))))
                              (recur (inc k) (str ws c))
                              [k ws])))
              ;; consume precision
              [j prec] (if (and (< j n) (= \. (.charAt fmt j)))
                         (loop [k (inc j) ps ""]
                           (let [c (when (< k n) (.charAt fmt k))]
                             (if (and c (and c (re-find #"[0-9]" (str c))))
                               (recur (inc k) (str ps c))
                               [k ps])))
                         [j nil])
              conv (when (< j n) (.charAt fmt j))
              fmt-spec (str "%" flags width (when prec (str "." prec))
                            (when conv (str conv)))]
          (case conv
            \% (do (push! "%") (recur (inc j) args))
            nil [(apply str @out) args "missing format char"]
            \s (let [a (or (first args) "")]
                 (push! (format-str fmt-spec a))
                 (recur (inc j) (rest args)))
            \c (let [a (or (first args) "")]
                 (push! (if (empty? a) "" (subs a 0 1)))
                 (recur (inc j) (rest args)))
            \b (let [a (or (first args) "")]
                 (push! (decode-escape a))
                 (recur (inc j) (rest args)))
            \q (let [a (or (first args) "")]
                 ;; minimal shell-quote: single-quote, escaping embedded '
                 (push! (str "'" (str/replace a "'" "'\\''") "'"))
                 (recur (inc j) (rest args)))
            (\d \i \u \o \x \X)
            (let [a (or (first args) "0")
                  num (parse-int-or a)]
              (push! (format-int fmt-spec num))
              (recur (inc j) (rest args)))
            ;; Unknown conv
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
                     e (if value
                         (env/set-var e name value)
                         (assoc-in e [:vars name]
                                   {:value "" :exported? false
                                    :readonly? false}))
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
                  env' (if (seq call-args)
                         (env/with-pos-args env (vec call-args))
                         env)
                  env'' (exec-stmts env' (:stmts ast) opts)
                  env''' (dissoc env'' :returning?)]
              (if (seq call-args)
                (assoc env''' :pos-args old-args)
                env'''))
            (catch #?(:clj Throwable :cljs :default) e
              (write-line opts :err (str "source: " (.getMessage e) "\n"))
              (env/record-exit env 1))))))))

(declare ^:private builtins)

(defn- builtin-type
  "`type CMD ...` — report how each CMD would be resolved.
   Output formats (bash-style):
     CMD is a shell builtin
     CMD is a function
     CMD is /path/to/cmd
     bash: type: CMD: not found    (stderr; exit nonzero)"
  [env args opts]
  (if (empty? args)
    (env/record-exit env 0)
    (let [any-bad? (volatile! false)]
      (doseq [name args]
        (cond
          (contains? builtins name)
          (write-line opts :out (str name " is a shell builtin\n"))

          (env/lookup-fn env name)
          (write-line opts :out (str name " is a function\n"))

          :else
          ;; Search PATH for the executable via host
          (let [h (:host opts)
                path-env (env/get-var env "PATH")
                paths (str/split (or path-env "") #":")
                found (some (fn [p]
                              (let [candidate (str p "/" name)]
                                (when (and (host/file-exists? h candidate)
                                           (host/file-executable? h candidate))
                                  candidate)))
                            paths)]
            (if found
              (write-line opts :out (str name " is " found "\n"))
              (do (write-line opts :err (str "type: " name ": not found\n"))
                  (vreset! any-bad? true))))))
      (env/record-exit env (if @any-bad? 1 0)))))

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
  [env args _opts]
  ;; `let expr [expr...]` — evaluate each arithmetic expression.
  ;; Exit status is 0 iff the LAST expression's value is non-zero.
  (let [[env' last-v]
        (reduce (fn [[env _] expr]
                  (arith/evaluate env expr))
                [env 0]
                args)]
    (env/record-exit env' (if (zero? last-v) 1 0))))

(defn- builtin-test
  "POSIX `test` / `[`. Builtin to avoid spawning a subprocess per
   iteration of a while/until loop.

   Supports the common forms:
     -e/-f/-d/-r/-w/-x/-s/-L PATH   file tests
     -z/-n STR                       empty / non-empty string
     STR1 = STR2 / != STR2           string compare
     N1 -eq -ne -lt -le -gt -ge N2   integer compare
     ! EXPR                          negate (only single-arg form here)
     no args                         false (exit 1)

   For `[`, the final arg must be `]`; we strip + validate."
  [env args opts]
  (let [args (if (= (last args) "]") (vec (butlast args)) (vec args))
        true!  (fn [] (env/record-exit env 0))
        false! (fn [] (env/record-exit env 1))
        err!   (fn [msg]
                 (write-line opts :err (str "test: " msg "\n"))
                 (env/record-exit env 2))
        ;; All file tests go through host so the cwd is honored
        ;; (resolved via host/resolve-path).
        h (:host opts)
        info     (fn [p] (host/file-info h (resolve-path env p)))
        file-exists? (fn [p] (:exists? (info p)))
        regular?     (fn [p] (:file? (info p)))
        dir?         (fn [p] (:dir? (info p)))
        readable?    (fn [p] (:readable? (info p)))
        writable?    (fn [p] (:writable? (info p)))
        executable?  (fn [p] (:executable? (info p)))
        nonempty?    (fn [p] (let [i (info p)]
                               (and (:exists? i) (pos? (or (:size i) 0)))))
        symlink?     (fn [p] (:symlink? (info p)))
        parse-int-or (fn [s fallback]
                       (try (parse-int* s) (catch #?(:clj Exception :cljs :default) _ fallback)))]
    (cond
      (empty? args) (false!)

      ;; ! EXPR — negate; recurse on the rest
      (and (= (first args) "!") (seq (rest args)))
      (let [inner-env (builtin-test env (vec (rest args)) opts)]
        (env/record-exit env (if (zero? (:last-exit inner-env)) 1 0)))

      ;; Single-arg form: `test STR` → true iff STR is non-empty
      (= 1 (count args))
      (if (not= "" (first args)) (true!) (false!))

      ;; Two-arg form: unary op + value
      (= 2 (count args))
      (let [[op v] args]
        (case op
          "-e" (if (file-exists? v) (true!) (false!))
          "-f" (if (regular? v) (true!) (false!))
          "-d" (if (dir? v) (true!) (false!))
          "-r" (if (readable? v) (true!) (false!))
          "-w" (if (writable? v) (true!) (false!))
          "-x" (if (executable? v) (true!) (false!))
          "-s" (if (nonempty? v) (true!) (false!))
          ("-L" "-h") (if (symlink? v) (true!) (false!))
          "-z" (if (= "" v) (true!) (false!))
          "-n" (if (not= "" v) (true!) (false!))
          ;; -v VAR — true if VAR is declared (even empty)
          "-v" (if (env/declared? env v) (true!) (false!))
          ;; set -o NAME option check
          "-o" (if (env/option env (keyword v)) (true!) (false!))
          ;; Unix-specific file tests we can't portably check in Java —
          ;; return false (matches bash for empty/missing operand).
          ("-b" "-c" "-g" "-k" "-p" "-S" "-u" "-N" "-O" "-G" "-t")
          (false!)
          (err! (str "unknown unary operator: " op))))

      ;; Three-arg form: STR op STR  /  NUM op NUM
      (= 3 (count args))
      (let [[a op b] args
            file-age (fn [p] (or (host/file-mtime-ms h (resolve-path env p)) 0))
            same-file? (fn [a b]
                         (try (= (host/resolve-path (:cwd env) a)
                                 (host/resolve-path (:cwd env) b))
                              (catch #?(:clj Throwable :cljs :default) _ false)))]
        (case op
          ("=" "==") (if (= a b) (true!) (false!))
          "!="       (if (not= a b) (true!) (false!))
          ;; Lexicographic string compare (bash semantics for `<` `>` in [[ ]])
          "<"        (if (neg? (compare a b)) (true!) (false!))
          ">"        (if (pos? (compare a b)) (true!) (false!))
          "-eq"      (if (= (parse-int-or a 0) (parse-int-or b 0)) (true!) (false!))
          "-ne"      (if (not= (parse-int-or a 0) (parse-int-or b 0)) (true!) (false!))
          "-lt"      (if (< (parse-int-or a 0) (parse-int-or b 0)) (true!) (false!))
          "-le"      (if (<= (parse-int-or a 0) (parse-int-or b 0)) (true!) (false!))
          "-gt"      (if (> (parse-int-or a 0) (parse-int-or b 0)) (true!) (false!))
          "-ge"      (if (>= (parse-int-or a 0) (parse-int-or b 0)) (true!) (false!))
          ;; File ops
          "-ef"      (if (same-file? a b) (true!) (false!))
          "-nt"      (if (> (file-age a) (file-age b)) (true!) (false!))
          "-ot"      (if (< (file-age a) (file-age b)) (true!) (false!))
          (err! (str "unknown binary operator: " op))))

      :else (err! "too many arguments (compound expressions not yet supported)"))))

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
  [env args opts]
  ;; `wait`           — wait for all bg jobs
  ;; `wait <pid>`     — wait for given pid
  ;; `wait %<job-id>` — wait for that job
  (if-let [s (:session opts)]
    (let [targets (if (empty? args)
                    (session/-jobs s)
                    (keep #(find-job s %) args))]
      (let [last-exit (reduce (fn [_ j] (session/await-job j))
                              0 targets)]
        (env/record-exit env last-exit)))
    (env/record-exit env 0)))

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
                        :err  (:err opts)}
            result (try (host/spawn h spawn-opts)
                        (catch #?(:clj Throwable :cljs :default) e
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
  (let [;; Expand args first (env may mutate from $X cmd-subst etc.)
        [env1 fields] (expand-words env (:args call) opts)]
    (cond
      ;; Naked assignments: `FOO=bar BAZ=qux` (no cmd) — permanent.
      (empty? fields)
      (let [env'
            (reduce
             (fn [env as]
               (let [[env' val] (expand-assign env (:value as) opts)]
                 (env/set-var env' (:name as) val)))
             env1 (:assigns call))]
        (env/record-exit env' 0))

      :else
      (let [name (first fields)
            args (rest fields)
            ;; Per-command env from assignments (visible to external
            ;; cmd only, not persisted in env).
            extra-env
            (reduce (fn [m as]
                      (let [[_ val] (expand-assign env1 (:value as) opts)]
                        (assoc m (:name as) val)))
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
          ((get builtins name) env1 args opts)

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
    :and
    (let [env' (exec-stmt env (:left binary) opts)]
      (if (zero? (:last-exit env'))
        (exec-stmt env' (:right binary) opts)
        env'))

    :or
    (let [env' (exec-stmt env (:left binary) opts)]
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
                              (catch #?(:clj Throwable :cljs :default) _
                                (when-not (= i last-idx)
                                  (host/close! h (nth sinks i)))
                                (env/record-exit env 1)))))))
          stmts))
        results (mapv #(host/await-async h %) tasks)
        ;; Per POSIX (and default bash), pipeline exit = last cmd's exit.
        ;; pipefail option uses leftmost non-zero.
        last-exit (or (:last-exit (last results)) 0)
        final-exit (if (env/option env :pipefail)
                     (or (some #(when (and % (not (zero? %))) %)
                               (map :last-exit results))
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
  (let [env-cond (exec-stmts env (:cond cmd) opts)]
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
                env-c (exec-stmts env cond opts)]
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
  (with-in-loop env
    (fn [env-in-loop]
      (loop [env env-in-loop last-body-exit 0 ran-body? false]
        (let [env-c (exec-stmts env (:cond cmd) opts)]
          (if (zero? (:last-exit env-c))
            (let [env-b (exec-stmts env-c (:body cmd) opts)
                  [k env-after] (handle-loop-iteration env-b)]
              (case k
                :abort env-after
                :done  env-after
                :next  (recur env-after (:last-exit env-b) true)))
            (env/record-exit env-c (if ran-body? last-body-exit 0))))))))

(defn- run-until [env cmd opts]
  (with-in-loop env
    (fn [env-in-loop]
      (loop [env env-in-loop last-body-exit 0 ran-body? false]
        (let [env-c (exec-stmts env (:cond cmd) opts)]
          (if (not (zero? (:last-exit env-c)))
            (let [env-b (exec-stmts env-c (:body cmd) opts)
                  [k env-after] (handle-loop-iteration env-b)]
              (case k
                :abort env-after
                :done  env-after
                :next  (recur env-after (:last-exit env-b) true)))
            (env/record-exit env-c (if ran-body? last-body-exit 0))))))))

(defn- test-primary
  "Evaluate one 'primary' test expression — 1, 2, or 3 string args
   like `-f file`, `a == b`. Returns true/false bool. For `==`/`!=`
   inside `[[ ]]`, the right side is treated as a GLOB pattern (bash
   semantics)."
  [env words double? opts]
  (let [h (:host opts)
        tmp-env (env/record-exit env 0)
        r (builtin-test tmp-env (vec words)
                        ;; Suppress error output during compound eval
                        (assoc opts
                               :err (host/string-sink h)
                               :out (host/string-sink h)))]
    (cond
      ;; In [[ ]], == and != do PATTERN matching, not literal compare.
      (and double? (= 3 (count words)) (#{"==" "!="} (second words)))
      (let [[a op b] words
            pat (expand/glob->regex b)
            rx (re-pattern (str "^" pat "$"))
            match? (boolean (re-find rx a))]
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
  [env strs double? opts]
  (let [or-groups (split-on #(= "||" %) strs)]
    (some
     (fn [or-group]
       (let [and-groups (split-on #(= "&&" %) or-group)]
         (every?
          (fn [and-group]
            (let [[negated? body]
                  (loop [n? false g and-group]
                    (if (= (first g) "!")
                      (recur (not n?) (rest g))
                      [n? (vec g)]))]
              (if (empty? body)
                false
                (let [r (test-primary env body double? opts)]
                  (if negated? (not r) r)))))
          and-groups)))
     or-groups)))

(defn- run-test-bracket
  [env cmd opts]
  (let [[env' args] (expand-words env (:args cmd) opts)]
    (case (:form cmd)
      :single (builtin-test env' args opts)
      :double (let [pass? (eval-compound-test env' (vec args) true opts)]
                (env/record-exit env' (if pass? 0 1))))))

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
    ;; ((expr)) — evaluate; exit status is 0 if result is nonzero, 1 if zero
    (let [[env' v] (arith/evaluate env (:expr cmd))]
      (env/record-exit env' (if (zero? v) 1 0)))

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
                              [env-p pats]
                              (reduce
                               (fn [[env acc] w]
                                 (let [[env' p] (expand/expand-assign-value
                                                 env w
                                                 :cmd-subst (:cmd-subst opts)
                                                 :arith (:arith opts))]
                                   [env' (conj acc p)]))
                               [env []]
                               (:patterns clause))
                              matches? (some (fn [pat]
                                               (let [rx (re-pattern
                                                         (str "^"
                                                              (expand/glob->regex pat)
                                                              "$"))]
                                                 (re-find rx word-val)))
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
        [env-r opts-r close!] (apply-redirs env all-redirs opts)
        cmd-without-redirs (dissoc cmd :redirs)]
    (try
      (let [env' (try
                   (exec-cmd env-r cmd-without-redirs opts-r)
                   (catch #?(:clj clojure.lang.ExceptionInfo
                             :cljs ExceptionInfo) e
                     (if (= :muschel.expand/param-error
                            (:type (ex-data e)))
                       (do (write-line opts-r :err (str (:msg (ex-data e)) "\n"))
                           (-> env-r (env/record-exit 1) (assoc :exiting? true)))
                       (throw e))))]
        (if (:neg? stmt)
          (env/record-exit env' (if (zero? (:last-exit env')) 1 0))
          env'))
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

(defn exec-stmts [env stmts opts]
  (reduce (fn [env st]
            (if (stop-propagating? env)
              (reduced env)
              (exec-stmt env st opts)))
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
  ([env src-or-ast {:keys [in out err session permit] :as opts}]
   (let [ast (if (string? src-or-ast) (parse/parse src-or-ast) src-or-ast)
         sess (or session (session/atom-session env))
         ;; Optional permit check before any exec.
         permit-result (when permit (permit/check (assoc permit :ast ast)))]
     (if (and permit-result (= :deny (:decision permit-result)))
       (let [denied (first (filter #(= :deny (:decision %))
                                   (:per-call permit-result)))]
         {:env env
          :exit 126                                       ; convention: permission denied
          :session sess
          :permit permit-result
          :denied-reason (:reason denied)})
       (let [h (or (:host opts) (default-host))
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
           permit-result (assoc :permit permit-result)))))))

(defn run-and-capture
  "Like `run` but captures stdout and stderr as strings via the host."
  ([env src-or-ast] (run-and-capture env src-or-ast {}))
  ([env src-or-ast opts]
   (let [h (or (:host opts) (default-host))
         out-buf (host/string-sink h)
         err-buf (host/string-sink h)
         {:keys [env exit session]}
         (run env src-or-ast
              (merge opts {:host h :out out-buf :err err-buf}))]
     {:env env
      :exit exit
      :session session
      :stdout (host/sink->string h out-buf)
      :stderr (host/sink->string h err-buf)})))
