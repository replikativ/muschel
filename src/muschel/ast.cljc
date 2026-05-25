(ns muschel.ast
  "Shape of the muschel AST + predicates + walk/zip helpers.

   Every node is a plain map with a `:type` keyword and node-specific
   fields. Source-position fields (`:line :col :offset :end-line
   :end-col :end-offset`) are present on parser-built nodes; helpers
   here treat them as optional.

   ## Node shapes

   ### Root

       {:type :program  :stmts [<stmt>]}

   ### Statement wrapper

   A statement carries one command plus per-statement flags and
   redirections that apply to the whole command (a `{ cmd1; cmd2; } > out`
   redirection attaches to the brace-group statement). The :cmd may be
   any of the command shapes below.

       {:type :stmt
        :cmd <cmd>
        :redirs [<redir>]   ; empty if none
        :bg?   bool         ; true if terminated by `&`
        :neg?  bool         ; true if pipeline starts with `!`
        :line :col :offset ...}

   ### Commands

       ;; Simple command: assignments + args
       {:type :call
        :assigns [<assign>]
        :args    [<word>]}

       ;; Binary: && || | |&  — left-associative
       {:type :binary
        :op    :and | :or | :pipe | :pipe-amp
        :left  <stmt>
        :right <stmt>}

       ;; Compound commands
       {:type :if   :cond [<stmt>] :then [<stmt>]
                    :elifs [{:cond [<stmt>] :then [<stmt>]}]
                    :else  [<stmt>]}        ; nil if absent
       {:type :for       :var <name> :words [<word>] :body [<stmt>]}
       {:type :c-for     :init <expr> :cond <expr> :update <expr> :body [<stmt>]}
       {:type :while     :cond [<stmt>] :body [<stmt>]}
       {:type :until     :cond [<stmt>] :body [<stmt>]}
       {:type :case      :word <word> :clauses [<case-clause>]}
       {:type :brace-group  :body [<stmt>]}
       {:type :subshell     :body [<stmt>]}
       {:type :arith-cmd    :expr <str>}
       {:type :function-def :name <name> :body <stmt>}
       {:type :test-bracket :form :single | :double :args [<word>]}

   ### Words

       {:type :word :parts [<word-part>]}

   #### Word parts

       {:type :lit     :value <str>}
       {:type :squoted :value <str>}
       {:type :dquoted :parts [<dquoted-part>]}   ; same shapes as word-part minus squoted/dquoted/tilde
       {:type :var-ref :braced false :name <str>}
       {:type :var-ref :braced false :name <str> :special?   true}  ; $? $$ $# $@ $*
       {:type :var-ref :braced false :name <str> :positional? true} ; $0-$9
       {:type :var-ref :braced true  :raw <str>}                    ; ${...} — inner string for expand to dissect
       {:type :cmd-subst :body <str> :form :paren | :backtick}
       {:type :arith     :expr <str>}
       {:type :tilde     :user <str>}                                ; \"\" for bare ~
       {:type :escape    :value <str>}                               ; single char
       {:type :brace-exp :raw <str> :kind :list | :range}
       {:type :ansi-c-quoted :raw <str>}

   ### Redirections

       {:type :redir
        :op    :out | :append | :in | :err | :err-append
               | :err-to-out | :rw | :clobber | :dup-out | :dup-in
               | :all | :all-append | :here-string
        :fd    <int>?       ; explicit FD prefix, else nil (default per op)
        :target <word>}

       ;; Heredoc — body is already captured by the lexer
       {:type :heredoc
        :op       :heredoc | :heredoc-strip
        :fd       <int>?
        :tag      <str>
        :body     <str>
        :expand?  bool}     ; false when delimiter was quoted

   ### Assignment (assigns slot in :call)

       {:type :assign :name <str> :value <word>}     ; FOO=bar
       {:type :assign :name <str> :append true :value <word>}  ; FOO+=bar (later)

   ### Case clause

       {:type :case-clause :patterns [<word>] :body [<stmt>]
        :terminator :semi-semi | :semi-amp | :semi-semi-amp}

   ## Conventions

   - Bodies of compound commands (if/for/while/etc.) are always a
     vector of statements (a 'list', in POSIX terms) — even when
     containing one statement.
   - `:redirs` is always a (possibly empty) vector, never nil.
   - `:bg?` and `:neg?` are always boolean.
   - Words have at least one part. An empty word is a parser bug.

   ## Helpers

   - `node?`, `program?`, `stmt?`, etc. — type predicates
   - `walk` — pre-order traversal, calls `(f node)` on every map node
   - `leaf-calls` — return all `:call` nodes anywhere in the tree
     (useful for the permit layer)
   - `command-names` — return the command-name strings of all :call
     nodes whose first arg is a pure literal (for allowlist checks)"
  (:require [clojure.walk :as cw]))

;; ============================================================================
;; Predicates
;; ============================================================================

(defn node?    [x] (and (map? x) (keyword? (:type x))))
(defn program? [x] (and (node? x) (= :program (:type x))))
(defn stmt?    [x] (and (node? x) (= :stmt (:type x))))
(defn call?    [x] (and (node? x) (= :call (:type x))))
(defn binary?  [x] (and (node? x) (= :binary (:type x))))
(defn word?    [x] (and (node? x) (= :word (:type x))))
(defn redir?   [x] (and (node? x) (#{:redir :heredoc} (:type x))))

(defn compound?
  "True for compound-command node types (everything except :call and :binary)."
  [x]
  (and (node? x)
       (#{:if :for :c-for :while :until :case
          :brace-group :subshell :arith-cmd :function-def :test-bracket}
        (:type x))))

;; ============================================================================
;; Walk
;; ============================================================================

(defn walk
  "Pre-order walk: invoke `(f node)` on every node-map (i.e. every map
   with a :type) reachable from `root`. Returns nil."
  [root f]
  (cw/prewalk
   (fn [x] (when (node? x) (f x)) x)
   root)
  nil)

(defn collect
  "Return a vector of all nodes matching `pred?` in pre-order."
  [root pred?]
  (let [out (volatile! [])]
    (walk root (fn [n] (when (pred? n) (vswap! out conj n))))
    @out))

(defn leaf-calls
  "All :call nodes anywhere in the tree."
  [root]
  (collect root call?))

(defn pure-literal-word?
  "True if `word` is a single literal part with no expansions —
   i.e. the value is statically known at parse time."
  [w]
  (and (word? w)
       (let [parts (:parts w)]
         (and (= 1 (count parts))
              (= :lit (:type (first parts)))))))

(defn word-literal
  "If `word` is a pure literal, return its string value; else nil."
  [w]
  (when (pure-literal-word? w)
    (:value (first (:parts w)))))

(defn command-names
  "All command-name strings of :call nodes whose first arg is a pure
   literal. Useful for the permit layer's allowlist check.

   Returns a vector with possible duplicates (so callers can count
   occurrences). Skips :call nodes whose first arg is dynamically
   constructed (cmd-subst, var-ref, etc.) — the permit layer must
   handle those separately."
  [root]
  (let [out (volatile! [])]
    (walk root
          (fn [n]
            (when (call? n)
              (when-let [first-arg (first (:args n))]
                (when-let [name (word-literal first-arg)]
                  (vswap! out conj name))))))
    @out))
