(ns muschel.parse
  "Recursive-descent parser for muschel.

   Input: a token vector from `muschel.lex/tokenize`.
   Output: a `:program` AST per `muschel.ast`.

   Errors are thrown via `muschel.errors/error!` with `:type ::parse-error`,
   carrying the same `:line :col :offset :source :source-context :hint`
   fields as lex errors so a single `format-error` covers both layers."
  (:require [muschel.lex :as lex]
            [muschel.errors :as err]))

;; ============================================================================
;; Parser state
;; ============================================================================

(defn- parser [tokens src]
  {:tokens (vec tokens)
   :len    (count tokens)
   :pos    (volatile! 0)
   :src    src})

(defn- peek-tok [p]
  (get (:tokens p) @(:pos p)))

(defn- peek-tok-at [p n]
  (get (:tokens p) (+ @(:pos p) n)))

(defn- advance-tok! [p]
  (let [t (peek-tok p)] (vswap! (:pos p) inc) t))

(defn- at-eof? [p]
  (= :eof (:type (peek-tok p))))

(defn- save-pos [p] @(:pos p))
(defn- restore-pos! [p n] (vreset! (:pos p) n))

;; ============================================================================
;; Errors
;; ============================================================================

(defn- parse-error!
  ([p msg]      (parse-error! p msg (peek-tok p) nil))
  ([p msg tok]  (parse-error! p msg tok nil))
  ([p msg tok extra]
   (err/error! msg
               (merge {:type   ::parse-error
                       :line   (:line tok)
                       :col    (:col tok)
                       :end-col (:end-col tok)
                       :offset (:offset tok)
                       :source (:src p)}
                      extra))))

(defn- expect!
  "Consume a token of `type` (or fail). Returns the consumed token."
  [p type msg]
  (let [t (peek-tok p)]
    (if (= type (:type t))
      (advance-tok! p)
      (parse-error! p msg t))))

(defn- expect-reserved!
  "Consume a reserved word equal to `name`, or fail with `msg`."
  [p name msg]
  (let [t (peek-tok p)]
    (if (and (= :word (:type t)) (= name (:reserved t)))
      (advance-tok! p)
      (parse-error! p msg t))))

;; ============================================================================
;; Token classification helpers
;; ============================================================================

(def ^:private redir-token-types
  #{:redir-out :redir-append :redir-in :redir-err :redir-err-append
    :redir-err-to-out :redir-rw :redir-clobber :redir-dup-out :redir-dup-in
    :redir-all :redir-all-append :redir-here-string :redir-heredoc})

(defn- redir-token? [t] (redir-token-types (:type t)))

(defn- terminator-token? [t]
  (#{:semi :amp :newline} (:type t)))

(defn- reserved? [t name]
  (and (= :word (:type t)) (= name (:reserved t))))

(defn- closing-reserved? [t]
  (and (= :word (:type t))
       (#{"then" "elif" "else" "fi" "do" "done" "esac"} (:reserved t))))

(defn- word-token? [t] (= :word (:type t)))

(defn- pure-lit-string?
  "If token is a :word with a single :lit part, return its string value."
  [t]
  (when (and (word-token? t) (= 1 (count (:parts t))))
    (let [p (first (:parts t))]
      (when (= :lit (:type p))
        (:value p)))))

(defn- assignment-prefix?
  "True if the word token looks like an assignment prefix (NAME=...
   where NAME is [A-Za-z_][A-Za-z_0-9]*). Recognises words whose first
   part is a :lit matching the pattern; the value after `=` may be
   anything (other parts of the word, or empty)."
  [t]
  (when (word-token? t)
    (let [parts (:parts t)
          first-part (first parts)]
      (when (and first-part (= :lit (:type first-part)))
        (let [v ^String (:value first-part)
              eq (.indexOf v "=")]
          (when (pos? eq)                                ; not at position 0
            (let [name (subs v 0 eq)]
              (and (re-matches #"[A-Za-z_][A-Za-z_0-9]*" name)
                   {:name name :rest-of-first (subs v (inc eq))}))))))))

;; ============================================================================
;; Forward declarations
;; ============================================================================

(declare parse-stmt-list parse-stmt parse-and-or parse-pipeline parse-cmd
         parse-simple-cmd parse-compound-cmd parse-redir
         parse-if parse-for parse-while parse-until parse-case
         parse-brace-group parse-subshell parse-function-def-after-name
         parse-test-bracket)

;; ============================================================================
;; Skipping
;; ============================================================================

(defn- skip-newlines! [p]
  (loop []
    (when (= :newline (:type (peek-tok p)))
      (advance-tok! p)
      (recur))))

(defn- skip-terminators! [p]
  "Skip any number of stmt-terminators (newline, ;). Stops at non-
   terminator. Does NOT skip `&` because that has semantic meaning
   (backgrounding the previous stmt)."
  (loop []
    (let [t (peek-tok p)]
      (when (#{:newline :semi} (:type t))
        (advance-tok! p)
        (recur)))))

;; ============================================================================
;; Redirections
;; ============================================================================

(def ^:private redir-type->op
  {:redir-out          :out
   :redir-append       :append
   :redir-in           :in
   :redir-err          :err
   :redir-err-append   :err-append
   :redir-err-to-out   :err-to-out
   :redir-rw           :rw
   :redir-clobber      :clobber
   :redir-dup-out      :dup-out
   :redir-dup-in       :dup-in
   :redir-all          :all
   :redir-all-append   :all-append
   :redir-here-string  :here-string})

(defn- parse-redir
  "Parse one redirection at the current position. The redir-op token
   is already at peek; consume it, then read the target word.
   For heredocs the body is already in the op token (no target needed).
   For `2>&1`-style dup ops the target is a word (e.g. `1` or `&-`)."
  [p]
  (let [t (advance-tok! p)]
    (case (:type t)
      :redir-heredoc
      {:type :heredoc
       :op   (if (:strip? t) :heredoc-strip :heredoc)
       :fd   (:fd t)
       :tag  (:tag t)
       :body (:body t)
       :expand? (boolean (:expand? t))
       :line (:line t) :col (:col t) :offset (:offset t)}

      ;; Everything else takes a word target.
      ;; Note: `2>&1` lexes as :redir-dup-out :fd 2 followed by a word
      ;; "1", which is captured as the target. The :err-to-out alias
      ;; was redundant once the FD-prefix+dup-out machinery existed.
      (let [op (redir-type->op (:type t))
            target-tok (peek-tok p)]
        (when-not (word-token? target-tok)
          (parse-error! p (str "syntax error near unexpected token `"
                               (name (:type target-tok)) "'")
                        target-tok
                        {:hint (str "expected a filename after `" (name op) "`")}))
        {:type   :redir
         :op     op
         :fd     (:fd t)
         :target (advance-tok! p)
         :line (:line t) :col (:col t) :offset (:offset t)}))))

;; ============================================================================
;; Simple command: assignments + args, with redirs interleaved
;; ============================================================================

(defn- op-token->word
  "Synthesise a :word from an operator token (so test-bracket-double
   can carry `&&`/`||` etc. as literal args alongside the actual
   word tokens)."
  [t]
  (let [s (case (:type t)
            :and "&&" :or "||" :pipe "|" :pipe-amp "|&"
            :semi ";" :amp "&"
            :lparen "(" :rparen ")"
            (str (name (:type t))))]
    {:type :word :parts [{:type :lit :value s}]
     :line (:line t) :col (:col t) :offset (:offset t)
     :synthetic? true}))

(defn- parse-test-bracket-double!
  "Called when the next word is `[[`. Reads ALL tokens (words and
   operators) until matching `]]`, converting operators to synthetic
   word args. This lets `[[ A && B || C == D ]]` parse — the executor
   layer handles the compound expression."
  [p]
  (let [open (advance-tok! p)
        args (volatile! [])]
    (loop []
      (let [t (peek-tok p)]
        (cond
          (at-eof? p)
          (parse-error! p "syntax error: missing closing `]]'" t)

          (and (= :word (:type t)) (= "]]" (pure-lit-string? t)))
          (advance-tok! p)                              ; consume ]]

          (= :word (:type t))
          (do (vswap! args conj (advance-tok! p)) (recur))

          :else
          (do (vswap! args conj (op-token->word (advance-tok! p)))
              (recur)))))
    {:type :test-bracket :form :double :args (vec @args)
     :line (:line open) :col (:col open) :offset (:offset open)}))

(declare parse-simple-cmd*)

(defn- parse-simple-cmd
  "Parse a simple command. Returns either a :call or a :test-bracket
   (when the first word is [ or [[)."
  [p]
  (let [first-tok (peek-tok p)]
    (cond
      ;; `[[` opens a double-bracket conditional — special grammar
      ;; where && and || are allowed inside.
      (and (= :word (:type first-tok))
           (= "[[" (pure-lit-string? first-tok)))
      (parse-test-bracket-double! p)
      :else
      (parse-simple-cmd* p))))

(defn- parse-simple-cmd*
  [p]
  (let [first-tok (peek-tok p)
        assigns  (volatile! [])
        args     (volatile! [])
        redirs   (volatile! [])
        saw-nonassign? (volatile! false)]
    ;; Consume tokens until we hit a non-cmd token (terminator, operator,
    ;; closing reserved word, EOF).
    (loop []
      (let [t (peek-tok p)]
        (cond
          (or (at-eof? p)
              (terminator-token? t)
              (= :pipe (:type t))
              (= :pipe-amp (:type t))
              (= :and (:type t))
              (= :or (:type t))
              (= :rparen (:type t))
              (closing-reserved? t)
              (and (word-token? t)
                   (= "}" (pure-lit-string? t))
                   @saw-nonassign?))
          nil

          (redir-token? t)
          (do (vswap! redirs conj (parse-redir p)) (recur))

          (word-token? t)
          (if (and (not @saw-nonassign?) (assignment-prefix? t))
            ;; Assignment: split the first-lit at the `=` into a NAME
            ;; and a residual-value, and synthesize a value word from
            ;; that residual plus the rest of the parts.
            ;;
            ;; Bash quirk: if the value starts with `~`, tilde
            ;; expansion applies — promote a leading `~` to a :tilde
            ;; part so expand-assign-value resolves it to $HOME.
            (let [{:keys [name rest-of-first]} (assignment-prefix? t)
                  parts (:parts t)
                  rest-parts (rest parts)
                  ;; Synthesise tilde-part if value starts with `~`
                  tilde-and-rest
                  (when (and (seq rest-of-first)
                             (= \~ (.charAt ^String rest-of-first 0)))
                    (let [after-tilde (subs rest-of-first 1)
                          slash-idx (.indexOf ^String after-tilde "/")
                          user (if (neg? slash-idx) after-tilde (subs after-tilde 0 slash-idx))
                          tail (if (neg? slash-idx) "" (subs after-tilde slash-idx))]
                      [{:type :tilde :user user}
                       (when (seq tail) {:type :lit :value tail})]))
                  value-parts (cond
                                tilde-and-rest
                                (into (vec (remove nil? tilde-and-rest)) rest-parts)
                                (clojure.core/empty? rest-of-first)
                                (vec rest-parts)
                                :else
                                (into [{:type :lit :value rest-of-first}]
                                      rest-parts))
                  value-word {:type :word :parts value-parts
                              :line (:line t) :col (+ (:col t) (count name) 1)
                              :offset (+ (:offset t) (count name) 1)}]
              (advance-tok! p)
              (vswap! assigns conj
                      {:type :assign :name name :value value-word
                       :line (:line t) :col (:col t) :offset (:offset t)})
              (recur))
            (do (vreset! saw-nonassign? true)
                (vswap! args conj (advance-tok! p))
                (recur)))

          ;; A subshell `(...)` here in cmd position is unusual but
          ;; can appear inside simple-cmd-like contexts; defer to
          ;; the compound dispatcher in parse-cmd, not here.
          :else nil)))
    ;; Build the result.
    (cond
      ;; Test bracket: first arg is `[` or `[[`
      (and (clojure.core/empty? @assigns)
           (pos? (count @args))
           (#{"[" "[["} (pure-lit-string? (first @args))))
      (let [bracket (pure-lit-string? (first @args))
            close   (if (= "[" bracket) "]" "]]")
            inner   (rest @args)
            last-w  (last inner)
            inner-words (vec (butlast inner))]
        (when-not (= close (pure-lit-string? last-w))
          (parse-error! p (str "syntax error near unexpected token `"
                               (pure-lit-string? last-w) "'")
                        last-w
                        {:hint (str "missing closing `" close "` for test bracket")}))
        (cond-> {:type :test-bracket
                 :form (if (= "[" bracket) :single :double)
                 :args inner-words
                 :line (:line first-tok) :col (:col first-tok)
                 :offset (:offset first-tok)}
          (seq @redirs) (assoc :redirs (vec @redirs))))

      :else
      (do
        (when (and (clojure.core/empty? @assigns)
                   (clojure.core/empty? @args)
                   (clojure.core/empty? @redirs))
          ;; We were asked to parse a command but the leading token
          ;; isn't a command-starter. Report it bash-style.
          (parse-error! p
                        (str "syntax error near unexpected token `"
                             (or (pure-lit-string? first-tok)
                                 (name (:type first-tok)))
                             "'")
                        first-tok))
        (cond-> {:type :call
                 :assigns (vec @assigns)
                 :args (vec @args)
                 :line (:line first-tok) :col (:col first-tok)
                 :offset (:offset first-tok)}
          (seq @redirs) (assoc :redirs (vec @redirs)))))))

;; ============================================================================
;; Compound commands
;; ============================================================================

(defn- parse-brace-group [p]
  ;; `{` already at peek as a word token with value "{"
  (let [open (advance-tok! p)]
    (skip-terminators! p)
    (let [body (parse-stmt-list
                p
                (fn [t]
                  (and (word-token? t) (= "}" (pure-lit-string? t)))))
          close (peek-tok p)]
      (when-not (and (word-token? close) (= "}" (pure-lit-string? close)))
        (parse-error! p "syntax error: expected `}'" close))
      (advance-tok! p)
      {:type :brace-group :body body
       :line (:line open) :col (:col open) :offset (:offset open)})))

(defn- parse-subshell [p]
  (let [open (advance-tok! p)]                       ; :lparen
    (skip-terminators! p)
    (let [body (parse-stmt-list p (fn [t] (= :rparen (:type t))))]
      (expect! p :rparen "syntax error: expected `)'")
      {:type :subshell :body body
       :line (:line open) :col (:col open) :offset (:offset open)})))

(defn- parse-if [p]
  (let [start (advance-tok! p)                       ; consume `if`
        cond-stmts (parse-stmt-list
                    p
                    (fn [t] (reserved? t "then")))]
    (when (clojure.core/empty? cond-stmts)
      (parse-error! p "syntax error: `if' requires a condition before `then'"
                    (peek-tok p)))
    (expect-reserved! p "then" "syntax error: expected `then'")
    (let [then-stmts (parse-stmt-list
                      p
                      (fn [t] (or (reserved? t "elif")
                                  (reserved? t "else")
                                  (reserved? t "fi"))))
          _ (when (clojure.core/empty? then-stmts)
              (parse-error! p "syntax error: `then' branch is empty"
                            (peek-tok p)))
          elifs (volatile! [])
          else-stmts (volatile! nil)]
      (loop []
        (let [t (peek-tok p)]
          (cond
            (reserved? t "elif")
            (do (advance-tok! p)
                (let [c (parse-stmt-list p #(reserved? % "then"))]
                  (expect-reserved! p "then" "syntax error: expected `then'")
                  (let [th (parse-stmt-list
                            p
                            (fn [t] (or (reserved? t "elif")
                                        (reserved? t "else")
                                        (reserved? t "fi"))))]
                    (vswap! elifs conj {:cond c :then th}))
                  (recur)))

            (reserved? t "else")
            (do (advance-tok! p)
                (vreset! else-stmts
                         (parse-stmt-list p #(reserved? % "fi"))))

            :else nil)))
      (expect-reserved! p "fi" "syntax error: expected `fi'")
      (cond-> {:type :if :cond cond-stmts :then then-stmts
               :line (:line start) :col (:col start) :offset (:offset start)}
        (seq @elifs)        (assoc :elifs (vec @elifs))
        (some? @else-stmts) (assoc :else @else-stmts)))))

(defn- parse-for [p]
  (let [start (advance-tok! p)                       ; consume `for`
        next (peek-tok p)]
    ;; C-style: for (( init ; cond ; update )) ; do ... ; done
    ;; We can detect it because after `for` we'd see :arith-cmd (which
    ;; the lexer emits when it sees `((`).
    (if (= :arith-cmd (:type next))
      (let [a (advance-tok! p)
            ;; The expr looks like `init ; cond ; update`. Split.
            expr (:expr a)
            parts (clojure.string/split expr #";" 3)
            [init c update] (mapv clojure.string/trim parts)]
        (skip-terminators! p)
        (expect-reserved! p "do" "syntax error: expected `do'")
        (let [body (parse-stmt-list p #(reserved? % "done"))]
          (expect-reserved! p "done" "syntax error: expected `done'")
          {:type :c-for :init init :cond c :update update :body body
           :line (:line start) :col (:col start) :offset (:offset start)}))
      ;; for NAME [in word*]; do ... done
      (let [name-tok (peek-tok p)
            name-str (pure-lit-string? name-tok)]
        (when-not (and name-str (re-matches #"[A-Za-z_][A-Za-z_0-9]*" name-str))
          (parse-error! p "syntax error: expected variable name after `for'" name-tok))
        (advance-tok! p)
        ;; Optional `in WORD*`. If omitted, body iterates $@ (handled
        ;; in exec).
        (let [in-tok (peek-tok p)
              has-in? (reserved? in-tok "in")
              words (volatile! [])]
          (when has-in?
            (advance-tok! p)
            (loop []
              (let [t (peek-tok p)]
                (when (word-token? t)
                  (vswap! words conj (advance-tok! p))
                  (recur)))))
          (skip-terminators! p)
          (expect-reserved! p "do" "syntax error: expected `do'")
          (let [body (parse-stmt-list p #(reserved? % "done"))]
            (expect-reserved! p "done" "syntax error: expected `done'")
            (cond-> {:type :for :var name-str :words (vec @words) :body body
                     :line (:line start) :col (:col start) :offset (:offset start)}
              (not has-in?) (assoc :iterate-positional? true))))))))

(defn- parse-while-or-until [p kw]
  (let [start (advance-tok! p)
        cond-stmts (parse-stmt-list p #(reserved? % "do"))]
    (expect-reserved! p "do" "syntax error: expected `do'")
    (let [body (parse-stmt-list p #(reserved? % "done"))]
      (expect-reserved! p "done" "syntax error: expected `done'")
      {:type (if (= kw "while") :while :until)
       :cond cond-stmts :body body
       :line (:line start) :col (:col start) :offset (:offset start)})))

(defn- parse-case [p]
  (let [start (advance-tok! p)
        word-t (peek-tok p)]
    (when-not (word-token? word-t)
      (parse-error! p "syntax error: expected word after `case'" word-t))
    (let [w (advance-tok! p)]
      (skip-newlines! p)
      (expect-reserved! p "in" "syntax error: expected `in'")
      (skip-newlines! p)
      (let [clauses (volatile! [])]
        (loop []
          (skip-newlines! p)
          (let [t (peek-tok p)]
            (when-not (reserved? t "esac")
              (when (= :lparen (:type t)) (advance-tok! p))
              ;; pattern (| pattern)*
              (let [patterns (volatile! [])]
                (loop []
                  (let [pt (peek-tok p)]
                    (cond
                      (word-token? pt)
                      (do (vswap! patterns conj (advance-tok! p))
                          (when (= :pipe (:type (peek-tok p)))
                            (advance-tok! p)
                            (recur)))
                      :else
                      (parse-error! p "syntax error: expected pattern in case clause" pt))))
                (expect! p :rparen "syntax error: expected `)' after case pattern")
                (skip-newlines! p)
                (let [body (parse-stmt-list
                            p
                            (fn [t]
                              (or (#{:semi-semi :semi-amp :semi-semi-amp} (:type t))
                                  (reserved? t "esac"))))
                      term (peek-tok p)
                      term-kw (case (:type term)
                                :semi-semi      :semi-semi
                                :semi-amp       :semi-amp
                                :semi-semi-amp  :semi-semi-amp
                                nil)]
                  (when (#{:semi-semi :semi-amp :semi-semi-amp} (:type term))
                    (advance-tok! p))
                  (vswap! clauses conj
                          {:type :case-clause
                           :patterns (vec @patterns)
                           :body body
                           :terminator term-kw}))
                (recur)))))
        (expect-reserved! p "esac" "syntax error: expected `esac'")
        {:type :case :word w :clauses (vec @clauses)
         :line (:line start) :col (:col start) :offset (:offset start)}))))

(defn- parse-function-def-after-name [p name-tok]
  ;; name-tok is the first word; we've already verified the next two
  ;; tokens are :lparen :rparen. Consume them and the body.
  (advance-tok! p)                                     ; consume name
  (expect! p :lparen "internal: function def lparen")
  (expect! p :rparen "internal: function def rparen")
  (skip-newlines! p)
  (let [body (parse-cmd p)]
    {:type :function-def
     :name (pure-lit-string? name-tok)
     :body body
     :line (:line name-tok) :col (:col name-tok) :offset (:offset name-tok)}))

(defn- parse-function-keyword-form [p]
  (let [start (advance-tok! p)                        ; consume `function`
        name-tok (peek-tok p)]
    (when-not (and (word-token? name-tok) (pure-lit-string? name-tok))
      (parse-error! p "syntax error: expected function name after `function'" name-tok))
    (advance-tok! p)
    (when (= :lparen (:type (peek-tok p)))
      (advance-tok! p)
      (expect! p :rparen "syntax error: expected `)'"))
    (skip-newlines! p)
    (let [body (parse-cmd p)]
      {:type :function-def
       :name (pure-lit-string? name-tok) :body body
       :line (:line start) :col (:col start) :offset (:offset start)})))

(defn- looks-like-function-def?
  "True if the next three tokens are: word(name), :lparen, :rparen."
  [p]
  (let [t1 (peek-tok p)
        t2 (peek-tok-at p 1)
        t3 (peek-tok-at p 2)]
    (and (word-token? t1)
         (pure-lit-string? t1)
         (= :lparen (:type t2))
         (= :rparen (:type t3)))))

(defn- parse-compound-cmd
  "Dispatch to the right compound-command parser based on the leading
   token. Returns nil if the leading token doesn't start a compound."
  [p]
  (let [t (peek-tok p)]
    (cond
      (reserved? t "if")       (parse-if p)
      (reserved? t "for")      (parse-for p)
      (reserved? t "while")    (parse-while-or-until p "while")
      (reserved? t "until")    (parse-while-or-until p "until")
      (reserved? t "case")     (parse-case p)
      (reserved? t "function") (parse-function-keyword-form p)
      (= :arith-cmd (:type t))
      (let [a (advance-tok! p)]
        {:type :arith-cmd :expr (:expr a)
         :line (:line a) :col (:col a) :offset (:offset a)})
      (= :lparen (:type t))    (parse-subshell p)
      (and (word-token? t) (= "{" (pure-lit-string? t)))
      (parse-brace-group p)
      (looks-like-function-def? p)
      (parse-function-def-after-name p t)
      :else nil)))

;; ============================================================================
;; Pipeline → and-or → stmt
;; ============================================================================

(defn- parse-cmd
  "Parse one command (compound or simple)."
  [p]
  (or (parse-compound-cmd p)
      (parse-simple-cmd p)))

(defn- parse-pipeline-body
  "Parse a pipeline: cmd ((| | |&) cmd)*. Returns either the single
   cmd (no pipes) or a left-folded :binary chain.
   Each piped cmd is wrapped in its own :stmt — pipe acts on stmts."
  [p]
  (let [left-cmd (parse-cmd p)
        left-stmt {:type :stmt :cmd left-cmd :redirs []
                   :bg? false :neg? false}]
    (loop [acc left-stmt]
      (let [t (peek-tok p)]
        (cond
          (or (= :pipe (:type t)) (= :pipe-amp (:type t)))
          (let [op (if (= :pipe (:type t)) :pipe :pipe-amp)]
            (advance-tok! p)
            (skip-newlines! p)
            (let [right-cmd (parse-cmd p)
                  right-stmt {:type :stmt :cmd right-cmd :redirs []
                              :bg? false :neg? false}]
              (recur {:type :stmt
                      :cmd {:type :binary :op op :left acc :right right-stmt}
                      :redirs [] :bg? false :neg? false})))
          :else acc)))))

(defn- parse-pipeline
  "Parse a pipeline including optional leading `!` for negation."
  [p]
  (let [t (peek-tok p)
        neg? (when (reserved? t "!")
               (advance-tok! p) true)
        pl (parse-pipeline-body p)]
    (cond-> pl
      neg? (assoc :neg? true))))

(defn- parse-and-or
  "Parse and-or: pipeline ((&& | ||) pipeline)*. Left-associative."
  [p]
  (let [left (parse-pipeline p)]
    (loop [acc left]
      (let [t (peek-tok p)]
        (cond
          (or (= :and (:type t)) (= :or (:type t)))
          (let [op (if (= :and (:type t)) :and :or)]
            (advance-tok! p)
            (skip-newlines! p)
            (let [right (parse-pipeline p)]
              (recur {:type :stmt
                      :cmd {:type :binary :op op :left acc :right right}
                      :redirs [] :bg? false :neg? false})))
          :else acc)))))

(defn- parse-stmt
  "Parse one statement (and-or chain). Backgrounding `&` is handled by
   `parse-stmt-list` since it acts as a separator-with-effect."
  [p]
  (parse-and-or p))

(defn- parse-stmt-list
  "Parse a sequence of statements separated by terminators (; & \\n).
   Stops when `(stop? next-tok)` returns true OR at EOF. `&` after a
   statement marks it `:bg? true`.
   Returns a vector of stmt nodes."
  [p stop?]
  (let [out (volatile! [])]
    (skip-terminators! p)
    (loop []
      (let [t (peek-tok p)]
        (cond
          (at-eof? p) nil
          (stop? t) nil
          :else
          (let [stmt (parse-stmt p)
                n (peek-tok p)]
            (cond
              (= :amp (:type n))
              (do (advance-tok! p)
                  (vswap! out conj (assoc stmt :bg? true))
                  (skip-terminators! p) (recur))
              (#{:semi :newline} (:type n))
              (do (vswap! out conj stmt)
                  (skip-terminators! p) (recur))
              (or (at-eof? p) (stop? n))
              (do (vswap! out conj stmt) nil)
              :else
              (parse-error! p
                            (str "syntax error near unexpected token `"
                                 (or (pure-lit-string? n) (name (:type n)))
                                 "'")
                            n))))))
    @out))

;; ============================================================================
;; Public API
;; ============================================================================

(defn parse
  "Parse bash source `src` into a `:program` AST. Throws an ex-info
   with `:type ::lex-error`, `::refused`, or `::parse-error` on
   failure.

   Cmd-substitution bodies (`$(...)`, `\\`...\\``) are kept as RAW
   STRINGS in the `:body` field of each `:cmd-subst` node — matching
   bash's lazy semantics. Parsing them happens at expand time, just
   before the inner commands run. The permit layer's runtime hook
   (in `muschel.exec/run-external`) does the safety check at the
   effect boundary."
  [src]
  (let [tokens (lex/tokenize src)
        p (parser tokens src)
        stmts (parse-stmt-list p (constantly false))]
    (when-not (at-eof? p)
      (parse-error! p
                    (str "syntax error near unexpected token `"
                         (or (pure-lit-string? (peek-tok p))
                             (name (:type (peek-tok p))))
                         "'")
                    (peek-tok p)))
    {:type :program :stmts stmts}))

(defn parses?
  "True if `src` parses cleanly."
  [src]
  (try (do (parse src) true)
       (catch #?(:clj Exception :cljs js/Error) _ false)))
