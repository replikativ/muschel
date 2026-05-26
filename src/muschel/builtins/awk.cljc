(ns muschel.builtins.awk
  "A bounded but faithful awk implementation. Portable to JVM and CLJS
   via `muschel.builtins.awk-compat`.

   Modeled on goawk's tree-walking interpreter (MIT-licensed reference at
   github.com/benhoyt/goawk). See doc/awk.md for the full surface,
   refused features, and the known engine-difference gaps. We port the
   **subset** that covers the 90% of agent awk-usage:

     BEGIN / END                — yes
     /re/ { … } and `expr { … }` patterns
     pat1,pat2 { … } ranges
     $0  $N  NR NF FS OFS ORS RS
     arithmetic + - * / % ^ **
     comparison == != < <= > >=
     logical && || !
     string concatenation by juxtaposition
     regex match ~ !~
     assignment = += -= *= /= %= ^= **=
     pre/post inc/dec
     ternary ?:
     if/else, while, do/while, for(;;), for(var in array)
     break / continue / next / exit
     delete a[k] / delete a
     print (comma list → OFS join, ORS terminator)
     printf (full %d %i %x %o %c %s %% format)
     length() substr() index() split()
     sub() gsub() sprintf() match()
     tolower() toupper() int()
     single-dim associative arrays
     -v VAR=val  -F SEP  -f FILE

   Skipped (refuses):  user-defined functions, getline, system(),
   redirects (> >> |), multi-dim arrays via SUBSEP, gensub.

   The value model is the tricky bit. Each value has one of four tags:

     :null    uninitialised. num=0, str=\"\", boolean=false.
     :num     pure numeric (literal, arithmetic result).
     :str     literal string or string-returning fn result.
     :numstr  string from an input field, split result, or -v.

   `=='s ' comparison branches on whether either side is a `true-string`
   — a `:str` always is, a `:numstr` is iff it doesn't parse as a number,
   `:num`/`:null` never are. This is why `$1 == 42` and `$1 == \"42\"`
   both match a field whose text is `42`."
  (:require [clojure.string :as str]
            [muschel.builtins.awk-compat :as cc]))

;; ============================================================================
;; Lexer
;; ============================================================================

(def ^:private keywords
  {"BEGIN"    :BEGIN  "END"      :END
   "break"    :break  "continue" :continue
   "delete"   :delete "do"       :do
   "else"     :else   "exit"     :exit
   "for"      :for    "function" :function   ;; function we reject at parse
   "if"       :if     "in"       :in
   "next"     :next   "print"    :print
   "printf"   :printf "return"   :return    ;; return we reject
   "while"    :while  "getline"  :getline   ;; getline we reject
   "nextfile" :nextfile})

(def ^:private builtin-fns
  #{"length" "substr" "index" "split" "sub" "gsub" "sprintf"
    "match" "tolower" "toupper" "int" "system" "atan2"
    "cos" "sin" "exp" "log" "sqrt" "rand" "srand"})

(defn- ws? [c] (or (= c \space) (= c \tab)))
(defn- nl? [c] (= c \newline))
(defn- digit? [c]
  (when c
    (let [n (cc/char-code c)]
      (and (>= n 48) (<= n 57)))))
(defn- name-start? [c]
  (when c
    (let [n (cc/char-code c)]
      (or (and (>= n 65) (<= n 90))
          (and (>= n 97) (<= n 122))
          (= c \_)))))
(defn- name-char? [c] (or (name-start? c) (digit? c)))

(defn- scan-string
  "Consume a \"…\" or '…' literal. Returns [value next-pos]."
  [^String src pos quote-ch]
  (let [n (count src)
        sb (cc/sbuf)]
    (loop [i (inc pos)]
      (cond
        (>= i n) (throw (ex-info "unterminated string literal" {:pos pos}))
        (= (.charAt src i) quote-ch) [(cc/sbstr sb) (inc i)]
        (= (.charAt src i) \\)
        (if (< (inc i) n)
          (let [c2 (.charAt src (inc i))]
            (cc/sappend! sb (case c2
                              \n \newline \t \tab \r \return
                              \\ \\ \/ \/
                              \" \" \' \'
                              \a (char 7) \b \backspace \f \formfeed
                              \v (char 11) \0 (char 0)
                              c2))
            (recur (+ i 2)))
          (throw (ex-info "trailing backslash" {:pos i})))
        :else
        (do (cc/sappend! sb (.charAt src i)) (recur (inc i)))))))

(defn- scan-number
  "Consume an integer / decimal / scientific literal. Returns
   [double-value next-pos]."
  [^String src pos]
  (let [n (count src)
        ;; Optional leading hex
        hex? (and (= \0 (.charAt src pos))
                  (< (inc pos) n)
                  (#{\x \X} (.charAt src (inc pos))))]
    (if hex?
      (let [end (loop [i (+ pos 2)]
                  (if (and (< i n)
                           (let [c (.charAt src i)
                                 cc (cc/char-code c)]
                             (or (digit? c)
                                 (and (>= cc 65) (<= cc 70))
                                 (and (>= cc 97) (<= cc 102)))))
                    (recur (inc i))
                    i))]
        [(double (cc/plong-hex (subs src (+ pos 2) end))) end])
      (let [end (loop [i pos saw-dot? false saw-e? false]
                  (if (>= i n)
                    i
                    (let [c (.charAt src i)]
                      (cond
                        (digit? c) (recur (inc i) saw-dot? saw-e?)
                        (and (not saw-dot?) (not saw-e?) (= c \.))
                        (recur (inc i) true saw-e?)
                        (and (not saw-e?) (or (= c \e) (= c \E)))
                        ;; Only consume the `e`/`E` if it's followed by
                        ;; an optional +/- AND at least one digit.
                        (let [j (inc i)
                              j' (if (and (< j n)
                                          (or (= (.charAt src j) \+)
                                              (= (.charAt src j) \-)))
                                   (inc j) j)]
                          (if (and (< j' n) (digit? (.charAt src j')))
                            (recur j' saw-dot? true)
                            i))
                        :else i))))]
        [(cc/pdouble (subs src pos end)) end]))))

(defn- scan-name
  [^String src pos]
  (let [n (count src)
        end (loop [i pos]
              (if (and (< i n) (name-char? (.charAt src i)))
                (recur (inc i)) i))]
    [(subs src pos end) end]))

;; Tokens that, when they're the most recent, can be FOLLOWED by a
;; regex literal (i.e. `/` opens a regex). Anything else means `/` is
;; division. The list comes from awk grammar: `/` is a regex iff it
;; appears in expression position with no left-hand operand.
(def ^:private regex-context-after
  #{:newline :semicolon :lparen :lbrace :lbracket :rbrace :comma
    :assign :add-assign :sub-assign :mul-assign :div-assign
    :mod-assign :pow-assign
    :eq :ne :lt :le :gt :ge :and :or :not :match :not-match
    :question :colon
    :add :sub :mul :div :mod :pow
    :print :printf :return :if :while :do :for :in
    :BEGIN :END
    :dollar})

(defn- forward-scan-regex
  "Read a regex literal starting at `pos` (which points at the `/`).
   Returns [pattern next-pos]."
  [^String src pos]
  (let [n (count src)
        sb (cc/sbuf)]
    (loop [i (inc pos)]
      (cond
        (>= i n) (throw (ex-info "unterminated regex" {:pos pos}))
        (= (.charAt src i) \/) [(cc/sbstr sb) (inc i)]
        (= (.charAt src i) \\)
        (if (< (inc i) n)
          (do (cc/sappend! sb \\) (cc/sappend! sb (.charAt src (inc i)))
              (recur (+ i 2)))
          (throw (ex-info "trailing backslash in regex" {:pos i})))
        :else (do (cc/sappend! sb (.charAt src i)) (recur (inc i)))))))

(defn- tokenize
  "Lex `src` into a vec of {:type kw :value any :line :col} maps.
   Emits :regex tokens directly when context indicates `/` opens a
   regex; otherwise emits :div / :div-assign."
  [^String src]
  (let [n (count src)
        out (transient [])
        line (volatile! 1)
        col (volatile! 1)
        bump (fn [c]
               (if (nl? c)
                 (do (vswap! line inc) (vreset! col 1))
                 (vswap! col inc)))
        ;; What was the last NON-newline token? Newlines don't reset
        ;; context — `/foo/\n` followed by `/bar/` should still let
        ;; the second `/` start a regex.
        last-tok (volatile! nil)
        emit! (fn [tok]
                (when (not= :newline (:type tok)) (vreset! last-tok (:type tok)))
                (conj! out tok))
        regex-here? (fn []
                      (let [lt @last-tok]
                        (or (nil? lt) (regex-context-after lt))))]
    (loop [i 0]
      (if (>= i n)
        (do (emit! {:type :eof :line @line :col @col})
            (persistent! out))
        (let [c (.charAt src i)
              ln @line cl @col]
          (cond
            ;; Comment to end of line
            (= c \#)
            (let [j (loop [k i] (if (or (>= k n) (nl? (.charAt src k))) k (recur (inc k))))]
              (recur j))

            ;; Newline → token (statement terminator)
            (nl? c)
            (do (emit! {:type :newline :line ln :col cl})
                (bump c) (recur (inc i)))

            ;; Other whitespace
            (ws? c) (do (bump c) (recur (inc i)))

            ;; Backslash-newline = continuation
            (and (= c \\) (< (inc i) n) (nl? (.charAt src (inc i))))
            (do (bump (.charAt src (inc i))) (recur (+ i 2)))

            ;; Regex literal — `/` (or `/=`) in regex-allowed context
            (and (= c \/) (regex-here?))
            (let [[pat end] (forward-scan-regex src i)]
              (emit! {:type :regex :value pat :line ln :col cl})
              (vswap! col + (- end i))
              (recur end))

            ;; String literals
            (or (= c \") (= c \'))
            (let [[v end] (scan-string src i c)]
              (emit! {:type :string :value v :line ln :col cl})
              (vswap! col + (- end i))
              (recur end))

            ;; Number
            (or (digit? c)
                (and (= c \.) (< (inc i) n) (digit? (.charAt src (inc i)))))
            (let [[v end] (scan-number src i)]
              (emit! {:type :number :value v :line ln :col cl})
              (vswap! col + (- end i))
              (recur end))

            ;; Name / keyword / builtin
            (name-start? c)
            (let [[nm end] (scan-name src i)]
              (cond
                (keywords nm)
                (emit! {:type (keywords nm) :value nm :line ln :col cl})
                (builtin-fns nm)
                (emit! {:type :builtin :value nm :line ln :col cl})
                :else
                (emit! {:type :name :value nm :line ln :col cl}))
              (vswap! col + (- end i))
              (recur end))

            ;; Multi-char operators (check longest first)
            :else
            (let [match (fn [s tok]
                          (when (and (<= (+ i (count s)) n)
                                     (= s (subs src i (+ i (count s)))))
                            tok))
                  [tok len]
                  (or (when-let [t (match "**=" :pow-assign)] [t 3])
                      (when-let [t (match "<<=" nil)] [t 3]) ;; unsupported
                      (when-let [t (match ">>=" nil)] [t 3]) ;; unsupported
                      (when-let [t (match "**" :pow)] [t 2])
                      (when-let [t (match "==" :eq)] [t 2])
                      (when-let [t (match "!=" :ne)] [t 2])
                      (when-let [t (match "<=" :le)] [t 2])
                      (when-let [t (match ">=" :ge)] [t 2])
                      (when-let [t (match "&&" :and)] [t 2])
                      (when-let [t (match "||" :or)] [t 2])
                      (when-let [t (match "++" :incr)] [t 2])
                      (when-let [t (match "--" :decr)] [t 2])
                      (when-let [t (match "+=" :add-assign)] [t 2])
                      (when-let [t (match "-=" :sub-assign)] [t 2])
                      (when-let [t (match "*=" :mul-assign)] [t 2])
                      (when-let [t (match "/=" :div-assign)] [t 2])
                      (when-let [t (match "%=" :mod-assign)] [t 2])
                      (when-let [t (match "^=" :pow-assign)] [t 2])
                      (when-let [t (match "!~" :not-match)] [t 2])
                      (when-let [t (match ">>" nil)] [t 2])
                      (case c
                        \+ [:add 1]   \- [:sub 1]
                        \* [:mul 1]   \/ [:div 1]
                        \% [:mod 1]   \^ [:pow 1]
                        \= [:assign 1]
                        \< [:lt 1]    \> [:gt 1]
                        \! [:not 1]   \~ [:match 1]
                        \? [:question 1] \: [:colon 1]
                        \( [:lparen 1]   \) [:rparen 1]
                        \{ [:lbrace 1]   \} [:rbrace 1]
                        \[ [:lbracket 1] \] [:rbracket 1]
                        \, [:comma 1]    \; [:semicolon 1]
                        \$ [:dollar 1]   \| [:pipe 1]
                        [nil 1]))]
              (if tok
                (do (emit! {:type tok :line ln :col cl})
                    (vswap! col + len)
                    (recur (+ i len)))
                (throw (ex-info (str "unexpected char: " (pr-str c))
                                {:line ln :col cl}))))))))))

;; ============================================================================
;; Parser — recursive descent. The token stream is held in an atom; helpers
;; mutate the cursor and return AST nodes.
;; ============================================================================

(declare parse-expr parse-stmt parse-stmts parse-pattern parse-postfix)

(defn- peek-tok [state]   (nth (:tokens state) @(:pos state) {:type :eof}))
(defn- peek-tok-at [state offset]
  (nth (:tokens state) (+ @(:pos state) offset) {:type :eof}))
(defn- advance! [state] (let [t (peek-tok state)] (vswap! (:pos state) inc) t))
(defn- at? [state & types] (boolean (some #(= % (:type (peek-tok state))) types)))
(defn- expect! [state ty]
  (let [t (peek-tok state)]
    (when-not (= ty (:type t))
      (throw (ex-info (str "expected " ty ", got " (:type t)
                           " at line " (:line t) ":" (:col t))
                      {:expected ty :actual t})))
    (advance! state)))
(defn- skip-terminators! [state]
  (while (at? state :newline :semicolon) (advance! state)))

;; ---------- Expression parsing (precedence-climbing) ----------

(defn- parse-primary [state]
  (let [t (peek-tok state)]
    (case (:type t)
      :number (do (advance! state) {:type :num :value (:value t)})
      :string (do (advance! state) {:type :str :value (:value t)})
      :dollar (do (advance! state)
                  {:type :field :index (parse-primary state)})
      :not    (do (advance! state) {:type :unary :op :not :value (parse-postfix state)})
      :sub    (do (advance! state) {:type :unary :op :neg :value (parse-postfix state)})
      :add    (do (advance! state) {:type :unary :op :pos :value (parse-postfix state)})
      :incr   (do (advance! state)
                  (let [tgt (parse-primary state)]
                    {:type :incr :op :incr :pre? true :expr tgt}))
      :decr   (do (advance! state)
                  (let [tgt (parse-primary state)]
                    {:type :incr :op :decr :pre? true :expr tgt}))
      :lparen (do (advance! state)
                  (let [e (parse-expr state)]
                    (expect! state :rparen)
                    ;; Wrap so `(a)++` doesn't treat (a) as an l-value
                    ;; for post-incr — POSIX awk forbids that.
                    {:type :paren :inner e}))
      :name   (let [nm (:value (advance! state))]
                (if (at? state :lbracket)
                  (do (advance! state)
                      (let [idx (parse-expr state)]
                        (expect! state :rbracket)
                        {:type :index :array nm :index [idx]}))
                  {:type :var :name nm}))
      :builtin
      (let [fname (:value (advance! state))]
        (cond
          ;; length with no parens
          (and (= fname "length") (not (at? state :lparen)))
          {:type :call :func :length :args []}
          :else
          (do (expect! state :lparen)
              (let [args (loop [acc []]
                           (if (at? state :rparen)
                             acc
                             (let [e (parse-expr state)
                                   acc' (conj acc e)]
                               (if (at? state :comma)
                                 (do (advance! state) (recur acc'))
                                 acc'))))]
                (expect! state :rparen)
                {:type :call :func (keyword fname) :args args}))))
      :regex (do (advance! state) {:type :regex :pattern (:value t)})
      (throw (ex-info (str "unexpected token in primary: " (:type t)
                           " at line " (:line t) ":" (:col t))
                      {:token t})))))

(defn- l-value?
  "True if `e` can appear as the LHS of an assignment / target of
   ++/--."
  [e]
  (case (:type e)
    :var true :index true :field true
    false))

(defn- parse-postfix [state]
  (let [e (parse-primary state)]
    (cond
      (and (at? state :incr) (l-value? e))
      (do (advance! state) {:type :incr :op :incr :pre? false :expr e})
      (and (at? state :decr) (l-value? e))
      (do (advance! state) {:type :incr :op :decr :pre? false :expr e})
      :else e)))

(defn- parse-pow [state]
  (let [l (parse-postfix state)]
    (if (at? state :pow)
      (do (advance! state)
          {:type :binary :op :pow :left l :right (parse-pow state)})
      l)))

(defn- parse-mul [state]
  (loop [l (parse-pow state)]
    (cond
      (at? state :mul) (do (advance! state) (recur {:type :binary :op :mul :left l :right (parse-pow state)}))
      (at? state :div) (do (advance! state) (recur {:type :binary :op :div :left l :right (parse-pow state)}))
      (at? state :mod) (do (advance! state) (recur {:type :binary :op :mod :left l :right (parse-pow state)}))
      :else l)))

(defn- parse-add [state]
  (loop [l (parse-mul state)]
    (cond
      (at? state :add) (do (advance! state) (recur {:type :binary :op :add :left l :right (parse-mul state)}))
      (at? state :sub) (do (advance! state) (recur {:type :binary :op :sub :left l :right (parse-mul state)}))
      :else l)))

;; Concatenation by juxtaposition — detect by peeking at the next
;; token; if it starts a primary, treat as :concat.
(def ^:private concat-starts
  #{:dollar :not :name :number :string :lparen :incr :decr :builtin :regex})

(defn- parse-concat [state]
  (loop [l (parse-add state)]
    (if (concat-starts (:type (peek-tok state)))
      (recur {:type :binary :op :concat :left l :right (parse-add state)})
      l)))

(defn- parse-compare [state]
  (let [l (parse-concat state)
        op (case (:type (peek-tok state))
             :eq :eq :ne :ne :lt :lt :le :le :gt :gt :ge :ge nil)]
    (if op
      (do (advance! state)
          {:type :binary :op op :left l :right (parse-concat state)})
      l)))

(defn- parse-match [state]
  (let [l (parse-compare state)]
    (cond
      (at? state :match)
      (do (advance! state)
          {:type :binary :op :match :left l :right (parse-compare state)})
      (at? state :not-match)
      (do (advance! state)
          {:type :binary :op :not-match :left l :right (parse-compare state)})
      :else l)))

(defn- parse-in [state]
  (loop [l (parse-match state)]
    (if (at? state :in)
      (do (advance! state)
          (let [arr (advance! state)]
            (when (not= :name (:type arr))
              (throw (ex-info "expected array name after `in`" {:tok arr})))
            (recur {:type :in :index [l] :array (:value arr)})))
      l)))

(defn- parse-and [state]
  (loop [l (parse-in state)]
    (if (at? state :and)
      ;; The RHS of `&&` may itself be an assignment in awk —
      ;; `if (1 && x = 2)` parses as `if (1 && (x = 2))`. Recurse
      ;; through parse-expr to allow that.
      (do (advance! state) (recur {:type :binary :op :and :left l :right (parse-expr state)}))
      l)))

(defn- parse-or [state]
  (loop [l (parse-and state)]
    (if (at? state :or)
      (do (advance! state) (recur {:type :binary :op :or :left l :right (parse-expr state)}))
      l)))

(defn- parse-cond [state]
  (let [l (parse-or state)]
    (if (at? state :question)
      (do (advance! state)
          (let [t (parse-expr state)
                _ (expect! state :colon)
                f (parse-expr state)]
            {:type :cond :cond l :true t :false f}))
      l)))

(def ^:private aug-ops
  {:add-assign :add :sub-assign :sub :mul-assign :mul
   :div-assign :div :mod-assign :mod :pow-assign :pow})

(defn- parse-assign [state]
  (let [l (parse-cond state)
        t (:type (peek-tok state))]
    (cond
      (and (= t :assign) (l-value? l))
      (do (advance! state) {:type :assign :left l :right (parse-assign state)})
      (and (aug-ops t) (l-value? l))
      (do (advance! state)
          {:type :aug-assign :op (aug-ops t) :left l :right (parse-assign state)})
      :else l)))

(defn- parse-expr [state] (parse-assign state))

;; ---------- Statements ----------

(defn- lookahead-paren-has-comma?
  "Given that the current token is `(`, scan ahead and report whether
   a comma appears at paren-depth 0 inside this paren. Used to decide
   whether `print (a, b)` is a paren-tuple of print args or a single
   parenthesised expression."
  [state]
  (let [toks @(:tokens-vol state)
        start (inc @(:pos state))
        n (count toks)]
    (loop [i start depth 1]
      (cond
        (>= i n) false
        (zero? depth) false
        :else
        (let [ty (:type (nth toks i))]
          (cond
            (= ty :lparen) (recur (inc i) (inc depth))
            (= ty :rparen) (if (= depth 1) false (recur (inc i) (dec depth)))
            (and (= ty :comma) (= depth 1)) true
            :else (recur (inc i) depth)))))))

(defn- parse-print-args [state]
  ;; Special case: `print (e1, e2, …)` — paren-tuple form. Triggered
  ;; only when the paren actually contains a top-level comma; bare
  ;; `print (1+2)` falls through to the normal path.
  (cond
    (and (at? state :lparen) (lookahead-paren-has-comma? state))
    (do (advance! state) ; lparen
        (let [args (loop [acc []]
                     (let [e (parse-expr state)
                           acc' (conj acc e)]
                       (if (at? state :comma)
                         (do (advance! state) (recur acc'))
                         acc')))]
          (expect! state :rparen)
          args))

    :else
    ;; Comma-separated expressions until we hit a redirect, terminator,
    ;; or rbrace.
    (loop [acc []]
      (let [t (peek-tok state)]
        (cond
          (#{:newline :semicolon :rbrace :eof :gt :pipe} (:type t)) acc
          :else
          (let [e (parse-expr state)
                acc' (conj acc e)]
            (if (at? state :comma)
              (do (advance! state) (recur acc'))
              acc')))))))

(defn- parse-simple-stmt [state]
  (let [t (peek-tok state)]
    (case (:type t)
      :print  (do (advance! state)
                  {:type :print :args (parse-print-args state)})
      :printf (do (advance! state)
                  {:type :printf :args (parse-print-args state)})
      :delete (do (advance! state)
                  (let [nm (advance! state)]
                    (when (not= :name (:type nm))
                      (throw (ex-info "delete needs array name" {:tok nm})))
                    (if (at? state :lbracket)
                      (do (advance! state)
                          (let [idx (parse-expr state)]
                            (expect! state :rbracket)
                            {:type :delete :array (:value nm) :index [idx]}))
                      {:type :delete :array (:value nm) :index nil})))
      ;; bare expression
      {:type :expr-stmt :expr (parse-expr state)})))

(defn- parse-stmt-block [state]
  (expect! state :lbrace)
  (skip-terminators! state)
  (let [body (loop [acc []]
               (if (at? state :rbrace)
                 acc
                 (let [s (parse-stmt state)]
                   (skip-terminators! state)
                   (recur (conj acc s)))))]
    (advance! state) ; rbrace
    {:type :block :body body}))

(defn- parse-stmt [state]
  (let [t (peek-tok state)]
    (case (:type t)
      :lbrace (parse-stmt-block state)
      :if     (do (advance! state)
                  (expect! state :lparen)
                  (let [c (parse-expr state)
                        _ (expect! state :rparen)
                        _ (skip-terminators! state)
                        b (parse-stmt state)
                        _ (skip-terminators! state)
                        e (when (at? state :else)
                            (advance! state) (skip-terminators! state)
                            (parse-stmt state))]
                    {:type :if :cond c :body b :else e}))
      :while  (do (advance! state)
                  (expect! state :lparen)
                  (let [c (parse-expr state)
                        _ (expect! state :rparen)
                        _ (skip-terminators! state)
                        b (parse-stmt state)]
                    {:type :while :cond c :body b}))
      :do     (do (advance! state)
                  (skip-terminators! state)
                  (let [b (parse-stmt state)
                        _ (skip-terminators! state)
                        _ (expect! state :while)
                        _ (expect! state :lparen)
                        c (parse-expr state)
                        _ (expect! state :rparen)]
                    {:type :do-while :body b :cond c}))
      :for    (do (advance! state)
                  (expect! state :lparen)
                  ;; Two forms: for (var in array) ... or C-style for(;;)
                  (let [save-pos @(:pos state)
                        ;; Probe: name, in, name, rparen
                        a (peek-tok-at state 0)
                        b (peek-tok-at state 1)
                        c (peek-tok-at state 2)
                        d (peek-tok-at state 3)]
                    (if (and (= :name (:type a)) (= :in (:type b))
                             (= :name (:type c)) (= :rparen (:type d)))
                      (let [v (:value (advance! state))
                            _ (advance! state) ; in
                            arr (:value (advance! state))
                            _ (advance! state) ; rparen
                            _ (skip-terminators! state)
                            body (parse-stmt state)]
                        {:type :for-in :var v :array arr :body body})
                      (let [pre (when-not (at? state :semicolon)
                                  (parse-simple-stmt state))
                            _ (expect! state :semicolon)
                            cond-e (when-not (at? state :semicolon)
                                     (parse-expr state))
                            _ (expect! state :semicolon)
                            post (when-not (at? state :rparen)
                                   (parse-simple-stmt state))
                            _ (expect! state :rparen)
                            ;; Empty body? `for(...) ;` or `for(...) \n ;`
                            body (cond
                                   (at? state :semicolon)
                                   (do (advance! state) {:type :block :body []})
                                   :else
                                   (do (skip-terminators! state)
                                       (parse-stmt state)))]
                        {:type :for :pre pre :cond cond-e :post post :body body}))))
      :break    (do (advance! state) {:type :break})
      :continue (do (advance! state) {:type :continue})
      :next     (do (advance! state) {:type :next})
      :exit     (do (advance! state)
                    (let [code (when-not (#{:newline :semicolon :rbrace :eof}
                                          (:type (peek-tok state)))
                                 (parse-expr state))]
                      {:type :exit :status code}))
      ;; default — simple stmt
      (parse-simple-stmt state))))

(defn- parse-pattern [state]
  ;; Pattern is either `expr`, `expr , expr` (range), or nothing.
  (let [t (peek-tok state)]
    (cond
      (#{:lbrace :eof :newline :semicolon} (:type t)) []
      :else
      (let [e1 (parse-expr state)]
        (if (at? state :comma)
          (do (advance! state) [e1 (parse-expr state)])
          [e1])))))

(defn parse
  "Parse awk source text into a program AST:
     {:type :program :begin [stmts] :actions [{:pattern :stmts}] :end [stmts]}"
  [src]
  (let [tokens-vol (volatile! (tokenize src))
        state {:src src
               :tokens-vol tokens-vol
               :pos (volatile! 0)}
        ;; Make :tokens dynamic — re-read each peek
        state (assoc state :tokens @tokens-vol)
        ;; Wrap peek/advance to always read fresh @tokens-vol
        ]
    (with-redefs [peek-tok (fn [_s] (nth @tokens-vol @(:pos state) {:type :eof}))
                  peek-tok-at (fn [_s o] (nth @tokens-vol (+ @(:pos state) o) {:type :eof}))
                  advance! (fn [_s]
                             (let [t (nth @tokens-vol @(:pos state) {:type :eof})]
                               (vswap! (:pos state) inc) t))]
      (let [begin (transient [])
            actions (transient [])
            end (transient [])]
        (skip-terminators! state)
        (while (not= :eof (:type (peek-tok state)))
          (cond
            (at? state :function)
            ;; Skip `function name(args) { body }` — we don't support
            ;; calling user-defined functions, but we tolerate their
            ;; presence in scripts (often boilerplate that's unused).
            ;; If something tries to CALL the function, it'll surface
            ;; later as "unknown function" at eval time.
            (do (advance! state)             ; function
                (advance! state)             ; name
                (when (at? state :lparen)    ; (args)
                  (loop [depth 1]
                    (advance! state)
                    (cond
                      (= :lparen (:type (peek-tok state))) (recur (inc depth))
                      (= :rparen (:type (peek-tok state)))
                      (if (= depth 1) (advance! state) (recur (dec depth)))
                      :else (recur depth))))
                (skip-terminators! state)
                (when (at? state :lbrace) (parse-stmt-block state)))

            (at? state :BEGIN)
            (do (advance! state) (skip-terminators! state)
                (let [b (parse-stmt-block state)]
                  (conj! begin b)))

            (at? state :END)
            (do (advance! state) (skip-terminators! state)
                (let [b (parse-stmt-block state)]
                  (conj! end b)))

            :else
            (let [pat (parse-pattern state)
                  _ (skip-terminators! state)
                  body (cond
                         (at? state :lbrace) (parse-stmt-block state)
                         :else nil)]
              (conj! actions {:pattern pat :stmts body})))
          (skip-terminators! state))
        {:type :program
         :begin (persistent! begin)
         :actions (persistent! actions)
         :end (persistent! end)}))))

;; ============================================================================
;; Value model — string/number coercion semantics
;; ============================================================================

(defn- v-null []        {:tag :null})
(defn- v-num [n]        {:tag :num   :n (double n)})
(defn- v-str [s]        {:tag :str   :s (str s)})
(defn- v-numstr [s]     {:tag :numstr :s (str s)})

(defn- parse-num-prefix
  "Match leading numeric prefix of `s`. Returns [num parsed?] —
   `parsed?` indicates whether the *whole* string was consumed
   (numeric throughout). Recognises:
     1, 1.5, .5, 1e10, 1E+10
     0x22, -0xa, 0XABCDEF      (hex int)
     nan, NAN, +nan, -nan      (NaN, sign ignored)
     inf, INF, infinity, +inf  (positive infinity)
     -inf                       (negative infinity)"
  [^String s]
  (let [s (str/triml s)]
    (cond
      (str/blank? s) [0.0 false]
      :else
      (let [;; nan / inf
            m-nan (re-find #"^[+-]?(?i:nan)" s)
            m-inf (re-find #"^([+-]?)(?i:infinity|inf)" s)
            m-hex (re-find #"^([+-]?)0[xX]([0-9a-fA-F]+)" s)
            m-dec (re-find #"^[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?" s)]
        (cond
          m-nan [cc/NaN (= (count m-nan) (count s))]
          m-inf (let [sign (nth m-inf 1)
                      v (if (= "-" sign) cc/-inf cc/+inf)]
                  [v (= (count (first m-inf)) (count s))])
          m-hex (let [whole (first m-hex)
                      sign (nth m-hex 1)
                      digits (nth m-hex 2)
                      n (cc/plong-hex digits)
                      v (double (if (= "-" sign) (- n) n))]
                  [v (= (count whole) (count s))])
          m-dec [(cc/pdouble m-dec) (= (count m-dec) (count s))]
          :else [0.0 false])))))

(defn- v->num [v]
  (case (:tag v)
    :null 0.0
    :num  (:n v)
    (:str :numstr) (first (parse-num-prefix (:s v)))))

(defn- fmt-num
  "awk's CONVFMT/OFMT formatting. Integers print as integers; doubles
   use the current CONVFMT (read from cc/convfmt), then trim trailing
   zeros (so 2.86 not 2.86000)."
  ([n] (fmt-num n (cc/get-convfmt)))
  ([n fmt]
   (cond
     (cc/nan? n) "nan"
     (cc/inf? n) (if (pos? n) "inf" "-inf")
     (== n (cc/floor n)) (str (long n))
     :else
     (let [s (cc/fmt1 fmt n)
           i (.indexOf s "e")
           [mant exp] (if (>= i 0) [(subs s 0 i) (subs s i)] [s ""])
           mant' (if (str/includes? mant ".")
                   (-> mant
                       (str/replace #"0+$" "")
                       (str/replace #"\.$" ""))
                   mant)]
       (str mant' exp)))))

(defn- v->str [v]
  (case (:tag v)
    :null ""
    :num (fmt-num (:n v))
    (:str :numstr) (:s v)))

(defn- v-true-string?
  "Returns true if `v` should be compared as a string in mixed ==
   / < / etc. comparisons. :str always is. :numstr is iff its
   content does NOT parse as a number. :num/:null never are."
  [v]
  (case (:tag v)
    :str true
    :numstr (let [[_ parsed?] (parse-num-prefix (:s v))]
              (not parsed?))
    false))

(defn- v->bool [v]
  (case (:tag v)
    :null false
    :num (not (zero? (:n v)))
    :str (not (= "" (:s v)))
    :numstr (let [[n parsed?] (parse-num-prefix (:s v))]
              (if parsed? (not (zero? n)) (not (= "" (:s v)))))))

(defn- v-cmp
  "Awk comparison: if either side is a 'true string', compare as strings;
   else compare as numbers."
  [a b]
  (if (or (v-true-string? a) (v-true-string? b))
    (compare (v->str a) (v->str b))
    (compare (v->num a) (v->num b))))

;; ============================================================================
;; Interpreter — tree walking
;; ============================================================================

(defn- init-state
  [{:keys [fs ofs ors rs vars] :or {ofs " " ors "\n" rs "\n"}}]
  {:globals (atom (or vars {}))
   :arrays  (atom {})
   :fields  (atom [])     ; $1..$N (each a tagged value)
   :line    (atom (v-numstr ""))   ; $0 (tagged value)
   :specials (atom {"NR" (v-num 0) "NF" (v-num 0) "FNR" (v-num 0)
                    "FS" (v-str (or fs " "))
                    "OFS" (v-str ofs)
                    "ORS" (v-str ors)
                    "RS" (v-str rs)
                    "FILENAME" (v-str "")
                    "RSTART" (v-num 0)
                    "RLENGTH" (v-num 0)
                    "SUBSEP" (v-str (str (char 28)))
                    "CONVFMT" (v-str "%.6g")
                    "OFMT" (v-str "%.6g")
                    "ARGC" (v-num 1)})
   :out     (cc/sbuf)
   :exit?   (atom false)
   :exit-code (atom 0)
   :range-state (atom {})})

(def ^:private special-names
  #{"NR" "NF" "FNR" "FS" "OFS" "ORS" "RS" "FILENAME" "RSTART" "RLENGTH"
    "SUBSEP" "CONVFMT" "OFMT" "ARGC"})

(defn- special-var? [nm] (boolean (special-names nm)))

(defn- get-special [state nm]
  (or (get @(:specials state) nm) (v-null)))

(declare rebuild-line!)

(defn- set-special! [state nm v]
  (swap! (:specials state) assoc nm v)
  ;; Some special vars have side effects.
  (case nm
    "NF"
    (let [n (long (max 0 (v->num v)))
          cur @(:fields state)
          cur-len (count cur)]
      (cond
        (< n cur-len) (reset! (:fields state) (vec (take n cur)))
        (> n cur-len) (reset! (:fields state) (vec (concat cur (repeat (- n cur-len) (v-str ""))))))
      (rebuild-line! state)
      (swap! (:specials state) assoc "NF" v))
    "CONVFMT"
    ;; CONVFMT controls number→string conversion in expression context.
    ;; fmt-num reads from cc/convfmt; update it now so any later v->str
    ;; picks it up.
    (cc/set-convfmt! (v->str v))
    nil))

(defn- get-global [state nm]
  (or (get @(:globals state) nm) (v-null)))

(defn- set-global! [state nm v]
  (swap! (:globals state) assoc nm v))

(defn- get-array [state nm]
  (or (get @(:arrays state) nm) {}))

(defn- set-array-elt! [state nm key v]
  (swap! (:arrays state) update nm assoc key v))

(defn- delete-array-elt! [state nm key]
  (swap! (:arrays state) update nm dissoc key))

(defn- delete-array! [state nm]
  (swap! (:arrays state) dissoc nm))

;; ---- regex cache --------------------------------------------------------

(def ^:private regex-cache (atom {}))
(defn- compile-re
  "Compile `pat` to a platform regex with DOTALL semantics — awk's `.`
   matches newline. Cached by pattern text."
  [^String pat]
  (or (get @regex-cache pat)
      (let [p (cc/re-compile pat)]
        (swap! regex-cache assoc pat p) p)))

(defn- split-fields
  "Split `line` by FS. FS rules:
     \" \"  — whitespace-trim, split on runs of whitespace
     single char — split on that literal
     multi-char — treat as regex
   Preserves trailing empties (custom split that keeps them) so that
   `split(\"a,b,\", arr, \",\")` yields 3 elements (a, b, \"\")."
  [^String line ^String fs]
  (cond
    (= line "") []
    (= fs " ")  (let [t (str/trim line)]
                  (if (= "" t) [] (str/split t #"\s+")))
    :else
    (let [pat (if (= 1 (count fs)) (cc/re-quote fs) fs)
          re  (compile-re pat)]
      (cc/split-by-regex re line))))

(defn- split-and-populate! [state ^String line-str]
  (let [fs (v->str (get-special state "FS"))
        parts (split-fields line-str fs)
        fields (mapv v-numstr parts)]
    (reset! (:fields state) fields)
    (swap! (:specials state) assoc "NF" (v-num (count fields)))))

(defn- set-line!
  "Used per-record: $0 = input as :numstr."
  [state ^String line-str]
  (reset! (:line state) (v-numstr line-str))
  (split-and-populate! state line-str))

(defn- get-field [state n]
  (cond
    (zero? n) @(:line state)
    (neg? n) (v-str "")
    :else
    (let [fs @(:fields state)]
      (if (> n (count fs))
        (v-str "")
        (nth fs (dec n))))))

(defn- rebuild-line!
  "Re-join fields by OFS into $0 (as :str — rebuilt strings are always
   true strings); sync NF directly (bypasses set-special! to avoid the
   resize-recursion loop)."
  [state]
  (let [ofs (v->str (get-special state "OFS"))
        fs @(:fields state)]
    (reset! (:line state) (v-str (str/join ofs (mapv v->str fs))))
    (swap! (:specials state) assoc "NF" (v-num (count fs)))))

(defn- set-field! [state n v]
  (cond
    (zero? n)
    ;; $0 = expr — store the tagged value and re-split as :numstr fields.
    (do (reset! (:line state) v)
        (split-and-populate! state (v->str v)))

    (pos? n)
    (let [fs @(:fields state)
          cur-len (count fs)
          padded (if (>= cur-len n)
                   fs
                   (vec (concat fs (repeat (- n cur-len) (v-str "")))))
          new-fields (assoc padded (dec n) v)]
      (reset! (:fields state) new-fields)
      (rebuild-line! state))))

(declare eval-expr exec-stmt)

;; ---- built-in functions --------------------------------------------------

(defn- bi-length
  "length() — defaults to length of $0; length(s) → char count;
   length(array) → entry count (gawk extension, often relied on)."
  [args state ast-args]
  (cond
    (empty? args)
    (v-num (count (v->str (get-field state 0))))
    ;; Array form: the arg AST is a bare :var whose name refers to an
    ;; array we've seen. Distinguish from a string by checking the
    ;; array registry.
    (and (= 1 (count ast-args))
         (= :var (:type (first ast-args)))
         (contains? @(:arrays state) (:name (first ast-args))))
    (v-num (count (get @(:arrays state) (:name (first ast-args)))))
    :else (v-num (count (v->str (first args))))))

(defn- bi-substr [args]
  (let [s (v->str (first args))
        m (long (v->num (second args)))
        n (when (>= (count args) 3) (long (v->num (nth args 2))))
        ;; awk substr is 1-based; m<1 is clamped to 1 but the start
        ;; counts as if from m (so substr("hello",-2,5)=="he").
        start (max 1 m)
        end   (if n
                (min (count s) (+ m n -1))
                (count s))
        end   (max 0 end)
        start (min start (inc (count s)))]
    (v-str (if (<= start end)
             (subs s (dec start) end)
             ""))))

(defn- bi-index [args]
  (let [s (v->str (first args))
        t (v->str (second args))]
    (cond
      ;; POSIX: index(s, "") returns 0 (no match), not 1.
      (= "" t) (v-num 0)
      :else (v-num (inc (.indexOf ^String s ^String t))))))

(defn- bi-split [ast-args state]
  ;; `ast-args` are the *unevaluated* parser nodes, because we need to
  ;; reach inside the 2nd arg (the array reference) and 3rd (which
  ;; may be a literal /regex/ vs a string).
  (let [s (v->str (eval-expr state (first ast-args)))
        arr-name (let [a (second ast-args)]
                   (when (not= :var (:type a))
                     (throw (ex-info "split: 2nd arg must be array name" {})))
                   (:name a))
        third (when (>= (count ast-args) 3) (nth ast-args 2))
        [pat regex?] (cond
                       (nil? third)
                       [(v->str (get-special state "FS")) false]
                       (= :regex (:type third))
                       [(:pattern third) true]
                       :else
                       [(v->str (eval-expr state third)) false])
        ;; Empty separator → split into individual characters (gawk
        ;; extension that many scripts rely on).
        parts (cond
                (and (not regex?) (= "" pat))
                (cond (= "" s) []
                      :else (mapv str (seq s)))
                regex?
                (cond
                  (= "" s) []
                  :else (cc/split-by-regex (compile-re pat) s))
                :else (split-fields s pat))]
    ;; Clear and refill the array.
    (swap! (:arrays state) assoc arr-name
           (into {} (map-indexed (fn [i p] [(str (inc i)) (v-numstr p)])
                                 parts)))
    (v-num (count parts))))

(defn- expand-repl
  "Replace `&` with the matched text and `\\&` with literal `&` in an
   awk replacement string."
  [^String repl ^String matched]
  (let [sb (cc/sbuf)
        n (count repl)]
    (loop [i 0]
      (when (< i n)
        (let [c (.charAt repl i)]
          (cond
            (and (= c \\) (< (inc i) n)
                 (= \& (.charAt repl (inc i))))
            (do (cc/sappend! sb \&) (recur (+ i 2)))
            (= c \&)
            (do (cc/sappend! sb matched) (recur (inc i)))
            :else
            (do (cc/sappend! sb c) (recur (inc i)))))))
    (cc/sbstr sb)))

(defn- bi-sub [args state replace-all?]
  ;; `args` are unevaluated parser nodes — we need the AST shape of
  ;; the 1st (regex-literal or string-expr) and 3rd (LHS to write back).
  (let [re-pat (let [a (first args)]
                 (if (= :regex (:type a))
                   (:pattern a)
                   (v->str (eval-expr state a))))
        repl (v->str (eval-expr state (second args)))
        target-ref (if (>= (count args) 3) (nth args 2) {:type :field :index {:type :num :value 0}})
        target-val (eval-expr state target-ref)
        target-s (v->str target-val)
        re (compile-re re-pat)
        [new-s n] (cc/re-replace re target-s
                                 (fn [matched] (expand-repl repl matched))
                                 replace-all?)
        new-v (v-str new-s)]
    (case (:type target-ref)
      :var   (set-global! state (:name target-ref) new-v)
      :field (let [idx (long (v->num (eval-expr state (:index target-ref))))]
               (set-field! state idx new-v))
      :index (let [arr (:array target-ref)
                   k (v->str (eval-expr state (first (:index target-ref))))]
               (set-array-elt! state arr k new-v))
      nil)
    (v-num n)))

(defn- bi-match [args state]
  ;; `args` are unevaluated parser nodes.
  (let [s (v->str (eval-expr state (first args)))
        re-pat (let [a (second args)]
                 (if (= :regex (:type a))
                   (:pattern a)
                   (v->str (eval-expr state a))))
        hit (cc/re-find-pos (compile-re re-pat) s 0)]
    (if hit
      (do (set-special! state "RSTART" (v-num (inc (:start hit))))
          (set-special! state "RLENGTH" (v-num (- (:end hit) (:start hit))))
          (v-num (inc (:start hit))))
      (do (set-special! state "RSTART" (v-num 0))
          (set-special! state "RLENGTH" (v-num -1))
          (v-num 0)))))

(defn- translate-printf-fmt
  "awk's printf accepts %i %u %c (with numeric arg → byte) which the
   platform's `format` doesn't. Pre-process the format string and
   translate each spec, returning [final-fmt arg-fn-coll] where
   arg-fn-coll contains coercer-fns for each spec."
  [^String fmt]
  (let [sb (cc/sbuf)
        coercers (transient [])
        n (count fmt)]
    (loop [i 0]
      (when (< i n)
        (let [c (.charAt fmt i)]
          (cond
            (and (= c \%) (< (inc i) n))
            (let [[end spec-body kind] (loop [j (inc i)]
                                         (if (>= j n) [j (subs fmt (inc i) j) \s]
                                             (let [ch (.charAt fmt j)]
                                               (case ch
                                                 \% [(inc j) "" \%]
                                                 (\d \i) [(inc j) (subs fmt (inc i) j) \d]
                                                 (\u)    [(inc j) (subs fmt (inc i) j) \u]
                                                 (\x \X \o) [(inc j) (subs fmt (inc i) j) ch]
                                                 (\c)    [(inc j) (subs fmt (inc i) j) \c]
                                                 (\s)    [(inc j) (subs fmt (inc i) j) \s]
                                                 (\e \E \f \g \G) [(inc j) (subs fmt (inc i) j) ch]
                                                 (recur (inc j))))))]
              (cond
                (= kind \%) (cc/sappend! sb "%%")
                (= kind \u) (do (cc/sappend! sb \%) (cc/sappend! sb spec-body) (cc/sappend! sb \d)
                                (conj! coercers (fn [v] (long (v->num v)))))
                (= kind \c) (do (cc/sappend! sb \%) (cc/sappend! sb spec-body) (cc/sappend! sb \s)
                                (conj! coercers (fn [v]
                                                  (cond
                                                    (= :num (:tag v))
                                                    (str (char (long (:n v))))
                                                    :else (let [s (v->str v)]
                                                            (if (empty? s) "" (subs s 0 1)))))))
                (= kind \d) (do (cc/sappend! sb \%) (cc/sappend! sb spec-body) (cc/sappend! sb \d)
                                (conj! coercers (fn [v] (long (v->num v)))))
                (#{\x \X \o} kind)
                ;; Pre-format the radix conversion in the coercer and
                ;; splice via %s — goog.string.format on CLJS doesn't
                ;; handle these specifiers, so we keep the platform's
                ;; `format` agnostic to them.
                (do (cc/sappend! sb "%s")
                    (conj! coercers
                           (let [body spec-body
                                 upper? (= kind \X)]
                             (fn [v]
                               (let [n (long (v->num v))
                                     s (case kind
                                         \o (cc/to-octal n)
                                         (cc/to-hex n upper?))]
                                 (if (= "" body) s
                                     (cc/fmt1 (str "%" body "s") s)))))))
                (= kind \s) (do (cc/sappend! sb \%) (cc/sappend! sb spec-body) (cc/sappend! sb \s)
                                (conj! coercers (fn [v] (v->str v))))
                (#{\e \E \f} kind)
                (do (cc/sappend! sb \%) (cc/sappend! sb spec-body) (cc/sappend! sb kind)
                    (conj! coercers (fn [v] (double (v->num v)))))
                (#{\g \G} kind)
                ;; The platform's `%g` pads to full precision (1234.50);
                ;; awk trims (1234.5). Format via cc/fmt1, post-trim
                ;; trailing zeros, splice into the output via %s.
                (do (cc/sappend! sb "%s")
                    (conj! coercers
                           (let [jfmt (str "%" spec-body kind)
                                 exp-ch (if (= kind \G) "E" "e")]
                             (fn [v]
                               (let [s (cc/fmt1 jfmt (double (v->num v)))
                                     i (.indexOf s exp-ch)
                                     [mant exp] (if (>= i 0)
                                                  [(subs s 0 i) (subs s i)]
                                                  [s ""])
                                     mant' (if (str/includes? mant ".")
                                             (-> mant
                                                 (str/replace #"0+$" "")
                                                 (str/replace #"\.$" ""))
                                             mant)]
                                 (str mant' exp)))))))
              (recur end))
            :else
            (do (cc/sappend! sb c) (recur (inc i)))))))
    [(cc/sbstr sb) (persistent! coercers)]))

(defn- bi-sprintf [args]
  (let [fmt (v->str (first args))
        [final-fmt coercers] (translate-printf-fmt fmt)
        coerced (mapv (fn [c v] (c v)) coercers (rest args))]
    (v-str (cc/fmt-many final-fmt coerced))))

(defn- bi-tolower [args] (v-str (str/lower-case (v->str (first args)))))
(defn- bi-toupper [args] (v-str (str/upper-case (v->str (first args)))))
(defn- bi-int     [args] (v-num (long (v->num (first args)))))
(defn- bi-sqrt    [args] (v-num (Math/sqrt (v->num (first args)))))
(defn- bi-exp     [args] (v-num (Math/exp (v->num (first args)))))
(defn- bi-log     [args] (v-num (Math/log (v->num (first args)))))
(defn- bi-sin     [args] (v-num (Math/sin (v->num (first args)))))
(defn- bi-cos     [args] (v-num (Math/cos (v->num (first args)))))
(defn- bi-atan2   [args] (v-num (Math/atan2 (v->num (first args)) (v->num (second args)))))

;; ---- Expression eval -----------------------------------------------------

(defn- arith [op a b]
  (let [x (v->num a) y (v->num b)]
    (v-num (case op
             :add (+ x y) :sub (- x y) :mul (* x y)
             :div (if (zero? y) (throw (ex-info "division by zero" {})) (/ x y))
             :mod (if (zero? y) (throw (ex-info "modulo by zero" {})) (rem x y))
             :pow (Math/pow x y)))))

(defn eval-expr [state expr]
  (case (:type expr)
    :num (v-num (:value expr))
    :str (v-str (:value expr))
    :regex
    ;; Bare regex in expression position → $0 ~ /pat/
    (v-num (if (cc/re-find-pos (compile-re (:pattern expr))
                               (v->str (get-field state 0)) 0)
             1 0))

    :paren
    (eval-expr state (:inner expr))

    :var
    (let [nm (:name expr)]
      (if (special-var? nm)
        (get-special state nm)
        (get-global state nm)))

    :field
    (let [n (long (v->num (eval-expr state (:index expr))))]
      (get-field state n))

    :index
    ;; awk quirk: reading a[k] inserts an empty entry if absent (so
    ;; later `(k in a)` is true). Match that.
    (let [nm (:array expr)
          arr (get-array state nm)
          k (v->str (eval-expr state (first (:index expr))))]
      (if (contains? arr k)
        (get arr k)
        (do (set-array-elt! state nm k (v-null))
            (v-null))))

    :unary
    (let [v (eval-expr state (:value expr))]
      (case (:op expr)
        :not (v-num (if (v->bool v) 0 1))
        :neg (v-num (- (v->num v)))
        :pos (v-num (v->num v))))

    :binary
    (let [op (:op expr)]
      (case op
        :and (v-num (if (and (v->bool (eval-expr state (:left expr)))
                             (v->bool (eval-expr state (:right expr))))
                      1 0))
        :or  (v-num (if (or (v->bool (eval-expr state (:left expr)))
                            (v->bool (eval-expr state (:right expr))))
                      1 0))
        :concat (v-str (str (v->str (eval-expr state (:left expr)))
                            (v->str (eval-expr state (:right expr)))))
        :match (let [s (v->str (eval-expr state (:left expr)))
                     re-pat (let [r (:right expr)]
                              (if (= :regex (:type r))
                                (:pattern r)
                                (v->str (eval-expr state r))))]
                 (v-num (if (cc/re-find-pos (compile-re re-pat) s 0) 1 0)))
        :not-match (let [s (v->str (eval-expr state (:left expr)))
                         re-pat (let [r (:right expr)]
                                  (if (= :regex (:type r))
                                    (:pattern r)
                                    (v->str (eval-expr state r))))]
                     (v-num (if (cc/re-find-pos (compile-re re-pat) s 0) 0 1)))
        (let [l (eval-expr state (:left expr))
              r (eval-expr state (:right expr))]
          (case op
            :eq (v-num (if (zero? (v-cmp l r)) 1 0))
            :ne (v-num (if (zero? (v-cmp l r)) 0 1))
            :lt (v-num (if (neg? (v-cmp l r)) 1 0))
            :le (v-num (if (<= (v-cmp l r) 0) 1 0))
            :gt (v-num (if (pos? (v-cmp l r)) 1 0))
            :ge (v-num (if (>= (v-cmp l r) 0) 1 0))
            (arith op l r)))))

    :cond
    (if (v->bool (eval-expr state (:cond expr)))
      (eval-expr state (:true expr))
      (eval-expr state (:false expr)))

    :in
    (let [k (v->str (eval-expr state (first (:index expr))))]
      (v-num (if (contains? (get-array state (:array expr)) k) 1 0)))

    :assign
    (let [l (:left expr)
          ;; Pre-compute any side-effecting subexpression of the target
          ;; (e.g. arr[k++]) BEFORE evaluating the RHS, to match awk's
          ;; left-to-right semantics.
          field-idx (when (= :field (:type l)) (long (v->num (eval-expr state (:index l)))))
          arr-key (when (= :index (:type l)) (v->str (eval-expr state (first (:index l)))))
          v (eval-expr state (:right expr))]
      (case (:type l)
        :var   (if (special-var? (:name l))
                 (set-special! state (:name l) v)
                 (set-global! state (:name l) v))
        :field (set-field! state field-idx v)
        :index (set-array-elt! state (:array l) arr-key v))
      v)

    :aug-assign
    (let [l (:left expr)
          ;; Awk's evaluation order: lvalue index/key first (its subexpr
          ;; can be side-effecting), then RHS (side-effects propagate),
          ;; THEN read the current value.
          field-idx (when (= :field (:type l)) (long (v->num (eval-expr state (:index l)))))
          arr-key (when (= :index (:type l)) (v->str (eval-expr state (first (:index l)))))
          rhs (eval-expr state (:right expr))
          cur (case (:type l)
                :var   (if (special-var? (:name l))
                         (get-special state (:name l))
                         (get-global state (:name l)))
                :field (get-field state field-idx)
                :index (let [arr (get-array state (:array l))]
                         (or (get arr arr-key) (v-null))))
          new (arith (:op expr) cur rhs)]
      (case (:type l)
        :var   (if (special-var? (:name l))
                 (set-special! state (:name l) new)
                 (set-global! state (:name l) new))
        :field (set-field! state field-idx new)
        :index (set-array-elt! state (:array l) arr-key new))
      new)

    :incr
    (let [target (:expr expr)
          field-idx (when (= :field (:type target)) (long (v->num (eval-expr state (:index target)))))
          arr-key (when (= :index (:type target)) (v->str (eval-expr state (first (:index target)))))
          cur (case (:type target)
                :var   (if (special-var? (:name target))
                         (get-special state (:name target))
                         (get-global state (:name target)))
                :field (get-field state field-idx)
                :index (let [arr (get-array state (:array target))]
                         (or (get arr arr-key) (v-null))))
          n (v->num cur)
          new (v-num (case (:op expr)
                       :incr (inc n)
                       :decr (dec n)))]
      (case (:type target)
        :var   (if (special-var? (:name target))
                 (set-special! state (:name target) new)
                 (set-global! state (:name target) new))
        :field (set-field! state field-idx new)
        :index (set-array-elt! state (:array target) arr-key new))
      (if (:pre? expr) new (v-num n)))

    :call
    (let [args (mapv #(eval-expr state %) (:args expr))]
      (case (:func expr)
        :length (bi-length args state (:args expr))
        :substr (bi-substr args)
        :index  (bi-index args)
        :split  (bi-split (:args expr) state)
        :sub    (bi-sub (:args expr) state false)
        :gsub   (bi-sub (:args expr) state true)
        :sprintf (bi-sprintf args)
        :match  (bi-match (:args expr) state)
        :tolower (bi-tolower args)
        :toupper (bi-toupper args)
        :int    (bi-int args)
        :sqrt   (bi-sqrt args)
        :exp    (bi-exp args)
        :log    (bi-log args)
        :sin    (bi-sin args)
        :cos    (bi-cos args)
        :atan2  (bi-atan2 args)
        :system (throw (ex-info "awk: system() refused in sandbox" {}))
        (throw (ex-info (str "awk: unknown function " (:func expr)) {}))))

    (throw (ex-info (str "eval: unknown expr type " (:type expr)) {:expr expr}))))

;; ---- Statement exec ------------------------------------------------------

(defn- emit [state ^String s]
  (cc/sappend! (:out state) s))

(defn exec-stmt [state stmt]
  (case (:type stmt)
    :block
    (doseq [s (:body stmt)] (exec-stmt state s))

    :expr-stmt
    (eval-expr state (:expr stmt))

    :print
    (let [args (:args stmt)
          ofs (v->str (get-special state "OFS"))
          ors (v->str (get-special state "ORS"))
          ofmt (v->str (get-special state "OFMT"))
          ;; OFMT formats numeric values (only :num — :numstr stays
          ;; as the original string).
          v->print-str (fn [v]
                         (case (:tag v)
                           :num (fmt-num (:n v) ofmt)
                           (v->str v)))
          parts (if (empty? args)
                  [(v->str (get-field state 0))]
                  (mapv #(v->print-str (eval-expr state %)) args))]
      (emit state (str (str/join ofs parts) ors)))

    :printf
    (let [args (mapv #(eval-expr state %) (:args stmt))
          fmt (v->str (first args))
          [final-fmt coercers] (translate-printf-fmt fmt)
          coerced (mapv (fn [c v] (c v)) coercers (rest args))]
      (emit state (cc/fmt-many final-fmt coerced)))

    :if
    (if (v->bool (eval-expr state (:cond stmt)))
      (exec-stmt state (:body stmt))
      (when-let [e (:else stmt)] (exec-stmt state e)))

    :while
    (try
      (while (v->bool (eval-expr state (:cond stmt)))
        (when-let [ifn (:interrupt-fn state)] (ifn))
        (try (exec-stmt state (:body stmt))
             (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
               (if (= :awk/continue (:awk/control (ex-data e))) nil (throw e)))))
      (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
        (if (= :awk/break (:awk/control (ex-data e))) nil (throw e))))

    :do-while
    (try
      (loop []
        (when-let [ifn (:interrupt-fn state)] (ifn))
        (try (exec-stmt state (:body stmt))
             (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
               (when-not (= :awk/continue (:awk/control (ex-data e))) (throw e))))
        (when (v->bool (eval-expr state (:cond stmt))) (recur)))
      (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
        (when-not (= :awk/break (:awk/control (ex-data e))) (throw e))))

    :for
    (do (when-let [pre (:pre stmt)] (exec-stmt state pre))
        (try
          (while (if-let [c (:cond stmt)] (v->bool (eval-expr state c)) true)
            (when-let [ifn (:interrupt-fn state)] (ifn))
            (try (exec-stmt state (:body stmt))
                 (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
                   (when-not (= :awk/continue (:awk/control (ex-data e))) (throw e))))
            (when-let [post (:post stmt)] (exec-stmt state post)))
          (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
            (when-not (= :awk/break (:awk/control (ex-data e))) (throw e)))))

    :for-in
    (try
      (doseq [k (keys (get-array state (:array stmt)))]
        (when-let [ifn (:interrupt-fn state)] (ifn))
        (set-global! state (:var stmt) (v-str k))
        (try (exec-stmt state (:body stmt))
             (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
               (when-not (= :awk/continue (:awk/control (ex-data e))) (throw e)))))
      (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
        (when-not (= :awk/break (:awk/control (ex-data e))) (throw e))))

    :break    (throw (ex-info "break"    {:awk/control :awk/break}))
    :continue (throw (ex-info "continue" {:awk/control :awk/continue}))
    :next     (throw (ex-info "next"     {:awk/control :awk/next}))
    :exit     (do (when-let [c (:status stmt)]
                    (reset! (:exit-code state) (long (v->num (eval-expr state c)))))
                  (reset! (:exit? state) true)
                  (throw (ex-info "exit" {:awk/control :awk/exit})))
    :delete
    (if (:index stmt)
      (let [k (v->str (eval-expr state (first (:index stmt))))]
        (delete-array-elt! state (:array stmt) k))
      (delete-array! state (:array stmt)))

    (throw (ex-info (str "unknown stmt type " (:type stmt)) {:stmt stmt}))))

;; ---- Pattern matching for actions ---------------------------------------

(defn- pattern-match? [state action-idx pat]
  (case (count pat)
    0 true ; no pattern → match every record
    1 (let [p (first pat)]
        (if (= :regex (:type p))
          (boolean (cc/re-find-pos (compile-re (:pattern p))
                                   (v->str (get-field state 0)) 0))
          (v->bool (eval-expr state p))))
    2 (let [[start end] pat
            in? (get @(:range-state state) action-idx false)
            start? (if (= :regex (:type start))
                     (boolean (cc/re-find-pos (compile-re (:pattern start))
                                              (v->str (get-field state 0)) 0))
                     (v->bool (eval-expr state start)))
            end? (if (= :regex (:type end))
                   (boolean (cc/re-find-pos (compile-re (:pattern end))
                                            (v->str (get-field state 0)) 0))
                   (v->bool (eval-expr state end)))]
        (cond
          (and (not in?) start?)
          (do (swap! (:range-state state) assoc action-idx (not end?))
              true)
          in?
          (do (when end? (swap! (:range-state state) assoc action-idx false))
              true)
          :else false))))

(defn- run-record! [state program record]
  (when-let [ifn (:interrupt-fn state)] (ifn))
  (set-line! state record)
  (set-special! state "NR" (v-num (inc (long (v->num (get-special state "NR"))))))
  (set-special! state "FNR" (v-num (inc (long (v->num (get-special state "FNR"))))))
  (try
    (doseq [[idx {:keys [pattern stmts]}] (map-indexed vector (:actions program))]
      (when (pattern-match? state idx pattern)
        (try
          (if stmts
            (exec-stmt state stmts)
            ;; Implicit `{print}`
            (exec-stmt state {:type :print :args []}))
          (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
            (when-not (= :awk/next (:awk/control (ex-data e))) (throw e))
            (throw e)))))
    (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
      (when-not (= :awk/next (:awk/control (ex-data e))) (throw e)))))

;; ============================================================================
;; Top-level entry — called from posix.clj/awk
;; ============================================================================

(defn- run-blocks [state blocks]
  (try
    (doseq [b blocks] (exec-stmt state b))
    (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
      (when-not (= :awk/exit (:awk/control (ex-data e))) (throw e)))))

(defn- split-input-by-rs
  "Split a raw input string into records using current RS. Standard
   awk semantics: a single-char RS is a literal separator; multi-char
   RS is treated as a regex; default \"\\n\" is paragraph-mode-free
   line split. Trailing empty record after a final terminator is
   dropped (so `a\\nb\\n` yields 2 records, not 3)."
  [^String raw ^String rs]
  (if (= "" raw) []
    (let [pat (if (= 1 (count rs)) (cc/re-quote rs) rs)
          re  (compile-re pat)
          parts (cc/split-by-regex re raw)
          parts (if (and (seq parts) (= "" (last parts)))
                  (vec (butlast parts))
                  parts)]
      parts)))

(defn run
  "Top-level awk run.

   Options:
     :program     the awk source (string) — required
     :input       seq of input record strings (already split by RS), OR
     :raw-input   a single string to split internally by RS after BEGIN
     :fs          initial FS (default \" \")
     :vars        {name → value-string} from `-v VAR=val` (initialised
                  as :numstr before BEGIN runs)
     :interrupt-fn 0-arg fn called at every loop boundary (record loop,
                  for/while/do-while bodies). Throws to abort. Pair
                  with muschel.budget/deadline-interrupt for timeouts.

   Returns {:stdout str :exit int}."
  [{:keys [program input raw-input fs vars interrupt-fn]}]
  ;; Reset CONVFMT so a prior run's setting doesn't leak in.
  (cc/set-convfmt! "%.6g")
  (let [state (-> (init-state {:fs fs
                               :vars (into {} (for [[k v] vars] [k (v-numstr v)]))})
                  (cond-> interrupt-fn (assoc :interrupt-fn interrupt-fn)))
        ast (parse program)]
    ;; BEGIN — may change RS, so don't split raw-input yet
    (run-blocks state (:begin ast))
    ;; Per-record actions. We also iterate records when ONLY an END
    ;; block is present, so its body can rely on NR / NF / $0 from
    ;; the last record (matches gawk + goawk).
    (when (and (or (seq (:actions ast)) (seq (:end ast)))
               (not @(:exit? state)))
      (let [records (cond
                      (some? input) input
                      (some? raw-input)
                      (split-input-by-rs raw-input
                                         (v->str (get-special state "RS")))
                      :else [])]
        (try
          (doseq [record records :while (not @(:exit? state))]
            (run-record! state ast record))
          (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
            (when-not (= :awk/exit (:awk/control (ex-data e))) (throw e))))))
    ;; END
    (run-blocks state (:end ast))
    {:stdout (cc/sbstr (:out state))
     :exit @(:exit-code state)}))
