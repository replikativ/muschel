(ns muschel.arith
  "Bash arithmetic evaluator: $((expr)), ((expr)), let.

   Bash arithmetic is integer-only, C-like. Variable references use the
   bare name (no `$` needed inside arithmetic context, though `$x` works
   too). Assignments and ++/-- mutate the env.

   ## API

       (evaluate env \"x + 1\")        → [env' int]
       (evaluate env \"x = 5, x++\")    → [env' int]
       (evaluate-truthy env expr)      → [env' bool]   ; for ((cond)) tests

   ## Supported operators (full bash set)

   precedence (high → low):
     ++ --        postfix
     ++ -- + - ! ~  unary
     **           exponent (right-assoc)
     * / %        multiplicative
     + -          additive
     << >>        shift
     < <= > >=    relational
     == !=        equality
     &            bit-and
     ^            bit-xor
     |            bit-or
     &&           logical-and (short-circuit)
     ||           logical-or  (short-circuit)
     ?:           ternary     (right-assoc)
     = += -= *= /= %= **= <<= >>= &= ^= |=   assignment (right-assoc)
     ,            comma (left-assoc; result is rightmost)

   ## Integer literals

     decimal    42
     octal      0NNN     (leading 0)
     hex        0xNNN    (or 0XNNN)
     base       B#NN     (B in 2..64)"
  (:require [clojure.string :as str]
            [muschel.env :as env]
            [muschel.errors :as err]))

;; ============================================================================
;; Portable number parsing
;; ============================================================================

(defn- parse-long-base
  "Parse `s` as a long in the given base. Throws on bad input."
  [^String s base]
  #?(:clj  (Long/parseLong s base)
     :cljs (let [n (js/parseInt s base)]
             (if (js/isNaN n)
               (throw (ex-info (str "not a number in base " base ": " s) {}))
               n))))

(defn- parse-long-dec [^String s] (parse-long-base s 10))

(defn- ipow ^long [^long a ^long b]
  ;; bash treats `**` overflow by wrapping into a 64-bit signed int.
  ;; Math/pow returns a double; for huge `b`, Infinity → Long throws.
  ;; Walk iteratively with multiplication; let JVM Long overflow happen
  ;; (matches bash's behavior on most platforms).
  (cond
    (neg? b) 0
    (zero? b) 1
    :else
    (loop [acc 1 i 0]
      (if (>= i b) acc (recur (unchecked-multiply acc a) (inc i))))))

;; ============================================================================
;; Tokenizer
;; ============================================================================

(defn- arith-error [^String src ^long pos msg]
  (err/error! msg
              {:type ::arith-error
               :line 1
               :col (inc pos)
               :offset pos
               :source src}))

(defn- ch-code
  "Portable char→int. In JVM Clojure `(int \\0)` returns 48; in cljs
   chars are 1-char strings and `int` coerces as a number, so we use
   `.charCodeAt`."
  [c]
  #?(:clj  (int ^Character c)
     :cljs (if (string? c) (.charCodeAt c 0) c)))

(defn- digit? [c] (and c (let [n (ch-code c)] (and (>= n 48) (<= n 57)))))
(defn- hex-digit? [c]
  (and c (let [n (ch-code c)]
           (or (and (>= n 48) (<= n 57))
               (and (>= n 65) (<= n 70))
               (and (>= n 97) (<= n 102))))))
(defn- name-start? [c]
  (and c (let [n (ch-code c)]
           (or (and (>= n 65) (<= n 90))
               (and (>= n 97) (<= n 122))
               (= c \_)))))
(defn- name-char? [c] (or (name-start? c) (digit? c)))
(defn- ws? [c] (and c (or (= c \space) (= c \tab) (= c \newline) (= c \return))))

;; Operators, ordered longest-first so we can find longest match.
(def ^:private ops
  ["**=" "<<=" ">>=" "&&" "||" "==" "!=" "<=" ">="
   "<<" ">>" "++" "--" "**" "+=" "-=" "*=" "/=" "%=" "&=" "^=" "|="
   "+" "-" "*" "/" "%" "<" ">" "&" "|" "^" "~" "!" "=" "?" ":" "," "(" ")"])

(defn- match-op
  "Try to match an operator at position `pos`. Returns the matched
   string (longest-first), or nil."
  [^String src ^long pos]
  (let [n (count src)]
    (some (fn [^String op]
            (let [oplen (count op)]
              (when (and (<= (+ pos oplen) n)
                         (= op (subs src pos (+ pos oplen))))
                op)))
          ops)))

(defn- scan-int
  "Scan an integer literal starting at `pos`. Returns [int new-pos]."
  [^String src ^long pos]
  (let [n (count src)
        c0 (.charAt src pos)]
    (cond
      ;; 0x / 0X hex
      (and (= c0 \0) (< (inc pos) n)
           (#{\x \X} (.charAt src (inc pos))))
      (let [start (+ pos 2)
            end (loop [i start]
                  (if (and (< i n) (hex-digit? (.charAt src i)))
                    (recur (inc i)) i))]
        (when (= end start) (arith-error src pos "expected hex digits after 0x"))
        [(parse-long-base (subs src start end) 16) end])

      ;; base#N (e.g., 2#1010)
      ;; Detect by scanning digits then `#`. Only after we know there are
      ;; subsequent digits, treat as base-prefixed.
      (digit? c0)
      (let [end-base (loop [i pos]
                       (if (and (< i n) (digit? (.charAt src i)))
                         (recur (inc i)) i))
            base-char (when (< end-base n) (.charAt src end-base))]
        (cond
          (= base-char \#)
          (let [base (parse-long-dec (subs src pos end-base))
                start (inc end-base)
                end (loop [i start]
                      (if (and (< i n)
                               (let [c (.charAt src i)]
                                 (or (name-char? c) (= c \@) (= c \_))))
                        (recur (inc i)) i))]
            (when-not (<= 2 base 64)
              (arith-error src pos (str "invalid base " base ", must be 2..64")))
            [(parse-long-base (subs src start end) base) end])

          ;; Leading 0 + more digits = octal
          (and (= c0 \0) (> end-base (inc pos)))
          [(parse-long-base (subs src pos end-base) 8) end-base]

          :else
          [(parse-long-dec (subs src pos end-base)) end-base]))

      :else (arith-error src pos (str "unexpected character `" c0 "'")))))

(defn- scan-name [^String src ^long pos]
  (let [n (count src)
        end (loop [i pos]
              (if (and (< i n) (name-char? (.charAt src i)))
                (recur (inc i)) i))]
    [(subs src pos end) end]))

(defn- tokenize [^String src]
  (let [n (count src)
        out (volatile! [])]
    (loop [pos 0]
      (cond
        (>= pos n)
        (do (vswap! out conj {:type :eof :pos pos}) @out)

        (ws? (.charAt src pos))
        (recur (inc pos))

        ;; $name — dollar-prefixed var; just strip the $
        (= (.charAt src pos) \$)
        (let [[nm end] (scan-name src (inc pos))]
          (when (= "" nm) (arith-error src pos "expected variable name after `$'"))
          (vswap! out conj {:type :name :value nm :pos pos})
          (recur end))

        (digit? (.charAt src pos))
        (let [[v end] (scan-int src pos)]
          (vswap! out conj {:type :num :value v :pos pos})
          (recur end))

        (name-start? (.charAt src pos))
        (let [[nm end] (scan-name src pos)]
          (vswap! out conj {:type :name :value nm :pos pos})
          (recur end))

        :else
        (if-let [op (match-op src pos)]
          (do (vswap! out conj {:type :op :value op :pos pos})
              (recur (+ pos (count op))))
          (arith-error src pos
                       (str "unexpected character `" (.charAt src pos) "'")))))))

;; ============================================================================
;; Parser: produces an expression AST
;;   {:type :num :value n}
;;   {:type :name :name s}
;;   {:type :unary :op str :operand expr}
;;   {:type :postfix :op str :operand expr}
;;   {:type :binary :op str :left expr :right expr}
;;   {:type :ternary :cond expr :then expr :else expr}
;;   {:type :assign :op str :target name-str :value expr}
;;   {:type :comma :exprs [expr]}
;; ============================================================================

(defn- p-state [tokens] {:toks (vec tokens) :pos (volatile! 0)})
(defn- p-peek [p] (get (:toks p) @(:pos p)))
(defn- p-peek-at [p k] (get (:toks p) (+ @(:pos p) k)))
(defn- p-advance! [p] (let [t (p-peek p)] (vswap! (:pos p) inc) t))
(defn- op? [t s] (and t (= :op (:type t)) (= s (:value t))))

(declare parse-expr)

(defn- parse-primary [p src]
  (let [t (p-peek p)]
    (cond
      (= :num (:type t)) (do (p-advance! p) {:type :num :value (:value t)})
      (= :name (:type t)) (do (p-advance! p) {:type :name :name (:value t)})
      (op? t "(")
      (do (p-advance! p)
          (let [e (parse-expr p src)]
            (when-not (op? (p-peek p) ")")
              (arith-error src (:pos (p-peek p)) "expected `)'"))
            (p-advance! p)
            e))
      :else
      (arith-error src (or (:pos t) 0) (str "unexpected token " (pr-str t))))))

(defn- parse-postfix [p src]
  (let [base (parse-primary p src)
        t (p-peek p)]
    (if (or (op? t "++") (op? t "--"))
      (do (p-advance! p)
          {:type :postfix :op (:value t) :operand base})
      base)))

(defn- parse-unary [p src]
  (let [t (p-peek p)]
    (cond
      (or (op? t "+") (op? t "-") (op? t "!") (op? t "~")
          (op? t "++") (op? t "--"))
      (do (p-advance! p)
          {:type :unary :op (:value t) :operand (parse-unary p src)})
      :else
      (parse-postfix p src))))

(defn- parse-exp [p src]
  (let [base (parse-unary p src)]
    (if (op? (p-peek p) "**")
      (do (p-advance! p)
          {:type :binary :op "**" :left base :right (parse-exp p src)})
      base)))

(defn- left-assoc [p src sub-fn ops]
  (loop [left (sub-fn p src)]
    (let [t (p-peek p)]
      (if (and (= :op (:type t)) (ops (:value t)))
        (do (p-advance! p)
            (recur {:type :binary :op (:value t) :left left :right (sub-fn p src)}))
        left))))

(defn- parse-mul [p src]   (left-assoc p src parse-exp   #{"*" "/" "%"}))
(defn- parse-add [p src]   (left-assoc p src parse-mul   #{"+" "-"}))
(defn- parse-shift [p src] (left-assoc p src parse-add   #{"<<" ">>"}))
(defn- parse-cmp [p src]   (left-assoc p src parse-shift #{"<" ">" "<=" ">="}))
(defn- parse-eq [p src]    (left-assoc p src parse-cmp   #{"==" "!="}))
(defn- parse-band [p src]  (left-assoc p src parse-eq    #{"&"}))
(defn- parse-bxor [p src]  (left-assoc p src parse-band  #{"^"}))
(defn- parse-bor [p src]   (left-assoc p src parse-bxor  #{"|"}))
(defn- parse-land [p src]  (left-assoc p src parse-bor   #{"&&"}))
(defn- parse-lor [p src]   (left-assoc p src parse-land  #{"||"}))

(defn- parse-ternary [p src]
  (let [c (parse-lor p src)]
    (if (op? (p-peek p) "?")
      (do (p-advance! p)
          (let [then-e (parse-expr p src)]
            (when-not (op? (p-peek p) ":")
              (arith-error src (:pos (p-peek p)) "expected `:' in ternary"))
            (p-advance! p)
            {:type :ternary :cond c :then then-e :else (parse-ternary p src)}))
      c)))

(def ^:private assign-ops #{"=" "+=" "-=" "*=" "/=" "%=" "**=" "<<=" ">>=" "&=" "|=" "^="})

(defn- parse-assign [p src]
  (let [save @(:pos p)
        lhs (parse-ternary p src)
        t (p-peek p)]
    (if (and (= :op (:type t)) (assign-ops (:value t)))
      (do
        (when-not (= :name (:type lhs))
          (arith-error src (:pos t) "left side of assignment must be a variable"))
        (p-advance! p)
        {:type :assign :op (:value t) :target (:name lhs)
         :value (parse-assign p src)})
      lhs)))

(defn- parse-expr [p src]
  (let [first-e (parse-assign p src)]
    (if (op? (p-peek p) ",")
      (let [out (volatile! [first-e])]
        (loop []
          (when (op? (p-peek p) ",")
            (p-advance! p)
            (vswap! out conj (parse-assign p src))
            (recur)))
        {:type :comma :exprs @out})
      first-e)))

;; ============================================================================
;; Evaluator
;; ============================================================================

(declare evaluate)

(defn- env-get-int-rec [env nm depth]
  (let [v (env/get-var env nm)]
    (cond
      (= "" v) 0
      (>= depth 16) 0   ; depth limit guards against `a=a` cycles
      :else
      (try (parse-long-dec (str/trim v))
           (catch #?(:clj Exception :cljs js/Error) _
             ;; Try evaluating the value as an expression (handles
             ;; the `a=b; b=5; $((a))` chain bash supports).
             (try (let [[_ result] (evaluate env (str/trim v))]
                    result)
                  (catch #?(:clj Throwable :cljs :default) _
                    ;; Fall back to leading-digits scan.
                    (let [m (re-find #"^-?\d+" (str/trim v))]
                      (if m (parse-long-dec m) 0)))))))))

(defn- env-get-int
  "Read variable `name` as integer with bash's recursive expression
   semantics."
  [env nm]
  (env-get-int-rec env nm 0))

(defn- env-set-int [env nm v]
  (env/set-var env nm (str v)))

(defn- truthy? [n] (not (zero? n)))
(defn- ->bool [b] (if b 1 0))

(declare eval-expr)

(defn- apply-binary [op a b]
  (case op
    "+"  (+ a b)
    "-"  (- a b)
    "*"  (* a b)
    "/"  (if (zero? b)
           (err/error! "division by zero" {:type ::arith-error})
           (quot a b))
    "%"  (if (zero? b)
           (err/error! "division by zero" {:type ::arith-error})
           (mod a b))
    "**" (ipow a b)
    "<<" (bit-shift-left a b)
    ">>" (bit-shift-right a b)
    "<"  (->bool (< a b))
    ">"  (->bool (> a b))
    "<=" (->bool (<= a b))
    ">=" (->bool (>= a b))
    "==" (->bool (= a b))
    "!=" (->bool (not= a b))
    "&"  (bit-and a b)
    "^"  (bit-xor a b)
    "|"  (bit-or a b)))

(defn- eval-expr [env ast]
  (case (:type ast)
    :num   [env (:value ast)]
    :name  [env (env-get-int env (:name ast))]

    :unary
    (let [[env' v] (eval-expr env (:operand ast))]
      (case (:op ast)
        "+" [env' v]
        "-" [env' (- v)]
        "!" [env' (->bool (zero? v))]
        "~" [env' (bit-not v)]
        ("++" "--")
        (do (when-not (= :name (:type (:operand ast)))
              (err/error! "++/-- requires a variable"
                          {:type ::arith-error}))
            (let [nm (:name (:operand ast))
                  old (env-get-int env nm)
                  new (if (= "++" (:op ast)) (inc old) (dec old))]
              [(env-set-int env nm new) new]))))

    :postfix
    (do (when-not (= :name (:type (:operand ast)))
          (err/error! "++/-- requires a variable"
                      {:type ::arith-error}))
        (let [nm (:name (:operand ast))
              old (env-get-int env nm)
              new (if (= "++" (:op ast)) (inc old) (dec old))]
          [(env-set-int env nm new) old]))     ; postfix returns OLD value

    :binary
    (case (:op ast)
      ;; Short-circuit logical ops
      "&&" (let [[env1 l] (eval-expr env (:left ast))]
             (if (zero? l)
               [env1 0]
               (let [[env2 r] (eval-expr env1 (:right ast))]
                 [env2 (->bool (truthy? r))])))
      "||" (let [[env1 l] (eval-expr env (:left ast))]
             (if (truthy? l)
               [env1 1]
               (let [[env2 r] (eval-expr env1 (:right ast))]
                 [env2 (->bool (truthy? r))])))
      ;; Regular eager binary
      (let [[env1 l] (eval-expr env (:left ast))
            [env2 r] (eval-expr env1 (:right ast))]
        [env2 (apply-binary (:op ast) l r)]))

    :ternary
    (let [[env1 c] (eval-expr env (:cond ast))]
      (if (truthy? c)
        (eval-expr env1 (:then ast))
        (eval-expr env1 (:else ast))))

    :assign
    (let [[env1 v] (eval-expr env (:value ast))
          nm (:target ast)
          new-val (case (:op ast)
                    "="   v
                    "+="  (+ (env-get-int env1 nm) v)
                    "-="  (- (env-get-int env1 nm) v)
                    "*="  (* (env-get-int env1 nm) v)
                    "/="  (quot (env-get-int env1 nm) v)
                    "%="  (mod (env-get-int env1 nm) v)
                    "**=" (ipow (env-get-int env1 nm) v)
                    "<<=" (bit-shift-left (env-get-int env1 nm) v)
                    ">>=" (bit-shift-right (env-get-int env1 nm) v)
                    "&="  (bit-and (env-get-int env1 nm) v)
                    "|="  (bit-or  (env-get-int env1 nm) v)
                    "^="  (bit-xor (env-get-int env1 nm) v))]
      [(env-set-int env1 nm new-val) new-val])

    :comma
    (reduce (fn [[env _] e] (eval-expr env e))
            [env 0]
            (:exprs ast))))

;; ============================================================================
;; Public API
;; ============================================================================

(defn evaluate
  "Evaluate `expr-str` against `env`. Returns [env' int-result]."
  [env ^String expr-str]
  (let [tokens (tokenize expr-str)
        p (p-state tokens)
        ast (parse-expr p (or expr-str ""))]
    (when-not (= :eof (:type (p-peek p)))
      (arith-error expr-str (:pos (p-peek p))
                   (str "unexpected trailing token " (pr-str (p-peek p)))))
    (eval-expr env ast)))

(defn evaluate-truthy
  "Like `evaluate` but returns a bool — true iff the result is non-zero.
   Used by `((cond))` in if/while contexts."
  [env expr-str]
  (let [[env' v] (evaluate env expr-str)]
    [env' (not (zero? v))]))
