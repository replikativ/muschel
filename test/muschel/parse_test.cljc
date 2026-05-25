(ns muschel.parse-test
  "Tests for the recursive-descent parser. Covers every command shape
   in the AST plus a 485-entry corpus regression drawn from real
   Claude Code transcripts (see test/muschel/parse_corpus.txt;
   provenance documented in doc/llm-corpus.md)."
  (:require #?(:clj [clojure.java.io :as io])
            [clojure.test :refer [deftest is testing]]
            [muschel.ast :as ast]
            [muschel.parse :as p]))

(defn- strip-positions [x]
  (clojure.walk/postwalk
   (fn [n]
     (if (map? n)
       (apply dissoc n [:line :col :offset :end-line :end-col :end-offset])
       n))
   x))

(defn- parse [src] (strip-positions (p/parse src)))

(defn- one-stmt [src]
  (let [{:keys [stmts]} (parse src)]
    (when (= 1 (count stmts)) (first stmts))))

(defn- one-cmd [src] (:cmd (one-stmt src)))

;; ============================================================================
;; Empty / trivial
;; ============================================================================

(deftest empty-program
  (is (= {:type :program :stmts []} (parse "")))
  (is (= {:type :program :stmts []} (parse "   ")))
  (is (= {:type :program :stmts []} (parse "\n\n"))))

;; ============================================================================
;; Simple commands
;; ============================================================================

(deftest simple-call
  (is (= {:type :call
          :assigns []
          :args [{:type :word :parts [{:type :lit :value "ls"}]}]}
         (one-cmd "ls")))
  (is (= 3 (count (:args (one-cmd "ls -la /tmp")))))
  (is (= "grep" (-> "grep clj src" one-cmd :args first :parts first :value))))

(deftest assignment-prefix
  (let [c (one-cmd "FOO=bar BAZ=qux echo hi")]
    (is (= [{:type :assign :name "FOO"
             :value {:type :word :parts [{:type :lit :value "bar"}]}}
            {:type :assign :name "BAZ"
             :value {:type :word :parts [{:type :lit :value "qux"}]}}]
           (:assigns c)))
    (is (= 2 (count (:args c))))))

;; ============================================================================
;; Pipes, sequences, logical ops
;; ============================================================================

(deftest pipe-binary
  (let [c (one-cmd "ls | grep clj")]
    (is (= :binary (:type c)))
    (is (= :pipe (:op c)))
    (is (= :call (-> c :left :cmd :type)))
    (is (= :call (-> c :right :cmd :type)))))

(deftest pipe-amp
  (is (= :pipe-amp (:op (one-cmd "make |& tee log")))))

(deftest sequence-semi
  (is (= 2 (count (:stmts (parse "ls; cat"))))))

(deftest sequence-newline
  (is (= 3 (count (:stmts (parse "ls\ncat\necho")))))
  (is (= 3 (count (:stmts (parse "ls\n\ncat\n\necho")))))
  (is (= 2 (count (:stmts (parse "ls;\ncat\n"))))))

(deftest and-or-left-assoc
  ;; cmd1 && cmd2 || cmd3 → :or { :left :and{cmd1, cmd2}, :right cmd3 }
  (let [c (one-cmd "cmd1 && cmd2 || cmd3")]
    (is (= :binary (:type c)))
    (is (= :or (:op c)))
    (is (= :and (-> c :left :cmd :op)))
    (is (= "cmd3" (-> c :right :cmd :args first :parts first :value)))))

;; ============================================================================
;; Backgrounding
;; ============================================================================

(deftest background-amp
  (let [s (one-stmt "long-cmd &")]
    (is (true? (:bg? s)))))

(deftest background-and-then-cmd
  ;; `cmd1 & cmd2` is two stmts; first is bg.
  (let [stmts (:stmts (parse "cmd1 & cmd2"))]
    (is (= 2 (count stmts)))
    (is (true? (:bg? (first stmts))))
    (is (false? (:bg? (second stmts))))))

;; ============================================================================
;; Pipeline negation
;; ============================================================================

(deftest pipeline-negation
  (let [s (one-stmt "! cmd | other")]
    (is (true? (:neg? s)))
    (is (= :binary (:type (:cmd s))))))

;; ============================================================================
;; Redirections
;; ============================================================================

(deftest redirect-out
  (let [c (one-cmd "echo hi > out")]
    (is (= :call (:type c)))
    (is (= 1 (count (:redirs c))))
    (is (= :out (-> c :redirs first :op)))))

(deftest redirect-err-to-out
  ;; `2>&1` = "duplicate fd 1 over fd 2" → :dup-out with fd=2, target word "1".
  (let [c (one-cmd "make 2>&1")
        r (first (:redirs c))]
    (is (= :dup-out (:op r)))
    (is (= 2 (:fd r)))
    (is (= "1" (-> r :target :parts first :value)))))

(deftest redirect-and-bg
  (let [s (one-stmt "srv > log 2>&1 &")
        c (:cmd s)]
    (is (true? (:bg? s)))
    (is (= 2 (count (:redirs c))))))

(deftest fd-prefix
  (let [r (first (:redirs (one-cmd "make 3>out")))]
    (is (= :out (:op r)))
    (is (= 3 (:fd r)))))

;; ============================================================================
;; Heredocs (the iconic git-commit pattern)
;; ============================================================================

(deftest heredoc-on-call
  (let [c (one-cmd "cat <<EOF\nbody\nEOF")
        r (first (:redirs c))]
    (is (= :heredoc (:type r)))
    (is (= "EOF" (:tag r)))
    (is (= "body\n" (:body r)))
    (is (true? (:expand? r)))))

(deftest heredoc-quoted-tag
  (let [r (first (:redirs (one-cmd "cat <<'EOF'\nliteral $X\nEOF")))]
    (is (false? (:expand? r)))))

;; ============================================================================
;; Compound: if / for / while / until / case
;; ============================================================================

(deftest if-then
  (let [c (one-cmd "if test -f foo; then echo yes; fi")]
    (is (= :if (:type c)))
    (is (= 1 (count (:cond c))))
    (is (= 1 (count (:then c))))
    (is (nil? (:else c)))))

(deftest if-then-else
  (let [c (one-cmd "if test -f foo; then echo yes; else echo no; fi")]
    (is (= 1 (count (:then c))))
    (is (= 1 (count (:else c))))))

(deftest if-elif
  (let [c (one-cmd "if a; then x; elif b; then y; else z; fi")]
    (is (= 1 (count (:elifs c))))
    (is (= 1 (count (:else c))))))

(deftest for-loop
  (let [c (one-cmd "for f in a b c; do echo $f; done")]
    (is (= :for (:type c)))
    (is (= "f" (:var c)))
    (is (= 3 (count (:words c))))
    (is (= 1 (count (:body c))))))

(deftest for-loop-no-in
  ;; `for f; do ...` uses $@
  (let [c (one-cmd "for f; do echo $f; done")]
    (is (= "f" (:var c)))
    (is (= [] (:words c)))))

(deftest c-style-for
  (let [c (one-cmd "for ((i=0; i<10; i++)); do echo $i; done")]
    (is (= :c-for (:type c)))
    (is (= "i=0" (:init c)))
    (is (= "i<10" (:cond c)))
    (is (= "i++" (:update c)))))

(deftest while-loop
  (let [c (one-cmd "while test -f flag; do sleep 1; done")]
    (is (= :while (:type c)))))

(deftest until-loop
  (let [c (one-cmd "until test -f flag; do sleep 1; done")]
    (is (= :until (:type c)))))

(deftest case-stmt
  (let [c (one-cmd "case x in a) echo a;; b|c) echo bc;; *) echo other;; esac")]
    (is (= :case (:type c)))
    (is (= 3 (count (:clauses c))))
    (is (= 2 (count (:patterns (second (:clauses c))))))))

;; ============================================================================
;; Compound: brace-group, subshell, arith-cmd
;; ============================================================================

(deftest brace-group
  (let [c (one-cmd "{ ls; cat; }")]
    (is (= :brace-group (:type c)))
    (is (= 2 (count (:body c))))))

(deftest subshell
  (let [c (one-cmd "(ls; cat)")]
    (is (= :subshell (:type c)))
    (is (= 2 (count (:body c))))))

(deftest arith-command
  (let [c (one-cmd "((x=1+2))")]
    (is (= :arith-cmd (:type c)))
    (is (= "x=1+2" (:expr c)))))

;; ============================================================================
;; Test brackets
;; ============================================================================

(deftest test-bracket-single
  (let [c (one-cmd "[ -f foo ]")]
    (is (= :test-bracket (:type c)))
    (is (= :single (:form c)))
    (is (= 2 (count (:args c))))))

(deftest test-bracket-double
  (let [c (one-cmd "[[ -d bar ]]")]
    (is (= :double (:form c)))))

;; ============================================================================
;; Function definitions
;; ============================================================================

(deftest function-paren-form
  (let [c (one-cmd "myfn() { echo hi; }")]
    (is (= :function-def (:type c)))
    (is (= "myfn" (:name c)))
    (is (= :brace-group (-> c :body :type)))))

(deftest function-keyword-form
  (let [c (one-cmd "function myfn { echo hi; }")]
    (is (= "myfn" (:name c)))))

;; ============================================================================
;; AST helpers
;; ============================================================================

(deftest leaf-calls-walk
  (let [ast (p/parse "{ ls -la; grep foo bar; }")
        calls (ast/leaf-calls ast)]
    (is (= 2 (count calls)))
    (is (= ["ls" "grep"] (mapv #(-> % :args first :parts first :value) calls)))))

(deftest command-names-helper
  (let [ast (p/parse "git status && git diff | head")]
    (is (= ["git" "git" "head"] (ast/command-names ast)))))

;; ============================================================================
;; Parse-error positions and wording (mirroring lex)
;; ============================================================================

(defn- catch-data [body-fn]
  (try (body-fn) nil
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) e (ex-data e))))

(deftest parse-error-shape
  (testing "empty if-cond is rejected"
    (let [d (catch-data #(p/parse "if; then ls; fi"))]
      (is (= ::p/parse-error (:type d)))
      (is (re-find #"syntax error" (:msg d)))
      (is (number? (:line d)))
      (is (number? (:col d)))))
  (testing "bash-style 'syntax error near unexpected token X'"
    (let [d (catch-data #(p/parse "ls | | grep"))]
      (is (= ::p/parse-error (:type d)))
      (is (re-find #"syntax error near unexpected token" (:msg d))))))

;; ============================================================================
;; Real-corpus regression (JVM only — uses io/resource)
;; ============================================================================

#?(:clj
   (defn- jread [^String s]
     (let [n (.length s) sb (StringBuilder.)]
       (loop [i 1]
         (when (< i (dec n))
           (let [c (.charAt s i)]
             (if (= c \\)
               (let [e (.charAt s (inc i))]
                 (case e
                   \" (do (.append sb \") (recur (+ i 2)))
                   \\ (do (.append sb \\) (recur (+ i 2)))
                   \/ (do (.append sb \/) (recur (+ i 2)))
                   \n (do (.append sb \newline) (recur (+ i 2)))
                   \t (do (.append sb \tab) (recur (+ i 2)))
                   \r (do (.append sb \return) (recur (+ i 2)))
                   \b (do (.append sb \backspace) (recur (+ i 2)))
                   \f (do (.append sb \formfeed) (recur (+ i 2)))
                   \u (let [h (subs s (+ i 2) (+ i 6))]
                        (.append sb (char (Integer/parseInt h 16)))
                        (recur (+ i 6)))
                   (do (.append sb e) (recur (+ i 2)))))
               (do (.append sb c) (recur (inc i)))))))
       (.toString sb))))

#?(:clj
   (deftest corpus-regression
     (let [src-lines (line-seq (io/reader (io/resource "muschel/parse_corpus.txt")))
           results (reduce
                    (fn [acc l]
                      (let [src (try (jread l) (catch Exception _ nil))]
                        (if-not (and (string? src) (pos? (count (.trim src))))
                          acc
                          (let [r (try (do (p/parse src) :ok)
                                       (catch #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) e
                                         (println "PARSE FAIL:" (.getMessage e))
                                         (println "  src:" (pr-str (subs src 0 (min 120 (count src)))))
                                         :err))]
                            (update acc r (fnil inc 0))))))
                    {:ok 0 :err 0}
                    src-lines)]
       (is (zero? (:err results))
           (str "parse errors on corpus: " (:err results)
                " (ok=" (:ok results) ")")))))
