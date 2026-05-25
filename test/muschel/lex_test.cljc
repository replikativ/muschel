(ns muschel.lex-test
  "Tests for the hand-written lexer. Covers every token type, every
   quoting form, every redirection, heredocs (including the
   inside-cmd-subst pattern), every refused construct.

   The `corpus-regression` test at the bottom loads
   `test/muschel/lex_corpus.txt` (486 real Bash tool inputs sampled
   from Claude Code session transcripts under
   `~/.claude/projects/**/*.jsonl`, with 14 corrupted entries removed
   — those failed `bash -n` syntax-check too). The full sample lives
   under `/tmp/muschel-corpus/sample-500.txt`; the filtering pipeline
   is in `doc/llm-corpus.md`."
  (:require #?(:clj [clojure.java.io :as io])
            [clojure.test :refer [deftest is testing]]
            [muschel.lex :as l]))

(defn- types [src]
  (mapv :type (l/tokenize src)))

(defn- one-word [src]
  (let [toks (l/tokenize src)]
    (when (and (= 2 (count toks)) (= :word (:type (first toks))))
      (first toks))))

(defn- one-word-parts [src]
  (some-> (one-word src) :parts))

;; ============================================================================
;; Basic words and operators
;; ============================================================================

(deftest simple-words
  (is (= [:word :eof] (types "ls")))
  (is (= [:word :word :eof] (types "ls -la")))
  (is (= [:word :word :word :eof] (types "ls -la /tmp"))))

(deftest pipes-and-sequences
  (is (= [:word :word :pipe :word :word :eof] (types "ls -la | grep clj")))
  (is (= [:word :word :semi :word :eof] (types "cd /tmp; ls")))
  (is (= [:word :word :word :and :word :word :eof] (types "git add . && git commit")))
  (is (= [:word :word :word :or :word :word :eof] (types "test -f x || echo missing")))
  (is (= [:word :pipe-amp :word :word :eof] (types "make |& tee log"))))

(deftest multi-statement-newlines
  (is (= [:word :newline :word :word :newline :word :word :eof]
         (types "ls\ncat foo\necho done")))
  (is (= [:word :newline :newline :word :word :eof]
         (types "ls\n\necho done"))))

(deftest backgrounding
  (is (= [:word :amp :eof] (types "long-cmd &")))
  (is (= [:word :redir-out :word :redir-dup-out :word :amp :eof]
         (types "srv > log 2>&1 &"))))

;; ============================================================================
;; Words containing the special chars that used to falsely terminate
;; ============================================================================

(deftest equals-in-literal
  ;; The v0 instaparse grammar choked on these; the hand-written lexer
  ;; treats `=` as a plain literal char inside a word.
  (is (= [{:type :lit :value "--color=never"}]
         (one-word-parts "--color=never")))
  (is (= [{:type :lit :value "--include=*.clj"}]
         (one-word-parts "--include=*.clj"))))

(deftest assignment-prefix-and-glob
  (is (= [{:type :lit :value "FOO=bar"}] (one-word-parts "FOO=bar")))
  ;; * is kept literal at lex time; expansion happens in expand.cljc
  (is (= [{:type :lit :value "*.clj"}] (one-word-parts "*.clj"))))

;; ============================================================================
;; Quoting
;; ============================================================================

(deftest single-quoted-literal
  (let [t (-> "'hello $X $(cmd)'" one-word :parts first)]
    (is (= :squoted (:type t)))
    (is (= "hello $X $(cmd)" (:value t)))))

(deftest double-quoted-with-interpolation
  (let [t (-> "\"$HOME/$(pwd)\"" one-word :parts first)
        parts (:parts t)]
    (is (= :dquoted (:type t)))
    (is (= 3 (count parts)))
    (is (= :var-ref (:type (nth parts 0))))
    (is (= "HOME" (:name (nth parts 0))))
    (is (= :lit (:type (nth parts 1))))
    (is (= "/" (:value (nth parts 1))))
    (is (= :cmd-subst (:type (nth parts 2))))
    (is (= "pwd" (:body (nth parts 2))))))

(deftest dquoted-with-escapes
  (let [t (-> "\"hello \\\"world\\\" and \\$var\"" one-word :parts first)]
    (is (= :dquoted (:type t)))
    (is (= [{:type :lit :value "hello \"world\" and $var"}]
           (:parts t)))))

(deftest ansi-c-quoted
  (let [t (-> "$'tab\\there'" one-word :parts first)]
    (is (= :ansi-c-quoted (:type t)))
    (is (= "tab\\there" (:raw t)))))

;; ============================================================================
;; Parameter expansion
;; ============================================================================

(deftest simple-var-ref
  (let [t (-> "$HOME" one-word :parts first)]
    (is (= :var-ref (:type t)))
    (is (not (:braced t)))
    (is (= "HOME" (:name t)))))

(deftest braced-var-ref
  (let [t (-> "${HOME}" one-word :parts first)]
    (is (= :var-ref (:type t)))
    (is (:braced t))
    (is (= "HOME" (:raw t)))))

(deftest param-default-op
  (let [t (-> "${VAR:-default}" one-word :parts first)]
    (is (= :var-ref (:type t)))
    (is (= "VAR:-default" (:raw t)))))

(deftest special-vars
  (doseq [[src expected-name] [["$?" "?"] ["$$" "$"] ["$#" "#"]
                               ["$@" "@"] ["$*" "*"] ["$!" "!"]]]
    (testing src
      (let [t (-> src one-word :parts first)]
        (is (= :var-ref (:type t)))
        (is (= expected-name (:name t)))
        (is (:special? t))))))

;; ============================================================================
;; Command substitution
;; ============================================================================

(deftest cmd-subst-paren
  (let [t (-> "$(date)" one-word :parts first)]
    (is (= :cmd-subst (:type t)))
    (is (= :paren (:form t)))
    (is (= "date" (:body t)))))

(deftest cmd-subst-backtick
  (let [t (-> "`date`" one-word :parts first)]
    (is (= :cmd-subst (:type t)))
    (is (= :backtick (:form t)))
    (is (= "date" (:body t)))))

(deftest cmd-subst-nested
  (let [t (-> "$(date $(id))" one-word :parts first)]
    (is (= :cmd-subst (:type t)))
    (is (= "date $(id)" (:body t)))))

;; ============================================================================
;; Arithmetic
;; ============================================================================

(deftest arith-expansion
  (let [parts (one-word-parts "x=$((1+2))")]
    (is (= [:lit :arith] (mapv :type parts)))
    (is (= "1+2" (:expr (second parts))))))

(deftest arith-command
  ;; `((...))` standalone (not as $((..))) is the arithmetic command form.
  (let [toks (l/tokenize "((x=1))")]
    (is (= [:arith-cmd :eof] (mapv :type toks)))
    (is (= "x=1" (:expr (first toks))))))

;; ============================================================================
;; Heredocs
;; ============================================================================

(deftest heredoc-basic
  (let [toks (l/tokenize "cat <<EOF\nhello\nworld\nEOF")
        h (second toks)]
    (is (= :redir-heredoc (:type h)))
    (is (= "EOF" (:tag h)))
    (is (= "hello\nworld\n" (:body h)))
    (is (:expand? h))))

(deftest heredoc-quoted-tag-no-expand
  (let [h (second (l/tokenize "cat <<'EOF'\nliteral $X\nEOF"))]
    (is (= "EOF" (:tag h)))
    (is (= "literal $X\n" (:body h)))
    (is (not (:expand? h)))))

(deftest heredoc-strip-tabs
  (let [h (second (l/tokenize "cat <<-EOF\n\tindented\n\tEOF"))]
    (is (= "EOF" (:tag h)))
    (is (:strip? h))
    (is (= "indented\n" (:body h)))))

(deftest heredoc-inside-cmd-subst
  ;; The iconic `git commit -m "$(cat <<'EOF' ... EOF\n)"` pattern.
  ;; A body line containing `'` (apostrophe in English) used to fool
  ;; the balanced-reader into entering quote-scanning. Regression test.
  (let [src "git commit -m \"$(cat <<'EOF'\nLine with don't apostrophe and ) paren\nEOF\n)\""
        toks (l/tokenize src)]
    (is (= [:word :word :word :word :eof] (mapv :type toks)))
    (let [m-arg (nth toks 3)
          dq (first (:parts m-arg))
          cs (first (:parts dq))]
      (is (= :dquoted (:type dq)))
      (is (= :cmd-subst (:type cs)))
      (is (clojure.string/includes? (:body cs) "don't")))))

;; ============================================================================
;; Redirections
;; ============================================================================

(deftest redirections
  (is (= :redir-out         (-> "ls > out" l/tokenize (nth 1) :type)))
  (is (= :redir-append      (-> "ls >> out" l/tokenize (nth 1) :type)))
  (is (= :redir-in          (-> "wc < in" l/tokenize (nth 1) :type)))
  (is (= :redir-dup-out     (-> "x 2>&1" l/tokenize (nth 1) :type)))
  (is (= 2                  (-> "x 2>&1" l/tokenize (nth 1) :fd)))
  (is (= :redir-clobber     (-> "ls >| out" l/tokenize (nth 1) :type)))
  (is (= :redir-rw          (-> "x <> file" l/tokenize (nth 1) :type)))
  (is (= :redir-all         (-> "x &> log" l/tokenize (nth 1) :type)))
  (is (= :redir-all-append  (-> "x &>> log" l/tokenize (nth 1) :type)))
  (is (= :redir-here-string (-> "tr <<<'x'" l/tokenize (nth 1) :type))))

(deftest fd-prefixed-redirect
  (let [t (-> "make 3>file" l/tokenize (nth 1))]
    (is (= :redir-out (:type t)))
    (is (= 3 (:fd t)))))

;; ============================================================================
;; Tilde and brace expansion
;; ============================================================================

(deftest tilde-prefix
  (let [parts (one-word-parts "~/Development")]
    (is (= :tilde (:type (first parts))))
    (is (= "" (:user (first parts))))
    (is (= :lit (:type (second parts))))
    (is (= "/Development" (:value (second parts))))))

(deftest tilde-user
  (let [parts (one-word-parts "~user/path")]
    (is (= :tilde (:type (first parts))))
    (is (= "user" (:user (first parts))))))

(deftest brace-expansion-list
  (let [b (-> "{a,b,c}" one-word :parts first)]
    (is (= :brace-exp (:type b)))
    (is (= :list (:kind b)))
    (is (= "a,b,c" (:raw b)))))

(deftest brace-expansion-range
  (let [b (-> "{1..10}" one-word :parts first)]
    (is (= :brace-exp (:type b)))
    (is (= :range (:kind b)))))

(deftest brace-group-not-misread-as-brace-exp
  ;; { followed by space can't be a brace-exp; should fall through to
  ;; literal `{` so the parser can recognise it as a group open.
  (let [toks (l/tokenize "{ ls; cat; }")]
    (is (= [:word :word :semi :word :semi :word :eof] (mapv :type toks)))
    (is (= "{" (-> toks first :parts first :value)))
    (is (= "}" (-> toks (nth 5) :parts first :value)))))

(deftest brace-group-with-quoted-comma
  ;; Regression: a comma inside a quoted literal must not trigger
  ;; brace-exp detection on the surrounding { ... }.
  (let [toks (l/tokenize "{ echo \"a,b\"; }")]
    (is (= [:word :word :word :semi :word :eof] (mapv :type toks)))
    (is (= "{" (-> toks first :parts first :value)))))

;; ============================================================================
;; Reserved words
;; ============================================================================

(deftest reserved-words-marked
  (doseq [r ["if" "then" "elif" "else" "fi"
             "for" "in" "do" "done"
             "while" "until"
             "case" "esac"
             "select" "function" "time"]]
    (testing r
      (let [t (-> r l/tokenize first)]
        (is (= r (:reserved t)))))))

;; ============================================================================
;; Refused constructs
;; ============================================================================

(deftest refused-process-substitution
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) (l/tokenize "cat <(echo a)")))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) (l/tokenize "tee >(cmd)"))))

(deftest refused-extended-glob
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) (l/tokenize "ls ?(a|b)"))))

(deftest locale-quoting-as-dquote
  ;; `$"..."` is bash's locale-translation syntax. With no LC_MESSAGES
  ;; catalog loaded, the result is identical to the inner "..." — we
  ;; lex it as a `:dquoted` part so the rest of the pipeline doesn't
  ;; have to care.
  (let [toks (l/tokenize "echo $\"foo $X\"")
        word (second toks)
        part (first (:parts word))]
    (is (= :dquoted (:type part)))
    (is (= [:lit :var-ref] (mapv :type (:parts part))))))

;; ============================================================================
;; Whitespace / comments / continuations
;; ============================================================================

(deftest comments-stripped
  (is (= [:word :eof] (types "ls # a comment"))))

(deftest line-continuation
  ;; backslash + newline is erased; what would have been two lines is
  ;; one logical line of words.
  (is (= [:word :word :word :word :eof]
         (types "git add \\\nfile1 file2"))))

(deftest empty-and-whitespace-only
  (is (= [:eof] (types "")))
  (is (= [:eof] (types "   ")))
  (is (= [:newline :eof] (types "\n"))))

;; ============================================================================
;; Real-corpus regression (JVM only — needs filesystem-backed resource I/O)
;; ============================================================================

#?(:clj
   (defn- jread
     "Minimal JSON-string decoder for our corpus file (each line is a JSON
   string). Faster than pulling in cheshire."
     [^String s]
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
  ;; Every entry in test/muschel/lex_corpus.txt must lex cleanly OR
  ;; throw a `::l/refused` (process-substitution / extended-glob /
  ;; locale-quoting — constructs we deliberately reject at parse).
  ;; Any `::l/lex-error` is a real regression.
     (let [src-lines (line-seq (io/reader (io/resource "muschel/lex_corpus.txt")))
           results (reduce
                    (fn [acc l]
                      (let [src (try (jread l) (catch Exception _ nil))]
                        (if-not (and (string? src) (pos? (count (.trim src))))
                          acc
                          (let [r (try (do (l/tokenize src) :ok)
                                       (catch #?(:clj clojure.lang.ExceptionInfo
                                                 :cljs cljs.core/ExceptionInfo) e
                                         (if (= ::l/refused (:type (ex-data e)))
                                           :refused
                                           (do (println "LEX FAIL:" (.getMessage e))
                                               (println "  src:" (pr-str (subs src 0 (min 150 (count src)))))
                                               :lex-error))))]
                            (update acc r (fnil inc 0))))))
                    {:ok 0 :refused 0 :lex-error 0}
                    src-lines)]
       (is (zero? (:lex-error results))
           (str "lex-errors on corpus: " (:lex-error results)
                " (ok=" (:ok results) " refused=" (:refused results) ")")))))
