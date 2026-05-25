(ns port-mvdan-sh-corpus
  "Extract `runTests` from `../mvdan-sh/interp/interp_test.go` into EDN
   for muschel's regression suite.

   The Go struct is `{in, want string}`. Strings come in two flavours:
   `\"...\"` (interpreted, escape sequences honored) and `` `...` ``
   (raw, no escapes). Adjacent string literals can be `+`-concatenated.
   We honor `// JUSTERR` etc. suffixes inside `want` by passing them
   through verbatim — the test runner strips them per mvdan/sh's
   convention.

   Usage:
     clojure -M tools/port_mvdan_sh_corpus.clj path/to/interp_test.go output.edn"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; ============================================================================
;; Char-based reader
;; ============================================================================

(defn- reader [s] {:s s :len (count s) :pos (volatile! 0)})
(defn- peek-ch [r] (when (< @(:pos r) (:len r)) (.charAt ^String (:s r) @(:pos r))))
(defn- peek-at [r n] (let [i (+ @(:pos r) n)] (when (< i (:len r)) (.charAt ^String (:s r) i))))
(defn- advance! [r] (let [c (peek-ch r)] (vswap! (:pos r) inc) c))
(defn- starts-with? [r ^String pre]
  (let [n (count pre)
        p @(:pos r)]
    (and (<= (+ p n) (:len r))
         (= pre (subs (:s r) p (+ p n))))))

(defn- skip-ws+comments! [r]
  (loop []
    (let [c (peek-ch r)]
      (cond
        (nil? c) nil
        (#{\space \tab \newline \return} c) (do (advance! r) (recur))
        ;; // comment
        (and (= c \/) (= \/ (peek-at r 1)))
        (do (loop [] (when (and (peek-ch r) (not= \newline (peek-ch r))) (advance! r) (recur)))
            (recur))
        ;; /* comment */
        (and (= c \/) (= \* (peek-at r 1)))
        (do (advance! r) (advance! r)
            (loop [] (when (not (and (= (peek-ch r) \*) (= (peek-at r 1) \/)))
                       (advance! r) (recur)))
            (advance! r) (advance! r)
            (recur))
        :else nil))))

;; ============================================================================
;; Go string literal readers
;; ============================================================================

(defn- decode-go-escape
  "Decode `\\X` after the backslash has been consumed. Returns the
   decoded string. Handles bash-relevant escapes; octal/hex are
   passed through as their literal byte if we can compute them."
  [r]
  (let [c (advance! r)]
    (case c
      \n  "\n"
      \t  "\t"
      \r  "\r"
      \\  "\\"
      \"  "\""
      \'  "'"
      \a  "\007"
      \b  "\b"
      \f  "\f"
      \v  "\013"
      \0  "\000"
      \x  (let [h1 (advance! r) h2 (advance! r)]
            (str (char (Integer/parseInt (str h1 h2) 16))))
      \u  (let [h1 (advance! r) h2 (advance! r) h3 (advance! r) h4 (advance! r)]
            (str (char (Integer/parseInt (str h1 h2 h3 h4) 16))))
      ;; default — keep literal
      (str "\\" c))))

(defn- read-quoted-string!
  "At opening `\"`. Returns the decoded string."
  [r]
  (advance! r)
  (let [sb (StringBuilder.)]
    (loop []
      (let [c (peek-ch r)]
        (cond
          (nil? c) (.toString sb)
          (= c \") (do (advance! r) (.toString sb))
          (= c \\) (do (advance! r) (.append sb (decode-go-escape r)) (recur))
          :else    (do (.append sb (advance! r)) (recur)))))))

(defn- read-raw-string!
  "At opening backtick. Returns the literal string."
  [r]
  (advance! r)
  (let [sb (StringBuilder.)]
    (loop []
      (let [c (peek-ch r)]
        (cond
          (nil? c) (.toString sb)
          (= c \`) (do (advance! r) (.toString sb))
          :else    (do (.append sb (advance! r)) (recur)))))))

(defn- read-string-or-concat!
  "Reads a string literal, possibly `+`-concatenated with more
   literals or with `pathProg` (which we expand to a placeholder).
   Returns the combined string, or nil if at this position there's no
   string (e.g. a comment-only entry)."
  [r]
  (skip-ws+comments! r)
  (let [parts (volatile! [])]
    (loop []
      (skip-ws+comments! r)
      (cond
        (= (peek-ch r) \")
        (do (vswap! parts conj (read-quoted-string! r))
            (skip-ws+comments! r)
            (when (= (peek-ch r) \+)
              (advance! r) (recur)))

        (= (peek-ch r) \`)
        (do (vswap! parts conj (read-raw-string! r))
            (skip-ws+comments! r)
            (when (= (peek-ch r) \+)
              (advance! r) (recur)))

        ;; identifier (e.g. pathProg) — emit as placeholder
        (re-matches #"[a-zA-Z_]" (str (peek-ch r)))
        (let [sb (StringBuilder.)]
          (loop []
            (let [c (peek-ch r)]
              (when (and c (re-matches #"[a-zA-Z_0-9.]" (str c)))
                (.append sb c) (advance! r) (recur))))
          (vswap! parts conj (str "<<<" (.toString sb) ">>>"))
          (skip-ws+comments! r)
          (when (= (peek-ch r) \+)
            (advance! r) (recur)))

        :else nil))
    (when (seq @parts)
      (apply str @parts))))

;; ============================================================================
;; Find `runTests = []runTest{ ... }` blocks and extract pairs
;; ============================================================================

(defn- read-pair!
  "At `{`. Reads `{ in-str , want-str }` and returns [in want].
   Returns nil if this `{` doesn't open a test pair (e.g. nested struct)."
  [r]
  (advance! r)                                   ; consume {
  (let [in-str (read-string-or-concat! r)]
    (skip-ws+comments! r)
    (when (and in-str (= (peek-ch r) \,))
      (advance! r)
      (let [want-str (read-string-or-concat! r)]
        (skip-ws+comments! r)
        (when (= (peek-ch r) \,) (advance! r))
        (skip-ws+comments! r)
        (when (= (peek-ch r) \})
          (advance! r)
          (when (and in-str want-str)
            [in-str want-str]))))))

(defn- find-block-start
  "Move reader to just after `var <name> = []runTest{`. Returns true
   if found, false if EOF."
  [r block-name]
  (let [marker (str "var " block-name " = []runTest{")]
    (loop []
      (cond
        (>= @(:pos r) (:len r)) false
        (starts-with? r marker)
        (do (vreset! (:pos r) (+ @(:pos r) (count marker))) true)
        :else (do (advance! r) (recur))))))

(defn- extract-block
  "From a reader sitting after `[]runTest{`, walk to the matching
   closing `}`, extracting each `{...}` pair along the way."
  [r]
  (let [out (volatile! [])]
    (loop [depth 1]
      (skip-ws+comments! r)
      (let [c (peek-ch r)]
        (cond
          (nil? c) @out
          (= c \}) (do (advance! r)
                       (if (= depth 1) @out (recur (dec depth))))
          (= c \{)
          (let [save @(:pos r)
                pair (try (read-pair! r) (catch Exception _ nil))]
            (if pair
              (do (vswap! out conj pair) (recur depth))
              (do (vreset! (:pos r) save)
                  (advance! r) (recur (inc depth)))))
          :else (do (advance! r) (recur depth)))))))

(defn extract-all [go-source]
  (let [r (reader go-source)]
    (if (find-block-start r "runTests")
      (extract-block r)
      [])))

;; ============================================================================
;; Output
;; ============================================================================

(defn -main [& [in-path out-path]]
  (let [in-path (or in-path "/home/christian-weilbach/Development/mvdan-sh/interp/interp_test.go")
        out-path (or out-path "test/muschel/mvdan_sh_corpus.edn")
        src (slurp in-path)
        pairs (extract-all src)]
    (println "extracted" (count pairs) "test cases")
    (with-open [w (io/writer out-path)]
      (.write w ";; Test cases extracted from mvdan/sh interp_test.go (BSD-3-Clause).\n")
      (.write w ";; Each entry is {:in <bash-source> :want <expected-stdout-stderr-merged>}.\n")
      (.write w ";; `<<<NAME>>>` placeholders mark Go identifiers we couldn't resolve\n")
      (.write w ";; (e.g. pathProg, GOSH_PROG); the test runner skips entries containing them.\n")
      (.write w ";;\n")
      (.write w ";; See LICENSE-mvdan-sh.txt for upstream copyright/license.\n\n")
      (.write w "[\n")
      (doseq [[in want] pairs]
        (.write w (pr-str {:in in :want want}))
        (.write w "\n"))
      (.write w "]\n"))
    (println "wrote" out-path)
    (System/exit 0)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

;; Run when invoked as `clojure -M tools/port_mvdan_sh_corpus.clj ...`
(apply -main *command-line-args*)
