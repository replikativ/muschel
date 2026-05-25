(ns muschel.errors-test
  "Tests for the error infrastructure and lexer error wording.

   The wording is deliberately aligned with bash's own error messages
   so an LLM agent reading muschel diagnostics stays in distribution
   (it's seen `unexpected EOF while looking for matching ...` in
   training data many times — let's not invent a parallel vocabulary).

   Captured from bash 5.x via `echo CASE | bash -n 2>&1`. We don't
   match exact prefixes (`bash: line N:` vs our line/col rendering),
   just the canonical phrase."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [muschel.errors :as err]
            [muschel.lex :as l]))

(defn- catch-data [body-fn]
  (try (body-fn) nil
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) e (ex-data e))))

;; ============================================================================
;; ex-data carries the structured fields we depend on
;; ============================================================================

(deftest ex-info-shape
  (let [d (catch-data #(l/tokenize "echo \"abc"))]
    (testing "lex errors carry :type :line :col :offset :msg"
      (is (= ::l/lex-error (:type d)))
      (is (= 1 (:line d)))
      (is (pos? (:col d)))
      (is (number? (:offset d)))
      (is (string? (:msg d))))
    (testing "source-context is the offending source line"
      (is (= "echo \"abc" (:source-context d))))
    (testing "unterminated forms get :incomplete true (REPL hint: needs more input)"
      (is (true? (:incomplete d))))))

(deftest refused-shape
  (let [d (catch-data #(l/tokenize "cat <(echo a)"))]
    (is (= ::l/refused (:type d)))
    (is (string? (:msg d)))
    (is (= "cat <(echo a)" (:source-context d)))))

;; ============================================================================
;; Wording mirrors bash
;; ============================================================================
;;
;; Captured from bash 5.x:
;;   echo "abc      → unexpected EOF while looking for matching `"'
;;   echo 'abc      → unexpected EOF while looking for matching `''
;;   echo $(abc     → unexpected EOF while looking for matching `)'
;;   echo $((abc    → unexpected EOF while looking for matching `)'
;;   echo `abc      → unexpected EOF while looking for matching ``'
;;   echo ${abc     → unexpected EOF while looking for matching `}'
;;   cat <<         → syntax error near unexpected token `newline'

(deftest bash-aligned-messages
  (let [cases [["echo \"abc"     "unexpected EOF while looking for matching `\"'"]
               ["echo 'abc"      "unexpected EOF while looking for matching `''"]
               ["echo $(abc"     "unexpected EOF while looking for matching `)'"]
               ["echo $((abc"    "unexpected EOF while looking for matching `)'"]
               ["echo `abc"      "unexpected EOF while looking for matching ``'"]
               ["echo ${abc"     "unexpected EOF while looking for matching `}'"]
               ["echo $'abc"     "unexpected EOF while looking for matching `''"]
               ["cat <<"         "syntax error near unexpected token `newline'"]]]
    (doseq [[src msg] cases]
      (testing src
        (let [d (catch-data #(l/tokenize src))]
          (is (= msg (:msg d))
              (str "got: " (pr-str (:msg d)))))))))

;; ============================================================================
;; format-error renders a multi-line display
;; ============================================================================

(deftest format-error-rendering
  (let [src "echo \"abc"
        out (try (l/tokenize src)
                 (catch #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) e (err/format-error e)))]
    (testing "starts with 'Error:'"
      (is (str/starts-with? out "Error:")))
    (testing "includes the source line"
      (is (str/includes? out "echo \"abc")))
    (testing "includes a line-gutter (line number + |)"
      (is (str/includes? out "1 | ")))
    (testing "includes the caret line"
      (is (re-find #"\^" out)))))

(deftest format-error-with-hint
  (let [d (catch-data #(l/tokenize "cat <<"))
        out (err/format-error d)]
    (is (str/includes? out "Hint: heredoc operator"))))

;; ============================================================================
;; Position accuracy
;; ============================================================================

(deftest error-points-at-opening
  ;; For unterminated constructs, the location should point at the
  ;; *opening* delimiter (where the error originated), not at EOF.
  ;; That matches what users want to see in an underline.
  (let [d (catch-data #(l/tokenize "echo \"unterm"))]
    ;; The opening " is at col 6 (1-indexed).
    (is (= 6 (:col d))))
  (let [d (catch-data #(l/tokenize "echo $(abc"))]
    ;; $( starts at col 6.
    (is (= 6 (:col d)))))

(deftest multi-line-error-points-at-right-line
  (let [d (catch-data #(l/tokenize "ls\necho \"abc"))]
    (is (= 2 (:line d)))
    (is (= "echo \"abc" (:source-context d)))))
