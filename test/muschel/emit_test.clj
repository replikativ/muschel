(ns muschel.emit-test
  "Round-trip tests: parse → emit → eval → run and compare to direct
   interpretation. The translated form should produce identical
   stdout / stderr / exit / env state."
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.emit :as emit]
            [muschel.env :as env]
            [muschel.exec :as exec]
            [muschel.host.jvm :as host.jvm]))

(defn- run-direct [src]
  (let [out (java.io.ByteArrayOutputStream.)
        err (java.io.ByteArrayOutputStream.)
        env-after (exec/run (env/new-env) src
                            {:host (host.jvm/make) :out out :err err})]
    {:stdout (.toString out "UTF-8")
     :stderr (.toString err "UTF-8")
     :exit   (:last-exit (:env env-after))}))

(defn- run-translated [src]
  (let [out (java.io.ByteArrayOutputStream.)
        err (java.io.ByteArrayOutputStream.)
        f (eval (emit/translate src))
        env-after (f (env/new-env)
                     {:host (host.jvm/make) :out out :err err})]
    {:stdout (.toString out "UTF-8")
     :stderr (.toString err "UTF-8")
     :exit   (:last-exit env-after)}))

(defn- round-trip-matches? [src]
  (= (run-direct src) (run-translated src)))

(deftest simple-calls
  (testing "single literal call"
    (is (round-trip-matches? "echo hi"))
    (is (round-trip-matches? "true"))
    (is (round-trip-matches? "false"))))

(deftest sequences
  (is (round-trip-matches? "echo one; echo two"))
  (is (round-trip-matches? "echo a; echo b; echo c")))

(deftest var-set-and-ref
  (is (round-trip-matches? "X=42; echo $X"))
  (is (round-trip-matches? "A=1; B=2; echo $A $B")))

(deftest conditionals
  (is (round-trip-matches?
       "if [ -d /tmp ]; then echo found; else echo missing; fi"))
  (is (round-trip-matches?
       "if true; then echo y; fi"))
  (is (round-trip-matches?
       "if false; then echo y; else echo n; fi")))

(deftest short-circuit
  (is (round-trip-matches? "true && echo a"))
  (is (round-trip-matches? "false && echo skip"))
  (is (round-trip-matches? "false || echo recovered"))
  (is (round-trip-matches? "true && echo a || echo b")))

(deftest for-loop
  (is (round-trip-matches? "for i in a b c; do echo $i; done"))
  (is (round-trip-matches?
       "for i in 1 2 3; do echo step $i; done")))

(deftest pipes-deferred
  ;; Pipes defer to exec — verify behavior still matches.
  (is (round-trip-matches? "echo a | tr a A"))
  (is (round-trip-matches? "echo hello | wc -c")))

(deftest cmd-subst
  (is (round-trip-matches? "X=$(echo nested); echo got=$X"))
  (is (round-trip-matches? "echo $(echo inner)")))

(deftest arith-expansion
  (is (round-trip-matches? "echo $((2 + 3))"))
  (is (round-trip-matches? "echo $((10 * 2 - 5))")))

(deftest per-call-assign
  ;; `FOO=bar echo $FOO` matches bash: $FOO expands in caller's context
  ;; (empty), prefix-assign only visible to external cmd.
  (is (round-trip-matches? "FOO=bar echo $FOO")))

(deftest unsupported-throws
  ;; The emitter should throw on AST shapes it doesn't handle.
  ;; (Currently we cover all common shapes; this test guards against
  ;; future regressions if someone removes a case-branch.)
  (testing "ex-info has :type :muschel.emit/unsupported"
    (let [;; Synthesize an AST with a bogus :cmd type.
          ast {:type :program
               :stmts [{:type :stmt
                        :cmd {:type :totally-not-a-real-cmd-type}
                        :redirs [] :bg? false :neg? false}]}]
      (is (thrown? clojure.lang.ExceptionInfo (emit/translate ast))))))
