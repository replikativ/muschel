(ns muschel.budget-test
  "Resource-budget tests. Each test installs an interrupt-fn that
   aborts after a known number of invocations / known wall-clock
   delay, runs a script that would otherwise loop forever or eat
   bytes, and asserts the throw fires."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [muschel.builtins.posix :as posix]
            [muschel.budget :as bud]
            [muschel.core :as m]
            [muschel.fs.virtual :as vfs]
            [muschel.host.builtin :as hb]
            [muschel.host.jvm :as jvm]))

(defn- mk-host []
  (hb/make {:fs (vfs/make {} {:cwd "/"})
            :fallback-host (jvm/make)
            :builtins posix/standard}))

(deftest step-interrupt-fires
  (testing "step-interrupt aborts after N invocations"
    (let [host (mk-host)
          ifn (bud/step-interrupt 5)
          ex (try
               (m/run-and-capture
                (m/new-env)
                "i=0; while [ \"$i\" -lt 1000 ]; do i=$((i+1)); done"
                {:host host :interrupt-fn ifn})
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex)
          "step-interrupt should have aborted the infinite-ish loop")
      (when ex
        (is (bud/budget-exceeded? ex))
        (is (= :steps (:muschel/budget (ex-data ex))))))))

(deftest timeout-aborts-infinite-loop
  (testing "timeout-ms synthesises an interrupt that aborts after the deadline"
    (let [host (mk-host)
          ex (try
               (m/run-and-capture
                (m/new-env)
                "while true; do :; done"
                {:host host :timeout-ms 50})
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex)
          "infinite while-loop should have been aborted by timeout")
      (when ex
        (is (bud/budget-exceeded? ex))
        (is (= :timeout (:muschel/budget (ex-data ex))))))))

(deftest awk-record-loop-honours-interrupt
  (testing "awk's per-record loop calls interrupt-fn"
    (let [host (mk-host)
          ex (try
               ;; Feed awk many records via seq + pipe. Note: bash's
               ;; pipe stages each tick the interrupt-fn too; we make
               ;; the budget generous enough that awk starts running
               ;; but tight enough that its record loop is what fires.
               (m/run-and-capture
                (m/new-env)
                "seq 1 10000 | awk '{print NR}'"
                {:host host
                 :interrupt-fn (bud/step-interrupt 15)})
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex)
          "awk's per-record loop should have been aborted")
      (when ex (is (bud/budget-exceeded? ex))))))

(deftest awk-for-loop-honours-interrupt
  (testing "awk's for-body calls interrupt-fn"
    (let [host (mk-host)
          ex (try
               (m/run-and-capture
                (m/new-env)
                "awk 'BEGIN { for (i=0; i<1000000; i++) {} }'"
                {:host host
                 :interrupt-fn (bud/step-interrupt 10)})
               nil
               (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "awk for-loop should have been aborted")
      (when ex (is (bud/budget-exceeded? ex))))))

(deftest no-interrupt-fn-does-not-throw
  (testing "running without an interrupt-fn is the default and works fine"
    (let [host (mk-host)
          r (m/run-and-capture
             (m/new-env)
             "echo hello"
             {:host host})]
      (is (zero? (:exit r)))
      (is (= "hello\n" (:stdout r))))))
