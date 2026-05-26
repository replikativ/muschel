(ns muschel.awk-host-test
  "Host-integration tests for awk: dispatcher through posix.clj + the
   BuiltinHost. JVM-only because muschel.builtins.posix and
   muschel.host.builtin live on the JVM side. Direct unit tests and the
   goawk corpus live in `awk_test.cljc` — those run on Node too."
  (:require [clojure.test :refer [deftest is]]
            [muschel.builtins.posix :as posix]
            [muschel.core :as m]
            [muschel.fs.virtual :as vfs]
            [muschel.host.builtin :as hb]
            [muschel.host.jvm :as jvm]))

(defn- host-fs []
  (vfs/make {"/work/data.csv" "name,age\nalice,30\nbob,25\n"
             "/work/script.awk" "BEGIN { print \"hi\" }"}
            {:cwd "/work"}))

(defn- run-host [cmd]
  (let [host (hb/make {:fs (host-fs)
                       :fallback-host (jvm/make)
                       :builtins posix/standard})]
    (m/run-and-capture (m/new-env) cmd {:host host})))

(deftest host-dash-F
  (let [r (run-host "awk -F , '{print $1}' data.csv")]
    (is (= 0 (:exit r)))
    (is (re-find #"alice" (:stdout r)))
    (is (re-find #"bob" (:stdout r)))))

(deftest host-dash-v
  (let [r (run-host "awk -v X=42 'BEGIN { print X }'")]
    (is (= 0 (:exit r)))
    (is (= "42\n" (:stdout r)))))

(deftest host-dash-f
  (let [r (run-host "awk -f script.awk")]
    (is (= 0 (:exit r)))
    (is (= "hi\n" (:stdout r)))))
