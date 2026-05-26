(ns muschel.awk-host-test
  "Host-integration tests for awk: dispatcher through posix + the
   BuiltinHost. Cross-platform: runs on JVM and Node / ClojureScript.
   Direct unit tests and the goawk corpus live in `awk_test.cljc`."
  (:require [clojure.test :refer [deftest is]]
            [muschel.core :as m]
            [muschel.test-helpers :as th]))

(defn- run-host [cmd]
  (let [host (th/mk-host {:files {"/work/data.csv" "name,age\nalice,30\nbob,25\n"
                                  "/work/script.awk" "BEGIN { print \"hi\" }"}
                          :cwd "/work"})]
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
