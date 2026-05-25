(ns muschel.env-test
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.env :as env]))

(deftest construction
  (let [e (env/new-env :cwd "/tmp" :pos-args ["a" "b" "c"] :script "myscript")]
    (is (= "/tmp" (:cwd e)))
    (is (= 3 (count (:pos-args e))))
    (is (= "myscript" (:script e)))
    (is (= 0 (:last-exit e)))))

(deftest special-vars
  (let [e (env/new-env :pos-args ["one" "two" "three"])]
    (is (= "0" (env/get-var e "?")))
    (is (= "3" (env/get-var e "#")))
    (is (= "one" (env/get-var e "1")))
    (is (= "two" (env/get-var e "2")))
    (is (= "" (env/get-var e "9")))
    (is (= "one two three" (env/get-var e "@")))
    (is (= "one two three" (env/get-var e "*")))))

(deftest get-var-unset-vs-empty
  (let [e (-> (env/empty-env) (env/set-var "EMPTY" ""))]
    (is (= "" (env/get-var e "EMPTY")))
    (is (= "" (env/get-var* e "EMPTY")))
    (is (= "" (env/get-var e "UNSET")))
    (is (nil? (env/get-var* e "UNSET")))))

(deftest set-var-preserves-exported
  (let [e (-> (env/empty-env)
              (env/export "FOO" "bar")
              (env/set-var "FOO" "baz"))]
    (is (= "baz" (env/get-var e "FOO")))
    (is (true? (env/exported? e "FOO")))))

(deftest export-flow
  (let [e (-> (env/empty-env)
              (env/set-var "FOO" "1")
              (env/export "FOO")
              (env/export "BAR" "2"))]
    (is (env/exported? e "FOO"))
    (is (env/exported? e "BAR"))
    (is (= {"FOO" "1" "BAR" "2"} (env/to-process-env e)))))

(deftest unset-var-respects-readonly
  (let [e (-> (env/empty-env)
              (env/set-var "X" "1")
              (env/mark-readonly "X"))]
    (is (= "1" (env/get-var (env/set-var e "X" "2") "X"))
        "readonly var refuses set-var")
    (is (= "1" (env/get-var (env/unset-var e "X") "X"))
        "readonly var refuses unset-var")))

(deftest cd-tracks-prev-cwd
  (let [e (env/new-env :cwd "/tmp")
        e2 (env/cd e "/usr/local")
        e3 (env/cd e2 "-")]    ; cd - swaps with OLDPWD
    (is (= "/usr/local" (:cwd e2)))
    (is (= "/tmp" (:prev-cwd e2)))
    (is (= "/tmp" (:cwd e3)))
    (is (= "/usr/local" (:prev-cwd e3)))))

(deftest cd-resolves-relative
  (let [e (env/new-env :cwd "/tmp")
        e2 (env/cd e "..")]    ; /tmp/.. → / (canonical)
    (is (= "/" (:cwd e2)))))

(deftest pwd-and-oldpwd-via-special-vars
  (let [e (env/cd (env/new-env :cwd "/tmp") "/usr/local")]
    (is (= "/usr/local" (env/get-var e "PWD")))
    (is (= "/tmp" (env/get-var e "OLDPWD")))))

(deftest ifs-special
  (let [e (env/empty-env)]
    (is (= " \t\n" (env/get-var e "IFS")))
    (let [e2 (env/set-var e "IFS" ":")]
      (is (= ":" (:ifs e2)))
      (is (= ":" (env/get-var e2 "IFS"))))))

(deftest pos-args-replacement
  (let [e (-> (env/empty-env)
              (env/with-pos-args ["x" "y"])
              (env/shift))]
    (is (= ["y"] (:pos-args e)))
    (is (= "y" (env/get-var e "1")))))

(deftest record-exit
  (is (= 42 (:last-exit (env/record-exit (env/empty-env) 42))))
  (is (= "42" (env/get-var (env/record-exit (env/empty-env) 42) "?"))))

(deftest function-defs
  (let [e (env/define-fn (env/empty-env) "myfn" {:type :stmt})]
    (is (some? (env/lookup-fn e "myfn")))
    (is (nil? (env/lookup-fn (env/unset-fn e "myfn") "myfn")))))

(deftest options
  (let [e (-> (env/empty-env)
              (env/set-option :errexit true)
              (env/set-option :pipefail true))]
    (is (true? (env/option e :errexit)))
    (is (true? (env/option e :pipefail)))
    (is (false? (env/option e :nounset)))))

(deftest to-process-env-only-exports-exported
  (let [e (-> (env/empty-env)
              (env/set-var "LOCAL" "1")
              (env/export "EXP" "2"))]
    (is (= {"EXP" "2"} (env/to-process-env e)))))
