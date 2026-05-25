(ns muschel.arith-test
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.arith :as arith]
            [muschel.env :as env]))

(defn- eval-int [src & {:keys [env]}]
  (second (arith/evaluate (or env (env/empty-env)) src)))

(defn- eval-env [src & {:keys [env]}]
  (first (arith/evaluate (or env (env/empty-env)) src)))

;; ============================================================================
;; Literals & bases
;; ============================================================================

(deftest literals
  (is (= 0    (eval-int "0")))
  (is (= 42   (eval-int "42")))
  (is (= 255  (eval-int "0xff")))
  (is (= 255  (eval-int "0XFF")))
  (is (= 8    (eval-int "010")))                 ; octal
  (is (= 10   (eval-int "2#1010")))
  (is (= 16   (eval-int "16#10")))
  (is (= 255  (eval-int "16#ff"))))

;; ============================================================================
;; Arithmetic operators
;; ============================================================================

(deftest arithmetic
  (is (= 3   (eval-int "1+2")))
  (is (= -1  (eval-int "1-2")))
  (is (= 12  (eval-int "3*4")))
  (is (= 3   (eval-int "10/3")))
  (is (= 1   (eval-int "10%3")))
  (is (= 8   (eval-int "2**3")))
  (is (= 14  (eval-int "2+3*4")))
  (is (= 20  (eval-int "(2+3)*4")))
  (is (= -5  (eval-int "-5")))
  (is (= 5   (eval-int "- -5")))                ; bash: `--5` is decrement-of-literal, error
  (is (= 5   (eval-int "+5"))))

;; ============================================================================
;; Comparison & logical
;; ============================================================================

(deftest comparison
  (is (= 1 (eval-int "5<10")))
  (is (= 0 (eval-int "5>10")))
  (is (= 1 (eval-int "5==5")))
  (is (= 1 (eval-int "5!=6")))
  (is (= 1 (eval-int "5<=5")))
  (is (= 1 (eval-int "5>=5"))))

(deftest logical
  (is (= 1 (eval-int "1 && 2")))
  (is (= 0 (eval-int "0 && 2")))
  (is (= 1 (eval-int "0 || 3")))
  (is (= 0 (eval-int "0 || 0")))
  (is (= 1 (eval-int "!0")))
  (is (= 0 (eval-int "!5"))))

(deftest logical-short-circuit
  ;; The right side must not be evaluated when short-circuit fires.
  (let [env (env/empty-env)
        [env' v] (arith/evaluate env "0 && (x = 5)")]
    (is (= 0 v))
    (is (= "" (env/get-var env' "x")))))

;; ============================================================================
;; Bitwise & shift
;; ============================================================================

(deftest bitwise
  (is (= 1 (eval-int "5&3")))
  (is (= 7 (eval-int "5|3")))
  (is (= 6 (eval-int "5^3")))
  (is (= -6 (eval-int "~5")))
  (is (= 8 (eval-int "1<<3")))
  (is (= 2 (eval-int "8>>2"))))

;; ============================================================================
;; Ternary
;; ============================================================================

(deftest ternary
  (is (= 100 (eval-int "1?100:200")))
  (is (= 200 (eval-int "0?100:200")))
  (is (= 6   (eval-int "1?(2+4):(2*4)"))))

;; ============================================================================
;; Variables & assignment
;; ============================================================================

(deftest var-lookup-unset-is-zero
  (is (= 0  (eval-int "unset_var")))
  (is (= 1  (eval-int "unset_var+1"))))

(deftest var-lookup-with-dollar
  (let [env (env/set-var (env/empty-env) "x" "7")]
    (is (= 7 (eval-int "x" :env env)))
    (is (= 7 (eval-int "$x" :env env)))))

(deftest assignment
  (let [env (env/empty-env)
        env' (eval-env "x = 5" :env env)]
    (is (= "5" (env/get-var env' "x"))))
  (let [env (env/set-var (env/empty-env) "x" "10")
        env' (eval-env "x += 3" :env env)]
    (is (= "13" (env/get-var env' "x"))))
  (let [env (env/set-var (env/empty-env) "x" "10")
        env' (eval-env "x *= 4" :env env)]
    (is (= "40" (env/get-var env' "x")))))

(deftest assignment-returns-rhs
  (let [env (env/empty-env)
        [_ v] (arith/evaluate env "x = 42")]
    (is (= 42 v))))

(deftest comma-mutates-then-reads
  (let [env (env/empty-env)
        [env' v] (arith/evaluate env "x = 10, x")]
    (is (= 10 v))
    (is (= "10" (env/get-var env' "x")))))

;; ============================================================================
;; Increment / decrement
;; ============================================================================

(deftest postfix-increment
  (let [env (env/set-var (env/empty-env) "x" "5")
        [env' v] (arith/evaluate env "x++")]
    (is (= 5 v) "postfix returns OLD value")
    (is (= "6" (env/get-var env' "x")))))

(deftest prefix-increment
  (let [env (env/set-var (env/empty-env) "x" "5")
        [env' v] (arith/evaluate env "++x")]
    (is (= 6 v) "prefix returns NEW value")
    (is (= "6" (env/get-var env' "x")))))

(deftest decrement
  (let [env (env/set-var (env/empty-env) "x" "5")
        [env' v] (arith/evaluate env "x--")]
    (is (= 5 v))
    (is (= "4" (env/get-var env' "x")))))

;; ============================================================================
;; Errors
;; ============================================================================

(deftest division-by-zero
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) (arith/evaluate (env/empty-env) "1/0")))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) (arith/evaluate (env/empty-env) "1%0"))))

(deftest invalid-assign-target
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) (arith/evaluate (env/empty-env) "5 = 6"))))

(deftest invalid-syntax
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) (arith/evaluate (env/empty-env) "1 +"))))

;; ============================================================================
;; evaluate-truthy
;; ============================================================================

(deftest evaluate-truthy-test
  (is (true?  (second (arith/evaluate-truthy (env/empty-env) "1"))))
  (is (false? (second (arith/evaluate-truthy (env/empty-env) "0"))))
  (is (true?  (second (arith/evaluate-truthy (env/empty-env) "3*4 > 10")))))
