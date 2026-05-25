(ns muschel.exec-cljs-test
  "End-to-end tests for the cljs-side exec layer. Uses the
   browser host (string buffers, virtual fs, virtual tool registry).
   These tests don't load on JVM."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [muschel.env :as env]
            [muschel.exec :as exec]
            [muschel.host.browser :as bh]))

(defn- run [src host-opts]
  (let [h (apply bh/make (mapcat identity host-opts))]
    (exec/run-and-capture (env/new-env :cwd "/") src {:host h})))

;; ============================================================================
;; Builtins (no host I/O involved)
;; ============================================================================

(deftest builtins-work-in-cljs
  (is (= "hello\n" (:stdout (run "echo hello" {}))))
  (is (= "" (:stdout (run "true" {}))))
  (is (zero? (:exit (run "true" {}))))
  (is (= 1 (:exit (run "false" {}))))
  (is (= "5\n" (:stdout (run "echo $((2+3))" {})))))

(deftest control-flow-works
  (is (= "a\nb\nc\n"
         (:stdout (run "for x in a b c; do echo $x; done" {}))))
  (is (= "yes\n"
         (:stdout (run "if true; then echo yes; else echo no; fi" {}))))
  (is (= "1\n2\n3\n"
         (:stdout (run "x=1; while [ $x -lt 4 ]; do echo $x; x=$((x+1)); done" {})))))

(deftest case-works
  (is (= "txt\n"
         (:stdout (run "case foo.txt in *.txt) echo txt;; esac" {})))))

(deftest function-with-local
  (is (= "inner\nouter\n"
         (:stdout (run "x=outer; f() { local x=inner; echo $x; }; f; echo $x"
                       {})))))

;; ============================================================================
;; Virtual tools
;; ============================================================================

(deftest virtual-tool-spawn
  (let [upper (fn [_args stdin _env]
                {:stdout (str/upper-case stdin) :exit 0})
        host (bh/make :tools {"upper" upper})
        {:keys [stdout]}
        (exec/run-and-capture (env/new-env :cwd "/")
                              "echo hello | upper"
                              {:host host})]
    (is (str/includes? stdout "HELLO"))))

(deftest virtual-tool-not-found
  (let [host (bh/make)
        {:keys [exit stderr]}
        (exec/run-and-capture (env/new-env :cwd "/") "nosuchcmd" {:host host})]
    (is (= 127 exit))
    (is (str/includes? stderr "nosuchcmd"))))

(deftest stock-tools-grep
  (let [host (bh/make :tools (bh/stock-tools))
        {:keys [stdout exit]}
        (exec/run-and-capture (env/new-env :cwd "/")
                              "echo 'one\ntwo\nthree' | grep two"
                              {:host host})]
    (is (zero? exit))
    (is (str/includes? stdout "two"))))

;; ============================================================================
;; Virtual fs
;; ============================================================================

(deftest virtual-fs-redirect-and-read
  (let [host (bh/make)
        {:keys [exit]}
        (exec/run-and-capture (env/new-env :cwd "/")
                              "echo data > /tmp/out"
                              {:host host})]
    (is (zero? exit))
    (is (true? (muschel.host/file-exists? host "/tmp/out")))
    (is (= "data\n" (muschel.host/read-file host "/tmp/out")))))

(deftest virtual-fs-preseeded
  (let [host (bh/make :files {"/etc/issue" "muschel\n"})]
    (is (= "muschel\n" (muschel.host/read-file host "/etc/issue")))
    (is (true? (muschel.host/file-exists? host "/etc/issue")))))

(deftest test-builtin-on-vfs
  (let [host (bh/make :files {"/data.txt" "hi"})
        {:keys [exit]}
        (exec/run-and-capture (env/new-env :cwd "/")
                              "[ -f /data.txt ]"
                              {:host host})]
    (is (zero? exit))))

;; ============================================================================
;; Pipelines — sequential semantics
;; ============================================================================

(deftest sequential-pipeline
  ;; echo writes stdout, downstream tool reads it.
  (let [host (bh/make :tools (bh/stock-tools))
        {:keys [stdout exit]}
        (exec/run-and-capture (env/new-env :cwd "/")
                              "echo 'a\nb\nc\nd' | head -2"
                              {:host host})]
    (is (zero? exit))
    (is (str/includes? stdout "a"))
    (is (str/includes? stdout "b"))
    (is (not (str/includes? stdout "d")))))
