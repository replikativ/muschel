(ns muschel.cli-test
  "Unit tests for the bash-shaped CLI argv splitter, sandbox validation,
   and one end-to-end shell-out smoke test. JVM-only; `muschel.cli` is
   `:gen-class`'d and not loaded under bb."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [muschel.cli :as cli]))

;; ============================================================================
;; split-mode — bash-style argv splitter
;; ============================================================================

(def split-mode @#'cli/split-mode)
(def validate-sandbox-flags @#'cli/validate-sandbox-flags)

(deftest split-mode-empty-argv-is-interactive
  (let [r (split-mode [])]
    (is (= :interactive (:mode r)))
    (is (= [] (:flag-argv r)))
    (is (= [] (:rest r)))))

(deftest split-mode-c-mode
  (testing "-c CMD: rest is positional [$0 args...]"
    (let [r (split-mode ["-c" "echo hi"])]
      (is (= :command (:mode r)))
      (is (= "echo hi" (:payload r)))
      (is (= [] (:rest r)))))
  (testing "-c CMD $0 $1 $2 — flags AFTER -c are positional, not flags"
    (let [r (split-mode ["-c" "echo $0 $1" "foo" "--sandbox" "bar"])]
      (is (= :command (:mode r)))
      (is (= "echo $0 $1" (:payload r)))
      (is (= ["foo" "--sandbox" "bar"] (:rest r))
          "bash treats every arg after -c CMD as positional")))
  (testing "flags BEFORE -c go to flag-argv"
    (let [r (split-mode ["--sandbox" "--root" "." "-c" "ls"])]
      (is (= :command (:mode r)))
      (is (= ["--sandbox" "--root" "."] (:flag-argv r)))
      (is (= "ls" (:payload r)))))
  (testing "-c with no value errors"
    (is (= "-c: option requires a value" (:error (split-mode ["-c"]))))))

(deftest split-mode-s-mode
  (testing "-s with no args"
    (let [r (split-mode ["-s"])]
      (is (= :stdin (:mode r)))
      (is (= [] (:rest r)))))
  (testing "-s with positional args"
    (let [r (split-mode ["-x" "-s" "a" "b"])]
      (is (= :stdin (:mode r)))
      (is (= ["-x"] (:flag-argv r)))
      (is (= ["a" "b"] (:rest r))))))

(deftest split-mode-script-file
  (testing "bare positional is the script"
    (let [r (split-mode ["script.sh" "foo" "bar"])]
      (is (= :script (:mode r)))
      (is (= "script.sh" (:script r)))
      (is (= ["foo" "bar"] (:rest r)))))
  (testing "-x script.sh -y — -y after script is $1, not a flag"
    (let [r (split-mode ["-x" "script.sh" "-y" "z"])]
      (is (= :script (:mode r)))
      (is (= ["-x"] (:flag-argv r)))
      (is (= "script.sh" (:script r)))
      (is (= ["-y" "z"] (:rest r))))))

(deftest split-mode-double-dash
  (testing "-- ends options; next positional is script"
    (let [r (split-mode ["-x" "--" "looks-like-flag" "-a"])]
      (is (= :script (:mode r)))
      (is (= ["-x"] (:flag-argv r)))
      (is (= "looks-like-flag" (:script r)))
      (is (= ["-a"] (:rest r)))))
  (testing "-- with nothing after is interactive"
    (let [r (split-mode ["-x" "--"])]
      (is (= :interactive (:mode r)))
      (is (= ["-x"] (:flag-argv r))))))

(deftest split-mode-verbs
  (testing "translate verb"
    (let [r (split-mode ["translate" "echo hi"])]
      (is (= :verb (:mode r)))
      (is (= "translate" (:verb r)))
      (is (= ["echo hi"] (:rest r)))))
  (testing "check verb with overlay flag"
    (let [r (split-mode ["check" "--permit" "p.edn" "echo hi"])]
      (is (= :verb (:mode r)))
      (is (= "check" (:verb r)))
      (is (= ["--permit" "p.edn" "echo hi"] (:rest r)))))
  (testing "verb only valid as first arg — `--sandbox translate` is NOT a verb"
    (let [r (split-mode ["--sandbox" "translate"])]
      (is (= :script (:mode r)))
      (is (= "translate" (:script r))
          "with prior flags, `translate` becomes a script-file name"))))

(deftest split-mode-virtual-flag
  (testing "--virtual with no path is normalised to empty string"
    (let [r (split-mode ["--sandbox" "--virtual" "-c" "ls"])]
      (is (= ["--sandbox" "--virtual" ""] (:flag-argv r))
          "standalone --virtual gets a \"\" value so tools.cli accepts it")
      (is (= :command (:mode r)))
      (is (= "ls" (:payload r)))))
  (testing "--virtual FILE consumes the file"
    (let [r (split-mode ["--sandbox" "--virtual" "seed.edn"])]
      (is (= ["--sandbox" "--virtual" "seed.edn"] (:flag-argv r)))
      (is (= :interactive (:mode r)))))
  (testing "--virtual at end of argv normalises to empty"
    (let [r (split-mode ["--sandbox" "--virtual"])]
      (is (= ["--sandbox" "--virtual" ""] (:flag-argv r))))))

(deftest split-mode-combined-short-flags
  (testing "-nx expands to -n -x"
    (let [r (split-mode ["-nx" "-c" "echo"])]
      (is (= ["-n" "-x"] (:flag-argv r)))
      (is (= :command (:mode r)))))
  (testing "cluster containing a value-flag errors"
    (let [r (split-mode ["-no"])]
      (is (re-find #"cannot combine value-taking flag -o" (:error r))))))

(deftest split-mode-unknown-flag
  (is (= "--bogus: unknown option" (:error (split-mode ["--bogus"])))))

(deftest split-mode-value-flag-missing-value
  (is (= "--root: option requires a value"
         (:error (split-mode ["--sandbox" "--root"])))))

;; ============================================================================
;; validate-sandbox-flags
;; ============================================================================

(deftest validate-sandbox-flags-cases
  (testing "no sandbox → fine"
    (is (nil? (validate-sandbox-flags {:virtual :unset}))))
  (testing "--sandbox alone → missing FS"
    (is (re-find #"requires --root .* --virtual"
                 (validate-sandbox-flags {:sandbox true :virtual :unset}))))
  (testing "--sandbox + --root → fine"
    (is (nil? (validate-sandbox-flags {:sandbox true :root "." :virtual :unset}))))
  (testing "--sandbox + --virtual → fine"
    (is (nil? (validate-sandbox-flags {:sandbox true :virtual ""}))))
  (testing "--sandbox + --root + --virtual → mutually exclusive"
    (is (re-find #"mutually exclusive"
                 (validate-sandbox-flags {:sandbox true :root "." :virtual ""}))))
  (testing "--root without --sandbox → error"
    (is (re-find #"require --sandbox"
                 (validate-sandbox-flags {:root "." :virtual :unset}))))
  (testing "--virtual without --sandbox → error"
    (is (re-find #"require --sandbox"
                 (validate-sandbox-flags {:virtual ""})))))

;; ============================================================================
;; End-to-end smoke test — shells out to clojure -M:cli
;; ============================================================================
;;
;; This is the only test that pays for full JVM startup, so we do ONE thing
;; that exercises split-mode + tools.cli + execution all the way through.

(defn- run-cli! [& args]
  (let [p (-> (ProcessBuilder. (cons "clojure" (cons "-M:cli" args)))
              (.redirectErrorStream false)
              .start)
        out (slurp (.getInputStream p))
        err (slurp (.getErrorStream p))
        exit (.waitFor p)]
    {:exit exit :stdout out :stderr err}))

(deftest ^:integration cli-end-to-end-smoke
  (testing "muschel -c 'echo hi' → hi on stdout, exit 0"
    (let [r (run-cli! "-c" "echo hi")]
      (is (= 0 (:exit r)) (str "stderr was: " (:stderr r)))
      (is (= "hi\n" (:stdout r)))))
  (testing "muschel --sandbox --root . -c 'sudo ls' → exit 126 with denial reason on stderr"
    (let [r (run-cli! "--sandbox" "--root" "." "-c" "sudo ls")]
      (is (= 126 (:exit r)))
      (is (re-find #"permit denied.*sudo" (:stderr r))))))
