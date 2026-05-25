(ns muschel.mvdan-sh-test
  "Regression test using `test/muschel/mvdan_sh_corpus.edn` — a port of
   `runTests` from mvdan/sh's `interp/interp_test.go` (BSD-3-Clause;
   see `LICENSE-mvdan-sh.txt`).

   This is the broadest behavioral pin we have on bash semantics. Each
   case is `{:in <bash-source> :want <expected-combined-output>}`. We
   run `:in` through `muschel.exec`, capturing stdout+stderr together
   (mvdan/sh uses a single concurrent buffer for both), and compare.

   ## Match policy

   mvdan/sh's want-string follows this convention:
     - `\"\"`                                 → empty stdout, exit 0
     - `\"some text\"`                        → stdout = text, exit 0
     - `\"text\\nexit status N\"`             → text + non-zero exit N
     - trailing ` #JUSTERR` suffix          → only stderr matters
     - trailing ` #IGNORE` / ` #VERSION...`  → variants we don't try

   We replicate `interp_test.go`'s post-processing: strip everything
   from ` #` onward in `want` before comparing.

   The runner classifies each case as:
     :pass        — outputs match
     :fail        — outputs differ (real divergence)
     :error       — exception during exec (parse/lex/permit error)
     :skip-unsupported — `:in` mentions an unsupported feature (arrays,
                         GOSH_PROG, mapfile, etc.) we know we don't do

   We assert that **:fail + :error ≤ a fixed budget** so improvements
   to muschel reduce the budget over time. Bumping the budget UP needs
   explicit justification."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [muschel.env :as env]
            [muschel.exec :as exec]))

;; ============================================================================
;; Load corpus
;; ============================================================================

(def corpus
  (with-open [r (io/reader (io/resource "muschel/mvdan_sh_corpus.edn"))]
    (edn/read (java.io.PushbackReader. r))))

;; ============================================================================
;; Filter: skip features we deliberately don't support
;; ============================================================================

(def ^:private unsupported-patterns
  ;; Substring matches in :in that mean "skip this entry — known
  ;; unsupported feature, not a regression of ours."
  [;; Arrays — refused at lex layer
   "[@]" "[*]" "${#" "declare -" "readarray" "mapfile"
   ;; Test fixtures that depend on mvdan/sh-specific runtime
   "<<<GOSH" "<<<pathProg" "<<<PATH_SEPARATOR"
   "GOSH_CMD" "GOSH_PROG"
   ;; coproc / select — explicitly out per our refusal list
   "coproc " "select "
   ;; process substitution / here-string — refused
   "<(" ">(" "<<<"
   ;; arrays via parens
   "=(" "+=("
   ;; Indirect array expansion
   "${!"
   ;; trap / shopt
   "trap " "shopt "
   ;; bash regex match operator =~
   " =~ "
   ;; case-conversion expansions ${var^} ${var,}
   "^^}" ",,}"
   ;; Process substitution + named pipes used in tests
   "mkfifo"
   ;; mvdan/sh test fixtures: env vars set up in their TestMain that we
   ;; don't replicate (INTERP_GLOBAL, MIXEDCASE_INTERP_GLOBAL, ENV_PROG)
   "INTERP_GLOBAL" "MIXEDCASE_INTERP_GLOBAL" "ENV_PROG"])

(defn- has-null-byte? [^String s]
  (some #(= 0 (int %)) s))

(defn skip? [{:keys [in]}]
  (or (str/includes? in "<<<")
      (has-null-byte? in)
      (some #(str/includes? in %) unsupported-patterns)))

;; ============================================================================
;; Run + classify
;; ============================================================================

(defn- strip-want-suffix [^String want]
  ;; mvdan/sh: strings.Index(want, " #") cut before comparison
  (let [i (.indexOf want " #")]
    (if (neg? i) want (subs want 0 i))))

(def ^:private case-timeout-ms 3000)

(defn- fresh-tmp-dir []
  ;; Each case runs in its own clean dir, matching mvdan/sh's
  ;; t.TempDir() semantics. Otherwise side effects from `mkdir`,
  ;; `touch`, `ln` etc. cross-contaminate later cases.
  (let [d (java.nio.file.Files/createTempDirectory
           "muschel-mvdan-" (into-array java.nio.file.attribute.FileAttribute []))]
    (.toString d)))

(defn- rm-rf [^String path]
  (try (let [f (java.io.File. path)]
         (when (.isDirectory f)
           (doseq [c (.listFiles f)] (rm-rf (.getAbsolutePath c))))
         (.delete f))
       (catch Throwable _ nil)))

(defn- run-case [{:keys [in want]}]
  ;; Per-case timeout (default 3s) prevents one bad case from hanging
  ;; the suite. Cases that exceed the budget are categorised :timeout.
  (let [tdir (fresh-tmp-dir)
        fut (future
              (try
                (let [buf (java.io.ByteArrayOutputStream.)
                      opts {:out buf :err buf
                            :in (java.io.ByteArrayInputStream. (.getBytes "" "UTF-8"))}
                      env0 (env/new-env :cwd tdir)
                      {:keys [exit]} (exec/run env0 in opts)
                      got (.toString buf "UTF-8")
                      want-clean (strip-want-suffix want)
                      got-with-exit (if (zero? exit) got
                                        (str got "exit status " exit))]
                  (if (= got-with-exit want-clean) :pass :fail))
                (catch Throwable _ :error)
                (finally (rm-rf tdir))))
        result (deref fut case-timeout-ms :timeout)]
    (when (= :timeout result) (future-cancel fut))
    result))

;; ============================================================================
;; The regression test
;; ============================================================================

;; Current pass/fail budget. This is a baseline number; reducing it
;; is good (signals improved bash conformance); raising it needs
;; explicit justification in the commit message.
;;
;; The baseline is set after the initial port. Known failure categories
;; (none of which are CORRECTNESS regressions in our model — they're
;; just features mvdan/sh covers that we don't yet):
;;
;;   - `printf` not yet a builtin (uses host printf which differs)
;;   - `break` / `continue` / `return` not yet builtins
;;   - `echo -e` / `-E` option parsing not implemented
;;   - error-message text differs from mvdan/sh's expected strings
;;     (e.g. \"shouldnotexist: Cannot run program …\" vs
;;      \"\\\"shouldnotexist\\\": executable file not found in $PATH\")
;;   - arg-validation messages differ (`cd a b`, `exit 1 2`)
;;   - some edge-case bash semantics (`$?` after backgrounded false)
(def ^:private failure-budget 170)

(defn- run-case-rich
  "Like run-case but returns {:result … :got … :want …} for analysis."
  [{:keys [in want]}]
  (let [tdir (fresh-tmp-dir)
        fut (future
              (try
                (let [buf (java.io.ByteArrayOutputStream.)
                      opts {:out buf :err buf
                            :in (java.io.ByteArrayInputStream. (.getBytes "" "UTF-8"))}
                      env0 (env/new-env :cwd tdir)
                      {:keys [exit]} (exec/run env0 in opts)
                      got (.toString buf "UTF-8")
                      want-clean (strip-want-suffix want)
                      got-with-exit (if (zero? exit) got
                                        (str got "exit status " exit))]
                  {:result (if (= got-with-exit want-clean) :pass :fail)
                   :got got-with-exit
                   :want want-clean})
                (catch Throwable e {:result :error :err (.getMessage e)})
                (finally (rm-rf tdir))))
        r (deref fut case-timeout-ms {:result :timeout})]
    (when (= :timeout (:result r)) (future-cancel fut))
    r))

(defn -inspect-failures
  "REPL helper: dump first N failing cases for inspection.
   Usage: (clojure -X muschel.mvdan-sh-test/-inspect-failures :n 20)"
  [& {:keys [n] :or {n 20}}]
  (let [active (remove skip? corpus)
        fails (->> active
                   (map (fn [c] (assoc (run-case-rich c) :in (:in c))))
                   (filter #(not= :pass (:result %)))
                   (take n))]
    (doseq [{:keys [in result got want err]} fails]
      (println "---")
      (println "result:" result "in:" (pr-str in))
      (when want (println "  want:" (pr-str want)))
      (when got  (println "  got: " (pr-str got)))
      (when err  (println "  err: " err)))))

(defn- categorize-fail
  "Heuristic bucket for a failing case based on its :in and (when
   available) :got/:want."
  [{:keys [in got want err result]}]
  (cond
    (= :timeout result) :timeout
    (= :error result)   (cond
                          (re-find #"unsupported redirect|For input string" (or err ""))
                          :exec-error
                          :else :parse-or-other-error)

    ;; Missing builtins (we shell out → "Cannot run program")
    (and got (re-find #"Cannot run program \"(printf|break|continue|return|declare|local|read|trap|alias|unalias|history|getopts|umask|mapfile|readarray|times|type|caller|complete|enable|builtin|command|hash|help|let|logout|popd|pushd|dirs|suspend|disown|fc|bind|kill|wait|jobs|fg|bg|exec|eval|source|\\.|exit|return|shift|set|unset|export)\"" got))
    :missing-builtin

    ;; echo flag handling (-e / -E / -n)
    (re-find #"^echo +-[neE]" in)
    :echo-flags

    ;; Error message text differences (we have output BUT it differs)
    (and got want
         (or (and (re-find #"command not found|executable file not found" want)
                  (re-find #"Cannot run program" got))
             (and (re-find #"usage:" want)
                  (= got ""))))
    :error-text-mismatch

    ;; Set -e (errexit) interaction
    (re-find #"set -e\b" in) :errexit

    ;; Arithmetic specifics
    (and (re-find #"\$\(\(|\bb-let " in)
         (re-find #"arithmetic" (or err "")))
    :arith

    ;; Quoting / expansion edge cases
    (and got want
         (not= got want)
         (or (re-find #"\$\{[!@*#]" in)
             (re-find #"\\\\[0-9xnrt]" in)))
    :expansion-edge

    ;; Function semantics
    (re-find #"\bf\(\)|function f\b|local\b" in)
    :function

    ;; Heredoc edge cases
    (re-find #"<<-?\w" in) :heredoc

    ;; Pipefail / status
    (re-find #"pipefail|\$\?" in) :status

    :else :other))

(defn -categorize-failures
  "Run all active cases, bucket failures by likely cause, print
   counts + 2 example inputs per bucket. Force-exits when done —
   a few cases (`wait` deref'ing dangling bg-job futures) keep
   non-daemon threads alive that would otherwise block JVM exit."
  [& _]
  (let [active (remove skip? corpus)
        all (mapv (fn [c] (assoc (run-case-rich c) :in (:in c))) active)
        fails (remove #(= :pass (:result %)) all)
        bucketed (group-by categorize-fail fails)]
    (println "Total active:" (count active)
             " pass:" (- (count active) (count fails))
             " fail:" (count fails))
    (println)
    (doseq [[k cases] (sort-by (comp - count val) bucketed)]
      (println (format "%-25s %4d  e.g. %s"
                       k (count cases)
                       (pr-str (vec (map :in (take 2 cases)))))))
    (flush)
    (System/exit 0)))

(deftest mvdan-sh-corpus-regression
  (let [active (remove skip? corpus)
        results (frequencies (map run-case active))
        pass (get results :pass 0)
        fail (get results :fail 0)
        err  (get results :error 0)
        timeout (get results :timeout 0)
        bad  (+ fail err timeout)]
    (println "\n[mvdan/sh corpus]"
             (count corpus) "total,"
             (- (count corpus) (count active)) "skipped (unsupported),"
             (count active) "active,"
             pass "pass,"
             fail "fail,"
             err "error,"
             timeout "timeout")
    (is (<= bad failure-budget)
        (str "regression: " bad " failures+errors+timeouts exceeds budget of "
             failure-budget
             ". Either fix the regression or — if intentional — bump the "
             "budget with a justification in the commit message."))))
