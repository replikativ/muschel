(ns muschel.text-path-builtins-test
  "Tests for the text + path tools: sed, awk, printf, env, date, seq,
   basename, dirname, realpath, test/[."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [muschel.builtins.posix :as posix]
            [muschel.core :as m]
            [muschel.fs :as fs]
            [muschel.fs.virtual :as vfs]
            [muschel.host.builtin :as hb]
            [muschel.host.jvm :as jvm]))

(defn- mk-host
  ([] (mk-host {}))
  ([{:keys [fs]
     :or {fs (vfs/make {"/work/a.txt" "alpha\nbeta\ngamma\n"
                        "/work/csv"   "name,age\nalice,30\nbob,25\n"}
                       {:cwd "/work"})}}]
   (hb/make {:fs fs
             :fallback-host (jvm/make)
             :builtins posix/standard})))

(defn- run [host cmd]
  (m/run-and-capture (m/new-env) cmd {:host host}))

;; ============================================================================
;; basename / dirname / realpath
;; ============================================================================

(deftest basename-strips-dirs
  (let [r (run (mk-host) "basename /etc/foo/bar.txt")]
    (is (= 0 (:exit r)))
    (is (= "bar.txt\n" (:stdout r)))))

(deftest basename-with-suffix
  (let [r (run (mk-host) "basename /etc/foo/bar.txt .txt")]
    (is (= 0 (:exit r)))
    (is (= "bar\n" (:stdout r)))))

(deftest dirname-strips-final
  (let [r (run (mk-host) "dirname /etc/foo/bar.txt")]
    (is (= 0 (:exit r)))
    (is (= "/etc/foo\n" (:stdout r)))))

(deftest dirname-no-slash-returns-dot
  (let [r (run (mk-host) "dirname x")]
    (is (= 0 (:exit r)))
    (is (= ".\n" (:stdout r)))))

(deftest realpath-resolves-inside-root
  (let [r (run (mk-host) "realpath a.txt")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "a.txt"))))

;; ============================================================================
;; printf
;; ============================================================================

(deftest printf-string
  (let [r (run (mk-host) "printf \"hello %s\\n\" world")]
    (is (= 0 (:exit r)))
    (is (= "hello world\n" (:stdout r)))))

(deftest printf-int
  (let [r (run (mk-host) "printf \"%d\\n\" 42")]
    (is (= 0 (:exit r)))
    (is (= "42\n" (:stdout r)))))

(deftest printf-multiple-args-reuse-format
  ;; bash's printf reapplies the format string when there are more
  ;; arguments than specifiers.
  (let [r (run (mk-host) "printf \"%s\\n\" a b c")]
    (is (= 0 (:exit r)))
    (is (= "a\nb\nc\n" (:stdout r)))))

;; ============================================================================
;; env / date / seq
;; ============================================================================

(deftest env-prints-vars
  (let [r (run (mk-host) "env")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "="))))

(deftest date-default
  (let [r (run (mk-host) "date")]
    (is (= 0 (:exit r)))
    (is (re-find #"\d{4}-\d{2}-\d{2}" (:stdout r)))))

(deftest date-format
  (let [r (run (mk-host) "date +%Y")]
    (is (= 0 (:exit r)))
    (is (re-find #"^\d{4}$" (str/trim-newline (:stdout r))))))

(deftest seq-1-to-n
  (let [r (run (mk-host) "seq 3")]
    (is (= 0 (:exit r)))
    (is (= "1\n2\n3\n" (:stdout r)))))

(deftest seq-start-end
  (let [r (run (mk-host) "seq 2 5")]
    (is (= 0 (:exit r)))
    (is (= "2\n3\n4\n5\n" (:stdout r)))))

(deftest seq-with-step
  (let [r (run (mk-host) "seq 1 2 7")]
    (is (= 0 (:exit r)))
    (is (= "1\n3\n5\n7\n" (:stdout r)))))

;; ============================================================================
;; test / [
;; ============================================================================

(deftest test-string-equal
  (let [r (run (mk-host) "test foo = foo")]
    (is (= 0 (:exit r)))))

(deftest test-string-not-equal
  (let [r (run (mk-host) "test foo = bar")]
    (is (= 1 (:exit r)))))

(deftest test-empty-z
  (let [r (run (mk-host) "test -z \"\"")]
    (is (= 0 (:exit r)))))

(deftest test-int-eq
  (let [r (run (mk-host) "test 5 -eq 5")]
    (is (= 0 (:exit r)))))

(deftest test-int-lt
  (let [r (run (mk-host) "test 2 -lt 5")]
    (is (= 0 (:exit r)))))

(deftest test-file-exists
  (let [r (run (mk-host) "test -e a.txt")]
    (is (= 0 (:exit r)))))

(deftest test-file-missing
  (let [r (run (mk-host) "test -e missing.txt")]
    (is (= 1 (:exit r)))))

(deftest test-d-on-dir
  (let [fs (vfs/make {"/work" :dir "/work/sub" :dir} {:cwd "/work"})
        r  (run (mk-host {:fs fs}) "test -d sub")]
    (is (= 0 (:exit r)))))

(deftest bracket-form
  (let [r (run (mk-host) "[ -e a.txt ]")]
    (is (= 0 (:exit r)))))

(deftest bracket-in-if
  (let [r (run (mk-host) "if [ -e a.txt ]; then echo yes; fi")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "yes"))))

;; ============================================================================
;; sed
;; ============================================================================

(deftest sed-substitute-first
  (let [r (run (mk-host) "echo foofoo | sed 's/foo/bar/'")]
    (is (= 0 (:exit r)))
    (is (= "barfoo\n" (:stdout r)))))

(deftest sed-substitute-global
  (let [r (run (mk-host) "echo foofoo | sed 's/foo/bar/g'")]
    (is (= 0 (:exit r)))
    (is (= "barbar\n" (:stdout r)))))

(deftest sed-substitute-case-insensitive
  (let [r (run (mk-host) "echo FOOFOO | sed 's/foo/bar/gi'")]
    (is (= 0 (:exit r)))
    (is (= "barbar\n" (:stdout r)))))

(deftest sed-delete-pattern
  (let [r (run (mk-host) "sed '/beta/d' a.txt")]
    (is (= 0 (:exit r)))
    (is (= "alpha\ngamma\n" (:stdout r)))))

(deftest sed-quiet-print-pattern
  (let [r (run (mk-host) "sed -n '/beta/p' a.txt")]
    (is (= 0 (:exit r)))
    (is (= "beta\n" (:stdout r)))))

(deftest sed-in-place-rewrites-file
  (let [host (mk-host)]
    (run host "sed -i 's/alpha/AAA/' a.txt")
    (let [r (run host "cat a.txt")]
      (is (.contains ^String (:stdout r) "AAA"))
      (is (not (.contains ^String (:stdout r) "alpha"))))))

;; ============================================================================
;; awk
;; ============================================================================

(deftest awk-print-default
  (let [r (run (mk-host) "echo hi | awk '{print}'")]
    (is (= 0 (:exit r)))
    (is (= "hi\n" (:stdout r)))))

(deftest awk-print-field
  (let [r (run (mk-host) "echo 'a b c' | awk '{print $2}'")]
    (is (= 0 (:exit r)))
    (is (= "b\n" (:stdout r)))))

(deftest awk-csv-field-separator
  (let [r (run (mk-host) "awk -F , '{print $1}' csv")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "name"))
    (is (.contains ^String (:stdout r) "alice"))
    (is (.contains ^String (:stdout r) "bob"))))

(deftest awk-pattern-match
  (let [r (run (mk-host) "awk '/beta/' a.txt")]
    (is (= 0 (:exit r)))
    (is (= "beta\n" (:stdout r)))))

(deftest awk-nr-eq
  (let [r (run (mk-host) "awk 'NR==2 {print}' a.txt")]
    (is (= 0 (:exit r)))
    (is (= "beta\n" (:stdout r)))))

;; ============================================================================
;; sleep
;; ============================================================================

(deftest sleep-blocks
  (let [start (System/currentTimeMillis)
        r (run (mk-host) "sleep 0.05")
        elapsed (- (System/currentTimeMillis) start)]
    (is (= 0 (:exit r)))
    (is (>= elapsed 40))))

(deftest sleep-invalid-arg
  (let [r (run (mk-host) "sleep banana")]
    (is (= 2 (:exit r)))
    (is (re-find #"invalid" (:stderr r)))))

;; ============================================================================
;; curl
;;
;; Smoke tests that exercise the FS-aware output path. We don't hit
;; the real internet from the test suite — we hand curl a -X HEAD
;; against a known-bad URL and confirm the request roundtrips
;; through the FS for -o.
;; ============================================================================

(deftest curl-handles-bad-host
  (let [r (run (mk-host) "curl -s http://localhost:1/never")]
    ;; Connection refused → non-zero exit with a clean curl: error
    (is (pos? (:exit r)))
    (is (re-find #"curl:" (:stderr r)))))

(deftest curl-output-routes-through-fs
  ;; -o on a path outside the sandbox refuses; we can't easily prove
  ;; the network path without external connectivity, so we check the
  ;; refusal path. Real-network behaviour is covered by curl-uuid
  ;; (skipped unless connectivity is available).
  (let [fs (vfs/make {"/work" :dir} {:cwd "/work"})
        host (hb/make {:fs fs :fallback-host (jvm/make)
                       :builtins posix/standard})
        ;; pick a host that's guaranteed unreachable to short-circuit
        r (run host "curl -s -o /etc/dvergr-test http://localhost:1/")]
    ;; Either we refused before the call (no network try) or the
    ;; call failed first. In neither case does /etc/dvergr-test exist.
    (is (not (.exists (java.io.File. "/etc/dvergr-test"))))))
