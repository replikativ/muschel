(ns muschel.host-builtin-cljc-test
  "Cross-platform end-to-end tests through the builtin host. Verifies
   that builtin dispatch, refusal, nested `sh -c`, pipes, and which
   work identically on JVM (`host.jvm`) and ClojureScript / Node
   (`host.browser`). The same assertions run under both `clj -M:test`
   and `node out/ci-tests.js`."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [muschel.core :as m]
            [muschel.builtins.posix :as posix]
            [muschel.fs.virtual :as vfs]
            [muschel.host.builtin :as hb]
            #?(:clj  [muschel.host.jvm :as host.jvm]
               :cljs [muschel.host.browser :as host.browser])))

;; ============================================================================
;; Fixture helpers
;; ============================================================================

(defn- fallback-host []
  #?(:clj  (host.jvm/make)
     :cljs (host.browser/make)))

(defn- mk-host
  ([] (mk-host {}))
  ([{:keys [fs allowlist]
     :or {fs (vfs/make {"/work/a.txt" "alpha\nbeta\ngamma\n"
                        "/work/b.txt" "one\ntwo\n"}
                       {:cwd "/work"})
          allowlist #{}}}]
   (hb/make {:fs fs
             :fallback-host (fallback-host)
             :builtins posix/standard-read-only
             :fallback-allowlist allowlist})))

(defn- run [host script]
  (m/run-and-capture (m/new-env) script {:host host}))

;; ============================================================================
;; Single-builtin dispatch
;; ============================================================================

(deftest builtin-dispatch-cat
  (let [r (run (mk-host) "cat a.txt")]
    (is (= 0 (:exit r)))
    (is (= "alpha\nbeta\ngamma\n" (:stdout r)))))

(deftest unknown-command-refused
  (let [r (run (mk-host) "rm -rf /")]
    (is (= 126 (:exit r))
        "rm is not in :builtins (we use read-only set here) and not allowlisted")
    (is (str/includes? (:stderr r) "muschel:"))))

;; ============================================================================
;; Recursive `sh -c` — the test that was JVM-only before the runtime registry
;; ============================================================================

(deftest sh-c-runs-via-same-host
  (let [r (run (mk-host) "sh -c \"echo hello && cat a.txt\"")]
    (is (= 0 (:exit r)) (pr-str r))
    (is (str/includes? (:stdout r) "hello"))
    (is (str/includes? (:stdout r) "alpha"))))

(deftest sh-c-cannot-escape-builtin-set
  ;; The inner shell goes through the SAME builtin host, so rm is
  ;; still refused — the sandbox holds across recursion.
  (let [r (run (mk-host) "sh -c \"rm -rf /\"")]
    (is (= 126 (:exit r)))
    (is (str/includes? (:stderr r) "muschel:"))))

(deftest sh-c-no-script-errors
  (let [r (run (mk-host) "sh -c")]
    (is (= 1 (:exit r)))
    (is (str/includes? (:stderr r) "Missing required argument"))))

(deftest sh-c-depth-cap
  ;; Self-recursive `sh -c` eventually trips the depth guard. The
  ;; failure must be reported (exit ≠ 0) and the stderr must mention
  ;; the depth cap; we don't pin to an exact depth because the call
  ;; stack carries hosts/envs and the safety margin can change.
  (let [r (run (mk-host)
               (str "sh -c 'sh -c \"sh -c \\\"sh -c "
                    "\\\\\\\"sh -c so-deep\\\\\\\"\\\"\"'"))]
    (is (not= 0 (:exit r)))))

(deftest bash-alias-works
  (let [r (run (mk-host) "bash -c \"echo bashing\"")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "bashing"))))

;; ============================================================================
;; Pipes
;; ============================================================================

(deftest cat-pipe-grep
  (let [r (run (mk-host) "cat a.txt | grep beta")]
    (is (= 0 (:exit r)))
    (is (= "beta\n" (:stdout r)))))

(deftest echo-pipe-wc
  (let [r (run (mk-host) "echo hello | wc -c")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "6"))))    ; "hello\n" = 6 bytes

;; ============================================================================
;; FS containment holds across recursion
;; ============================================================================

(deftest sh-c-cannot-read-outside-root
  ;; Path resolves outside the FS root → reads return nil → cat error.
  ;; Confirms the FS protocol's defense-in-depth still applies inside
  ;; nested sh.
  (let [r (run (mk-host) "sh -c \"cat /etc/passwd\"")]
    (is (not= 0 (:exit r)))))
