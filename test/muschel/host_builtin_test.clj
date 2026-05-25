(ns muschel.host-builtin-test
  "End-to-end tests through the builtin host: dispatch, refusal,
   recursive sh -c, allowlist fallthrough."
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.core :as m]
            [muschel.fs.virtual :as vfs]
            [muschel.host.builtin :as hb]
            [muschel.host.jvm :as jvm]
            [muschel.builtins.posix :as posix]))

(defn- mk-host
  ([] (mk-host {}))
  ([{:keys [fs allowlist]
     :or {fs (vfs/make {"/work/a.txt" "alpha\nbeta\ngamma"
                        "/work/b.txt" "one\ntwo"}
                       {:cwd "/work"})
          allowlist #{}}}]
   (hb/make {:fs fs
             :fallback-host (jvm/make)
             :builtins posix/standard-read-only
             :fallback-allowlist allowlist})))

(defn- run [host cmd]
  (m/run-and-capture (m/new-env) cmd {:host host}))

;; ============================================================================
;; Dispatch — builtin wins, unknown refuses
;; ============================================================================

(deftest builtin-dispatch-pwd-handled-by-muschel-shell
  ;; pwd is a shell-builtin in muschel.exec itself (matches bash's
  ;; behaviour), so it never reaches our host -spawn override. The
  ;; muschel shell answers from the env's cwd directly. Verify it
  ;; just runs cleanly — our posix/pwd is only hit if the muschel
  ;; shell ever forwards a `pwd` to -spawn (rare).
  (let [r (run (mk-host) "pwd")]
    (is (= 0 (:exit r)))
    (is (seq (:stdout r)))))

(deftest builtin-dispatch-cat
  (let [r (run (mk-host) "cat a.txt")]
    (is (= 0 (:exit r)))
    (is (= "alpha\nbeta\ngamma\n" (:stdout r)))))

(deftest unknown-command-refused
  (let [r (run (mk-host) "rm -rf /")]
    (is (= 126 (:exit r))
        "rm is not a builtin and not allowlisted → refused")
    (is (re-find #"muschel:" (:stderr r)))))

(deftest fallback-allowlist-runs
  ;; `true` always exits 0 on any POSIX system. Allowlist it and
  ;; verify the fallback host actually executes it.
  (let [h (mk-host {:allowlist #{"true"}})
        r (run h "true")]
    (is (= 0 (:exit r)))))

;; ============================================================================
;; Recursive sh -c — the bash-of-our-own-shell trick
;; ============================================================================

(deftest sh-c-runs-via-same-host
  (let [r (run (mk-host) "sh -c \"echo hello && cat a.txt\"")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "hello"))
    (is (.contains ^String (:stdout r) "alpha"))))

(deftest sh-c-cannot-escape-builtin-set
  ;; rm isn't a builtin nor allowlisted — bash -c can't reach it.
  (let [r (run (mk-host) "sh -c \"rm -rf /\"")]
    (is (= 126 (:exit r)))
    (is (re-find #"muschel:" (:stderr r)))))

(deftest sh-c-no-script-errors
  (let [r (run (mk-host) "sh -c")]
    (is (= 2 (:exit r)))
    (is (re-find #"option requires an argument" (:stderr r)))))

(deftest bash-alias-works
  (let [r (run (mk-host) "bash -c \"echo bashing\"")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "bashing"))))

;; ============================================================================
;; which — registry visibility
;; ============================================================================

(deftest which-finds-builtin
  (let [r (run (mk-host) "which cat")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "cat"))))

(deftest which-misses-unknown
  (let [r (run (mk-host) "which nopenope")]
    (is (= 1 (:exit r)))))
