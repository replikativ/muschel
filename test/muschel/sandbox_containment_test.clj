(ns muschel.sandbox-containment-test
  "End-to-end tests proving the muschel sandbox doesn't leak file
   reads, writes, or globs to the real filesystem when the host is a
   BuiltinHost wrapping a virtual FS.

   These are regression tests for the FS-escape audit (PR #4):

   - `wc -l < /etc/passwd` used to read the real file via the JVM
     host's open-file-source.
   - `> /tmp/leak` used to write the real disk via the JVM host's
     open-file-sink.
   - `ls /etc/*` used to glob the real filesystem via
     babashka.fs/glob.
   - `cat ../etc/passwd` used to traverse out of the virtual root."
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.builtins.posix :as posix]
            [muschel.core :as m]
            [muschel.fs.virtual :as vfs]
            [muschel.host.builtin :as hb]
            [muschel.host.jvm :as jvm]))

(defn- mk-host
  ([] (mk-host {}))
  ([{:keys [fs allowlist]
     :or {fs (vfs/make {"/work/a.txt" "alpha\nbeta\n"}
                       {:cwd "/work"})
          allowlist #{}}}]
   (hb/make {:fs fs
             :fallback-host (jvm/make)
             :builtins posix/standard-read-only
             :fallback-allowlist allowlist})))

(defn- run [host cmd]
  (m/run-and-capture (m/new-env) cmd {:host host}))

;; ============================================================================
;; Redirects cannot escape root
;; ============================================================================

(deftest redirect-input-rejects-outside-root
  (let [r (run (mk-host) "wc -l < /etc/passwd")]
    (is (= 1 (:exit r)))
    (is (re-find #"redirect" (:stderr r)))))

(deftest redirect-input-rejects-traversal
  (let [r (run (mk-host) "wc -l < ../../etc/passwd")]
    (is (= 1 (:exit r)))))

(deftest redirect-output-cannot-write-real-disk
  ;; Even if the sandbox accepted the write (it would route to the
  ;; vfs), no file lands on the real FS.
  (let [marker (str "/tmp/muschel-sandbox-test-marker-"
                    (System/currentTimeMillis))]
    (try
      (let [r (run (mk-host) (str "echo PWNED > " marker))]
        ;; Either we refused (preferred) or we accepted by writing into
        ;; the vfs — either way, no real file.
        (is (not (.exists (java.io.File. marker)))
            "real /tmp file must not be created"))
      (finally
        ;; Defensive cleanup
        (try (.delete (java.io.File. marker)) (catch Throwable _))))))

(deftest builtin-cat-cannot-read-outside-root
  (let [r (run (mk-host) "cat /etc/passwd")]
    (is (= 1 (:exit r)))
    (is (re-find #"No such file" (:stderr r)))))

(deftest builtin-cat-cannot-traverse-up
  (let [r (run (mk-host) "cat ../etc/passwd")]
    (is (= 1 (:exit r)))))

;; ============================================================================
;; Globs walk the sandboxed FS only
;; ============================================================================

(deftest glob-does-not-match-real-disk
  (let [r (run (mk-host) "ls /etc/*")]
    ;; Either ls reports no matches OR exits 2 (no such file). Either
    ;; way no real /etc contents leak into stdout.
    (is (not (re-find #"passwd|hosts|shadow" (:stdout r))))))

(deftest glob-matches-sandbox-only
  (let [fs (vfs/make {"/work/foo.txt" "1"
                      "/work/bar.txt" "2"
                      "/work/baz.md"  "3"}
                     {:cwd "/work"})
        r  (run (mk-host {:fs fs}) "ls *.txt")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "foo.txt"))
    (is (.contains ^String (:stdout r) "bar.txt"))
    (is (not (.contains ^String (:stdout r) "baz.md")))
    ;; And no real-disk file made it in:
    (is (not (re-find #"deps\.edn|LICENSE" (:stdout r))))))

;; ============================================================================
;; Legitimate operations inside the sandbox keep working
;; ============================================================================

(deftest legit-read-inside-root-works
  (let [r (run (mk-host) "cat a.txt")]
    (is (= 0 (:exit r)))
    (is (= "alpha\nbeta\n" (:stdout r)))))

(deftest legit-redirect-input-inside-root-works
  (let [r (run (mk-host) "wc -l < a.txt")]
    (is (= 0 (:exit r)))
    (is (re-find #"^\s*2" (:stdout r)))))

(deftest legit-redirect-output-into-vfs
  (let [host (mk-host)
        r1 (run host "echo hi > new.txt")
        r2 (run host "cat new.txt")]
    (is (= 0 (:exit r1)))
    (is (= 0 (:exit r2)))
    (is (.contains ^String (:stdout r2) "hi"))))

(deftest legit-redirect-append-into-vfs
  (let [host (mk-host)]
    (run host "echo first > log.txt")
    (run host "echo second >> log.txt")
    (let [r (run host "cat log.txt")]
      (is (= 0 (:exit r)))
      (is (.contains ^String (:stdout r) "first"))
      (is (.contains ^String (:stdout r) "second")))))
