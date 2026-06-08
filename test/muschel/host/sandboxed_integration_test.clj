(ns muschel.host.sandboxed-integration-test
  "Integration tests for the SandboxedHost decorator — these EXEC
   bwrap and so are gated on bwrap actually being installed. Each
   deftest prints SKIP and returns early when bwrap is missing, so
   CI without bwrap stays green.

   Run locally:
     clojure -M:test --namespace muschel.host.sandboxed-integration-test

   Each test creates its own temp dir under /tmp, spawns directly
   against `JvmHost` wrapped in `SandboxedHost` (no BuiltinHost in
   between — we're exercising the bwrap layer specifically), and
   inspects stdout / stderr / exit."
  (:require [babashka.fs :as bbfs]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [muschel.host :as host]
            [muschel.host.jvm :as jvm]
            [muschel.host.sandboxed :as sb]))

;; ============================================================================
;; Bwrap availability + helpers
;; ============================================================================

(defn- bwrap-available? []
  (try
    (let [p (.exec (Runtime/getRuntime)
                   ^"[Ljava.lang.String;"
                   (into-array String ["bwrap" "--version"]))]
      (.waitFor p)
      (zero? (.exitValue p)))
    (catch Throwable _ false)))

(defmacro deftest-bwrap
  "Like deftest, but each test body short-circuits with a SKIP message
   when bwrap isn't on PATH."
  [name & body]
  `(deftest ~name
     (if (bwrap-available?)
       (do ~@body)
       (println (str "SKIP " '~name " — bwrap not available")))))

(defn- mk-tmpdir []
  (let [p (bbfs/create-temp-dir {:prefix "muschel-sbx-it-"})]
    (str p)))

(defn- run-in [sbox argv]
  (let [inner (jvm/make)
        out (host/-string-sink inner)
        err (host/-string-sink inner)
        r   (host/-spawn sbox {:cmd (first argv)
                               :args (vec (rest argv))
                               :dir nil
                               :out out :err err})
        exit ((:wait r))]
    {:exit exit
     :stdout (host/-sink->string inner out)
     :stderr (host/-sink->string inner err)}))

;; ============================================================================
;; Tests
;; ============================================================================

(deftest-bwrap path-alignment
  ;; A file at <bind-root>/foo.txt is visible as /foo.txt inside the
  ;; sandbox — proving muschel's agent view and bwrap's view of `/`
  ;; line up.
  (let [tmp (mk-tmpdir)
        f   (io/file tmp "foo.txt")
        _   (spit f "hello sandbox\n")
        sbox (sb/make {:wrapped (jvm/make) :bind-root tmp})
        r (run-in sbox ["cat" "/foo.txt"])]
    (try
      (is (= 0 (:exit r)))
      (is (= "hello sandbox\n" (:stdout r)))
      (finally (bbfs/delete-tree tmp)))))

(deftest-bwrap network-off-blocks-egress
  ;; --net :off (default) → --unshare-net → no DNS, no sockets.
  ;; `getent hosts <name>` returns non-zero with no answers.
  (let [tmp (mk-tmpdir)
        sbox (sb/make {:wrapped (jvm/make) :bind-root tmp})
        r (run-in sbox ["getent" "hosts" "1.1.1.1"])]
    (try
      (is (not (zero? (:exit r)))
          "getent should fail when network is unshared")
      (finally (bbfs/delete-tree tmp)))))

(deftest-bwrap network-on-allows-egress
  ;; --net :on → no --unshare-net → the sandboxed process can resolve
  ;; loopback. We don't reach the real network in tests; loopback +
  ;; getent for an IP literal is enough to confirm the namespace is
  ;; shared.
  (let [tmp (mk-tmpdir)
        sbox (sb/make {:wrapped (jvm/make) :bind-root tmp :net :on})
        ;; `getent hosts 127.0.0.1` is offline-safe and returns 0.
        r (run-in sbox ["getent" "hosts" "127.0.0.1"])]
    (try
      (is (zero? (:exit r)) (str "stderr: " (:stderr r)))
      (finally (bbfs/delete-tree tmp)))))

(deftest-bwrap ro-binds-are-read-only
  ;; /etc is mounted --ro-bind-try; writes inside it must fail with
  ;; EROFS (or similar). Use `sh -c` so the shell's redirect handles
  ;; the failure properly.
  (let [tmp (mk-tmpdir)
        sbox (sb/make {:wrapped (jvm/make) :bind-root tmp})
        r (run-in sbox ["sh" "-c" "echo evil > /etc/passwd 2>&1; echo exit=$?"])]
    (try
      (is (not (zero? (-> (:stdout r)
                          (clojure.string/split #"exit=")
                          last
                          clojure.string/trim
                          Integer/parseInt)))
          (str "writing to /etc should have failed; got: " (:stdout r)))
      (finally (bbfs/delete-tree tmp)))))

(deftest-bwrap writes-to-bind-root-persist
  ;; Writes to / inside the sandbox land on the host's bind-root.
  (let [tmp (mk-tmpdir)
        sbox (sb/make {:wrapped (jvm/make) :bind-root tmp})
        _ (run-in sbox ["sh" "-c" "echo persisted > /written.txt"])]
    (try
      (is (= "persisted\n" (slurp (io/file tmp "written.txt"))))
      (finally (bbfs/delete-tree tmp)))))

(deftest-bwrap chdir-translates-real-disk-path-to-sandbox
  ;; muschel passes :dir = <bind-root>/work (DiskFS-real path); bwrap
  ;; must --chdir to /work inside the sandbox.
  (let [tmp (mk-tmpdir)
        _ (bbfs/create-dirs (io/file tmp "work"))
        sbox (sb/make {:wrapped (jvm/make) :bind-root tmp})
        ;; Pass the real-disk path as :dir; expect sandbox /work as pwd.
        inner (jvm/make)
        out (host/-string-sink inner)
        err (host/-string-sink inner)
        r (host/-spawn sbox {:cmd "pwd"
                             :dir (str tmp "/work")
                             :out out :err err})
        _ ((:wait r))]
    (try
      (is (= "/work\n" (host/-sink->string inner out))
          (str "stderr: " (host/-sink->string inner err)))
      (finally (bbfs/delete-tree tmp)))))

(deftest-bwrap pid-namespace-isolated
  ;; --unshare-pid → the sandboxed process sees itself as pid 1-ish,
  ;; not the JVM's pid. `pgrep`-style introspection is intentionally
  ;; limited from inside.
  (let [tmp (mk-tmpdir)
        sbox (sb/make {:wrapped (jvm/make) :bind-root tmp})
        r (run-in sbox ["sh" "-c" "ls /proc | grep -c '^[0-9]'"])]
    (try
      (is (zero? (:exit r)))
      ;; Should be a small number — at most a handful of PIDs in the
      ;; isolated namespace (the shell, ls, grep, init).
      (let [n (-> (:stdout r) clojure.string/trim Integer/parseInt)]
        (is (< n 20)
            (str "expected few PIDs in unshared namespace, saw " n)))
      (finally (bbfs/delete-tree tmp)))))
