(ns muschel.host.sandboxed-integration-test
  "Integration tests for the SandboxedHost decorator — these EXEC
   bwrap and so are gated on bwrap actually being installed. Each
   deftest prints SKIP and returns early when bwrap is missing, so
   CI without bwrap stays green.

   Run locally:
     clojure -M:test --namespace muschel.host.sandboxed-integration-test

   Each test creates its own temp wrapper under /tmp, pre-creates
   `<wrapper>/home/agent` (DiskFS would do this automatically; we're
   bypassing DiskFS here to exercise the bwrap layer specifically),
   spawns directly against `JvmHost` wrapped in `SandboxedHost`, and
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

(defn- mk-sandbox
  "Create a temp wrapper + the home/agent subdir bwrap will bind.
   Returns the wrapper path. Caller is responsible for delete-tree."
  []
  (let [wrapper (str (bbfs/create-temp-dir {:prefix "muschel-sbx-it-"}))]
    (bbfs/create-dirs (io/file wrapper "home/agent"))
    wrapper))

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
  ;; A file at <wrapper>/home/agent/foo.txt is visible as
  ;; /home/agent/foo.txt inside the sandbox — proving the agent's
  ;; view (DiskFS) and bwrap's view of /home/agent line up.
  (let [wrapper (mk-sandbox)
        _ (spit (io/file wrapper "home/agent/foo.txt") "hello sandbox\n")
        sbox (sb/make {:wrapped (jvm/make) :bind-root wrapper})
        r (run-in sbox ["cat" "/home/agent/foo.txt"])]
    (try
      (is (= 0 (:exit r)))
      (is (= "hello sandbox\n" (:stdout r)))
      (finally (bbfs/delete-tree wrapper)))))

(deftest-bwrap default-cwd-is-home-agent
  ;; --chdir defaults to /home/agent (the mount), so commands without
  ;; an explicit :dir land there.
  (let [wrapper (mk-sandbox)
        sbox (sb/make {:wrapped (jvm/make) :bind-root wrapper})
        r (run-in sbox ["pwd"])]
    (try
      (is (= 0 (:exit r)))
      (is (= "/home/agent\n" (:stdout r)))
      (finally (bbfs/delete-tree wrapper)))))

(deftest-bwrap cd-up-to-root-is-real
  ;; `cd /` lands at the real bwrap tmpfs root with system mounts
  ;; visible — no aliasing.
  (let [wrapper (mk-sandbox)
        sbox (sb/make {:wrapped (jvm/make) :bind-root wrapper})
        r (run-in sbox ["sh" "-c" "cd /; ls / | sort | tr '\\n' ',' | sed 's/,$//'"])]
    (try
      (is (zero? (:exit r)))
      (let [entries (clojure.string/split (clojure.string/trim (:stdout r)) #",")]
        ;; bwrap exposes the FHS layout at /; /home/agent is the
        ;; agent's workspace (a sibling of /usr, /etc, …).
        (is (some #{"home"} entries))
        (is (some #{"usr"} entries))
        (is (some #{"etc"} entries))
        (is (some #{"proc"} entries))
        (is (some #{"tmp"} entries)))
      (finally (bbfs/delete-tree wrapper)))))

(deftest-bwrap network-off-blocks-egress
  (let [wrapper (mk-sandbox)
        sbox (sb/make {:wrapped (jvm/make) :bind-root wrapper})
        r (run-in sbox ["getent" "hosts" "1.1.1.1"])]
    (try
      (is (not (zero? (:exit r)))
          "getent should fail when network is unshared")
      (finally (bbfs/delete-tree wrapper)))))

(deftest-bwrap network-on-allows-egress
  (let [wrapper (mk-sandbox)
        sbox (sb/make {:wrapped (jvm/make) :bind-root wrapper :net :on})
        r (run-in sbox ["getent" "hosts" "127.0.0.1"])]
    (try
      (is (zero? (:exit r)) (str "stderr: " (:stderr r)))
      (finally (bbfs/delete-tree wrapper)))))

(deftest-bwrap ro-binds-are-read-only
  (let [wrapper (mk-sandbox)
        sbox (sb/make {:wrapped (jvm/make) :bind-root wrapper})
        r (run-in sbox ["sh" "-c" "echo evil > /etc/passwd 2>&1; echo exit=$?"])]
    (try
      (is (not (zero? (-> (:stdout r)
                          (clojure.string/split #"exit=")
                          last
                          clojure.string/trim
                          Integer/parseInt)))
          (str "writing to /etc should have failed; got: " (:stdout r)))
      (finally (bbfs/delete-tree wrapper)))))

(deftest-bwrap writes-to-workspace-persist
  ;; Writes to /home/agent inside the sandbox land on the host's
  ;; <wrapper>/home/agent.
  (let [wrapper (mk-sandbox)
        sbox (sb/make {:wrapped (jvm/make) :bind-root wrapper})
        _ (run-in sbox ["sh" "-c" "echo persisted > /home/agent/written.txt"])]
    (try
      (is (= "persisted\n" (slurp (io/file wrapper "home/agent/written.txt"))))
      (finally (bbfs/delete-tree wrapper)))))

(deftest-bwrap chdir-translates-real-disk-path-to-sandbox
  ;; muschel passes :dir = <wrapper>/home/agent/work (DiskFS-real
  ;; path); bwrap must --chdir to /home/agent/work.
  (let [wrapper (mk-sandbox)
        _ (bbfs/create-dirs (io/file wrapper "home/agent/work"))
        sbox (sb/make {:wrapped (jvm/make) :bind-root wrapper})
        inner (jvm/make)
        out (host/-string-sink inner)
        err (host/-string-sink inner)
        r (host/-spawn sbox {:cmd "pwd"
                             :dir (str wrapper "/home/agent/work")
                             :out out :err err})
        _ ((:wait r))]
    (try
      (is (= "/home/agent/work\n" (host/-sink->string inner out))
          (str "stderr: " (host/-sink->string inner err)))
      (finally (bbfs/delete-tree wrapper)))))

(deftest-bwrap pid-namespace-isolated
  (let [wrapper (mk-sandbox)
        sbox (sb/make {:wrapped (jvm/make) :bind-root wrapper})
        r (run-in sbox ["sh" "-c" "ls /proc | grep -c '^[0-9]'"])]
    (try
      (is (zero? (:exit r)))
      (let [n (-> (:stdout r) clojure.string/trim Integer/parseInt)]
        (is (< n 20)
            (str "expected few PIDs in unshared namespace, saw " n)))
      (finally (bbfs/delete-tree wrapper)))))
