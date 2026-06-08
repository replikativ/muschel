(ns muschel.host.sandboxed-test
  "Unit tests for the SandboxedHost decorator — argv construction
   only. End-to-end bwrap containment tests live in the integration
   namespace and are gated on a real bwrap binary.

   Default mount-at is `/home/agent`, so the bind source is
   `<bind-root>/home/agent` and `--chdir` defaults to `/home/agent`.
   Tests that exercise only the chdir-translation surface pass an
   explicit `:mount-at \"/\"` for clarity."
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.host :as host]
            [muschel.host.sandboxed :as sb]))

;; ============================================================================
;; A stub host that records the last spawn-opts it received
;; ============================================================================

(defn- stub-host []
  (let [recorded (atom nil)]
    [recorded
     (reify host/Host
       (-write-string!    [_ _ _]   nil)
       (-read-all-string  [_ _]     "")
       (-close!           [_ _]     nil)
       (-string-sink      [_]       (StringBuilder.))
       (-sink->string     [_ s]     (str s))
       (-string-source    [_ s]     s)
       (-open-file-sink   [_ _ _]   nil)
       (-open-file-source [_ _]     nil)
       (-file-info        [_ _]     {:exists? false})
       (-read-file        [_ _]     "")
       (-make-pipe        [_]       [nil nil])
       (-spawn            [_ opts]
         (reset! recorded opts)
         {:wait (fn [] 0) :handle ::stub})
       (-async            [_ f]     (future (f)))
       (-await            [_ h]     (deref h)))]))

(defn- spawn! [host opts]
  (host/-spawn host opts))

(defn- recorded-cmd+args [recorded-atom]
  (let [opts @recorded-atom]
    (into [(:cmd opts)] (:args opts))))

(defn- value-after [argv flag]
  (nth argv (inc (.indexOf ^java.util.List (vec argv) flag))))

;; ============================================================================
;; Defaults: mount-at /home/agent, bind <root>/home/agent at /home/agent
;; ============================================================================

(deftest spawn-rewrites-to-bwrap
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "git" :args ["status"]})
    (let [argv (recorded-cmd+args recorded)]
      (is (= "bwrap" (first argv)))
      (is (some #{"--unshare-net"} argv))
      (is (some #{"--unshare-pid"} argv))
      (is (some #{"--die-with-parent"} argv))
      (is (= "git" (nth argv (inc (.indexOf ^java.util.List (vec argv) "--")))))
      (is (= "status" (last argv))))))

(deftest bind-defaults-to-home-agent
  ;; Default mount-at /home/agent: bwrap binds <bind-root>/home/agent → /home/agent.
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "ls"})
    (let [argv (vec (recorded-cmd+args recorded))]
      (is (= "/tmp/sbx/home/agent" (value-after argv "--bind")))
      ;; Sandbox path of the bind. It's the second arg after --bind.
      (is (= "/home/agent"
             (nth argv (+ 2 (.indexOf ^java.util.List argv "--bind"))))))))

(deftest default-chdir-is-mount-at
  ;; With default mount-at, no :dir → --chdir /home/agent.
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "ls"})
    (is (= "/home/agent" (value-after (vec (recorded-cmd+args recorded)) "--chdir")))))

(deftest chdir-passes-sandbox-dir-through
  ;; Under the sandbox-space FS protocol contract, :dir is already
  ;; sandbox-shaped (env :cwd is sandbox; the OS-spawn boundary
  ;; translation happens in JvmHost via fs/physical-path). The
  ;; decorator just plumbs it into bwrap's --chdir.
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "ls" :dir "/home/agent/work"})
    (is (= "/home/agent/work"
           (value-after (vec (recorded-cmd+args recorded)) "--chdir")))))

(deftest chdir-passes-sandbox-relative-through
  ;; If :dir is already sandbox-relative (after a `cd /home/agent/x`
  ;; in bash), pass it through.
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "ls" :dir "/home/agent/x"})
    (is (= "/home/agent/x"
           (value-after (vec (recorded-cmd+args recorded)) "--chdir")))))

(deftest dir-at-mount-root-is-mount-at
  ;; :dir = mount-at (the agent's HOME) → --chdir /home/agent.
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "ls" :dir "/home/agent"})
    (is (= "/home/agent" (value-after (vec (recorded-cmd+args recorded)) "--chdir")))))

(deftest dir-is-consumed-not-passed-through
  ;; bwrap handles cwd via --chdir; the wrapped host should see no :dir.
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "ls" :dir "/home/agent"})
    (is (nil? (:dir @recorded)))))

;; ============================================================================
;; Custom mounts
;; ============================================================================

(defn- bind-pairs
  "Extract every [src dst] pair following a --bind flag from argv."
  [argv]
  (->> (partition 3 1 argv)
       (keep (fn [[a src dst]]
               (when (= "--bind" a) [src dst])))))

(deftest each-mount-becomes-a-bind
  ;; Default mounts: /home/agent + /tmp. Both should appear as --bind
  ;; <bind-root>/<wrapper-subdir> <sandbox-path> pairs.
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "ls"})
    (let [pairs (set (bind-pairs (recorded-cmd+args recorded)))]
      (is (contains? pairs ["/tmp/sbx/home/agent" "/home/agent"]))
      (is (contains? pairs ["/tmp/sbx/tmp"        "/tmp"])))))

(deftest tmp-mount-replaces-default-tmpfs
  ;; When a mount targets /tmp, the default `--tmpfs /tmp` is dropped
  ;; (the bind covers it). With no /tmp mount, the tmpfs comes back.
  (let [[recorded-with inner] (stub-host)
        with-tmp (sb/make {:wrapped inner :bind-root "/tmp/sbx"})
        [recorded-without inner2] (stub-host)
        without-tmp (sb/make {:wrapped inner2 :bind-root "/tmp/sbx"
                              :mounts [["/home/agent" "home/agent"]]})]
    (spawn! with-tmp {:cmd "ls"})
    (spawn! without-tmp {:cmd "ls"})
    (let [with-argv (recorded-cmd+args recorded-with)
          without-argv (recorded-cmd+args recorded-without)]
      (is (not (some #{"--tmpfs"} with-argv))
          "/tmp mount replaces the ephemeral tmpfs")
      (is (some #{"--tmpfs"} without-argv)
          "without /tmp mount, fall back to ephemeral tmpfs"))))

(deftest custom-mounts
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"
                    :mounts [["/work" "work"]]})]
    (spawn! h {:cmd "ls"})
    (let [argv (vec (recorded-cmd+args recorded))
          pairs (set (bind-pairs argv))]
      (is (contains? pairs ["/tmp/sbx/work" "/work"]))
      (is (= "/work" (value-after argv "--chdir"))
          "first mount is the default chdir"))))

(deftest mount-at-root-flat-layout
  ;; Mount at / is the legacy single-mount flat layout. Bind source is
  ;; the wrapper itself; default chdir is /.
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"
                    :mounts [["/" ""]]})]
    (spawn! h {:cmd "ls" :dir "/sub"})
    (let [argv (vec (recorded-cmd+args recorded))
          pairs (set (bind-pairs argv))]
      (is (contains? pairs ["/tmp/sbx" "/"]))
      (is (= "/sub" (value-after argv "--chdir"))))))

;; ============================================================================
;; Networking + binds + cgroups
;; ============================================================================

(deftest net-on-skips-unshare
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx" :net :on})]
    (spawn! h {:cmd "curl" :args ["https://example.com"]})
    (is (not (some #{"--unshare-net"} (recorded-cmd+args recorded))))))

(deftest ro-binds-default-usr-and-etc
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "git" :args ["status"]})
    (let [argv (vec (recorded-cmd+args recorded))
          ro-pairs (->> argv
                        (partition 3 1)
                        (filter (fn [[a _ _]] (= "--ro-bind-try" a)))
                        (map (fn [[_ src _]] src)))]
      (is (some #{"/usr"} ro-pairs))
      (is (some #{"/etc"} ro-pairs)))))

(deftest extra-binds-flow-through
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"
                    :extra-binds [["/host/cache" "/cache"]]})]
    (spawn! h {:cmd "git" :args ["status"]})
    (let [argv (vec (recorded-cmd+args recorded))]
      (is (some (fn [[a src dst]]
                  (and (= "--bind-try" a)
                       (= "/host/cache" src)
                       (= "/cache" dst)))
                (partition 3 1 argv))))))

(deftest cgroup-prepends-systemd-run
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"
                    :mem-max "512M" :cpu-quota "100%" :tasks-max 64})]
    (spawn! h {:cmd "git" :args ["status"]})
    (let [argv (vec (recorded-cmd+args recorded))]
      (is (= "systemd-run" (first argv)))
      (is (some #{"--scope"} argv))
      (is (some #{"--user"}  argv))
      (is (some #{"MemoryMax=512M"} argv))
      (is (some #{"CPUQuota=100%"}  argv))
      (is (some #{"TasksMax=64"}    argv))
      (is (some #{"bwrap"} argv)))))

(deftest no-cgroup-no-systemd
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "git" :args ["status"]})
    (is (not (some #{"systemd-run"} (recorded-cmd+args recorded))))))

(deftest partial-cgroup-still-uses-systemd
  ;; Setting just :mem-max is enough to engage systemd-run.
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx" :mem-max "1G"})]
    (spawn! h {:cmd "ls"})
    (let [argv (vec (recorded-cmd+args recorded))]
      (is (= "systemd-run" (first argv)))
      (is (some #{"MemoryMax=1G"} argv))
      (is (not (some #{"CPUQuota"} argv))))))

;; ============================================================================
;; Non-spawn methods delegate untouched
;; ============================================================================

(deftest non-spawn-methods-delegate
  (let [[_ inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (is (= "" (host/-read-all-string h :anything)))
    (is (= {:exists? false} (host/-file-info h "/whatever")))))
