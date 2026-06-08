(ns muschel.host.sandboxed-test
  "Unit tests for the SandboxedHost decorator — argv construction
   only. End-to-end bwrap containment tests live separately and are
   gated on a real bwrap binary; see task #8 / the doc for how to run
   them locally."
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.host :as host]
            [muschel.host.sandboxed :as sb]))

;; ============================================================================
;; A stub host that records the last spawn-opts it received
;; ============================================================================

(defn- stub-host []
  (let [last (atom nil)]
    [last
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
         (reset! last opts)
         {:wait (fn [] 0) :handle ::stub})
       (-async            [_ f]     (future (f)))
       (-await            [_ h]     (deref h)))]))

(defn- spawn! [host opts]
  (host/-spawn host opts))

(defn- recorded-cmd+args [last-atom]
  (let [opts @last-atom]
    (into [(:cmd opts)] (:args opts))))

;; ============================================================================
;; Defaults
;; ============================================================================

(deftest spawn-rewrites-to-bwrap
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "git" :args ["status"] :dir "/work"})
    (let [argv (recorded-cmd+args recorded)]
      (is (= "bwrap" (first argv)))
      (is (= "--" (nth argv (dec (count (take-while #(not= "git" %) argv))))))
      (is (some #{"--bind"} argv))
      (is (= "/tmp/sbx" (nth argv (inc (.indexOf ^java.util.List (vec argv) "--bind")))))
      (is (some #{"--unshare-net"} argv))
      (is (some #{"--unshare-pid"} argv))
      (is (some #{"--die-with-parent"} argv))
      (is (= "git" (nth argv (inc (.indexOf ^java.util.List (vec argv) "--")))))
      (is (= "status" (last argv))))))

(deftest spawn-passes-chdir-from-dir
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "ls" :dir "/work"})
    (let [argv (vec (recorded-cmd+args recorded))]
      (is (= "/work" (nth argv (inc (.indexOf ^java.util.List argv "--chdir"))))))))

(deftest spawn-no-chdir-when-dir-absent
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "ls" :args []})
    (let [argv (vec (recorded-cmd+args recorded))]
      (is (not (some #{"--chdir"} argv))))))

(deftest dir-is-consumed-not-passed-through
  ;; bwrap handles cwd via --chdir; the wrapped host should see no :dir.
  (let [[recorded inner] (stub-host)
        h (sb/make {:wrapped inner :bind-root "/tmp/sbx"})]
    (spawn! h {:cmd "ls" :dir "/work"})
    (is (nil? (:dir @recorded)))))

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

;; ============================================================================
;; Cgroup limits via systemd-run
;; ============================================================================

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
      ;; bwrap shows up after the systemd-run -- terminator.
      (is (some #{"bwrap"} argv))
      (is (= "git" (last (take-while #(not= "status" %) argv)))))))

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
    ;; Just exercise a couple so the protocol surface stays wired.
    (is (= "" (host/-read-all-string h :anything)))
    (is (= {:exists? false} (host/-file-info h "/whatever")))))
