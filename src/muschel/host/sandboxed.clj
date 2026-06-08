(ns muschel.host.sandboxed
  "OS-level sandbox decorator over an underlying `Host`.

   Rewrites `-spawn` to run the command inside `bubblewrap (bwrap)`
   with the muschel FS root bind-mounted at `/` in the sandbox. All
   other Host protocol methods delegate to the wrapped host
   unchanged.

   ## Why

   muschel's existing `--sandbox --root DIR` mode constrains the
   *agent's view of the filesystem* via the `muschel.fs` protocol —
   paths outside DIR return nil at the FS layer and builtins see
   \"no such file\". That's enough for builtin-only workloads, but
   commands the user opts into via `:fallback-allowlist` (`git`,
   `npm`, `python`, …) bypass the FS protocol entirely: they
   execvp() against the host kernel and can reach any path the JVM
   process can.

   `SandboxedHost` closes that gap by wrapping each spawn in a bwrap
   invocation. The DiskFS real-root directory is bind-mounted at `/`
   inside the sandbox, so paths align: muschel's view of `/work` is
   the same file as the spawned process's view of `/work`. Network,
   PID, and mount namespaces are unshared; resource limits go
   through `systemd-run --user --scope` for cgroup enforcement.

   This decorator is **JVM-only** (Linux + bwrap installed) and
   **DiskFS-only** (VirtualFS has no real path to bind). The CLI
   wires it in only when `--os-sandbox=bwrap` is set, which requires
   `--sandbox --root`.

   ## Composition

   The expected stack from outer to inner:

       SandboxedHost   ; +bwrap argv, +cgroup limits
         └─ BuiltinHost ; +builtin dispatch, +FS containment
              └─ JvmHost ; raw spawn / files / async

   `bind-root` must be the same real-disk path that DiskFS is
   pinned to, otherwise the agent's view (`/work`) and the
   sandboxed process's view (`/work`) would point at different
   files. The CLI guarantees this by passing the same `--root`
   value to both.

   ## Defaults

   - `:ro-binds`     `[\"/usr\" \"/etc\"]`         — host tools + system config
   - `:net`          `:off`                        — `--unshare-net`
   - `:mem-max`      `nil` (no cgroup memory cap)
   - `:cpu-quota`    `nil`
   - `:tasks-max`    `nil`
   - `:extra-binds`  `[]`                          — `[[host-path sandbox-path] …]`

   When any of `:mem-max` / `:cpu-quota` / `:tasks-max` is set, the
   spawn is also wrapped in `systemd-run --user --scope` so the
   limits land in a transient cgroup. Without those, the bwrap call
   runs directly under the JVM's session cgroup."
  (:require [muschel.host :as host]))

;; ============================================================================
;; bwrap argv construction
;; ============================================================================

(defn- ro-bind-args [ro-binds]
  (mapcat (fn [p] ["--ro-bind-try" p p]) ro-binds))

(defn- extra-bind-args [extra-binds]
  (mapcat (fn [[host-p sbox-p]] ["--bind-try" host-p sbox-p]) extra-binds))

(def ^:private fhs-symlinks
  ;; On merged-/usr distros (Arch, Fedora, modern Debian/Ubuntu) the
  ;; classic top-level dirs `/bin /sbin /lib /lib64` are symlinks
  ;; into `/usr`. We re-create those symlinks inside the sandbox so
  ;; common binaries resolve. --symlink TARGET LINK_PATH — creates
  ;; LINK_PATH pointing at TARGET.
  ["--symlink" "usr/bin"   "/bin"
   "--symlink" "usr/sbin"  "/sbin"
   "--symlink" "usr/lib"   "/lib"
   "--symlink" "usr/lib64" "/lib64"])

(defn- translate-cwd
  "Map a spawn :dir into a sandbox-relative `--chdir` argument.

   muschel's env :cwd is inconsistent — sometimes a real-disk path
   (DiskFS syncs to its canonical root), sometimes a sandbox-relative
   path (after `cd /work` the env stores the literal). We handle both:

   - dir = bind-root             → `/`
   - dir starts with bind-root/  → strip prefix, the rest IS the sandbox path
   - dir starts with `/` but not bind-root → assume already sandbox-relative
   - anything else / nil         → `/` (sandbox root)"
  [^String bind-root ^String dir]
  (cond
    (or (nil? dir) (= "" dir)) "/"
    (= dir bind-root) "/"
    (.startsWith dir (str bind-root "/")) (subs dir (count bind-root))
    (.startsWith dir "/") dir
    :else "/"))

(defn- bwrap-argv
  "Construct the bwrap arg vector preceding `-- cmd args`."
  [{:keys [bind-root ro-binds extra-binds net]} dir]
  (vec
   (concat
    ["bwrap"
     "--bind" bind-root "/"
     "--proc" "/proc"
     "--dev"  "/dev"
     "--tmpfs" "/tmp"]
    (ro-bind-args (or ro-binds ["/usr" "/etc"]))
    fhs-symlinks
    (extra-bind-args extra-binds)
    (when (= :off net) ["--unshare-net"])
    ["--unshare-pid" "--die-with-parent"
     "--chdir" (translate-cwd bind-root dir)]
    ["--"])))

(defn- cgroup-enabled? [{:keys [mem-max cpu-quota tasks-max]}]
  (boolean (or mem-max cpu-quota tasks-max)))

(defn- systemd-argv
  "Prefix for cgroup limits — `systemd-run --user --scope --quiet`
   plus any -p VAR=VAL properties. Returns nil if no limits set."
  [{:keys [mem-max cpu-quota tasks-max] :as self}]
  (when (cgroup-enabled? self)
    (vec
     (concat
      ["systemd-run" "--user" "--scope" "--quiet"]
      (when mem-max    ["-p" (str "MemoryMax=" mem-max)])
      (when cpu-quota  ["-p" (str "CPUQuota=" cpu-quota)])
      (when tasks-max  ["-p" (str "TasksMax=" tasks-max)])
      ["--"]))))

(defn- wrap-spawn-opts
  "Given `opts` from `host/-spawn`, return new spawn-opts whose
   `:cmd` + `:args` execute the original via bwrap (+ optional
   systemd-run). `:dir` is consumed (becomes bwrap's `--chdir`) so
   the underlying host runs in the caller's cwd."
  [self {:keys [cmd args dir] :as opts}]
  (let [bw  (bwrap-argv self dir)
        sd  (systemd-argv self)
        ;; bwrap argv = [bwrap ...flags -- ]; append the user command.
        wrapped (concat bw [cmd] args)
        [final-cmd final-args]
        (if sd
          [(first sd) (vec (concat (rest sd) wrapped))]
          [(first wrapped) (vec (rest wrapped))])]
    (-> opts
        (assoc :cmd final-cmd :args final-args)
        (dissoc :dir))))

;; ============================================================================
;; Host protocol — delegate everything except -spawn
;; ============================================================================

(defrecord SandboxedHost [wrapped bind-root ro-binds extra-binds net
                          mem-max cpu-quota tasks-max])

(extend-type SandboxedHost
  host/Host
  ;; buffers
  (-write-string!     [this sink s]   (host/-write-string!    (:wrapped this) sink s))
  (-read-all-string   [this source]   (host/-read-all-string  (:wrapped this) source))
  (-close!            [this io]       (host/-close!           (:wrapped this) io))
  (-string-sink       [this]          (host/-string-sink      (:wrapped this)))
  (-sink->string      [this sink]     (host/-sink->string     (:wrapped this) sink))
  (-string-source     [this s]        (host/-string-source    (:wrapped this) s))
  ;; files
  (-open-file-sink    [this p ap?]    (host/-open-file-sink   (:wrapped this) p ap?))
  (-open-file-source  [this p]        (host/-open-file-source (:wrapped this) p))
  (-file-info         [this p]        (host/-file-info        (:wrapped this) p))
  (-read-file         [this p]        (host/-read-file        (:wrapped this) p))
  ;; pipes
  (-make-pipe         [this]          (host/-make-pipe        (:wrapped this)))
  ;; spawn — the override
  (-spawn [this opts]
    (host/-spawn (:wrapped this) (wrap-spawn-opts this opts)))
  ;; async
  (-async             [this thunk]    (host/-async            (:wrapped this) thunk))
  (-await             [this h]        (host/-await            (:wrapped this) h)))

(defn make
  "Wrap `wrapped-host` so every spawn runs inside bwrap.

   Required:
     :wrapped     the underlying Host (typically a BuiltinHost over
                  JvmHost)
     :bind-root   real-disk path that DiskFS is pinned to; bound at
                  `/` inside the sandbox

   Optional:
     :ro-binds     [\"/usr\" \"/etc\"]  read-only host dirs to expose
     :extra-binds  [[host sbox] …]      additional read-write binds
     :net          :off | :on           default :off (unshare network)
     :mem-max      \"512M\" / \"2G\"    cgroup MemoryMax
     :cpu-quota    \"200%\"             cgroup CPUQuota (200% = 2 cores)
     :tasks-max    512                  cgroup TasksMax

   Setting any of :mem-max / :cpu-quota / :tasks-max prepends
   `systemd-run --user --scope` to the spawn so the limits take
   effect in a transient cgroup."
  [{:keys [wrapped bind-root ro-binds extra-binds net
           mem-max cpu-quota tasks-max]
    :or   {ro-binds    ["/usr" "/etc"]
           extra-binds []
           net         :off}}]
  {:pre [(some? wrapped)
         (string? bind-root)]}
  (->SandboxedHost wrapped bind-root ro-binds extra-binds net
                   mem-max cpu-quota tasks-max))
