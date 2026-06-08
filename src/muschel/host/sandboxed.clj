(ns muschel.host.sandboxed
  "OS-level sandbox decorator over an underlying `Host`.

   Rewrites `-spawn` to run the command inside `bubblewrap (bwrap)`
   so that allowlisted external tools (git, npm, python, …) execute
   under namespace + resource isolation, with the agent's workspace
   bind-mounted at the same path bwrap and DiskFS agree on. All
   other Host protocol methods delegate to the wrapped host
   unchanged.

   ## Why

   muschel's `--sandbox --root WRAPPER` mode constrains the agent's
   view of the filesystem via the `muschel.fs` protocol — paths
   outside `<WRAPPER>/home/agent` (the agent workspace) return nil
   at the FS layer and builtins see \"no such file\". That's enough
   for builtin-only workloads, but commands the user opts into via
   `:fallback-allowlist` bypass the FS protocol entirely: they
   `execvp()` against the host kernel and can reach any path the
   JVM process can.

   `SandboxedHost` closes that gap by wrapping each spawn in a bwrap
   invocation. The agent's workspace (`<WRAPPER>/<mount-at>` on
   disk, default `<WRAPPER>/home/agent`) is bind-mounted at the
   same `<mount-at>` (`/home/agent`) inside the sandbox. Paths
   align: muschel's view of `/home/agent/foo` is the same file as
   the spawned process's view of `/home/agent/foo`. Network, PID,
   and mount namespaces are unshared; resource limits go through
   `systemd-run --user --scope` for cgroup enforcement.

   ## Layout

   - **Host disk:** `<WRAPPER>/home/agent/…` holds the project.
   - **Inside the sandbox:** the bwrap tmpfs root `/` carries
     `/usr`, `/etc`, `/proc`, `/dev`, `/tmp` plus FHS symlinks
     (`/bin → usr/bin`, …), and `/home/agent` is the bind-mounted
     project. The agent starts at `--chdir /home/agent`.
   - `cd /` inside the sandbox is *real* `/` (the bwrap tmpfs with
     system mounts) — no aliasing, no synthesis. Builtins (which go
     through the FS protocol, not bwrap) still can't see anything
     above `/home/agent` — that asymmetry is intentional: builtins
     stay tightly jailed, externals get a usable Unix env.

   This decorator is **JVM-only** (Linux + bwrap installed) and
   **DiskFS-only** (VirtualFS has no real path to bind). The CLI
   wires it in only when `--os-sandbox=bwrap` is set, which
   requires `--sandbox --root`.

   ## Composition

       SandboxedHost   ; +bwrap argv, +cgroup limits
         └─ BuiltinHost ; +builtin dispatch, +FS containment
              └─ JvmHost ; raw spawn / files / async

   `bind-root` MUST be the wrapper directory DiskFS is pinned to
   AND `mount-at` MUST match DiskFS's, otherwise the agent's view
   and the sandboxed process's view would diverge. The CLI
   guarantees this by passing the same values to both.

   ## Defaults

   - `:mount-at`     `\"/home/agent\"`             — where the workspace lives
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
  (:require [clojure.string :as str]
            [muschel.host :as host]))

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

(defn- mount-bind-source
  "Real-disk path of a mount's wrapper-subdir under `bind-root`."
  [^String bind-root ^String wrapper-subdir]
  (let [trimmed (str/replace bind-root #"/+$" "")]
    (if (or (= "" wrapper-subdir) (nil? wrapper-subdir))
      trimmed
      (str trimmed "/" wrapper-subdir))))

(defn- mount-bind-args
  "Emit --bind args for every mount entry in `mounts` (vec of
   [sandbox-path wrapper-subdir])."
  [^String bind-root mounts]
  (mapcat (fn [[sandbox-path wrapper-subdir]]
            ["--bind" (mount-bind-source bind-root wrapper-subdir) sandbox-path])
          mounts))

(defn- mount-occupies-tmp?
  "True if any mount targets `/tmp` (exactly) — we drop the default
   tmpfs /tmp when a real bind covers it."
  [mounts]
  (some (fn [[sp _]] (= sp "/tmp")) mounts))

(defn- default-chdir
  "When no :dir is provided, chdir to the first mount's sandbox-path
   — the muschel convention is that the first mount IS the agent's
   workspace (`/home/agent` by default)."
  [mounts]
  (or (ffirst mounts) "/"))

(defn- chdir-arg
  "Map a spawn :dir into bwrap's `--chdir` value. With the sandbox-
   space FS protocol, `:dir` is already a sandbox path; we pass it
   through and default to the first mount when absent."
  [mounts dir]
  (if (and (string? dir) (not (str/blank? dir)))
    dir
    (default-chdir mounts)))

(defn- bwrap-argv
  "Construct the bwrap arg vector preceding `-- cmd args`."
  [{:keys [bind-root mounts ro-binds extra-binds net]} dir]
  (vec
   (concat
    ["bwrap"]
    (mount-bind-args bind-root mounts)
    ["--proc" "/proc"
     "--dev"  "/dev"]
    ;; If no mount covers /tmp, keep the ephemeral bwrap tmpfs so
    ;; the FHS still has something at /tmp. Otherwise the mount
    ;; provides it.
    (when-not (mount-occupies-tmp? mounts) ["--tmpfs" "/tmp"])
    (ro-bind-args (or ro-binds ["/usr" "/etc"]))
    fhs-symlinks
    (extra-bind-args extra-binds)
    (when (= :off net) ["--unshare-net"])
    ["--unshare-pid" "--die-with-parent"
     "--chdir" (chdir-arg mounts dir)]
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

(defrecord SandboxedHost [wrapped bind-root mounts ro-binds extra-binds net
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

(def ^:private default-mounts
  ;; Mirrors muschel.fs.disk/default-mounts; duplicated here to avoid
  ;; a cyclic require (sandboxed.clj is a pure host decorator, doesn't
  ;; depend on the FS impls). The CLI explicitly threads the FS's
  ;; mounts through, so this default only fires when SandboxedHost is
  ;; used standalone (tests, library callers).
  [["/home/agent" "home/agent"]
   ["/tmp"        "tmp"]])

(defn make
  "Wrap `wrapped-host` so every spawn runs inside bwrap.

   Required:
     :wrapped     the underlying Host (typically a BuiltinHost over
                  JvmHost)
     :bind-root   the wrapper directory DiskFS is pinned to. Each
                  mount entry's wrapper-subdir is resolved against
                  this root and bind-mounted at the mount's
                  sandbox-path inside bwrap.

   Optional:
     :mounts      [[sandbox-path wrapper-subdir] …]
                  Default `[[/home/agent home/agent] [/tmp tmp]]`.
                  MUST match the FS layer's mount table so the
                  agent's view and the spawned process's view align.
                  If any mount targets /tmp, the default ephemeral
                  bwrap tmpfs at /tmp is dropped.
     :ro-binds    [\"/usr\" \"/etc\"]  read-only host dirs to expose
     :extra-binds [[host sbox] …]      additional read-write binds
     :net         :off | :on           default :off (unshare network)
     :mem-max     \"512M\" / \"2G\"    cgroup MemoryMax
     :cpu-quota   \"200%\"             cgroup CPUQuota (200% = 2 cores)
     :tasks-max   512                  cgroup TasksMax

   Setting any of :mem-max / :cpu-quota / :tasks-max prepends
   `systemd-run --user --scope` to the spawn so the limits take
   effect in a transient cgroup."
  [{:keys [wrapped bind-root mounts ro-binds extra-binds net
           mem-max cpu-quota tasks-max]
    :or   {mounts      default-mounts
           ro-binds    ["/usr" "/etc"]
           extra-binds []
           net         :off}}]
  {:pre [(some? wrapped)
         (string? bind-root)
         (sequential? mounts)
         (seq mounts)]}
  (->SandboxedHost wrapped bind-root mounts ro-binds extra-binds net
                   mem-max cpu-quota tasks-max))
