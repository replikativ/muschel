(ns muschel.host.builtin
  "Host that dispatches commands to a Clojure builtin registry, with
   a fallback chain for explicitly-allowed system tools.

   ## Why

   Running arbitrary system binaries means the permit layer has to
   chase every CVE in coreutils / bash / awk forever. Reimplementing
   the core in Clojure inverts the problem: vetting becomes reviewing
   our own code, and the same builtins run on JVM / babashka /
   browser (with a virtual FS).

   ## Dispatch

   For each `-spawn` call:

   1. If `cmd` is in `:builtins`, invoke the builtin fn. It reads
      from the FS handle (containment-aware) and writes to the
      provided sinks. Returns synchronously.

   2. Else if `cmd` is in `:fallback-allowlist`, delegate to
      `:fallback-host`. Use this for vetted system tools we want
      verbatim: `git`, `clojure`, `npm`, `cargo`, project-specific
      build scripts. The allowlist matches on the command NAME, not
      its argv — pair with permit rules if you need finer control
      (e.g. allow `git status` but deny `git push --force`).

   3. Otherwise refuse with exit 126 and a clear stderr message.
      No silent fallthrough to the host's exec.

   ## Non-spawn protocol methods

   All other Host operations (file I/O, pipes, async, buffers) are
   delegated to `:fallback-host`. The builtin host's only override
   is `-spawn`. Hosts compose.

   ## Cross-platform

   This namespace compiles to JVM, ClojureScript and (via the :clj
   branch) babashka. The few platform-specific bits — currentTimeMillis,
   the executable-bit octal mask, `/dev/null` stream impls — sit
   behind reader conditionals."
  (:require [muschel.builtins.awk-compat :as cc]
            [muschel.builtins.posix :as posix]
            [muschel.fs :as fs]
            [muschel.fs.traced :as fs.traced]
            [muschel.host :as host]
            [muschel.trace :as trace]))

;; ============================================================================
;; Platform helpers
;; ============================================================================

(defn- now-ms []
  #?(:clj  (System/currentTimeMillis)
     :cljs (.now js/Date)))

(defn- budget-ex? [t]
  (and (instance? #?(:clj clojure.lang.ExceptionInfo
                     :cljs cljs.core/ExceptionInfo) t)
       (:muschel/budget (ex-data t))))

;; `/dev/null` semantics, portably: we want any agent writing to
;; `2>/dev/null` to succeed and any read from `< /dev/null` to yield
;; empty. JVM previously used java.io stream impls; here we delegate
;; to the fallback host's neutral buffers — a write-only string sink
;; (writes are accepted but never read) and an empty string source.
(defn- dev-null-sink   [fallback-host] (host/-string-sink   fallback-host))
(defn- dev-null-source [fallback-host] (host/-string-source fallback-host ""))

(defn- not-in-root-ex [path verb]
  (ex-info (str path " (not in muschel FS root or " verb ")")
           {:type ::not-in-root :path path}))

;; ============================================================================
;; Builtin invocation
;; ============================================================================

(defn- argv-of [opts]
  (into [(:cmd opts)] (or (:args opts) [])))

(defn- build-env
  "Minimal env value for builtin fns. Carries :cwd from :dir, any
   :extra-env vars under :vars, and the resolved :stdin string for
   builtins that read from it (grep/cat with no file args, tr, cut,
   xargs, …). We resolve :in eagerly via the fallback host's
   -read-all-string — fine for the bounded inputs builtins handle.

   Also forwards `:interrupt-fn` (resource budget) and `:trace` (the
   introspection state) so builtins / awk can record their own
   events when needed."
  [{:keys [dir extra-env in interrupt-fn trace]} fallback-host fallback-fs]
  (let [stdin (when in
                (try (host/-read-all-string fallback-host in)
                     (catch #?(:clj Throwable :cljs :default) _ nil)))]
    (cond-> {:cwd  (or dir (fs/cwd fallback-fs))
             :vars (or extra-env {})}
      stdin        (assoc :stdin stdin)
      interrupt-fn (assoc :interrupt-fn interrupt-fn)
      trace        (assoc :trace trace))))

(defn- invoke-builtin!
  "Run a builtin fn synchronously, write its stdout/stderr to the
   sinks, return a wait-fn that yields the exit code.

   Binds `muschel.builtins.posix/*host*` / `*session*` / `*depth*`
   around the call so builtins that need to dispatch recursively
   (e.g. `sh` re-running its -c script, `find -exec`) can re-enter
   through the same gates."
  [self fallback-host builtin-fn opts fs]
  (let [argv (argv-of opts)
        ;; If the caller installed a trace state, wrap fs so every
        ;; protocol op the builtin makes is recorded. Wrap is per-call
        ;; so it doesn't leak between invocations.
        fs   (if-let [ts (:trace opts)] (fs.traced/wrap fs ts) fs)
        env  (build-env opts fallback-host fs)
        started-at (now-ms)
        result
        (try
          (binding [posix/*host*    self
                    posix/*session* (:session opts)
                    posix/*depth*   (or posix/*depth* 0)]
            (builtin-fn argv fs env))
          (catch #?(:clj Throwable :cljs :default) t
            ;; Don't swallow resource-budget interrupts — those need
            ;; to propagate so the caller's run-and-capture can abort.
            (when (budget-ex? t) (throw t))
            {:stdout ""
             :stderr (str (:cmd opts) ": "
                          (or (ex-message t) (str t)) "\n")
             :exit 1}))
        {:keys [stdout stderr exit]} result]
    (trace/record-tool!
     (:trace opts)
     {:type :tool
      :name (:cmd opts)
      :argv argv
      :exit (or exit 0)
      :stdout-bytes (count (or stdout ""))
      :stderr-bytes (count (or stderr ""))
      :duration-ms (- (now-ms) started-at)})
    (host/write-string! fallback-host (:out opts) (or stdout ""))
    (host/write-string! fallback-host (:err opts) (or stderr ""))
    {:wait   (fn [] (or exit 0))
     :handle ::builtin}))

(defn- refuse!
  "Write a denial message to :err and return exit 126."
  [fallback-host opts reason]
  (let [msg (cc/fmt-many "muschel: %s: %s\n" [(:cmd opts) reason])]
    (host/write-string! fallback-host (:err opts) msg))
  {:wait   (fn [] 126)
   :handle ::refused})

;; ============================================================================
;; File-info translation
;; ============================================================================

(defn- stat->info
  "Translate the muschel.fs `:stat` shape into the predicate-style
   map the rest of muschel (test/[, redirect open-checks, …) consumes.
   Outside-root or missing paths return `:exists? false`, matching the
   jvm host's behaviour for a missing file."
  [s]
  (if s
    {:exists?     true
     :file?       (= :file (:type s))
     :dir?        (= :dir  (:type s))
     :symlink?    (= :symlink (:type s))
     ;; muschel.fs doesn't model unix-style perm bits richly enough
     ;; to distinguish r/w/x. Treat all in-root files as readable +
     ;; writable; executable only if the FS reports it via mode bits.
     :readable?   true
     :writable?   true
     ;; 0o111 = 73 (any-exec mask). Use a decimal literal so CLJS
     ;; doesn't choke on the JVM-style octal `0111`.
     :executable? (when-let [m (:perms-mode s)]
                    (pos? (bit-and m 73)))
     :size        (:size s)
     :mtime-ms    (:mtime-ms s)}
    {:exists? false}))

;; ============================================================================
;; Host wrapping
;; ============================================================================

(defrecord BuiltinHost [fallback-host builtins fs fallback-allowlist]
  host/Host
  ;; ---- buffers ----
  (-write-string!    [_ sink s] (host/-write-string!    fallback-host sink s))
  (-read-all-string  [_ source] (host/-read-all-string  fallback-host source))
  (-close!           [_ io]     (host/-close!           fallback-host io))
  (-string-sink      [_]        (host/-string-sink      fallback-host))
  (-sink->string     [_ sink]   (host/-sink->string     fallback-host sink))
  (-string-source    [_ s]      (host/-string-source    fallback-host s))

  ;; ---- files ---- routed through FS for containment.
  ;;
  ;; Redirect targets (`< file`, `> file`, `>> file`) and direct
  ;; file_info / read_file callers all go through the FS handle. The
  ;; FS resolves each path; if it lands outside the root, ops fail
  ;; with nil — same behaviour as opening a non-existent file. We do
  ;; NOT delegate to the fallback host: a leaked path would otherwise
  ;; reach raw java.io / fs and read/write real disk.
  ;;
  ;; Special case: `/dev/null` is the universal write-and-discard /
  ;; read-zero-bytes sink. Agents reach for `2>/dev/null` in almost
  ;; every pipeline. Refusing it would force `2>&1 | grep -v error`
  ;; gymnastics. We model it as a stream that swallows writes /
  ;; produces no bytes, regardless of FS containment.
  (-open-file-sink [_ p _append?]
    (cond
      (= "/dev/null" p) (dev-null-sink fallback-host)
      :else
      (or (fs/-open-sink fs p _append?)
          (throw (not-in-root-ex p "not writable")))))
  (-open-file-source [_ p]
    (cond
      (= "/dev/null" p) (dev-null-source fallback-host)
      :else
      (or (fs/-open-source fs p)
          (throw (not-in-root-ex p "missing")))))
  (-file-info [_ p]
    (cond
      (= "/dev/null" p)
      {:exists? true :file? true :dir? false :symlink? false
       :readable? true :writable? true :executable? false :size 0}
      :else
      (stat->info (fs/-stat fs p))))
  (-read-file [_ p]
    (cond
      (= "/dev/null" p) ""
      :else             (fs/-read-file fs p)))

  ;; ---- pipes ----
  (-make-pipe        [_]        (host/-make-pipe fallback-host))

  ;; ---- spawn — the override ----
  (-spawn [this opts]
    (let [cmd (:cmd opts)]
      (cond
        ;; Built-in: invoke in-process against the FS.
        (contains? builtins cmd)
        (invoke-builtin! this fallback-host (get builtins cmd) opts fs)

        ;; Explicitly allowlisted system tool: delegate.
        (contains? fallback-allowlist cmd)
        (host/-spawn fallback-host opts)

        ;; Refuse anything else.
        :else
        (refuse! fallback-host opts
                 (cc/fmt-many
                  "not a builtin and not in fallback-allowlist (allowed: %s)"
                  [(pr-str (sort (concat (keys builtins)
                                         fallback-allowlist)))])))))

  ;; ---- async ----
  (-async [_ thunk] (host/-async fallback-host thunk))
  (-await [_ h]     (host/-await fallback-host h)))

(defn make
  "Construct a builtin-dispatching host.

   Required:
     :fs                   the muschel.fs/FS handle the builtins read
                           through (containment + cwd)
     :fallback-host        a host impl to delegate non-builtin /
                           non-spawn ops to (typically jvm/node host)

   Optional:
     :builtins             map of cmd-name → builtin-fn (default:
                           muschel.builtins.posix/standard-read-only)
     :fallback-allowlist   set of cmd-names the fallback host may run
                           (default: #{})"
  [{:keys [fs fallback-host builtins fallback-allowlist]
    :or {fallback-allowlist #{}}}]
  {:pre [(some? fs) (some? fallback-host)]}
  (->BuiltinHost
   fallback-host
   (or builtins posix/standard-read-only)
   fs
   fallback-allowlist))
