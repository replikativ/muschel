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
   is `-spawn`. Hosts compose."
  (:require [clojure.string :as str]
            [muschel.host :as host]
            [muschel.fs :as fs]))

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
                     (catch Throwable _ nil)))]
    (cond-> {:cwd  (or dir (fs/cwd fallback-fs))
             :vars (or extra-env {})}
      stdin        (assoc :stdin stdin)
      interrupt-fn (assoc :interrupt-fn interrupt-fn)
      trace        (assoc :trace trace))))

(defn- invoke-builtin!
  "Run a builtin fn synchronously, write its stdout/stderr to the
   sinks, return a wait-fn that yields the exit code.

   Binds muschel.builtins.posix/*host* / *session* / *depth* around
   the call so builtins that need to dispatch recursively (e.g. `sh`
   re-running its -c script, or `find -exec`) can re-enter through
   the same gates."
  [self fallback-host builtin-fn opts fs]
  (require 'muschel.builtins.posix)
  (require 'muschel.trace)
  (require 'muschel.fs.traced)
  (let [argv (argv-of opts)
        ;; If the caller installed a trace state, wrap fs so every
        ;; protocol op the builtin makes is recorded. The wrap is per
        ;; call so it doesn't leak between invocations.
        wrap-fn (resolve 'muschel.fs.traced/wrap)
        fs (if-let [ts (:trace opts)] (wrap-fn fs ts) fs)
        env  (build-env opts fallback-host fs)
        host-var    (resolve 'muschel.builtins.posix/*host*)
        session-var (resolve 'muschel.builtins.posix/*session*)
        depth-var   (resolve 'muschel.builtins.posix/*depth*)
        record-tool! (resolve 'muschel.trace/record-tool!)
        started-at  (System/currentTimeMillis)
        {:keys [stdout stderr exit]}
        (try
          (push-thread-bindings
           {host-var    self
            session-var (:session opts)
            depth-var   (or @depth-var 0)})
          (try (builtin-fn argv fs env)
               (finally (pop-thread-bindings)))
          (catch Throwable t
            ;; Don't swallow resource-budget interrupts — those need
            ;; to propagate so the caller's run-and-capture can abort.
            (when (and (instance? clojure.lang.ExceptionInfo t)
                       (:muschel/budget (ex-data t)))
              (throw t))
            {:stdout ""
             :stderr (str (:cmd opts) ": " (.getMessage t) "\n")
             :exit 1}))]
    ;; Record into the trace state if one is installed on the env.
    (record-tool! (:trace opts)
                  {:type :tool
                   :name (:cmd opts)
                   :argv argv
                   :exit (or exit 0)
                   :stdout-bytes (count (or stdout ""))
                   :stderr-bytes (count (or stderr ""))
                   :duration-ms (- (System/currentTimeMillis) started-at)})
    (host/write-string! fallback-host (:out opts) (or stdout ""))
    (host/write-string! fallback-host (:err opts) (or stderr ""))
    {:wait   (fn [] (or exit 0))
     :handle ::builtin}))

(defn- refuse!
  "Write a denial message to :err and return exit 126."
  [fallback-host opts reason]
  (let [msg (format "muschel: %s: %s\n" (:cmd opts) reason)]
    (host/write-string! fallback-host (:err opts) msg))
  {:wait   (fn [] 126)
   :handle ::refused})

;; ============================================================================
;; Host wrapping
;; ============================================================================

(defrecord BuiltinHost [fallback-host builtins fs fallback-allowlist]
  host/Host
  ;; ---- buffers ----
  (-write-string!    [_ sink s] (host/-write-string! fallback-host sink s))
  (-read-all-string  [_ source] (host/-read-all-string fallback-host source))
  (-close!           [_ io]     (host/-close! fallback-host io))
  (-string-sink      [_]        (host/-string-sink fallback-host))
  (-sink->string     [_ sink]   (host/-sink->string fallback-host sink))
  (-string-source    [_ s]      (host/-string-source fallback-host s))

  ;; ---- files ---- routed through FS for containment.
  ;;
  ;; Redirect targets (`< file`, `> file`, `>> file`) and direct
  ;; file_info / read_file callers all go through the FS handle. The
  ;; FS resolves each path; if it lands outside the root, ops fail
  ;; with nil — same behaviour as opening a non-existent file. We do
  ;; NOT delegate to the fallback host: a leaked path would otherwise
  ;; reach raw java.io and read/write real disk.
  ;;
  ;; Special case: `/dev/null` is the universal write-and-discard /
  ;; read-zero-bytes sink. Agents reach for `2>/dev/null` in almost
  ;; every pipeline. Refusing it would force `2>&1 | grep -v error`
  ;; gymnastics. We model it as a stream that swallows writes /
  ;; produces no bytes, regardless of FS containment.
  (-open-file-sink [_ p append?]
    (cond
      (= "/dev/null" p)
      (proxy [java.io.OutputStream] []
        (write
          ([_b])
          ([_b _o _l]))
        (flush []))
      :else
      (or (fs/-open-sink fs p append?)
          (throw (java.io.FileNotFoundException.
                  (str p " (not in muschel FS root or not writable)"))))))
  (-open-file-source [_ p]
    (cond
      (= "/dev/null" p)
      (java.io.ByteArrayInputStream. (byte-array 0))
      :else
      (or (fs/-open-source fs p)
          (throw (java.io.FileNotFoundException.
                  (str p " (not in muschel FS root or missing)"))))))
  (-file-info [_ p]
    ;; Translate the muschel.fs stat shape into the predicate-style
    ;; map the rest of muschel (test/[, redirect open-checks, …)
    ;; consume. Outside-root or missing paths return `:exists? false`,
    ;; matching the jvm host's behaviour for a missing file.
    (cond
      (= "/dev/null" p)
      {:exists? true :file? true :dir? false :symlink? false
       :readable? true :writable? true :executable? false :size 0}
      :else
      (if-let [s (fs/-stat fs p)]
        {:exists?     true
         :file?       (= :file (:type s))
         :dir?        (= :dir  (:type s))
         :symlink?    (= :symlink (:type s))
         ;; muschel.fs doesn't model unix-style perm bits richly enough
         ;; to distinguish r/w/x. Treat all in-root files as readable +
         ;; writable; executable only if the FS reports it via mode bits.
         :readable?   true
         :writable?   true
         :executable? (when-let [m (:perms-mode s)]
                        (pos? (bit-and m 0111)))
         :size        (:size s)
         :mtime-ms    (:mtime-ms s)}
        {:exists? false})))
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
                 (format "not a builtin and not in fallback-allowlist (allowed: %s)"
                         (pr-str (sort (concat (keys builtins)
                                               fallback-allowlist))))))))

  ;; ---- async ----
  (-async [_ thunk] (host/-async fallback-host thunk))
  (-await [_ h]     (host/-await fallback-host h)))

(defn make
  "Construct a builtin-dispatching host.

   Required:
     :fs                   the muschel.fs/FS handle the builtins read
                           through (containment + cwd)
     :fallback-host        a host impl to delegate non-builtin /
                           non-spawn ops to (typically jvm host)

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
   (or builtins
       (do (require 'muschel.builtins.posix)
           @(resolve 'muschel.builtins.posix/standard-read-only)))
   fs
   fallback-allowlist))
