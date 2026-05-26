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
   -read-all-string — fine for the bounded inputs builtins handle."
  [{:keys [dir extra-env in]} fallback-host fallback-fs]
  (let [stdin (when in
                (try (host/-read-all-string fallback-host in)
                     (catch Throwable _ nil)))]
    (cond-> {:cwd  (or dir (fs/cwd fallback-fs))
             :vars (or extra-env {})}
      stdin (assoc :stdin stdin))))

(defn- invoke-builtin!
  "Run a builtin fn synchronously, write its stdout/stderr to the
   sinks, return a wait-fn that yields the exit code.

   Binds muschel.builtins.posix/*host* / *session* / *depth* around
   the call so builtins that need to dispatch recursively (e.g. `sh`
   re-running its -c script, or `find -exec`) can re-enter through
   the same gates."
  [self fallback-host builtin-fn opts fs]
  (require 'muschel.builtins.posix)
  (let [argv (argv-of opts)
        env  (build-env opts fallback-host fs)
        host-var    (resolve 'muschel.builtins.posix/*host*)
        session-var (resolve 'muschel.builtins.posix/*session*)
        depth-var   (resolve 'muschel.builtins.posix/*depth*)
        {:keys [stdout stderr exit]}
        (try
          (push-thread-bindings
           {host-var    self
            session-var (:session opts)
            depth-var   (or @depth-var 0)})
          (try (builtin-fn argv fs env)
               (finally (pop-thread-bindings)))
          (catch Throwable t
            {:stdout ""
             :stderr (str (:cmd opts) ": " (.getMessage t) "\n")
             :exit 1}))]
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
  (-open-file-sink [_ p append?]
    (or (fs/-open-sink fs p append?)
        (throw (java.io.FileNotFoundException.
                (str p " (not in muschel FS root or not writable)")))))
  (-open-file-source [_ p]
    (or (fs/-open-source fs p)
        (throw (java.io.FileNotFoundException.
                (str p " (not in muschel FS root or missing)")))))
  (-file-info [_ p]
    (fs/-stat fs p))
  (-read-file [_ p]
    (fs/-read-file fs p))

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
