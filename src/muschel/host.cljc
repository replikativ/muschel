(ns muschel.host
  "Platform abstraction for `muschel.exec`'s side-effecting operations.

   The shell semantics (env mutations, control flow, builtins that
   only touch env) live in `muschel.exec` as portable .cljc. The bits
   that genuinely require platform support — spawning a child
   process, opening files, capturing stream output — go through the
   `Host` protocol so JVM / Node / Browser can each plug in.

   ## Concrete impls

   - `muschel.host.jvm`     — uses `babashka.process` + `java.io.*`.
                              Full support: streams, real files,
                              concurrent pipelines via futures.
   - `muschel.host.node`    — uses Node's `child_process` + `fs`.
                              Full support but pipelines are
                              sequential (output collected before
                              next stage starts).
   - `muschel.host.browser` — string-buffer streams, virtual tool
                              registry, virtual in-memory fs.
                              No real subprocesses. Good enough for
                              a playground.

   ## Buffer abstraction

   Most JVM exec passes `java.io.OutputStream` around. To stay
   portable we introduce a generic notion of a *buffer*:

     - A `:host/sink` accepts writes (`write-bytes!`, `write-string!`).
     - A `:host/source` produces reads (`read-all-string`).
     - `string-sink` makes a sink that accumulates into a string
       fetchable via `sink->string`.
     - `string-source` makes a source from a string literal.

   For the JVM host, sinks/sources are `OutputStream`/`InputStream`
   wrappers. For cljs they're plain volatiles holding strings.

   ## Spawn

   `spawn` accepts:

       {:cmd      \"program-name\"
        :args     [\"arg1\" ...]
        :dir      \"/abs/path\"
        :extra-env {\"FOO\" \"bar\"}
        :in       <source or string>
        :out      <sink>
        :err      <sink>}

   and returns a map:

       {:wait (fn [] exit-int)   ; blocks until completion
        :handle <opaque>}        ; for kill/inspection

   For sync platforms (cljs node sync, browser virtual), `wait`
   returns the cached exit code; the actual execution already
   completed."
  (:require [clojure.string :as str]))

(defprotocol Host
  ;; --- buffers ---
  (-write-string! [this sink s] "Write `s` to `sink`. nil sink = drop.")
  (-read-all-string [this source] "Read everything from `source` as one string.")
  (-close! [this io] "Close a sink or source. May be a no-op.")
  (-string-sink [this] "Make a new sink that accumulates string output.")
  (-sink->string [this sink] "Extract the accumulated string from a `string-sink`.")
  (-string-source [this s] "Wrap `s` as a source.")

  ;; --- files ---
  (-open-file-sink [this path append?])
  (-open-file-source [this path])
  (-file-info [this path]
    "Returns map: {:exists? :file? :dir? :readable? :writable?
                   :executable? :symlink? :size :mtime-ms}")
  (-read-file [this path] "Slurp file at path. Throws if not found.")

  ;; --- pipes (in-process) ---
  (-make-pipe [this] "Returns [source sink] connected; writes to sink
                       become readable on source.")

  ;; --- spawn ---
  (-spawn [this opts]
    "Spawn a subprocess. Returns {:wait fn :handle x}. See ns docs.")

  ;; --- async (used for pipelines and bg) ---
  (-async [this thunk]
    "Run thunk in a separate context. Returns a handle.")
  (-await [this handle]
    "Block until thunk completed, return its value."))

;; ============================================================================
;; Public helpers — built on the protocol
;; ============================================================================

(defn write-string!
  "Write `s` to `sink`. Buffers/streams created by either host work."
  [host sink s]
  (when sink (-write-string! host sink s)))

(defn close! [host io] (-close! host io))

(defn read-all-string [host source]
  (-read-all-string host source))

(defn string-sink [host] (-string-sink host))
(defn sink->string [host sink] (-sink->string host sink))
(defn string-source [host s] (-string-source host s))

(defn open-file-sink [host path append?] (-open-file-sink host path append?))
(defn open-file-source [host path] (-open-file-source host path))

(defn file-info [host path] (-file-info host path))
(defn file-exists?    [host path] (boolean (:exists? (file-info host path))))
(defn file-regular?   [host path] (boolean (:file? (file-info host path))))
(defn file-directory? [host path] (boolean (:dir?  (file-info host path))))
(defn file-readable?  [host path] (boolean (:readable? (file-info host path))))
(defn file-writable?  [host path] (boolean (:writable? (file-info host path))))
(defn file-executable? [host path] (boolean (:executable? (file-info host path))))
(defn file-symlink?   [host path] (boolean (:symlink? (file-info host path))))
(defn file-size       [host path] (:size (file-info host path)))
(defn file-mtime-ms   [host path] (:mtime-ms (file-info host path)))

(defn read-file [host path] (-read-file host path))

(defn make-pipe [host] (-make-pipe host))

(defn spawn [host opts] (-spawn host opts))

(defn async [host thunk] (-async host thunk))
(defn await-async [host h] (-await host h))

;; ============================================================================
;; Path resolution (portable — works on any host)
;; ============================================================================

(defn- normalize-segments
  "Walk POSIX path segments collapsing `.` and `..`. Returns nil if
   `..` underflows the absolute root."
  [segments]
  (loop [in segments out []]
    (if-let [s (first in)]
      (cond
        (or (= "" s) (= "." s)) (recur (rest in) out)
        (= ".." s)
        (if (seq out)
          (recur (rest in) (vec (butlast out)))
          nil)        ; .. underflow
        :else (recur (rest in) (conj out s)))
      out)))

(defn resolve-path
  "Join `path` to `cwd` if relative and collapse `.`/`..`. Pure-string
   operation; doesn't touch the filesystem. Returns nil when the path
   escapes (`..` past root); callers should treat nil as a forbidden
   path. Both host impls share this."
  [^String cwd ^String path]
  (let [joined (cond
                 (or (nil? path) (= "" path)) cwd
                 (str/starts-with? path "/") path
                 :else (str (str/replace cwd #"/$" "") "/" path))]
    (when-let [segs (normalize-segments (str/split joined #"/"))]
      (str "/" (str/join "/" segs)))))
