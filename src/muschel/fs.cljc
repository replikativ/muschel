(ns muschel.fs
  "Filesystem abstraction.

   muschel.host's protocol exposes raw file ops with no containment
   guarantees. This namespace adds a separate FS protocol whose impls
   pin operations to a root (chroot-equivalent) without depending on
   OS-level isolation. Builtins (ls, cat, …) take an FS handle and
   refuse any path that the FS resolves to nil — paths outside the
   root become \"no such file\" without any special permit-layer
   logic in callers.

   Three impls land in this slice:

   - `muschel.fs.disk` — real filesystem, rooted at a directory. Paths
                         that resolve (post-canonicalisation, after
                         symlink follow) outside the root return nil.
   - `muschel.fs.virtual` — in-memory map of path → bytes. No root
                         escape possible (nothing outside the map).
   - `muschel.fs.passthrough` (future) — real disk, no root. For the
                         existing tests / legacy callers.

   Read-only operations only in v1; write ops come once we've validated
   the read surface against agent behaviour and built the per-fork
   yggdrasil-backed FS for substrate isolation."
  (:refer-clojure :exclude [resolve exists?])
  (:require [clojure.string :as str]))

(defprotocol FS
  "Containment-aware filesystem.

   `resolve` is the safety hinge: callers MUST run untrusted paths
   through it before any other op. A nil return means the path is
   outside the FS root (or invalid). All read/list/stat ops also
   internally resolve before touching anything, so a builtin that
   forgot to resolve still can't escape — defense in depth."

  (-resolve [this path]
    "Canonicalise `path` (relative to the FS's notion of cwd, with
     `..` collapsed and symlinks followed where the impl supports
     that). Return a string of the canonical absolute path if it
     falls inside the root, else nil.")

  (-cwd [this]
    "Current working directory (absolute path inside the root).
     For mutation, FS impls hold a reference to the env or a cwd
     atom; muschel's builtins call -cd! to update it.")

  (-cd! [this path]
    "Change cwd. Returns the new cwd on success, nil if `path`
     resolves outside the root.")

  (-exists? [this path]
    "True if `path` resolves and a file or directory exists there.")

  (-stat [this path]
    "{:type :file|:dir|:symlink|nil :size :mtime-ms :perms} for a
     resolvable path, or nil if outside / missing.")

  (-list-dir [this path]
    "Sorted seq of {:name :type :size :mtime-ms} entries in the
     directory at `path`, or nil if path is not a directory / is
     outside root. Does not recurse.")

  (-read-file [this path]
    "Slurp `path` as a string. Returns nil if path is outside root,
     missing, a directory, or unreadable. Impls may cap size to
     defend against `cat /dev/zero`-style attacks — callers receive
     truncated content with no flag; cap is impl-defined (default
     8 MiB).")

  (-read-bytes [this path]
    "Like -read-file but returns a byte array (or nil).")

  (-open-source [this path]
    "Open `path` for reading. Returns an InputStream (or platform
     equivalent) if `path` resolves inside the root and exists; nil
     otherwise. Used by the BuiltinHost to route shell-redirect
     `< file` reads through the FS.")

  (-open-sink [this path append?]
    "Open `path` for writing. Returns an OutputStream (or platform
     equivalent) if `path` resolves inside the root; nil if outside.
     `append?` controls truncate-vs-append semantics. Used by the
     BuiltinHost to route shell-redirect `> file` / `>> file` writes.

     For impls that don't otherwise persist (virtual FS), closing the
     returned stream commits the bytes into the FS's storage.")

  ;; ---- write-side protocol -----------------------------------------------

  (-mkdir [this path]
    "Create the directory at `path`. Returns truthy on success, nil
     if outside root, if the parent doesn't exist, or if a non-dir
     entry already lives there. Use for both plain `mkdir` and the
     leaf step of `mkdir -p`.")

  (-delete [this path]
    "Delete the file or empty directory at `path`. Returns truthy
     on success, nil if outside root, missing, or a non-empty dir.
     Callers wanting `rm -rf` walk the tree themselves and call
     -delete on each leaf before its parent.")

  (-rename [this from to]
    "Move/rename `from` to `to`. Both must resolve inside the root.
     Returns truthy on success.")

  (-touch [this path]
    "Create `path` as an empty file if missing; otherwise update its
     mtime. Returns truthy on success.")

  (-chmod [this path mode]
    "Set the mode (octal int, e.g. 0o755) on `path`. Returns truthy
     on success. Virtual impls may store the bits opaquely for
     stat-roundtripping without enforcing permission semantics.")

  (-symlink [this target link-path]
    "Create a symbolic link at `link-path` pointing at `target`.
     `link-path` must resolve inside the root; `target` is stored as
     given (may be relative or absolute — symlink-resolution at
     read-time still goes through the FS, which catches outside-root
     targets at follow time).")

  (-chown [this path owner group]
    "Set owner / group on `path`. On real-disk FS this requires the
     OS user has the capability (typically root for changing across
     users; same-user is a no-op). On virtual FS the values are
     stored opaquely and surface through `:owner` / `:group` on
     `-stat`. Either argument may be nil to leave unchanged. Returns
     truthy on success.")

  (-sandbox-relativize [this real-path-str]
    "Translate a backend-internal absolute path (e.g. a real disk path
     like `/tmp/muschel-xyz/foo`) into a sandbox-rooted display path
     (e.g. `/foo`). Used by builtins like `pwd` and `realpath` to keep
     the host's mount prefix out of the agent's view. For VFS this is
     identity — VFS paths are already sandbox-rooted."))

;; ============================================================================
;; Public wrappers
;; ============================================================================

(defn resolve   [fs path] (-resolve fs path))
(defn cwd       [fs]      (-cwd fs))
(defn cd!       [fs path] (-cd! fs path))
(defn exists?   [fs path] (-exists? fs path))
(defn stat      [fs path] (-stat fs path))
(defn list-dir  [fs path] (-list-dir fs path))
(defn read-file [fs path] (-read-file fs path))
(defn read-bytes [fs path] (-read-bytes fs path))
(defn open-source [fs path] (-open-source fs path))
(defn open-sink   [fs path append?] (-open-sink fs path append?))
(defn mkdir       [fs path] (-mkdir fs path))
(defn delete      [fs path] (-delete fs path))
(defn rename      [fs from to] (-rename fs from to))
(defn touch       [fs path] (-touch fs path))
(defn chmod       [fs path mode] (-chmod fs path mode))
(defn symlink     [fs target link-path] (-symlink fs target link-path))
(defn chown       [fs path owner group] (-chown fs path owner group))
(defn sandbox-relativize [fs real-path-str] (-sandbox-relativize fs real-path-str))

(defn write-string!
  "Portable write-string-to-path. Opens a sink via `-open-sink`,
   writes `content` (UTF-8), and closes. Returns truthy on success,
   nil if the path resolves outside root or the open fails.

   Sink shape varies by FS impl. We handle two canonical shapes
   uniformly on every host:

   - Map with an `:acc` atom — used by `muschel.fs.virtual` and
     `muschel.host.browser`. Both JVM and CLJS reach this branch;
     writes mutate the atom and the FS's add-watch mirrors them
     back into entries-atom.
   - Bare CLJS atom — fallback for older shape compatibility.

   On JVM `OutputStream`-backed sinks (real-disk DiskFS) fall through
   to a `with-open + .write` write."
  [fs path ^String content append?]
  (when-let [sink (-open-sink fs path append?)]
    (cond
      (and (map? sink)
           #?(:clj  (instance? clojure.lang.Atom (:acc sink))
              :cljs (instance? cljs.core/Atom    (:acc sink))))
      (do (if append?
            (swap!  (:acc sink) str content)
            (reset! (:acc sink) content))
          true)

      #?(:cljs (instance? cljs.core/Atom sink) :clj false)
      #?(:cljs (do (if append?
                     (swap! sink str content)
                     (reset! sink content))
                   true)
         :clj nil)

      :else
      #?(:clj
         (with-open [^java.io.OutputStream o sink]
           (.write o ^bytes (.getBytes content "UTF-8"))
           true)
         :cljs true))))

;; ============================================================================
;; Path utilities (impl-agnostic)
;; ============================================================================

(defn normalize-segments
  "Collapse a sequence of path segments, removing empty segments and
   resolving `.` / `..`. Returns the canonical vector of segments
   (no leading slash; caller adds it). `..` past the root returns
   nil to signal an escape attempt."
  [segments]
  (loop [in segments
         out []]
    (if-let [s (first in)]
      (cond
        (or (= s "") (= s "."))
        (recur (rest in) out)

        (= s "..")
        (if (empty? out)
          nil
          (recur (rest in) (pop out)))

        :else
        (recur (rest in) (conj out s)))
      out)))

(defn split-path
  "Break a path on `/` into raw segments. Leading slash → absolute;
   we discard it (callers pin to a root)."
  [path]
  (when (string? path)
    (str/split path #"/")))

(defn join-path
  "Build a path string from a root prefix + a vector of segments."
  [root segments]
  (let [r (str/replace root #"/+$" "")]
    (if (empty? segments)
      r
      (str r "/" (str/join "/" segments)))))
