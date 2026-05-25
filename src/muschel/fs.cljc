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
    "Like -read-file but returns a byte array (or nil)."))

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
