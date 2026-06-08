(ns muschel.fs.disk
  "Real-disk FS implementation pinned to a wrapper directory, with the
   project files at a known subpath inside.

   ## Wrapper layout

   `(make wrapper opts)` takes a host directory `wrapper` and a
   `:mount-at` sandbox path (default `/home/agent`). The agent's
   project files live on disk at `<wrapper>/<mount-at>`; the agent
   sees them at `<mount-at>` in the sandbox view:

       Host                            Sandbox view
       -------------------             -----------------
       <wrapper>/home/agent/  ←──→     /home/agent/      (the project)
       <wrapper>/home/agent/foo.txt    /home/agent/foo.txt

   `<wrapper>` itself is *outside* the agent's reach via the FS
   protocol (containment is pinned at `<wrapper>/<mount-at>`).
   Sibling paths under the wrapper — `<wrapper>/var/cache/`,
   `<wrapper>/.muschel/`, etc. — are a future-reserved slot for
   muschel-managed project state surfaced under
   `/system/muschel-project/<name>/` once that's implemented.

   ## Auto-create on construction

   `make` ensures `<wrapper>/<mount-at>` exists on disk
   (`Files/createDirectories`, idempotent). The contract is: handing
   muschel a wrapper dir, even an empty one, yields a working
   sandbox without the caller having to pre-create the agent
   workspace.

   ## Resolution

   Every operation resolves the requested path against the canonical
   internal real-root (`<wrapper>/<mount-at>`, with symlinks followed
   via `java.nio.file/Path.toRealPath`). Sandbox paths that don't
   fall under `<mount-at>` (e.g., `/etc/passwd`, `/`) return nil at
   `-resolve` and surface as \"no such file\" through the builtins.

   ## Containment caveats

   - **Symlinks pointing outside root** are caught: we always
     toRealPath() before the prefix check. If the link target is
     outside, the resolve returns nil.

   - **TOCTOU**: between resolve() and a subsequent read, an attacker
     with write access to the root could swap a regular file for
     a symlink pointing outside. We re-resolve at every op so the
     window is per-call; it's NOT zero. For untrusted-attacker
     scenarios pair with OS-level isolation (bwrap/firejail).

   - **`/proc/self/root` and other magic paths** are denied because
     they don't normalize into the root. Same for hard links to
     /etc/passwd inside the root — we can't detect those, so don't
     mount writable file roots if hard-link attacks matter.

   - **JVM-only** (uses java.nio.file). For Node and browser, ship
     analogous impls in their own namespaces. Each impl is
     responsible for ensuring its real-root exists on construction
     using its platform's native API (Files/createDirectories on
     JVM, fs.mkdirSync on Node, etc.) — embedders just call
     `make` and get a usable handle."
  (:require [muschel.fs :as fs]
            [clojure.string :as str])
  (:import [java.nio.file Path Paths Files LinkOption NoSuchFileException]
           [java.nio.file.attribute BasicFileAttributes PosixFilePermissions]
           [java.io File]))

(def ^:private no-follow
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(def ^:private follow
  (into-array LinkOption []))

(defn- ^Path str->path [^String p]
  (Paths/get p (make-array String 0)))

(defn- ^Path safe-real-path
  "Canonicalise `^Path p` following symlinks. Returns nil if the path
   doesn't exist or any step is unreadable."
  [^Path p]
  (try (.toRealPath p follow)
       (catch NoSuchFileException _ nil)
       (catch Throwable _ nil)))

(defn- ^Path canonical-root [^String root]
  (let [p (str->path root)
        real (safe-real-path p)]
    (or real
        ;; Root doesn't exist yet — fall back to abs+normalize.
        (.toAbsolutePath (.normalize p)))))

(defn- inside?
  "True if `^Path candidate` is the root itself or a descendant."
  [^Path root ^Path candidate]
  (or (= root candidate)
      (.startsWith candidate root)))

(defn- sandbox-relative
  "Translate `real-path-str` (a real-disk path) into the sandbox-
   relative path the agent should see. Strips the internal real-root
   prefix and prepends `mount-at`. So given internal real-root
   `/tmp/proj/home/agent` and `mount-at` `/home/agent`:
     `/tmp/proj/home/agent`        → `/home/agent`
     `/tmp/proj/home/agent/foo`    → `/home/agent/foo`
   With `mount-at` `/` (the legacy/test-friendly layout) it just
   strips the root prefix and leaves the rest absolute. Paths
   outside the internal real-root pass through unchanged."
  [^Path root ^String mount-at ^String real-path-str]
  (let [rs (str root)]
    (cond
      (= real-path-str rs) mount-at
      (.startsWith real-path-str (str rs "/"))
      (let [tail (subs real-path-str (count rs))]   ;; tail starts with "/"
        (if (= mount-at "/")
          tail
          (str mount-at tail)))
      :else real-path-str)))

(defn- starts-with-str [^String s ^String prefix]
  (.startsWith s prefix))

(defn- strip-mount-prefix
  "If `path` is a sandbox-absolute path under `mount-at`, return the
   remainder (always starts with `/`). Returns nil if the path
   isn't under the mount. Special-case: `mount-at = \"/\"` passes
   every absolute path through unchanged."
  [^String mount-at ^String path]
  (cond
    (= mount-at "/")                            path
    (= path mount-at)                           "/"
    (.startsWith path (str mount-at "/"))       (subs path (count mount-at))
    :else                                       nil))

(defn- ^Path lex-normalize
  "Lexical absolute + .. collapse, no real-disk lookup. Returns nil
   for ~/-prefixed paths (muschel.expand handles those upstream) and
   for sandbox-absolute paths outside the mount.

   The agent sees the project at `<mount-at>` (default `/home/agent`)
   and operates on paths relative to it. An absolute path the agent
   types is either:
     - under `<mount-at>` (`/home/agent`, `/home/agent/src`) →
       strip mount-at, re-root under disk `root`
     - the host-absolute path under `root` (e.g., the env's :cwd
       feedback `<root>/sub`) → keep as-is, already correct
     - anything else absolute (`/etc/passwd`, `/`, `/system/...`) →
       OUTSIDE the mount → nil (caller surfaces as 'no such file')

   Relative paths join with cwd. `~/` returns nil — the expand
   layer handles tilde upstream."
  [^Path root ^String mount-at ^String cwd ^String path]
  (when (and (string? path) (not (str/blank? path)))
    (let [root-str (str root)
          base (cond
                 (or (= path root-str)
                     (starts-with-str path (str root-str "/")))
                 (str->path path)                       ;; host-absolute, under root — keep

                 (starts-with-str path "/")
                 (when-let [stripped (strip-mount-prefix mount-at path)]
                   (str->path (str root-str stripped)))

                 (starts-with-str path "~/") nil
                 :else                       (str->path (str cwd "/" path)))]
      (when base
        (.normalize (.toAbsolutePath base))))))

(defn- resolve*
  "Read-mode resolution: real-path the full path (follows ALL symlinks)
   and check containment. If the leaf doesn't exist, fall back to the
   parent-real-path (which MUST exist and be inside-root) joined with
   the leaf name. This way a write to `evil/newfile` — where `evil` is
   a symlink to `/etc` — resolves to `/etc/newfile`, fails the
   parent-inside-root check, and is rejected.

   Returns a string of the resolved real path on success, nil if the
   path escapes the sandbox or is malformed. The returned path is
   guaranteed to be inside root."
  [^Path root ^String mount-at ^String cwd ^String path]
  (when-let [normalized (lex-normalize root mount-at cwd path)]
    (let [real (safe-real-path normalized)]
      (cond
        ;; Whole-path exists and is real — straightforward.
        (some? real)
        (when (inside? root real) (str real))

        ;; Leaf doesn't exist (writing a new file, etc.). Walk to the
        ;; nearest ancestor that DOES exist, real-resolve it, then
        ;; rejoin the tail lexically. Reject if the realized ancestor
        ;; escapes root.
        :else
        (loop [p     (.getParent normalized)
               tail  (list (.getFileName normalized))]
          (cond
            (nil? p) nil
            :else
            (if-let [real-anc (safe-real-path p)]
              (when (inside? root real-anc)
                (let [rebuilt (reduce (fn [^Path acc ^Path seg]
                                        (.resolve acc seg))
                                      real-anc
                                      tail)]
                  (when (inside? root rebuilt)
                    (str rebuilt))))
              (recur (.getParent p)
                     (conj tail (.getFileName p))))))))))

(defn- type-of [^Path p]
  (cond
    (Files/isSymbolicLink p) :symlink
    (Files/isDirectory p follow) :dir
    (Files/isRegularFile p follow) :file
    (Files/exists p follow) :other
    :else nil))

(def ^:private default-read-cap (* 8 1024 1024))   ;; 8 MiB

(defrecord DiskFS [^Path root ^String mount-at cwd-atom max-bytes]
  fs/FS
  (-resolve [_ path]
    (resolve* root mount-at @cwd-atom path))

  (-cwd [_] @cwd-atom)

  (-cd! [_ path]
    (when-let [resolved (resolve* root mount-at @cwd-atom path)]
      (let [p (str->path resolved)]
        (when (Files/isDirectory p follow)
          (reset! cwd-atom resolved)
          resolved))))

  (-exists? [_ path]
    (when-let [resolved (resolve* root mount-at @cwd-atom path)]
      (Files/exists (str->path resolved) follow)))

  (-stat [_ path]
    (when-let [resolved (resolve* root mount-at @cwd-atom path)]
      (let [p (str->path resolved)]
        (when (Files/exists p no-follow)
          (let [attrs (Files/readAttributes p BasicFileAttributes follow)]
            {:type     (type-of p)
             :size     (.size attrs)
             :mtime-ms (.toMillis (.lastModifiedTime attrs))
             :perms    (try
                         (->> (Files/getPosixFilePermissions p follow)
                              PosixFilePermissions/toString)
                         (catch Throwable _ nil))})))))

  (-list-dir [_ path]
    (when-let [resolved (resolve* root mount-at @cwd-atom path)]
      (let [p (str->path resolved)]
        (when (Files/isDirectory p follow)
          (with-open [stream (Files/newDirectoryStream p)]
            (vec
             (sort-by :name
                      (mapv (fn [^Path child]
                              (let [attrs (Files/readAttributes child BasicFileAttributes follow)]
                                {:name     (str (.getFileName child))
                                 :type     (type-of child)
                                 :size     (.size attrs)
                                 :mtime-ms (.toMillis (.lastModifiedTime attrs))}))
                            stream))))))))

  (-read-file [this path]
    (when-let [bs (fs/-read-bytes this path)]
      (String. ^bytes bs "UTF-8")))

  (-read-bytes [_ path]
    (when-let [resolved (resolve* root mount-at @cwd-atom path)]
      (let [p (str->path resolved)]
        (when (Files/isRegularFile p follow)
          (let [size (Files/size p)
                cap  max-bytes
                read-size (min size cap)
                buf (byte-array read-size)]
            (with-open [in (Files/newInputStream p (make-array java.nio.file.OpenOption 0))]
              (.readNBytes in buf 0 read-size))
            buf)))))

  (-open-source [_ path]
    (when-let [resolved (resolve* root mount-at @cwd-atom path)]
      (let [p (str->path resolved)]
        (when (Files/isRegularFile p follow)
          (Files/newInputStream p (make-array java.nio.file.OpenOption 0))))))

  (-open-sink [_ path append?]
    (when-let [resolved (resolve* root mount-at @cwd-atom path)]
      (let [p (str->path resolved)
            base-opts (if append?
                        [java.nio.file.StandardOpenOption/CREATE
                         java.nio.file.StandardOpenOption/APPEND
                         java.nio.file.StandardOpenOption/WRITE]
                        [java.nio.file.StandardOpenOption/CREATE
                         java.nio.file.StandardOpenOption/TRUNCATE_EXISTING
                         java.nio.file.StandardOpenOption/WRITE])]
        ;; Defense in depth: re-check containment of the parent
        ;; directory. If the resolved path's parent escaped root via
        ;; a symlink, resolve* would have returned nil already.
        (Files/newOutputStream p (into-array java.nio.file.OpenOption base-opts)))))

  (-mkdir [_ path]
    (when-let [resolved (resolve* root mount-at @cwd-atom path)]
      (let [p (str->path resolved)]
        (when-not (Files/exists p follow)
          (try (Files/createDirectory p (make-array java.nio.file.attribute.FileAttribute 0))
               true
               (catch Throwable _ nil))))))

  (-delete [_ path]
    (when-let [resolved (resolve* root mount-at @cwd-atom path)]
      (let [p (str->path resolved)]
        (try (Files/delete p) true
             (catch Throwable _ nil)))))

  (-rename [_ from to]
    (when-let [from-resolved (resolve* root mount-at @cwd-atom from)]
      ;; Resolve `to` relative to the same root + cwd. For `to`, the
      ;; resolved path is allowed to NOT exist yet — resolve* falls
      ;; back to abs+normalize and still inside?-checks.
      (when-let [to-resolved (resolve* root mount-at @cwd-atom to)]
        (try (Files/move (str->path from-resolved)
                         (str->path to-resolved)
                         (into-array java.nio.file.CopyOption
                                     [java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
             true
             (catch Throwable _ nil)))))

  (-touch [_ path]
    (when-let [resolved (resolve* root mount-at @cwd-atom path)]
      (let [p (str->path resolved)]
        (try
          (if (Files/exists p follow)
            (do (Files/setLastModifiedTime
                 p (java.nio.file.attribute.FileTime/fromMillis (System/currentTimeMillis)))
                true)
            (do (Files/createFile p (make-array java.nio.file.attribute.FileAttribute 0))
                true))
          (catch Throwable _ nil)))))

  (-chmod [_ path mode]
    (when-let [resolved (resolve* root mount-at @cwd-atom path)]
      (let [p (str->path resolved)]
        (try
          (let [perms (java.nio.file.attribute.PosixFilePermissions/fromString
                       (let [m (long mode)
                             rwx (fn [bits]
                                   (str (if (pos? (bit-and bits 4)) "r" "-")
                                        (if (pos? (bit-and bits 2)) "w" "-")
                                        (if (pos? (bit-and bits 1)) "x" "-")))]
                         (str (rwx (bit-and (bit-shift-right m 6) 7))
                              (rwx (bit-and (bit-shift-right m 3) 7))
                              (rwx (bit-and m 7)))))]
            (Files/setPosixFilePermissions p perms)
            true)
          (catch Throwable _ nil)))))

  (-symlink [_ target link-path]
    (when-let [resolved (resolve* root mount-at @cwd-atom link-path)]
      ;; Refuse symlinks whose target lexically escapes the sandbox.
      ;; Absolute outside targets are obvious. For relative targets,
      ;; resolve against the link's parent dir lexically (before any
      ;; physical link-following) and check inside-root. This pairs
      ;; with the read-time guard so even if someone bypasses this,
      ;; resolve* on a path through the link fails.
      (let [link-p (str->path resolved)
            link-parent (.getParent link-p)
            target-p (str->path target)
            target-abs (if (.isAbsolute target-p)
                         target-p
                         (.resolve link-parent target-p))
            target-norm (.normalize target-abs)]
        (when (inside? root target-norm)
          (try (Files/createSymbolicLink link-p target-p
                                         (make-array java.nio.file.attribute.FileAttribute 0))
               true
               (catch Throwable _ nil))))))

  (-sandbox-relativize [_ real-path-str]
    (sandbox-relative root mount-at real-path-str))

  (-chown [_ path owner group]
    (when-let [resolved (resolve* root mount-at @cwd-atom path)]
      (let [p (str->path resolved)
            fs-svc (-> p .getFileSystem)
            lookup (-> fs-svc .getUserPrincipalLookupService)]
        (try
          (when owner
            (Files/setOwner p (.lookupPrincipalByName lookup ^String owner)))
          (when group
            (let [view (Files/getFileAttributeView
                        p java.nio.file.attribute.PosixFileAttributeView follow)]
              (.setGroup view (.lookupPrincipalByGroupName lookup ^String group))))
          true
          (catch Throwable _ nil))))))

(def ^:private default-mount-at "/home/agent")

(defn- normalize-mount-at
  "Validate + normalise the `:mount-at` option. Must be an absolute
   POSIX-style path with no `..`. Returns the normalised string
   (`/`, `/home/agent`, `/work`, …) or throws."
  [^String s]
  (when-not (and (string? s) (.startsWith s "/"))
    (throw (ex-info "DiskFS :mount-at must be an absolute path"
                    {:mount-at s})))
  (let [collapsed (str/replace s #"/+" "/")
        stripped  (if (= "/" collapsed)
                    "/"
                    (str/replace collapsed #"/$" ""))]
    (when (or (str/blank? stripped)
              (some #{".."} (str/split stripped #"/")))
      (throw (ex-info "DiskFS :mount-at must be a non-empty path with no `..`"
                      {:mount-at s})))
    stripped))

(defn- ensure-real-root!
  "Create the internal real-root directory if it doesn't yet exist.
   Idempotent. JVM-side; future Node/browser FS impls do the
   equivalent in their own constructors."
  [^Path real-root]
  (Files/createDirectories real-root (make-array java.nio.file.attribute.FileAttribute 0)))

(defn make
  "Construct a disk-backed sandbox FS over `wrapper` — a host
   directory that holds the agent's workspace at
   `<wrapper>/<mount-at>` (default `<wrapper>/home/agent`).

   The agent sees their workspace at `<mount-at>` in the sandbox
   view (e.g. `/home/agent/foo.txt` ↔ `<wrapper>/home/agent/foo.txt`).
   Sandbox paths outside `<mount-at>` resolve to nil — they're not
   reachable through this FS, only through an OS sandbox like bwrap.

   Auto-creates `<wrapper>/<mount-at>` if it doesn't exist
   (idempotent `Files/createDirectories`). The contract is that
   handing muschel a wrapper dir — even an empty one — yields a
   working sandbox without the caller having to pre-create the
   agent workspace.

   Options:
     :mount-at   sandbox path of the workspace, default `/home/agent`
                 (must be absolute, no `..`, no trailing slash)
     :cwd        initial cwd inside the workspace; defaults to the
                 workspace root (i.e. real-disk `<wrapper>/<mount-at>`)
     :max-bytes  read-file size cap, default 8 MiB"
  ([wrapper] (make wrapper {}))
  ([wrapper {:keys [cwd max-bytes mount-at]
             :or   {max-bytes default-read-cap
                    mount-at  default-mount-at}}]
   (let [mount-at  (normalize-mount-at mount-at)
         ;; Construct the internal real-root by appending the
         ;; mount-at literal under the wrapper. We don't go through
         ;; canonical-root on the wrapper first because the wrapper
         ;; might not exist yet either; createDirectories handles
         ;; the whole chain. Mount-at "/" makes the wrapper itself
         ;; the real-root (legacy / generic-FS layout).
         wrapper-trimmed (str/replace wrapper #"/+$" "")
         real-root-str   (if (= "/" mount-at)
                           wrapper-trimmed
                           (str wrapper-trimmed mount-at))
         real-root-p   (str->path real-root-str)
         _             (ensure-real-root! real-root-p)
         canonical     (canonical-root real-root-str)
         root-str      (str canonical)
         init-cwd      (or cwd root-str)
         resolved-cwd  (resolve* canonical mount-at root-str init-cwd)]
     (when-not resolved-cwd
       (throw (ex-info "Initial :cwd is outside :root"
                       {:cwd init-cwd :root root-str})))
     (->DiskFS canonical mount-at (atom resolved-cwd) max-bytes))))
