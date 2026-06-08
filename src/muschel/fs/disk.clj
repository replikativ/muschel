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

   ## Sandbox-space contract

   Every value crossing the `muschel.fs/FS` protocol is a
   **sandbox-relative path** — `/home/agent`, `/home/agent/src`, `/`,
   `/home`. Real-disk paths are an implementation detail private to
   this namespace. `-resolve`, `-cwd`, `-cd!` all produce / consume
   sandbox paths. `-physical-path` is the one explicit translator
   (sandbox → real disk) and is only called at the
   `run-external` → OS-spawn boundary by `JvmHost`.

   ## Synthetic ancestor view

   The strict prefixes of `mount-at` (`/`, `/home` for the default
   `/home/agent`) are virtual read-only directories. `cd /` works,
   `ls /` returns `[\"home\"]`, `stat /home` reports a directory.
   Writes anywhere above the mount return nil. This keeps the
   builtin shell environment bash-credible without aliasing or
   syscall-tower trickery — bwrap-spawned externals see the real
   FHS at `/`, builtins see just the path leading down to the
   workspace. Different namespaces, neither lies about being the
   other.

   ## Auto-create on construction

   `make` ensures `<wrapper>/<mount-at>` exists on disk
   (`Files/createDirectories`, idempotent). The contract is: handing
   muschel a wrapper dir, even an empty one, yields a working
   sandbox without the caller having to pre-create the agent
   workspace.

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
           [java.nio.file.attribute BasicFileAttributes PosixFilePermissions]))

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
        (.toAbsolutePath (.normalize p)))))

(defn- inside?
  "True if `^Path candidate` is the root itself or a descendant."
  [^Path root ^Path candidate]
  (or (= root candidate)
      (.startsWith candidate root)))

;; ============================================================================
;; Mount-at and sandbox-path helpers
;; ============================================================================

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

(defn- absolutize-sandbox
  "Pure-string absolute + normalize for sandbox-space paths. `cwd`
   must be sandbox-absolute. Relative paths join with cwd; `.` and
   `..` segments collapse; `..` past `/` returns nil. `~/` returns
   nil (caller handles tilde). Always returns a string starting with
   `/`, or nil for malformed input."
  [^String cwd ^String path]
  (when (and (string? path) (not (str/blank? path)))
    (cond
      (str/starts-with? path "~/") nil
      :else
      (let [joined (cond
                     (str/starts-with? path "/") path
                     :else (str (str/replace cwd #"/$" "") "/" path))
            segs   (str/split joined #"/")
            normalized (loop [in segs out []]
                         (if-let [s (first in)]
                           (cond
                             (or (= "" s) (= "." s)) (recur (rest in) out)
                             (= ".." s)
                             (if (seq out) (recur (rest in) (vec (butlast out))) nil)
                             :else (recur (rest in) (conj out s)))
                           out))]
        (when normalized
          (if (empty? normalized)
            "/"
            (str "/" (str/join "/" normalized))))))))

(defn- under-mount?
  "True if sandbox `path` is `mount-at` itself or below."
  [^String mount-at ^String path]
  (or (= path mount-at)
      (str/starts-with? path (str mount-at "/"))
      (= mount-at "/")))

(defn- ancestor-of-mount?
  "True if sandbox `path` is a strict prefix of `mount-at` — e.g.,
   `/` or `/home` when mount-at is `/home/agent`. Always false when
   mount-at is `/` (nothing above `/`)."
  [^String mount-at ^String path]
  (and (not= mount-at "/")
       (not= path mount-at)
       (or (= path "/")
           (str/starts-with? (str mount-at "/") (str path "/")))))

(defn- next-segment-toward-mount
  "For an ancestor `path` of `mount-at`, return the segment leading
   one level toward the mount. E.g. `/` → `\"home\"`, `/home` →
   `\"agent\"` when mount-at is `/home/agent`."
  [^String mount-at ^String path]
  (let [start-idx (if (= path "/") 1 (inc (count path)))
        tail (subs mount-at start-idx)]
    (first (str/split tail #"/"))))

(defn- ^String sandbox->physical-str
  "Translate a sandbox path that's under-mount into its real-disk
   path under `root`. Returns nil for ancestor or out-of-mount
   paths."
  [^Path root ^String mount-at ^String sandbox-path]
  (when (under-mount? mount-at sandbox-path)
    (let [root-str (str root)
          tail (cond
                 (= mount-at "/") sandbox-path
                 (= sandbox-path mount-at) ""
                 :else (subs sandbox-path (count mount-at)))]
      (if (= "" tail) root-str (str root-str tail)))))

(defn- ^String physical->sandbox
  "Translate a canonical real-disk path under `root` back to its
   sandbox representation. Returns nil for paths outside root."
  [^Path root ^String mount-at ^String real-path-str]
  (let [rs (str root)]
    (cond
      (= real-path-str rs) mount-at
      (.startsWith real-path-str (str rs "/"))
      (let [tail (subs real-path-str (count rs))]
        (if (= mount-at "/") tail (str mount-at tail)))
      :else nil)))

;; ============================================================================
;; Resolution
;; ============================================================================

(defn- canonicalize-in-mount
  "For a sandbox path known to be under-mount, translate to a real-
   disk path, canonicalise via toRealPath (follow symlinks; check
   containment), then translate back to sandbox. If the leaf doesn't
   exist, walk up to the nearest existing ancestor on disk and re-
   join the missing tail lexically — preserves the existing write-
   target behaviour (open-sink can target a path whose parent exists)
   but rejects any symlink-mediated escape.

   Returns the canonical sandbox path on success, nil on escape."
  [^Path root ^String mount-at ^String sandbox-path]
  (let [physical-str (sandbox->physical-str root mount-at sandbox-path)]
    (when physical-str
      (let [normalized (.normalize (.toAbsolutePath (str->path physical-str)))
            real (safe-real-path normalized)]
        (cond
          (some? real)
          (when (inside? root real)
            (physical->sandbox root mount-at (str real)))

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
                      (physical->sandbox root mount-at (str rebuilt)))))
                (recur (.getParent p)
                       (conj tail (.getFileName p)))))))))))

(defn- resolve-sandbox
  "Sandbox-space resolution. Returns the canonical sandbox path of
   `path` (resolved against `cwd`), or nil if the path is malformed
   or escapes the sandbox. Ancestor paths above the mount return the
   sandbox path itself; in-mount paths are real-disk canonicalised
   then translated back to sandbox."
  [^Path root ^String mount-at ^String cwd ^String path]
  (when-let [sandbox-abs (absolutize-sandbox cwd path)]
    (cond
      (ancestor-of-mount? mount-at sandbox-abs)
      sandbox-abs

      (under-mount? mount-at sandbox-abs)
      (canonicalize-in-mount root mount-at sandbox-abs)

      :else nil)))

(defn- type-of [^Path p]
  (cond
    (Files/isSymbolicLink p) :symlink
    (Files/isDirectory p follow) :dir
    (Files/isRegularFile p follow) :file
    (Files/exists p follow) :other
    :else nil))

(def ^:private default-read-cap (* 8 1024 1024))

;; ============================================================================
;; Record
;; ============================================================================

(defrecord DiskFS [^Path root ^String mount-at cwd-atom max-bytes]
  fs/FS
  (-resolve [_ path]
    (resolve-sandbox root mount-at @cwd-atom path))

  (-cwd [_] @cwd-atom)

  (-cd! [_ path]
    (when-let [sandbox (resolve-sandbox root mount-at @cwd-atom path)]
      (cond
        ;; Ancestor: virtual dir, cd succeeds without real-disk check.
        (ancestor-of-mount? mount-at sandbox)
        (do (reset! cwd-atom sandbox) sandbox)

        :else
        (let [physical (sandbox->physical-str root mount-at sandbox)
              p (str->path physical)]
          (when (Files/isDirectory p follow)
            (reset! cwd-atom sandbox)
            sandbox)))))

  (-exists? [_ path]
    (when-let [sandbox (resolve-sandbox root mount-at @cwd-atom path)]
      (cond
        (ancestor-of-mount? mount-at sandbox) true
        :else (Files/exists (str->path (sandbox->physical-str root mount-at sandbox)) follow))))

  (-stat [_ path]
    (when-let [sandbox (resolve-sandbox root mount-at @cwd-atom path)]
      (cond
        (ancestor-of-mount? mount-at sandbox)
        {:type :dir :size 0 :mtime-ms 0 :perms nil}

        :else
        (let [p (str->path (sandbox->physical-str root mount-at sandbox))]
          (when (Files/exists p no-follow)
            (let [attrs (Files/readAttributes p BasicFileAttributes follow)]
              {:type     (type-of p)
               :size     (.size attrs)
               :mtime-ms (.toMillis (.lastModifiedTime attrs))
               :perms    (try
                           (->> (Files/getPosixFilePermissions p follow)
                                PosixFilePermissions/toString)
                           (catch Throwable _ nil))}))))))

  (-list-dir [_ path]
    (when-let [sandbox (resolve-sandbox root mount-at @cwd-atom path)]
      (cond
        (ancestor-of-mount? mount-at sandbox)
        ;; Virtual ancestor — return just the segment leading toward
        ;; the mount. Reading deeper into ancestors gives no real
        ;; entries.
        (when-let [next-seg (next-segment-toward-mount mount-at sandbox)]
          [{:name next-seg :type :dir :size 0 :mtime-ms 0}])

        :else
        (let [p (str->path (sandbox->physical-str root mount-at sandbox))]
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
                              stream))))))))) ; sort-by + mapv

  (-read-file [this path]
    (when-let [bs (fs/-read-bytes this path)]
      (String. ^bytes bs "UTF-8")))

  (-read-bytes [_ path]
    (when-let [sandbox (resolve-sandbox root mount-at @cwd-atom path)]
      (when-not (ancestor-of-mount? mount-at sandbox)
        (let [p (str->path (sandbox->physical-str root mount-at sandbox))]
          (when (Files/isRegularFile p follow)
            (let [size (Files/size p)
                  cap  max-bytes
                  read-size (min size cap)
                  buf (byte-array read-size)]
              (with-open [in (Files/newInputStream p (make-array java.nio.file.OpenOption 0))]
                (.readNBytes in buf 0 read-size))
              buf))))))

  (-open-source [_ path]
    (when-let [sandbox (resolve-sandbox root mount-at @cwd-atom path)]
      (when-not (ancestor-of-mount? mount-at sandbox)
        (let [p (str->path (sandbox->physical-str root mount-at sandbox))]
          (when (Files/isRegularFile p follow)
            (Files/newInputStream p (make-array java.nio.file.OpenOption 0)))))))

  (-open-sink [_ path append?]
    (when-let [sandbox (resolve-sandbox root mount-at @cwd-atom path)]
      (when-not (ancestor-of-mount? mount-at sandbox)
        (let [p (str->path (sandbox->physical-str root mount-at sandbox))
              base-opts (if append?
                          [java.nio.file.StandardOpenOption/CREATE
                           java.nio.file.StandardOpenOption/APPEND
                           java.nio.file.StandardOpenOption/WRITE]
                          [java.nio.file.StandardOpenOption/CREATE
                           java.nio.file.StandardOpenOption/TRUNCATE_EXISTING
                           java.nio.file.StandardOpenOption/WRITE])]
          (Files/newOutputStream p (into-array java.nio.file.OpenOption base-opts))))))

  (-mkdir [_ path]
    (when-let [sandbox (resolve-sandbox root mount-at @cwd-atom path)]
      (when-not (ancestor-of-mount? mount-at sandbox)
        (let [p (str->path (sandbox->physical-str root mount-at sandbox))]
          (when-not (Files/exists p follow)
            (try (Files/createDirectory p (make-array java.nio.file.attribute.FileAttribute 0))
                 true
                 (catch Throwable _ nil)))))))

  (-delete [_ path]
    (when-let [sandbox (resolve-sandbox root mount-at @cwd-atom path)]
      (when-not (ancestor-of-mount? mount-at sandbox)
        (let [p (str->path (sandbox->physical-str root mount-at sandbox))]
          (try (Files/delete p) true
               (catch Throwable _ nil))))))

  (-rename [_ from to]
    (when-let [from-sb (resolve-sandbox root mount-at @cwd-atom from)]
      (when-not (ancestor-of-mount? mount-at from-sb)
        (when-let [to-sb (resolve-sandbox root mount-at @cwd-atom to)]
          (when-not (ancestor-of-mount? mount-at to-sb)
            (try (Files/move (str->path (sandbox->physical-str root mount-at from-sb))
                             (str->path (sandbox->physical-str root mount-at to-sb))
                             (into-array java.nio.file.CopyOption
                                         [java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
                 true
                 (catch Throwable _ nil)))))))

  (-touch [_ path]
    (when-let [sandbox (resolve-sandbox root mount-at @cwd-atom path)]
      (when-not (ancestor-of-mount? mount-at sandbox)
        (let [p (str->path (sandbox->physical-str root mount-at sandbox))]
          (try
            (if (Files/exists p follow)
              (do (Files/setLastModifiedTime
                   p (java.nio.file.attribute.FileTime/fromMillis (System/currentTimeMillis)))
                  true)
              (do (Files/createFile p (make-array java.nio.file.attribute.FileAttribute 0))
                  true))
            (catch Throwable _ nil))))))

  (-chmod [_ path mode]
    (when-let [sandbox (resolve-sandbox root mount-at @cwd-atom path)]
      (when-not (ancestor-of-mount? mount-at sandbox)
        (let [p (str->path (sandbox->physical-str root mount-at sandbox))]
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
            (catch Throwable _ nil))))))

  (-symlink [_ target link-path]
    (when-let [link-sb (resolve-sandbox root mount-at @cwd-atom link-path)]
      (when-not (ancestor-of-mount? mount-at link-sb)
        (let [link-physical (sandbox->physical-str root mount-at link-sb)
              link-p (str->path link-physical)
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
                 (catch Throwable _ nil)))))))

  (-chown [_ path owner group]
    (when-let [sandbox (resolve-sandbox root mount-at @cwd-atom path)]
      (when-not (ancestor-of-mount? mount-at sandbox)
        (let [p (str->path (sandbox->physical-str root mount-at sandbox))
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

  (-sandbox-relativize [_ p]
    ;; Vestigial — every method now returns sandbox paths directly.
    ;; If callers feed a real-disk path under root (legacy), translate.
    ;; Otherwise pass through (already sandbox).
    (or (physical->sandbox root mount-at p) p))

  (-physical-path [_ sandbox-path]
    (when-let [sandbox (resolve-sandbox root mount-at @cwd-atom sandbox-path)]
      (when-not (ancestor-of-mount? mount-at sandbox)
        (sandbox->physical-str root mount-at sandbox)))))

;; ============================================================================
;; Constructor
;; ============================================================================

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
   The strict prefixes of `<mount-at>` (`/`, `/home` for the default)
   are virtual read-only ancestor directories — `cd /` works, but
   reading or writing anywhere above the mount returns nil.

   Auto-creates `<wrapper>/<mount-at>` if it doesn't exist
   (idempotent `Files/createDirectories`). The contract is that
   handing muschel a wrapper dir — even an empty one — yields a
   working sandbox.

   Options:
     :mount-at   sandbox path of the workspace, default `/home/agent`
                 (must be absolute, no `..`, no trailing slash; `/`
                 is the legacy flat layout where `<wrapper>` itself
                 is the workspace)
     :cwd        initial sandbox cwd; defaults to `mount-at`
     :max-bytes  read-file size cap, default 8 MiB"
  ([wrapper] (make wrapper {}))
  ([wrapper {:keys [cwd max-bytes mount-at]
             :or   {max-bytes default-read-cap
                    mount-at  default-mount-at}}]
   (let [mount-at  (normalize-mount-at mount-at)
         wrapper-trimmed (str/replace wrapper #"/+$" "")
         real-root-str   (if (= "/" mount-at)
                           wrapper-trimmed
                           (str wrapper-trimmed mount-at))
         real-root-p     (str->path real-root-str)
         _               (ensure-real-root! real-root-p)
         canonical       (canonical-root real-root-str)
         init-cwd        (or cwd mount-at)
         sandbox-cwd     (resolve-sandbox canonical mount-at mount-at init-cwd)]
     (when-not sandbox-cwd
       (throw (ex-info "Initial :cwd is outside the sandbox"
                       {:cwd init-cwd :mount-at mount-at})))
     (->DiskFS canonical mount-at (atom sandbox-cwd) max-bytes))))
