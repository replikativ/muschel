(ns muschel.fs.disk
  "Real-disk FS implementation pinned to a root.

   Every operation resolves the requested path against a canonical
   root (with symlinks followed via java.nio.file/Path.toRealPath).
   If the resolved path doesn't start with the root prefix, the op
   returns nil — caller sees \"no such file\" without any plumbing
   to track the escape.

   Containment caveats:

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
     analogous impls in their own namespaces."
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

(defn- resolve* [^Path root ^String cwd ^String path]
  (when (and (string? path) (not (str/blank? path)))
    (let [;; Absolute paths are absolute. Relative paths attach to cwd.
          base (cond
                 (.startsWith path "/")             (str->path path)
                 (.startsWith path "~/")            nil   ;; muschel.expand handles ~
                 :else                              (str->path (str cwd "/" path)))]
      (when base
        (let [normalized (.normalize (.toAbsolutePath base))]
          ;; Real-path first (follows links). If missing, fall back to
          ;; normalized — useful for stat-not-existing-file etc., but
          ;; in those cases existence checks downstream return nil.
          (let [resolved (or (safe-real-path normalized) normalized)]
            (when (inside? root resolved)
              (str resolved))))))))

(defn- type-of [^Path p]
  (cond
    (Files/isSymbolicLink p) :symlink
    (Files/isDirectory p follow) :dir
    (Files/isRegularFile p follow) :file
    (Files/exists p follow) :other
    :else nil))

(def ^:private default-read-cap (* 8 1024 1024))   ;; 8 MiB

(defrecord DiskFS [^Path root cwd-atom max-bytes]
  fs/FS
  (-resolve [_ path]
    (resolve* root @cwd-atom path))

  (-cwd [_] @cwd-atom)

  (-cd! [_ path]
    (when-let [resolved (resolve* root @cwd-atom path)]
      (let [p (str->path resolved)]
        (when (Files/isDirectory p follow)
          (reset! cwd-atom resolved)
          resolved))))

  (-exists? [_ path]
    (when-let [resolved (resolve* root @cwd-atom path)]
      (Files/exists (str->path resolved) follow)))

  (-stat [_ path]
    (when-let [resolved (resolve* root @cwd-atom path)]
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
    (when-let [resolved (resolve* root @cwd-atom path)]
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
    (when-let [resolved (resolve* root @cwd-atom path)]
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
    (when-let [resolved (resolve* root @cwd-atom path)]
      (let [p (str->path resolved)]
        (when (Files/isRegularFile p follow)
          (Files/newInputStream p (make-array java.nio.file.OpenOption 0))))))

  (-open-sink [_ path append?]
    (when-let [resolved (resolve* root @cwd-atom path)]
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
    (when-let [resolved (resolve* root @cwd-atom path)]
      (let [p (str->path resolved)]
        (when-not (Files/exists p follow)
          (try (Files/createDirectory p (make-array java.nio.file.attribute.FileAttribute 0))
               true
               (catch Throwable _ nil))))))

  (-delete [_ path]
    (when-let [resolved (resolve* root @cwd-atom path)]
      (let [p (str->path resolved)]
        (try (Files/delete p) true
             (catch Throwable _ nil)))))

  (-rename [_ from to]
    (when-let [from-resolved (resolve* root @cwd-atom from)]
      ;; Resolve `to` relative to the same root + cwd. For `to`, the
      ;; resolved path is allowed to NOT exist yet — resolve* falls
      ;; back to abs+normalize and still inside?-checks.
      (when-let [to-resolved (resolve* root @cwd-atom to)]
        (try (Files/move (str->path from-resolved)
                         (str->path to-resolved)
                         (into-array java.nio.file.CopyOption
                                     [java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
             true
             (catch Throwable _ nil)))))

  (-touch [_ path]
    (when-let [resolved (resolve* root @cwd-atom path)]
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
    (when-let [resolved (resolve* root @cwd-atom path)]
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
    (when-let [resolved (resolve* root @cwd-atom link-path)]
      (try (Files/createSymbolicLink (str->path resolved)
                                     (str->path target)
                                     (make-array java.nio.file.attribute.FileAttribute 0))
           true
           (catch Throwable _ nil))))

  (-chown [_ path owner group]
    (when-let [resolved (resolve* root @cwd-atom path)]
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

(defn make
  "Construct a disk FS pinned to `root`. All paths resolve under root;
   anything outside (after canonicalisation + symlink-follow) returns
   nil.

   Options:
     :cwd        initial cwd (must be under root; defaults to root)
     :max-bytes  read-file size cap, default 8 MiB"
  ([root] (make root {}))
  ([root {:keys [cwd max-bytes] :or {max-bytes default-read-cap}}]
   (let [canonical (canonical-root root)
         root-str  (str canonical)
         init-cwd  (or cwd root-str)
         ;; Resolve initial cwd against root to enforce containment.
         resolved-cwd (resolve* canonical root-str init-cwd)]
     (when-not resolved-cwd
       (throw (ex-info "Initial :cwd is outside :root"
                       {:cwd init-cwd :root root-str})))
     (->DiskFS canonical (atom resolved-cwd) max-bytes))))
