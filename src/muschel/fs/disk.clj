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
            buf))))))

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
