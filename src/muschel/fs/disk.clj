(ns muschel.fs.disk
  "Real-disk FS implementation pinned to a wrapper directory, with one
   or more sandbox-path → wrapper-subdir mounts inside.

   ## Wrapper layout

   `(make wrapper opts)` takes a host directory `wrapper` and a list
   of `:mounts` (default `[[\"/home/agent\" \"home/agent\"]
   [\"/tmp\" \"tmp\"]]`). Each mount maps a sandbox-relative path to
   a real-disk subdirectory under `wrapper`. The default gives the
   agent both a project workspace at `/home/agent` and a writable
   `/tmp` whose contents persist across spawns inside the wrapper
   — both layers (builtin FS protocol + bwrap externals) see the
   same files.

       Host                              Sandbox view
       -------------------               -----------------
       <wrapper>/home/agent/      ←──→   /home/agent/      (project)
       <wrapper>/tmp/             ←──→   /tmp/             (scratch)

   `<wrapper>` itself is *outside* the agent's reach via the FS
   protocol (containment is per-mount). Sibling paths under the
   wrapper are not exposed by default — they're a future-reserved
   slot for muschel-managed project state surfaced under
   `/system/muschel-project/<name>/` once that's implemented.

   ## Sandbox-space contract

   Every value crossing the `muschel.fs/FS` protocol is a
   **sandbox-relative path** — `/home/agent`, `/home/agent/src`,
   `/tmp/x`, `/`, `/home`. Real-disk paths are an implementation
   detail private to this namespace. `-resolve`, `-cwd`, `-cd!`
   all produce / consume sandbox paths. `-physical-path` is the
   one explicit translator (sandbox → real disk) and is only
   called at the `run-external` → OS-spawn boundary by `JvmHost`.

   ## Synthetic ancestor view

   The strict prefixes of every mount's sandbox-path are virtual
   read-only directories. With the default `[/home/agent /tmp]`,
   the union is `{/ /home}`. `ls /` returns `[home tmp]` (one
   segment toward each mount); `ls /home` returns `[agent]`.
   Writes anywhere above a mount return nil. This keeps the
   builtin shell environment bash-credible without aliasing or
   syscall-tower trickery.

   ## Auto-create on construction

   `make` ensures every mount's real-disk subdirectory exists
   under `wrapper` (`Files/createDirectories`, idempotent). Hand
   muschel a wrapper dir — even an empty one — and you get a
   working sandbox with all configured mounts ready.

   ## Back-compat: `:mount-at`

   `:mount-at X` is the legacy single-mount API and creates
   `:mounts [[X wrapper-subdir-of-X]]` with NO auto-`/tmp`. Used
   by FS-protocol-level tests that want to exercise the generic
   contract without the multi-mount default.

   ## Containment caveats

   - **Symlinks pointing outside any mount** are caught: we always
     toRealPath() before the prefix check. If the link target is
     outside, the resolve returns nil.

   - **TOCTOU**: between resolve() and a subsequent read, an attacker
     with write access to a mount could swap a regular file for a
     symlink pointing outside. We re-resolve at every op so the
     window is per-call; it's NOT zero. For untrusted-attacker
     scenarios pair with OS-level isolation (bwrap/firejail).

   - **JVM-only** (uses java.nio.file). For Node and browser, ship
     analogous impls in their own namespaces. Each impl is
     responsible for ensuring its real-root exists on construction
     using its platform's native API."
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
  [^Path p]
  (try (.toRealPath p follow)
       (catch NoSuchFileException _ nil)
       (catch Throwable _ nil)))

(defn- ^Path canonical-or-abs [^String s]
  (let [p (str->path s)
        real (safe-real-path p)]
    (or real (.toAbsolutePath (.normalize p)))))

(defn- inside-any?
  "True if `^Path candidate` is at or under any of the mount real-roots."
  [mount-real-roots ^Path candidate]
  (some (fn [^Path root]
          (or (= root candidate)
              (.startsWith candidate root)))
        mount-real-roots))

;; ============================================================================
;; Mount + path validation
;; ============================================================================

(defn- normalize-sandbox-prefix
  "Validate + normalise an absolute sandbox path used as a mount
   point. No `..`, no trailing slash (except for `/` itself)."
  [^String s]
  (when-not (and (string? s) (.startsWith s "/"))
    (throw (ex-info "mount sandbox-path must be absolute" {:path s})))
  (let [collapsed (str/replace s #"/+" "/")
        stripped  (if (= "/" collapsed) "/" (str/replace collapsed #"/$" ""))]
    (when (or (str/blank? stripped) (some #{".."} (str/split stripped #"/")))
      (throw (ex-info "mount sandbox-path must be non-empty and not contain `..`"
                      {:path s})))
    stripped))

(defn- normalize-wrapper-subdir
  "Validate the wrapper-subdir for a mount. A relative POSIX path,
   no `..`. `\"\"` means the wrapper itself (used for the legacy
   `:mount-at \"/\"` mode)."
  [^String s]
  (when-not (string? s)
    (throw (ex-info "wrapper-subdir must be a string" {:value s})))
  (let [collapsed (str/replace s #"^/+" "")
        stripped  (str/replace collapsed #"/+$" "")]
    (when (some #{".."} (str/split stripped #"/"))
      (throw (ex-info "wrapper-subdir must not contain `..`" {:value s})))
    stripped))

(defn- normalize-mount [[sp ws]]
  [(normalize-sandbox-prefix sp) (normalize-wrapper-subdir ws)])

(defn- mount-for
  "Return the mount entry [sandbox-prefix wrapper-subdir] whose
   sandbox-prefix is at-or-above `path`. Longest match wins (so
   nested mounts work, though we don't ship any by default)."
  [mounts ^String path]
  (->> mounts
       (filter (fn [[sp _]]
                 (or (= sp "/")
                     (= path sp)
                     (.startsWith path (str sp "/")))))
       (sort-by (fn [[sp _]] (- (count sp))))
       first))

(defn- ^String sandbox->physical-str
  "Translate `path` (sandbox-absolute, under some mount) to its
   real-disk path. Returns nil if no mount contains it."
  [^Path root mounts ^String path]
  (when-let [[sp ws] (mount-for mounts path)]
    (let [tail (cond
                 (= path sp)        ""
                 (= sp "/")         path        ; everything is tail under root-mount
                 :else              (subs path (count sp)))
          base (if (= ws "") (str root) (str root "/" ws))]
      (str base tail))))

(defn- ^String physical->sandbox
  "Translate a real-disk path under one of the mount real-roots back
   to its sandbox form. Returns nil if outside every mount."
  [^Path root mounts ^String real-path-str]
  (let [rs (str root)]
    (when (or (= real-path-str rs) (.startsWith real-path-str (str rs "/")))
      (let [under-root (if (= real-path-str rs) "" (subs real-path-str (count rs)))]
        ;; under-root is "" or "/<rest>"; find the mount whose
        ;; wrapper-subdir is the prefix of the rest.
        (when-let [[sp ws]
                   (->> mounts
                        (filter (fn [[_ ws]]
                                  (let [ws-prefix (if (= ws "") "" (str "/" ws))]
                                    (or (= under-root ws-prefix)
                                        (and (not= ws-prefix "")
                                             (.startsWith under-root (str ws-prefix "/")))
                                        (and (= ws-prefix "")
                                             (or (= under-root "") (.startsWith under-root "/")))))))
                        (sort-by (fn [[_ ws]] (- (count ws))))
                        first)]
          (let [ws-prefix (if (= ws "") "" (str "/" ws))
                tail (if (= under-root ws-prefix) "" (subs under-root (count ws-prefix)))]
            (cond
              (and (= sp "/") (= tail "")) "/"
              (= sp "/") tail
              :else (str sp tail))))))))

;; ============================================================================
;; Ancestor view
;; ============================================================================

(defn- ancestors-of [^String sandbox-path]
  ;; "/home/agent" → ("/" "/home"); "/tmp" → ("/"); "/" → ()
  (when (not= sandbox-path "/")
    (let [segs (rest (str/split sandbox-path #"/"))]
      (for [n (range (count segs))
            :let [taken (take n segs)]]
        (if (empty? taken) "/" (str "/" (str/join "/" taken)))))))

(defn- all-ancestors [mounts]
  (->> mounts
       (mapcat (fn [[sp _]] (ancestors-of sp)))
       distinct
       set))

(defn- ancestor-of-mount?
  "True if `path` is a strict prefix of any mount sandbox-path."
  [mounts ^String path]
  (contains? (all-ancestors mounts) path))

(defn- ancestor-children
  "For an ancestor `ancestor-path`, return the deduped next-segments
   leading toward each mount below."
  [mounts ^String ancestor-path]
  (->> mounts
       (keep (fn [[sp _]]
               (when (and (not= sp ancestor-path)
                          (or (= ancestor-path "/")
                              (str/starts-with? sp (str ancestor-path "/"))))
                 (let [start (if (= ancestor-path "/") 1 (inc (count ancestor-path)))
                       remainder (subs sp start)]
                   (first (str/split remainder #"/"))))))
       distinct
       vec))

(defn- under-some-mount? [mounts ^String path]
  (some? (mount-for mounts path)))

;; ============================================================================
;; Lex absolutize + resolution
;; ============================================================================

(defn- absolutize-sandbox
  "Pure-string absolute + normalize for sandbox-space paths. `cwd`
   must be sandbox-absolute. Returns nil for ~/-prefixed paths and
   for malformed input."
  [^String cwd ^String path]
  (when (and (string? path) (not (str/blank? path)))
    (cond
      (str/starts-with? path "~/") nil
      :else
      (let [joined (if (str/starts-with? path "/")
                     path
                     (str (str/replace cwd #"/$" "") "/" path))
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

(defn- mount-real-roots [^Path root mounts]
  (mapv (fn [[_ ws]]
          (let [s (if (= ws "") (str root) (str root "/" ws))]
            (canonical-or-abs s)))
        mounts))

(defn- canonicalize-in-mount
  "Translate a sandbox path that's under some mount into its
   canonical sandbox form by going through real-disk symlink
   resolution + containment check. If the leaf doesn't exist, walk
   up to the nearest existing ancestor on disk and re-join the
   missing tail lexically."
  [^Path root mounts ^String sandbox-path]
  (let [physical-str (sandbox->physical-str root mounts sandbox-path)]
    (when physical-str
      (let [normalized (.normalize (.toAbsolutePath (str->path physical-str)))
            real (safe-real-path normalized)
            real-roots (mount-real-roots root mounts)]
        (cond
          (some? real)
          (when (inside-any? real-roots real)
            (physical->sandbox root mounts (str real)))

          :else
          (loop [p     (.getParent normalized)
                 tail  (list (.getFileName normalized))]
            (cond
              (nil? p) nil
              :else
              (if-let [real-anc (safe-real-path p)]
                (when (inside-any? real-roots real-anc)
                  (let [rebuilt (reduce (fn [^Path acc ^Path seg] (.resolve acc seg))
                                        real-anc
                                        tail)]
                    (when (inside-any? real-roots rebuilt)
                      (physical->sandbox root mounts (str rebuilt)))))
                (recur (.getParent p)
                       (conj tail (.getFileName p)))))))))))

(defn- resolve-sandbox
  "Sandbox-space resolution. Returns the canonical sandbox path of
   `path` (resolved against `cwd`), or nil on escape."
  [^Path root mounts ^String cwd ^String path]
  (when-let [sandbox-abs (absolutize-sandbox cwd path)]
    (cond
      (ancestor-of-mount? mounts sandbox-abs)
      sandbox-abs

      (under-some-mount? mounts sandbox-abs)
      (canonicalize-in-mount root mounts sandbox-abs)

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

(defrecord DiskFS [^Path root mounts cwd-atom max-bytes]
  fs/FS
  (-resolve [_ path]
    (resolve-sandbox root mounts @cwd-atom path))

  (-cwd [_] @cwd-atom)

  (-cd! [_ path]
    (when-let [sandbox (resolve-sandbox root mounts @cwd-atom path)]
      (cond
        (ancestor-of-mount? mounts sandbox)
        (do (reset! cwd-atom sandbox) sandbox)

        :else
        (let [physical (sandbox->physical-str root mounts sandbox)
              p (str->path physical)]
          (when (Files/isDirectory p follow)
            (reset! cwd-atom sandbox)
            sandbox)))))

  (-exists? [_ path]
    (when-let [sandbox (resolve-sandbox root mounts @cwd-atom path)]
      (cond
        (ancestor-of-mount? mounts sandbox) true
        :else (Files/exists (str->path (sandbox->physical-str root mounts sandbox)) follow))))

  (-stat [_ path]
    (when-let [sandbox (resolve-sandbox root mounts @cwd-atom path)]
      (cond
        (ancestor-of-mount? mounts sandbox)
        {:type :dir :size 0 :mtime-ms 0 :perms nil}

        :else
        (let [p (str->path (sandbox->physical-str root mounts sandbox))]
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
    (when-let [sandbox (resolve-sandbox root mounts @cwd-atom path)]
      (cond
        (ancestor-of-mount? mounts sandbox)
        (mapv (fn [name]
                {:name name :type :dir :size 0 :mtime-ms 0})
              (ancestor-children mounts sandbox))

        :else
        (let [p (str->path (sandbox->physical-str root mounts sandbox))]
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
                              stream)))))))))

  (-read-file [this path]
    (when-let [bs (fs/-read-bytes this path)]
      (String. ^bytes bs "UTF-8")))

  (-read-bytes [_ path]
    (when-let [sandbox (resolve-sandbox root mounts @cwd-atom path)]
      (when-not (ancestor-of-mount? mounts sandbox)
        (let [p (str->path (sandbox->physical-str root mounts sandbox))]
          (when (Files/isRegularFile p follow)
            (let [size (Files/size p)
                  cap  max-bytes
                  read-size (min size cap)
                  buf (byte-array read-size)]
              (with-open [in (Files/newInputStream p (make-array java.nio.file.OpenOption 0))]
                (.readNBytes in buf 0 read-size))
              buf))))))

  (-open-source [_ path]
    (when-let [sandbox (resolve-sandbox root mounts @cwd-atom path)]
      (when-not (ancestor-of-mount? mounts sandbox)
        (let [p (str->path (sandbox->physical-str root mounts sandbox))]
          (when (Files/isRegularFile p follow)
            (Files/newInputStream p (make-array java.nio.file.OpenOption 0)))))))

  (-open-sink [_ path append?]
    (when-let [sandbox (resolve-sandbox root mounts @cwd-atom path)]
      (when-not (ancestor-of-mount? mounts sandbox)
        (let [p (str->path (sandbox->physical-str root mounts sandbox))
              base-opts (if append?
                          [java.nio.file.StandardOpenOption/CREATE
                           java.nio.file.StandardOpenOption/APPEND
                           java.nio.file.StandardOpenOption/WRITE]
                          [java.nio.file.StandardOpenOption/CREATE
                           java.nio.file.StandardOpenOption/TRUNCATE_EXISTING
                           java.nio.file.StandardOpenOption/WRITE])]
          (Files/newOutputStream p (into-array java.nio.file.OpenOption base-opts))))))

  (-mkdir [_ path]
    (when-let [sandbox (resolve-sandbox root mounts @cwd-atom path)]
      (when-not (ancestor-of-mount? mounts sandbox)
        (let [p (str->path (sandbox->physical-str root mounts sandbox))]
          (when-not (Files/exists p follow)
            (try (Files/createDirectory p (make-array java.nio.file.attribute.FileAttribute 0))
                 true
                 (catch Throwable _ nil)))))))

  (-delete [_ path]
    (when-let [sandbox (resolve-sandbox root mounts @cwd-atom path)]
      (when-not (ancestor-of-mount? mounts sandbox)
        (let [p (str->path (sandbox->physical-str root mounts sandbox))]
          (try (Files/delete p) true
               (catch Throwable _ nil))))))

  (-rename [_ from to]
    (when-let [from-sb (resolve-sandbox root mounts @cwd-atom from)]
      (when-not (ancestor-of-mount? mounts from-sb)
        (when-let [to-sb (resolve-sandbox root mounts @cwd-atom to)]
          (when-not (ancestor-of-mount? mounts to-sb)
            (try (Files/move (str->path (sandbox->physical-str root mounts from-sb))
                             (str->path (sandbox->physical-str root mounts to-sb))
                             (into-array java.nio.file.CopyOption
                                         [java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
                 true
                 (catch Throwable _ nil)))))))

  (-touch [_ path]
    (when-let [sandbox (resolve-sandbox root mounts @cwd-atom path)]
      (when-not (ancestor-of-mount? mounts sandbox)
        (let [p (str->path (sandbox->physical-str root mounts sandbox))]
          (try
            (if (Files/exists p follow)
              (do (Files/setLastModifiedTime
                   p (java.nio.file.attribute.FileTime/fromMillis (System/currentTimeMillis)))
                  true)
              (do (Files/createFile p (make-array java.nio.file.attribute.FileAttribute 0))
                  true))
            (catch Throwable _ nil))))))

  (-chmod [_ path mode]
    (when-let [sandbox (resolve-sandbox root mounts @cwd-atom path)]
      (when-not (ancestor-of-mount? mounts sandbox)
        (let [p (str->path (sandbox->physical-str root mounts sandbox))]
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
    (when-let [link-sb (resolve-sandbox root mounts @cwd-atom link-path)]
      (when-not (ancestor-of-mount? mounts link-sb)
        (let [link-physical (sandbox->physical-str root mounts link-sb)
              link-p (str->path link-physical)
              link-parent (.getParent link-p)
              target-p (str->path target)
              target-abs (if (.isAbsolute target-p)
                           target-p
                           (.resolve link-parent target-p))
              target-norm (.normalize target-abs)
              real-roots (mount-real-roots root mounts)]
          (when (inside-any? real-roots target-norm)
            (try (Files/createSymbolicLink link-p target-p
                                           (make-array java.nio.file.attribute.FileAttribute 0))
                 true
                 (catch Throwable _ nil)))))))

  (-chown [_ path owner group]
    (when-let [sandbox (resolve-sandbox root mounts @cwd-atom path)]
      (when-not (ancestor-of-mount? mounts sandbox)
        (let [p (str->path (sandbox->physical-str root mounts sandbox))
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
    (or (physical->sandbox root mounts p) p))

  (-physical-path [_ sandbox-path]
    (when-let [sandbox (resolve-sandbox root mounts @cwd-atom sandbox-path)]
      (when-not (ancestor-of-mount? mounts sandbox)
        (sandbox->physical-str root mounts sandbox)))))

;; ============================================================================
;; Constructor
;; ============================================================================

(def default-mounts
  "Default mount table for the standard wrapper layout: the agent's
   project workspace at `/home/agent` and a writable scratch dir at
   `/tmp`, both backed by subdirectories of the wrapper. Both layers
   (builtin FS protocol + bwrap externals) share these files."
  [["/home/agent" "home/agent"]
   ["/tmp" "tmp"]])

(defn- mount-at->mounts [^String mount-at]
  ;; Back-compat shim for the single-mount API.
  (let [sp (normalize-sandbox-prefix mount-at)
        ws (if (= sp "/") "" (subs sp 1))]
    [[sp ws]]))

(defn- validate-mounts [mounts]
  (let [paths (map first mounts)]
    (when (not= (count paths) (count (distinct paths)))
      (throw (ex-info "Duplicate mount sandbox-paths" {:mounts mounts}))))
  mounts)

(defn make
  "Construct a disk-backed sandbox FS over `wrapper` — a host
   directory that holds one or more agent-accessible mounts.

   Options (mutually exclusive, choose one or the other):
     :mounts    explicit vec of [sandbox-path wrapper-subdir] pairs;
                bypasses defaults. Use for custom layouts.
     :mount-at  back-compat single-mount API; equivalent to
                `:mounts [[mount-at <derived-subdir>]]`. No auto-/tmp.

   With NEITHER, the default mount table is used:
     [[\"/home/agent\" \"home/agent\"]
      [\"/tmp\"        \"tmp\"]]

   `make` auto-creates each mount's real-disk subdirectory under
   `wrapper` (idempotent).

   Other options:
     :cwd        initial sandbox cwd; defaults to the first mount's
                 sandbox path
     :max-bytes  read-file size cap, default 8 MiB"
  ([wrapper] (make wrapper {}))
  ([wrapper {:keys [cwd max-bytes mounts mount-at]
             :or   {max-bytes default-read-cap}}]
   (when (and mounts mount-at)
     (throw (ex-info "Pass :mounts OR :mount-at, not both"
                     {:mounts mounts :mount-at mount-at})))
   (let [mounts (cond
                  mounts   (validate-mounts (mapv normalize-mount mounts))
                  mount-at (mount-at->mounts mount-at)
                  :else    default-mounts)
         wrapper-trimmed (str/replace wrapper #"/+$" "")
         ;; Auto-create every mount's real-disk subdir under the wrapper.
         _ (doseq [[_ ws] mounts]
             (let [target (if (= ws "")
                            wrapper-trimmed
                            (str wrapper-trimmed "/" ws))]
               (Files/createDirectories (str->path target)
                                        (make-array java.nio.file.attribute.FileAttribute 0))))
         canonical (canonical-or-abs wrapper-trimmed)
         init-cwd  (or cwd (ffirst mounts))
         sandbox-cwd (resolve-sandbox canonical mounts (ffirst mounts) init-cwd)]
     (when-not sandbox-cwd
       (throw (ex-info "Initial :cwd is outside the sandbox"
                       {:cwd init-cwd :mounts mounts})))
     (->DiskFS canonical mounts (atom sandbox-cwd) max-bytes))))

;; ============================================================================
;; Introspection helpers (for SandboxedHost binds, debugging)
;; ============================================================================

(defn mounts
  "Return the mount table of a DiskFS: a vector of [sandbox-path
   wrapper-subdir] pairs, in the order the FS uses them."
  [^DiskFS fs]
  (:mounts fs))

(defn wrapper-root
  "Return the canonical real-disk path of the wrapper (the directory
   `make` was given). Used by SandboxedHost to compute the bind
   sources for each mount."
  [^DiskFS fs]
  (str (:root fs)))
