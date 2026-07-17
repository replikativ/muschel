(ns muschel.fs.geschichte
  "JVM Muschel FS adapter for a Geschichte working tree.

   Geschichte stays authoritative: this adapter never materializes a native
   directory and `-physical-path` deliberately returns nil. The separate
   projection backend can implement that boundary later without changing Git
   command semantics."
  (:refer-clojure :exclude [resolve])
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.bytes :as bytes]
            [geschichte.fs :as gfs]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]
            [muschel.fs :as fs]
            [muschel.fs.mount :as mount])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.util Arrays UUID]))

(def ^:private regular-mode 33188)    ; 0100644
(def ^:private executable-mode 33261) ; 0100755
(def ^:private symlink-mode 40960)    ; 0120000

(defn- canonicalize [cwd path]
  (when (string? path)
    (let [absolute (if (str/starts-with? path "/") path (str cwd "/" path))]
      (when-let [segments (fs/normalize-segments (fs/split-path absolute))]
        (let [p (fs/join-path "" segments)]
          (if (str/blank? p) "/" p))))))

(defn- repo-path [absolute]
  (str/replace absolute #"^/+" ""))

(defn- absolute-path [path]
  (if (str/blank? path) "/" (str "/" path)))

(defn- symlink-mode? [mode]
  (= symlink-mode (bit-and (long (or mode 0)) 61440)))

(defn- muschel-stat [entry]
  (when entry
    {:type (cond
             (= :dir (:type entry)) :dir
             (symlink-mode? (:mode entry)) :symlink
             :else :file)
     :size (long (or (:size entry) 0))
     :mtime-ms 0
     :perms-mode (bit-and (long (or (:mode entry) regular-mode)) 511)}))

(defn- raw-stat [conn absolute]
  (some-> (gfs/stat conn (repo-path absolute)) muschel-stat))

(defn- parent-path [absolute]
  (let [i (.lastIndexOf ^String absolute "/")]
    (if (<= i 0) "/" (subs absolute 0 i))))

(defn- follow-path
  "Resolve Geschichte symlinks without permitting an escape past its root."
  [conn absolute]
  (loop [path absolute, remaining 40]
    (let [st (raw-stat conn path)]
      (if (and (= :symlink (:type st)) (pos? remaining))
        (let [target (bytes/decode-utf8 (gfs/read conn (repo-path path)))
              next-path (canonicalize (parent-path path) target)]
          (when next-path (recur next-path (dec remaining))))
        (when (pos? remaining) path)))))

(defrecord GeschichteFS [conn cwd-atom repository config-atom]
  fs/FS
  (-resolve [_ path] (canonicalize @cwd-atom path))
  (-cwd [_] @cwd-atom)
  (-cd! [_ path]
    (when-let [absolute (canonicalize @cwd-atom path)]
      (when (= :dir (:type (raw-stat conn absolute)))
        (reset! cwd-atom absolute)
        absolute)))
  (-exists? [_ path]
    (boolean (some->> (canonicalize @cwd-atom path) (raw-stat conn))))
  (-stat [_ path]
    (some->> (canonicalize @cwd-atom path) (raw-stat conn)))
  (-list-dir [_ path]
    (when-let [absolute (canonicalize @cwd-atom path)]
      (when (= :dir (:type (raw-stat conn absolute)))
        (mapv (fn [{:keys [name path] :as entry}]
                (merge {:name name}
                       (muschel-stat entry)
                       (when path {:path (absolute-path path)})))
              (gfs/list-dir conn (repo-path absolute))))))
  (-read-file [_ path]
    (when-let [absolute (canonicalize @cwd-atom path)]
      (when-let [resolved (follow-path conn absolute)]
        (when (= :file (:type (raw-stat conn resolved)))
          (bytes/decode-utf8 (gfs/read conn (repo-path resolved)))))))
  (-read-bytes [_ path]
    (when-let [absolute (canonicalize @cwd-atom path)]
      (when-let [resolved (follow-path conn absolute)]
        (when (= :file (:type (raw-stat conn resolved)))
          (gfs/read conn (repo-path resolved))))))
  (-open-source [this path]
    (some-> (fs/-read-bytes this path) ByteArrayInputStream.))
  (-open-sink [_ path append?]
    (when-let [absolute (canonicalize @cwd-atom path)]
      (let [path (repo-path absolute)
            old (when append? (gfs/read conn path))
            mode (or (:mode (gfs/stat conn path)) regular-mode)]
        (proxy [ByteArrayOutputStream] []
          (close []
            (let [fresh (.toByteArray ^ByteArrayOutputStream this)
                  value (if old (bytes/concat-bytes old fresh) fresh)]
              (gfs/write! conn path value {:mode mode})
              (proxy-super close)))))))
  (-mkdir [_ path]
    (when-let [absolute (canonicalize @cwd-atom path)]
      (gfs/mkdir! conn (repo-path absolute))
      true))
  (-delete [_ path]
    (when-let [absolute (canonicalize @cwd-atom path)]
      (gfs/delete! conn (repo-path absolute))))
  (-rename [_ from to]
    (let [from (canonicalize @cwd-atom from)
          to (canonicalize @cwd-atom to)]
      (when (and from to)
        (gfs/rename! conn (repo-path from) (repo-path to))
        true)))
  (-touch [_ path]
    (when-let [absolute (canonicalize @cwd-atom path)]
      (let [path (repo-path absolute)]
        (when-not (gfs/stat conn path)
          (gfs/write! conn path (byte-array 0)))
        true)))
  (-chmod [_ path mode]
    (when-let [absolute (canonicalize @cwd-atom path)]
      (let [path (repo-path absolute)]
        (when-let [value (gfs/read conn path)]
          (gfs/write! conn path value
                      {:mode (if (pos? (bit-and (long mode) 73))
                               executable-mode regular-mode)})
          true))))
  (-symlink [_ target link-path]
    (when-let [absolute (canonicalize @cwd-atom link-path)]
      (gfs/write! conn (repo-path absolute) (bytes/utf8 target)
                  {:mode symlink-mode})
      true))
  (-chown [this path _owner _group] (fs/-exists? this path))
  (-sandbox-relativize [_ path] path)
  (-physical-path [_ _] nil))

(defn make
  "Adapt an initialized Geschichte connection to Muschel's synchronous JVM FS."
  ([conn] (make conn {}))
  ([conn {:keys [cwd repository config] :or {cwd "/" config {}}}]
   (->GeschichteFS conn (atom cwd) repository (atom config))))

(defn close!
  "Release resources owned by an adapter's repository factory. Persistent
   harnesses normally close all mounted repositories with their session."
  [geschichte-fs]
  (when-let [close! (get-in geschichte-fs [:repository :close!])]
    (close!)))

(defn memory-repository!
  "Default ephemeral repository factory used by an in-memory agent sandbox."
  [{:keys [name] :or {name "repository"}}]
  (let [config {:store {:backend :memory :id (UUID/randomUUID)}
                :schema-flexibility :write}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (repo/init! conn {:name (or name "repository")})
      {:conn conn
       :config config
       :close! #(do (d/release conn) (d/delete-database config))})))

(defn mount-repository!
  "Publish an already-populated Geschichte repository at an empty Muschel
  directory. Clone uses this after transport succeeds, keeping partial imports
  invisible to the sandbox."
  ([mount-fs root repository]
   (mount-repository! mount-fs root repository {}))
  ([mount-fs root {:keys [conn] :as repository} {:keys [allow-nested?]}]
   (let [root (fs/resolve mount-fs root)]
     (when-not (= :dir (:type (fs/stat mount-fs root)))
       (throw (ex-info "Geschichte mount target is not a directory" {:root root})))
     (when-let [[owner _] (mount/owning-mount mount-fs root)]
       (when-not allow-nested?
         (throw (ex-info "Path is already inside a Geschichte repository"
                         {:root root :repository-root owner}))))
     (let [adapter (make conn {:repository repository})]
       (mount/mount! mount-fs root adapter {:allow-nested? allow-nested?})
       (assoc repository :root root :fs adapter)))))

(defn fork-and-mount-workspace!
  "Fork an existing Geschichte mount into an independent Datahike workspace,
  optionally prepare its logical HEAD, and mount it at `root`. The returned
  mount owns only the new branch connection; the source repository continues
  to own the shared store lifecycle."
  [mount-fs root source-fs {:keys [branch prepare!] :as _opts}]
  (let [{source-conn :conn source-config :config :as source-repository}
        (:repository source-fs)
        source-conn (or source-conn (:conn source-fs))
        source-config (or source-config (get-in @source-conn [:config]))
        branch (or branch (workspace/branch-key (UUID/randomUUID)))]
    (workspace/fork! source-conn branch)
    (let [conn (d/connect (assoc source-config :branch branch))
          repository {:conn conn
                      :config source-config
                      :workspace-branch branch
                      :repository-id (get-in @conn [:config :store :id])
                      :source-repository source-repository
                      :close! #(d/release conn)}]
      (try
        (when prepare! (prepare! conn))
        (mount-repository! mount-fs root repository {:allow-nested? true})
        (catch Throwable error
          (d/release conn)
          (workspace/remove! source-conn branch)
          (throw error))))))

(defn- import-entry! [source root conn relative entry]
  (let [source-path (if (str/blank? relative) root (str root "/" relative))]
    (case (:type entry)
      :dir
      (do
        (when-not (str/blank? relative) (gfs/mkdir! conn relative))
        (doseq [child (fs/list-dir source source-path)]
          (let [child-relative (if (str/blank? relative)
                                 (:name child)
                                 (str relative "/" (:name child)))]
            (import-entry! source root conn child-relative child))))

      :file
      (let [value (or (fs/read-bytes source source-path)
                      (throw (ex-info "Could not read file during import"
                                      {:path source-path})))
            mode (if (pos? (bit-and (long (or (:perms-mode entry) 0)) 73))
                   executable-mode regular-mode)]
        (gfs/write! conn relative value {:mode mode})
        (when-not (Arrays/equals ^bytes value ^bytes (gfs/read conn relative))
          (throw (ex-info "Geschichte import verification failed"
                          {:path source-path}))))

      :symlink
      (throw (ex-info "Importing existing symlinks requires readlink support"
                      {:path source-path}))

      (throw (ex-info "Unsupported filesystem entry during import"
                      {:path source-path :type (:type entry)})))))

(defn init-and-mount!
  "Create a Geschichte repository from an existing Muschel subtree and publish
   it as a mount only after byte-for-byte import verification.

   `create-repository!` receives `{:root :name}` and returns at least `:conn`;
   it may also return `:close!` for rollback cleanup."
  ([mount-fs root] (init-and-mount! mount-fs root {}))
  ([mount-fs root {:keys [create-repository! name]
                   :or {create-repository! memory-repository!}}]
   (let [root (fs/resolve mount-fs root)]
     (when-not (= :dir (:type (fs/stat mount-fs root)))
       (throw (ex-info "git init target is not a directory" {:root root})))
     (when-let [[owner _] (mount/owning-mount mount-fs root)]
       (throw (ex-info "Path is already inside a Geschichte repository"
                       {:root root :repository-root owner})))
     (let [{:keys [conn close!] :as repository}
           (create-repository! {:root root
                                :name (or name
                                          (last (remove str/blank?
                                                        (str/split root #"/"))))})]
       (try
         (import-entry! mount-fs root conn "" (fs/stat mount-fs root))
         (mount-repository! mount-fs root repository)
         (catch Throwable error
           (when close! (close!))
           (throw error)))))))
