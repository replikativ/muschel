(ns muschel.fs.virtual
  "In-memory FS for tests, the browser playground, and substrate-fork
   scenarios where we want a cheap, serializable copy of the
   filesystem state.

   ## Underlying state

   An atom holding `{path-string → entry-map}`. Entry shapes:

     {:type :dir     :mtime-ms long}
     {:type :file    :content str :mtime-ms long :perms-mode int?}
     {:type :symlink :target str :mtime-ms long}

   Content is stored as a Clojure string. JVM `String`s and cljs
   strings are immutable, so forks (via `clojure.core/atom` deref +
   re-wrap, or via spindel `fork-context`) share the underlying data
   without copying and never see each other's writes.

   This shape is pure persistent Clojure data — fully serialisable
   via EDN or Transit. Use `serialise` / `deserialise` (below) to
   round-trip a snapshot between muschel instances.

   ## Trade-off

   Storing files as strings means a 10 MB write produces a 10 MB
   immutable string allocation. For agent workloads (KB-sized files)
   that's irrelevant. If we ever need GB-scale binary blobs the
   entry shape can grow a `{:type :file :chunks [strs]}` variant
   without changing the FS protocol surface — readers still see a
   string.

   ## Containment

   The 'root' is implicit — paths use Unix-style absolute strings,
   typically `/`. There's no external filesystem to escape to, so
   containment is structural: anything not in the map doesn't exist.

   :cwd is tracked in an atom alongside the entries. -cd! refuses
   paths that don't resolve to a :dir entry."
  (:require [muschel.fs :as fs]
            [clojure.string :as str]))

(defn- normalize
  "Resolve `path` against `cwd`, collapse `.` / `..`. Returns nil on
   `..`-escape past `/`."
  [cwd path]
  (when (string? path)
    (let [combined (if (str/starts-with? path "/")
                     path
                     (str cwd "/" path))
          segments (fs/split-path combined)
          normalized (fs/normalize-segments segments)]
      (when normalized
        (fs/join-path "" normalized)))))

(defn- mtime []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

(defrecord VirtualFS [entries-atom cwd-atom]
  fs/FS
  (-resolve [_ path]
    (when-let [n (normalize @cwd-atom path)]
      ;; In the virtual FS, ALL absolute paths are inside the implicit
      ;; root `/`. The resolve doesn't need to check existence; that's
      ;; what -exists?/-stat are for.
      (let [n (if (= "" n) "/" n)]
        n)))

  (-cwd [_] @cwd-atom)

  (-cd! [this path]
    (when-let [resolved (fs/-resolve this path)]
      (let [entry (get @entries-atom resolved)]
        (when (= :dir (:type entry))
          (reset! cwd-atom resolved)
          resolved))))

  (-exists? [this path]
    (when-let [resolved (fs/-resolve this path)]
      (some? (get @entries-atom resolved))))

  (-stat [this path]
    (when-let [resolved (fs/-resolve this path)]
      (when-let [entry (get @entries-atom resolved)]
        {:type     (:type entry)
         :size     (case (:type entry)
                     :dir     0
                     :symlink (count (or (:target entry) ""))
                     :file    (count (or (:content entry) ""))
                     0)
         :mtime-ms   (or (:mtime-ms entry) 0)
         :perms      nil
         :perms-mode (:perms-mode entry)
         :target     (:target entry)
         :owner      (:owner entry)
         :group      (:group entry)})))

  (-list-dir [this path]
    (when-let [resolved (fs/-resolve this path)]
      (when (= :dir (:type (get @entries-atom resolved)))
        (let [prefix (str (str/replace resolved #"/$" "") "/")
              plen   (count prefix)
              all    @entries-atom]
          (->> all
               (keep (fn [[p e]]
                       (when (and (str/starts-with? p prefix)
                                  (> (count p) plen))
                         (let [tail (subs p plen)]
                           ;; Direct child: no nested separator AND the
                           ;; entry isn't the dir itself (guards root,
                           ;; where prefix is "/" and the dir would
                           ;; otherwise list itself with name "").
                           (when-not (str/includes? tail "/")
                             {:name tail
                              :type (:type e)
                              :size (case (:type e)
                                      :dir 0
                                      :file (count (or (:content e) ""))
                                      :symlink (count (or (:target e) ""))
                                      0)
                              :mtime-ms (or (:mtime-ms e) 0)})))))
               (sort-by :name))))))

  (-read-file [this path]
    (when-let [resolved (fs/-resolve this path)]
      (let [entry (get @entries-atom resolved)]
        (when (= :file (:type entry))
          (or (:content entry) "")))))

  (-read-bytes [this path]
    ;; The VFS stores text. Callers that want bytes get UTF-8.
    (when-let [s (fs/-read-file this path)]
      #?(:clj  (.getBytes ^String s "UTF-8")
         :cljs s)))

  (-open-source [this path]
    #?(:clj
       (when-let [s (fs/-read-file this path)]
         (java.io.ByteArrayInputStream.
          (.getBytes ^String s "UTF-8")))
       :cljs
       ;; In cljs there's no InputStream — callers should use
       ;; -read-file directly. Return the string itself, which any
       ;; builtin reading "from stdin via -read-all-string" can
       ;; treat as content.
       (when-let [s (fs/-read-file this path)] s)))

  (-open-sink [this path append?]
    ;; Cross-platform sink shape: a Clojure map carrying an `:acc`
    ;; atom. `muschel.host.builtin` recognises this shape directly so
    ;; the same code path runs on JVM, babashka, and CLJS — none of
    ;; which can fully support a `proxy [java.io.ByteArrayOutputStream]`
    ;; with the close-hook commit semantics we used to rely on. An
    ;; `add-watch` mirrors every swap! into entries-atom so a follow-up
    ;; read sees the bytes without needing an explicit close.
    (when-let [resolved (fs/-resolve this path)]
      (let [existing (when append?
                       (when-let [e (get @entries-atom resolved)]
                         (when (= :file (:type e)) (:content e))))
            initial  (or existing "")
            acc      (atom initial)]
        (swap! entries-atom assoc resolved
               {:type :file :content initial :mtime-ms (mtime)})
        (add-watch acc ::vfs-flush
                   (fn [_ _ _ new]
                     (swap! entries-atom assoc resolved
                            {:type :file
                             :content new
                             :mtime-ms (mtime)})))
        {::buf :sink :acc acc ::path resolved ::vfs-sink? true})))

  (-mkdir [this path]
    (when-let [resolved (fs/-resolve this path)]
      ;; Refuse if a non-dir entry already exists, or if the parent
      ;; doesn't exist (so the user does mkdir -p deliberately).
      (let [existing (get @entries-atom resolved)
            parent   (let [segs (vec (fs/split-path resolved))]
                       (if (<= (count segs) 2)
                         "/"
                         (fs/join-path "" (filter seq (butlast segs)))))
            parent-e (get @entries-atom parent)]
        (cond
          existing                                            nil
          (or (= "/" resolved)
              (and parent-e (= :dir (:type parent-e))))
          (do (swap! entries-atom assoc resolved
                     {:type :dir :mtime-ms (mtime)})
              true)
          :else nil))))

  (-delete [this path]
    (when-let [resolved (fs/-resolve this path)]
      (when-let [entry (get @entries-atom resolved)]
        ;; For dirs, refuse if any children exist (POSIX rmdir).
        (let [prefix (str (str/replace resolved #"/+$" "") "/")
              child? (some (fn [[p _]]
                             (and (str/starts-with? p prefix)
                                  (not= p resolved)))
                           @entries-atom)]
          (when-not (and (= :dir (:type entry)) child?)
            (swap! entries-atom dissoc resolved)
            true)))))

  (-rename [this from to]
    (when-let [from-r (fs/-resolve this from)]
      (when-let [to-r (fs/-resolve this to)]
        (when-let [entry (get @entries-atom from-r)]
          ;; Atomic in the atom: remove from, write to, plus walk any
          ;; descendants and re-key under the new prefix.
          (swap! entries-atom
                 (fn [m]
                   (let [prefix (str (str/replace from-r #"/+$" "") "/")
                         to-prefix (str (str/replace to-r #"/+$" "") "/")
                         descendants
                         (->> m
                              (filter (fn [[p _]] (str/starts-with? p prefix)))
                              (mapv (fn [[p e]]
                                      [p (str to-prefix (subs p (count prefix))) e])))
                         m' (apply dissoc m (mapv first descendants))
                         m' (assoc m' to-r (assoc entry :mtime-ms (mtime)))
                         m' (reduce (fn [acc [_ to-p e]]
                                      (assoc acc to-p e))
                                    m'
                                    descendants)]
                     (dissoc m' from-r))))
          true))))

  (-touch [this path]
    (when-let [resolved (fs/-resolve this path)]
      (let [now (mtime)]
        (swap! entries-atom
               (fn [m]
                 (if-let [e (get m resolved)]
                   (assoc m resolved (assoc e :mtime-ms now))
                   (assoc m resolved {:type :file :content "" :mtime-ms now}))))
        true)))

  (-chmod [this path mode]
    (when-let [resolved (fs/-resolve this path)]
      (when (get @entries-atom resolved)
        (swap! entries-atom update resolved assoc :perms-mode mode)
        true)))

  (-symlink [this target link-path]
    (when-let [resolved (fs/-resolve this link-path)]
      ;; Refuse symlinks whose target lexically escapes the sandbox.
      ;; The VFS doesn't currently follow symlinks on read either, so
      ;; this pairs with that — disk-vs-virtual behave the same way
      ;; on both create AND read.
      (let [parent-segs (vec (butlast (fs/split-path resolved)))
            target-segs (fs/split-path
                         (cond
                           (clojure.string/starts-with? target "/") target
                           :else
                           (str (fs/join-path "" parent-segs) "/" target)))
            normalized (fs/normalize-segments target-segs)]
        (when (and (some? normalized)
                   (not (get @entries-atom resolved)))
          (swap! entries-atom assoc resolved
                 {:type :symlink :target target :mtime-ms (mtime)})
          true))))

  (-chown [this path owner group]
    (when-let [resolved (fs/-resolve this path)]
      (when (get @entries-atom resolved)
        (swap! entries-atom update resolved
               (fn [e]
                 (cond-> e
                   owner (assoc :owner owner)
                   group (assoc :group group))))
        true)))

  (-sandbox-relativize [_ path]
    ;; VFS paths are already sandbox-rooted (start at "/"). Pass through.
    path)

  (-physical-path [_ sandbox-path]
    ;; VFS has no real-disk path — identity for protocol conformance.
    ;; Consumers that need a real cwd for OS spawn (run-external) only
    ;; use this when the FS is disk-backed; for VFS the value is moot.
    sandbox-path))

(defn- coerce-content
  "Accept legacy `:bytes` keys as well as the canonical `:content`,
   and JVM byte-arrays for callers that still hand them in. Return
   a Clojure string."
  [^String _path v]
  (cond
    (nil? v) ""
    (string? v) v
    #?@(:clj [(bytes? v) (String. ^bytes v "UTF-8")])
    :else (str v)))

(defn make
  "Construct a virtual FS pre-populated with `entries`, a map of
   absolute-path → either:

     :dir                       ; bare keyword
     {:type :dir}
     {:type :file :content STR}
     STR                        ; sugar for `{:type :file :content STR}`

   The map is normalised internally to the entry shape -stat expects.
   Parent directories are auto-created.

   Options:
     :cwd  initial cwd, default `/`."
  ([] (make {} {}))
  ([entries] (make entries {}))
  ([entries {:keys [cwd] :or {cwd "/"}}]
   (let [now (mtime)
         norm-entries
         (reduce-kv
          (fn [acc path v]
            (let [entry (cond
                          (= :dir v)
                          {:type :dir :mtime-ms now}

                          (map? v)
                          (case (:type v)
                            :dir     {:type :dir :mtime-ms (or (:mtime-ms v) now)}
                            :symlink {:type :symlink
                                      :target (:target v)
                                      :mtime-ms (or (:mtime-ms v) now)}
                            :file    {:type :file
                                      :content  (coerce-content path
                                                                (or (:content v) (:bytes v)))
                                      :mtime-ms (or (:mtime-ms v) now)
                                      :perms-mode (:perms-mode v)}
                            ;; default: file
                            {:type :file
                             :content  (coerce-content path
                                                       (or (:content v) (:bytes v)))
                             :mtime-ms (or (:mtime-ms v) now)})

                          (string? v)
                          {:type :file :content v :mtime-ms now}

                          :else
                          (throw (ex-info "Invalid virtual FS entry"
                                          {:path path :value v})))]
              (assoc acc path entry)))
          {}
          entries)
         ;; Auto-create intermediate dirs.
         with-dirs (reduce
                    (fn [acc path]
                      (loop [segs (drop-last (fs/split-path path))
                             acc  acc]
                        (let [dirpath (fs/join-path "" (filter seq segs))
                              dirpath (if (= "" dirpath) "/" dirpath)]
                          (cond
                            (contains? acc dirpath) acc
                            (= "/" dirpath)         (assoc acc "/" {:type :dir :mtime-ms now})
                            :else (recur (butlast segs)
                                         (assoc acc dirpath {:type :dir :mtime-ms now}))))))
                    norm-entries
                    (keys norm-entries))
         all-entries (if (contains? with-dirs "/")
                       with-dirs
                       (assoc with-dirs "/" {:type :dir :mtime-ms now}))]
     (->VirtualFS (atom all-entries) (atom cwd)))))

;; ============================================================================
;; Serialisation: send / restore a VFS snapshot between instances
;; ============================================================================

(defn snapshot
  "Return a serialisable plain-data view of the FS state:

     {:entries {path → entry} :cwd path}

   The result is pure persistent Clojure data — `pr-str`, EDN write,
   Transit, anything that can serialise nested maps + strings will
   round-trip it. No byte-arrays, no atoms, no live references."
  [^VirtualFS fs]
  {:entries @(:entries-atom fs)
   :cwd     @(:cwd-atom fs)})

(defn restore
  "Re-hydrate a VirtualFS from a snapshot map (the result of
   `snapshot`). The new VFS shares no mutable state with anything
   that previously held the snapshot — write the snapshot to a file,
   read it back later, restore it, and you have an independent fork."
  [{:keys [entries cwd]}]
  (->VirtualFS (atom (or entries {"/" {:type :dir :mtime-ms (mtime)}}))
               (atom (or cwd "/"))))
