(ns muschel.fs.virtual
  "In-memory FS for tests and the browser playground.

   Underlying state: an atom holding {path-string → entry-map}, where
   entry is one of:

     {:type :dir}
     {:type :file :bytes <byte-array-or-string> :mtime-ms long}

   The 'root' is implicit — paths use Unix-style absolute strings,
   typically `/`. There's no external filesystem to escape to, so
   containment is structural: anything not in the map doesn't exist.
   Used primarily for unit tests of the builtin host + commands, and
   as the storage backend for a browser playground.

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
         :size     (cond
                     (= :dir (:type entry)) 0
                     (string? (:bytes entry)) (count (:bytes entry))
                     :else (alength ^bytes (:bytes entry)))
         :mtime-ms (or (:mtime-ms entry) 0)
         :perms    nil})))

  (-list-dir [this path]
    (when-let [resolved (fs/-resolve this path)]
      (when (= :dir (:type (get @entries-atom resolved)))
        (let [prefix (str (str/replace resolved #"/$" "") "/")
              all   @entries-atom]
          (->> all
               (filter (fn [[p _]]
                         (and (str/starts-with? p prefix)
                              ;; Direct children only — no nested.
                              (not (str/includes? (subs p (count prefix)) "/")))))
               (mapv (fn [[p e]]
                       (let [name (subs p (count prefix))]
                         {:name name
                          :type (:type e)
                          :size (cond
                                  (= :dir (:type e)) 0
                                  (string? (:bytes e)) (count (:bytes e))
                                  :else (alength ^bytes (:bytes e)))
                          :mtime-ms (or (:mtime-ms e) 0)})))
               (sort-by :name))))))

  (-read-file [this path]
    (when-let [bs (fs/-read-bytes this path)]
      (if (string? bs) bs (String. ^bytes bs "UTF-8"))))

  (-read-bytes [this path]
    (when-let [resolved (fs/-resolve this path)]
      (let [entry (get @entries-atom resolved)]
        (when (= :file (:type entry))
          (:bytes entry))))))

(defn make
  "Construct a virtual FS pre-populated with `entries`, a map of
   absolute-path → either:

     :dir                       ; bare keyword
     {:type :dir}
     {:type :file :content str-or-bytes}

   The map is normalised internally to the entry shape -stat expects.
   Parent directories are auto-created.

   Options:
     :cwd  initial cwd, default `/`."
  ([] (make {} {}))
  ([entries] (make entries {}))
  ([entries {:keys [cwd] :or {cwd "/"}}]
   (let [now (mtime)
         ;; Normalise entries into the internal shape.
         norm-entries (reduce-kv
                       (fn [acc path v]
                         (let [entry (cond
                                       (= :dir v)
                                       {:type :dir :mtime-ms now}

                                       (map? v)
                                       (case (:type v)
                                         :dir  {:type :dir :mtime-ms (or (:mtime-ms v) now)}
                                         :file {:type :file
                                                :bytes (or (:bytes v) (:content v) "")
                                                :mtime-ms (or (:mtime-ms v) now)}
                                          ;; default to file
                                         {:type :file
                                          :bytes (or (:bytes v) (:content v) "")
                                          :mtime-ms (or (:mtime-ms v) now)})

                                       (string? v)
                                       {:type :file :bytes v :mtime-ms now}

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
