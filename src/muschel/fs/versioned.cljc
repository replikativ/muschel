(ns muschel.fs.versioned
  "Version-recording decorator for any `muschel.fs/FS`.

   Tree/path semantics remain with the wrapped FS. Successful file writes are
   committed once to `muschel.version-store`, and reads of versioned paths are
   reconstructed from that store. This makes the wrapper a small stand-in for
   dvergr's Datahike tree + konserve payload integration."
  (:require [muschel.fs :as fs]
            [muschel.version-store :as version-store]))

(defn- text-bytes [text]
  #?(:clj (.getBytes ^String text "UTF-8")
     :cljs text))

(defrecord VersionedFS [inner store]
  fs/FS
  (-resolve [_ path] (fs/-resolve inner path))
  (-cwd [_] (fs/-cwd inner))
  (-cd! [_ path] (fs/-cd! inner path))
  (-exists? [_ path] (fs/-exists? inner path))
  (-stat [_ path] (fs/-stat inner path))
  (-list-dir [_ path] (fs/-list-dir inner path))

  (-read-file [_ path]
    (when-let [resolved (fs/-resolve inner path)]
      (if (version-store/head store resolved)
        (version-store/read-head store resolved)
        (fs/-read-file inner resolved))))

  (-read-bytes [this path]
    (some-> (fs/-read-file this path) text-bytes))

  (-open-source [this path]
    #?(:clj
       (some-> (fs/-read-file this path)
               text-bytes
               java.io.ByteArrayInputStream.)
       :cljs (fs/-read-file this path)))

  (-open-sink [_ path append?]
    (when-let [resolved (fs/-resolve inner path)]
      (let [initial (if append?
                      (or (version-store/read-head store resolved)
                          (fs/-read-file inner resolved)
                          "")
                      "")
            acc (atom initial)
            closed? (atom false)]
        {:acc acc
         :closed? closed?
         :commit!
         (fn [text]
           ;; The tree/file mutation must succeed before its version head moves.
           (when (fs/write-string! inner resolved text false)
             (version-store/commit-text! store resolved text)
             true))})))

  (-mkdir [_ path] (fs/-mkdir inner path))

  (-delete [_ path]
    (when-let [resolved (fs/-resolve inner path)]
      (when (fs/-delete inner resolved)
        (version-store/remove-heads-under! store resolved)
        true)))

  (-rename [_ from to]
    (let [from-resolved (fs/-resolve inner from)
          to-resolved (fs/-resolve inner to)]
      (when (and from-resolved to-resolved
                 (fs/-rename inner from-resolved to-resolved))
        (version-store/move-head! store from-resolved to-resolved)
        true)))

  (-touch [_ path]
    (when-let [resolved (fs/-resolve inner path)]
      (let [existed? (fs/-exists? inner resolved)]
        (when (fs/-touch inner resolved)
          (when-not existed?
            (version-store/commit-text! store resolved ""))
          true))))

  (-chmod [_ path mode] (fs/-chmod inner path mode))
  (-symlink [_ target link-path] (fs/-symlink inner target link-path))
  (-chown [_ path owner group] (fs/-chown inner path owner group))
  (-sandbox-relativize [_ path] (fs/-sandbox-relativize inner path))
  (-physical-path [_ path] (fs/-physical-path inner path)))

(defn make
  "Wrap `inner`; creates an experimental store when one is not supplied."
  ([inner] (make inner (version-store/make-store)))
  ([inner store] (->VersionedFS inner store)))
