(ns muschel.version-store
  "Experimental content-addressed, delta-capable text version store.

   Metadata is held behind atoms for the Muschel experiment; its values are
   deliberately plain maps shaped to move into Datahike. Immutable payloads sit
   behind `PayloadStore`, the seam intended for konserve."
  (:require [clojure.string :as str]
            [muschel.diff :as diff]
            #?(:cljs [goog.crypt :as gcrypt])
            #?(:cljs [goog.crypt.Sha256]))
  #?(:clj (:import [java.security MessageDigest])))

(defprotocol PayloadStore
  (-payload-put! [store id value byte-size])
  (-payload-get [store id])
  (-payload-delete! [store id])
  (-payload-entries [store]))

(defrecord MemoryPayloadStore [state]
  PayloadStore
  (-payload-put! [_ id value byte-size]
    (let [added? (volatile! false)]
      (swap! state
             (fn [entries]
               (if (contains? entries id)
                 entries
                 (do (vreset! added? true)
                     (assoc entries id {:value value :bytes byte-size})))))
      {:id id :added? @added? :bytes byte-size}))
  (-payload-get [_ id] (get-in @state [id :value]))
  (-payload-delete! [_ id] (swap! state dissoc id) true)
  (-payload-entries [_] @state))

(defn memory-payload-store [] (->MemoryPayloadStore (atom {})))

(defn- utf8-size [s]
  #?(:clj (alength (.getBytes ^String s "UTF-8"))
     :cljs (count (gcrypt/stringToUtf8ByteArray s))))

(defn sha256
  "Portable lowercase SHA-256 hex for a string."
  [s]
  #?(:clj
     (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                           (.getBytes ^String s "UTF-8"))]
       (apply str (map #(format "%02x" (bit-and % 0xff)) digest)))
     :cljs
     (let [digest (goog.crypt.Sha256.)]
       (.update digest (gcrypt/stringToUtf8ByteArray s))
       (gcrypt/byteArrayToHex (.digest digest)))))

(defn- encoded [value]
  (if (string? value) value (pr-str value)))

(defn- payload-info [payload-store kind value]
  (let [serialized (encoded value)
        id (sha256 (str (name kind) "\u0000" serialized))]
    {:id id
     :bytes (utf8-size serialized)
     :exists? (contains? (-payload-entries payload-store) id)}))

(defn- put-payload! [payload-store kind value]
  (let [{:keys [id bytes]} (payload-info payload-store kind value)]
    (-payload-put! payload-store id value bytes)))

(defrecord VersionStore [payload-store versions heads reconstruction-cache config])

(defn make-store
  "Create a version store.

   Config:
   - `:representation` `:auto` (default), `:full`, `:line-delta`,
     `:copy-insert`, or `:chunks`
   - `:max-delta-depth` default 8
   - `:delta-ratio` store a delta only below this fraction of full size (0.8)."
  ([] (make-store nil))
  ([config]
   (->VersionStore (or (:payload-store config) (memory-payload-store))
                   (atom {})
                   (atom {})
                   (atom {})
                   (merge {:representation :auto
                           :max-delta-depth 8
                           :delta-ratio 0.8
                           :copy-line-slack 1.15
                           :chunk-delta-slack 1.15
                           :chunk-full-slack 1.2
                           :chunk-min-bytes 8192
                           :reconstruction-cache-size 32}
                          (dissoc config :payload-store)))))

(defn version [store version-id] (get @(:versions store) version-id))
(defn head [store path] (get @(:heads store) path))
(defn versions [store] @(:versions store))

(defn- line-delta [base target]
  (let [result (diff/diff-text base target)]
    {:kind :line-delta
     :final-newline? (:b-final-newline? result)
     :edits
     (mapv (fn [{:keys [op b-start b-count] :as edit}]
             (cond-> (select-keys edit [:op :a-start :a-count :b-start :b-count])
               (= :insert op)
               (assoc :lines (subvec (:b-lines result)
                                     b-start (+ b-start b-count)))))
           (:edits result))}))

(defn- apply-line-delta [base {:keys [edits final-newline?]}]
  (let [base-lines (:lines (diff/text-lines base))
        target-lines
        (persistent!
         (reduce (fn [out {:keys [op a-start a-count lines]}]
                   (case op
                     :equal (reduce conj! out
                                    (subvec base-lines a-start (+ a-start a-count)))
                     :delete out
                     :insert (reduce conj! out lines)))
                 (transient []) edits))]
    (str (str/join "\n" target-lines)
         (when final-newline? "\n"))))

(def ^:private copy-block-size 16)

(declare stable-line-hash)

(defn- copy-insert-delta [base target]
  ;; Git pack deltas are byte-oriented; this experimental text variant uses
  ;; UTF-16 character offsets, which are identical on JVM String and JS string.
  ;; The representation has the same essential COPY/INSERT instruction shape.
  (let [base-count (count base)
        target-count (count target)
        index
        (reduce (fn [idx offset]
                  (let [fingerprint (stable-line-hash
                                     (subs base offset (+ offset copy-block-size)))]
                    (update idx fingerprint
                            (fn [positions]
                              (if (< (count positions) 64)
                                (conj (or positions []) offset)
                                positions)))))
                {}
                (range 0 (inc (- base-count copy-block-size)) copy-block-size))]
    (loop [i 0 insert-start 0 ops []]
      (if (>= i target-count)
        {:kind :copy-insert
         :ops (cond-> ops
                (< insert-start target-count)
                (conj {:insert (subs target insert-start target-count)}))}
        (let [candidates
              (when (<= (+ i copy-block-size) target-count)
                (get index (stable-line-hash
                            (subs target i (+ i copy-block-size)))))
              [best-offset best-length]
              (reduce
               (fn [[best-o best-l] offset]
                 (let [length
                       (loop [n 0]
                         (if (and (< (+ offset n) base-count)
                                  (< (+ i n) target-count)
                                  (= (.charAt ^String base (+ offset n))
                                     (.charAt ^String target (+ i n))))
                           (recur (inc n))
                           n))]
                   (if (> length best-l) [offset length] [best-o best-l])))
               [nil 0] candidates)]
          (if (>= best-length copy-block-size)
            (recur (+ i best-length)
                   (+ i best-length)
                   (cond-> ops
                     (< insert-start i) (conj {:insert (subs target insert-start i)})
                     true (conj {:copy [best-offset best-length]})))
            (recur (inc i) insert-start ops)))))))

(defn- apply-copy-insert [base {:keys [ops]}]
  (apply str
         (map (fn [{:keys [insert copy]}]
                (if insert
                  insert
                  (let [[offset length] copy]
                    (subs base offset (+ offset length)))))
              ops)))

(defn- stable-line-hash [line]
  ;; Stable across CLJ/CLJS. This is only a chunk-boundary fingerprint; content
  ;; identity still uses SHA-256.
  (loop [i 0 h 5381]
    (if (= i (count line))
      h
      (let [code #?(:clj (int (.charAt ^String line i))
                    :cljs (.charCodeAt line i))]
        (recur (inc i) (mod (+ (* h 33) code) 2147483647))))))

(defn- text-segments [text]
  (let [{:keys [lines final-newline?]} (diff/text-lines text)
        last-index (dec (count lines))]
    (mapv (fn [i line]
            (str line (when (or (< i last-index) final-newline?) "\n")))
          (range) lines)))

(defn- content-defined-chunks [text]
  ;; Natural boundaries depend on the current line, not the preceding chunk, so
  ;; an insertion disturbs at most the chunks up to the next natural boundary.
  (loop [segments (seq (text-segments text))
         line-count 0
         chunk ""
         out []]
    (if-let [segment (first segments)]
      (let [chunk' (str chunk segment)
            count' (inc line-count)
            boundary? (or (>= count' 64)
                          (and (>= count' 8)
                               (zero? (mod (stable-line-hash segment) 32))))]
        (if boundary?
          (recur (next segments) 0 "" (conj out chunk'))
          (recur (next segments) count' chunk' out)))
      (cond-> out (seq chunk) (conj chunk)))))

(declare read-version)

(defn- chunks-incremental-bytes [store chunks]
  (let [payload-store (:payload-store store)
        infos (mapv #(payload-info payload-store :chunk %) chunks)
        unique-infos (vals (into {} (map (juxt :id identity)) infos))
        chunk-ids (mapv :id infos)
        manifest-info (payload-info payload-store :chunk-manifest
                                    {:chunks chunk-ids})]
    (+ (reduce + (map #(if (:exists? %) 0 (:bytes %)) unique-infos))
       (if (:exists? manifest-info) 0 (:bytes manifest-info)))))

(defn- choose-representation [store parent-id text requested]
  (let [{:keys [max-delta-depth delta-ratio copy-line-slack chunk-delta-slack
                chunk-full-slack chunk-min-bytes]} (:config store)
        parent (version store parent-id)
        base (when parent (read-version store parent-id))
        full-bytes (utf8-size text)
        delta (when base (line-delta base text))
        delta-bytes (when delta (utf8-size (pr-str delta)))
        copy-delta (when base (copy-insert-delta base text))
        copy-bytes (when copy-delta (utf8-size (pr-str copy-delta)))
        prefer-copy? (and copy-delta
                          (<= copy-bytes (* copy-line-slack delta-bytes)))
        best-delta (if prefer-copy? copy-delta delta)
        best-delta-bytes (if prefer-copy? copy-bytes delta-bytes)
        chunks (when (#{:chunks :auto} requested) (content-defined-chunks text))
        chunk-bytes (when chunks (chunks-incremental-bytes store chunks))
        delta-allowed? (and parent
                            (< (:version/delta-depth parent 0) max-delta-depth))]
    (case requested
      :full {:kind :full :value text :delta-depth 0}
      :chunks {:kind :chunks :value chunks :delta-depth 0}
      :line-delta (if delta-allowed?
                    {:kind :line-delta :value delta
                     :delta-depth (inc (:version/delta-depth parent 0))}
                    {:kind :full :value text :delta-depth 0})
      :copy-insert (if delta-allowed?
                     {:kind :copy-insert :value copy-delta
                      :delta-depth (inc (:version/delta-depth parent 0))}
                     {:kind :full :value text :delta-depth 0})
      ;; :auto: chunks win when they are close to the smallest line delta,
      ;; because they reconstruct directly without a base-chain walk.
      (cond
        (and (nil? parent)
             (>= full-bytes chunk-min-bytes)
             (<= chunk-bytes (* chunk-full-slack full-bytes)))
        {:kind :chunks :value chunks :delta-depth 0}

        (and chunks
             (< chunk-bytes full-bytes)
             (or (not delta-allowed?)
                 (<= chunk-bytes (* chunk-delta-slack best-delta-bytes))))
        {:kind :chunks :value chunks :delta-depth 0}

        (and delta-allowed?
             (< best-delta-bytes (* delta-ratio (max 1 full-bytes))))
        {:kind (:kind best-delta) :value best-delta
         :delta-depth (inc (:version/delta-depth parent 0))}

        :else
        {:kind :full :value text :delta-depth 0}))))

(defn- persist-representation! [store {:keys [kind value delta-depth]} parent-id]
  (case kind
    :full
    (let [{:keys [id]} (put-payload! (:payload-store store) :full value)]
      {:kind :full :payload-id id :delta-depth delta-depth})

    :line-delta
    (let [{:keys [id]} (put-payload! (:payload-store store) :line-delta value)]
      {:kind :line-delta :payload-id id :base-version parent-id
       :delta-depth delta-depth})

    :copy-insert
    (let [{:keys [id]} (put-payload! (:payload-store store) :copy-insert value)]
      {:kind :copy-insert :payload-id id :base-version parent-id
       :delta-depth delta-depth})

    :chunks
    (let [chunk-ids (mapv (fn [chunk]
                            (:id (put-payload! (:payload-store store) :chunk chunk)))
                          value)
          manifest {:chunks chunk-ids}
          {:keys [id]} (put-payload! (:payload-store store) :chunk-manifest manifest)]
      {:kind :chunks :payload-id id :delta-depth 0})))

(defn commit-text!
  "Commit `text` as the new head of `path`; return the explicit version map.
   Identical content is still a version event, while its payload deduplicates."
  ([store path text] (commit-text! store path text nil))
  ([store path text opts]
   (let [parent-id (head store path)
         requested (or (:representation opts)
                       (:representation (:config store)))
         choice (choose-representation store parent-id text requested)
         representation (persist-representation! store choice parent-id)
         version-id (str (random-uuid))
         value {:version/id version-id
                :version/path path
                :version/content-id (sha256 text)
                :version/parent parent-id
                :version/representation (dissoc representation :delta-depth)
                :version/delta-depth (:delta-depth representation)
                :version/size (utf8-size text)}]
     (swap! (:versions store) assoc version-id value)
     (swap! (:heads store) assoc path version-id)
     value)))

(defn read-version
  "Reconstruct and hash-validate a version's complete text."
  [store version-id]
  (if (contains? @(:reconstruction-cache store) version-id)
    (get @(:reconstruction-cache store) version-id)
    (when-let [{:version/keys [content-id representation] :as v}
               (version store version-id)]
      (let [{:keys [kind payload-id base-version]} representation
            payload (-payload-get (:payload-store store) payload-id)
            text (case kind
                   :full payload
                   :line-delta (apply-line-delta (read-version store base-version) payload)
                   :copy-insert (apply-copy-insert (read-version store base-version) payload)
                   :chunks (apply str
                                  (map #(-payload-get (:payload-store store) %)
                                       (:chunks payload))))]
        (when-not (= content-id (sha256 text))
          (throw (ex-info "version content hash mismatch"
                          {:version-id version-id :expected content-id
                           :actual (sha256 text) :version v})))
        (let [limit (:reconstruction-cache-size (:config store))]
          (when (pos? limit)
            (swap! (:reconstruction-cache store)
                   (fn [cache]
                     (let [cache (if (and (>= (count cache) limit)
                                          (not (contains? cache version-id)))
                                   (dissoc cache (first (keys cache)))
                                   cache)]
                       (assoc cache version-id text))))))
        text))))

(defn clear-reconstruction-cache! [store]
  (reset! (:reconstruction-cache store) {})
  true)

(defn read-head [store path] (some->> (head store path) (read-version store)))

(defn move-head!
  "Move one file head or an entire path prefix (directory rename)."
  [store from to]
  (let [prefix (str (str/replace from #"/+$" "") "/")
        moved (into {}
                    (keep (fn [[path version-id]]
                            (cond
                              (= path from) [to version-id]
                              (str/starts-with? path prefix)
                              [(str to (subs path (count from))) version-id])))
                    @(:heads store))]
    (when (seq moved)
      (swap! (:heads store)
             (fn [heads]
               (let [old-paths (filter #(or (= % from)
                                            (str/starts-with? % prefix))
                                       (keys heads))]
                 (into (apply dissoc heads old-paths) moved))))
      true)))

(defn remove-head! [store path]
  (let [present? (contains? @(:heads store) path)]
    (swap! (:heads store) dissoc path)
    present?))

(defn remove-heads-under!
  "Remove the head at `path` and every descendant head."
  [store path]
  (let [prefix (str (str/replace path #"/+$" "") "/")
        doomed (filter #(or (= % path) (str/starts-with? % prefix))
                       (keys @(:heads store)))]
    (when (seq doomed)
      (swap! (:heads store) #(apply dissoc % doomed))
      true)))

(defn reachable-version-ids
  "Parent closure from explicit roots (defaults to all current path heads)."
  ([store] (reachable-version-ids store (vals @(:heads store))))
  ([store roots]
   (loop [pending (seq roots) seen #{}]
     (if-let [id (first pending)]
       (if (contains? seen id)
         (recur (next pending) seen)
         (recur (cond-> (next pending)
                  (:version/parent (version store id))
                  (conj (:version/parent (version store id))))
                (conj seen id)))
       seen))))

(defn gc!
  "Delete versions and immutable payloads unreachable from `roots`."
  ([store] (gc! store (vals @(:heads store))))
  ([store roots]
   (let [reachable (reachable-version-ids store roots)
         live-versions (select-keys @(:versions store) reachable)
         top-payloads (into #{} (map #(get-in % [:version/representation :payload-id]))
                            (vals live-versions))
         chunk-payloads
         (into #{}
               (mapcat (fn [id]
                         (let [payload (-payload-get (:payload-store store) id)]
                           (or (:chunks payload) []))))
               top-payloads)
         live-payloads (into top-payloads chunk-payloads)
         all-payloads (set (keys (-payload-entries (:payload-store store))))
         dead-payloads (remove live-payloads all-payloads)
         dead-versions (remove reachable (keys @(:versions store)))]
     (swap! (:versions store) #(select-keys % reachable))
     (swap! (:reconstruction-cache store) #(select-keys % reachable))
     (doseq [id dead-payloads] (-payload-delete! (:payload-store store) id))
     {:deleted-versions (count dead-versions)
      :deleted-payloads (count dead-payloads)})))

(defn storage-stats [store]
  (let [entries (-payload-entries (:payload-store store))]
    {:versions (count @(:versions store))
     :heads (count @(:heads store))
     :payloads (count entries)
     :stored-bytes (reduce + (map :bytes (vals entries)))
     :logical-bytes (reduce + (map :version/size (vals @(:versions store))))}))
