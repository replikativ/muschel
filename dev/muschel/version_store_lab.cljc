(ns muschel.version-store-lab
  "Cross-runtime storage/reconstruction bake-off for version representations.

   JVM:  clojure -M:version-lab
   CLJS: npx shadow-cljs compile version-lab && node out/version-lab.js"
  (:require [clojure.string :as str]
            [muschel.version-store :as version-store]))

(defn- now-nanos []
  #?(:clj (System/nanoTime)
     :cljs (* 1000000 (.now js/performance))))

(defn- elapsed-ms [start] (/ (- (now-nanos) start) 1000000.0))

(defn- corpus [line-count version-count]
  (loop [i 0
         lines (mapv #(str "line-" % " stable source text") (range line-count))
         texts []]
    (if (= i version-count)
      texts
      (let [line-index (mod (* i 97) line-count)
            lines' (assoc lines line-index
                          (str "line-" line-index " edited-at-version-" i))]
        (recur (inc i) lines' (conj texts (str (str/join "\n" lines') "\n")))))))

(defn- run-strategy [texts representation]
  (let [store (version-store/make-store {:representation representation
                                         :max-delta-depth 8})
        started (now-nanos)
        committed (mapv #(version-store/commit-text! store "/corpus" %) texts)
        write-ms (elapsed-ms started)
        _ (version-store/clear-reconstruction-cache! store)
        read-start (now-nanos)
        reconstructed (mapv #(version-store/read-version store (:version/id %)) committed)
        read-all-ms (elapsed-ms read-start)
        _ (version-store/clear-reconstruction-cache! store)
        head-start (now-nanos)
        _ (version-store/read-head store "/corpus")
        read-head-ms (elapsed-ms head-start)
        stats (version-store/storage-stats store)]
    (when-not (= texts reconstructed)
      (throw (ex-info "storage strategy failed roundtrip"
                      {:representation representation})))
    (merge {:representation representation
            :write-ms write-ms
            :read-all-ms read-all-ms
            :read-head-ms read-head-ms
            :kind-counts (frequencies
                          (map #(get-in % [:version/representation :kind]) committed))
            :storage-ratio (double (/ (:stored-bytes stats)
                                      (max 1 (:logical-bytes stats))))}
           stats)))

(defn main [& _]
  (let [texts (corpus 1000 100)]
    (println
     (pr-str
      {:runtime #?(:clj :jvm :cljs :node)
       :corpus {:versions (count texts) :lines-per-version 1000}
       :results (mapv #(run-strategy texts %)
                      [:full :line-delta :copy-insert :chunks :auto])}))))

(defn -main [& args] (apply main args))

#?(:cljs (set! *main-cli-fn* -main))
