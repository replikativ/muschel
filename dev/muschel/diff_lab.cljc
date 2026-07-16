(ns muschel.diff-lab
  "Small reproducible benchmark/oracle harness for the experimental diff core.

   JVM:  clojure -M:diff-lab
   CLJS: npx shadow-cljs compile diff-lab && node out/diff-lab.js

   This is intentionally dependency-free. It is a smoke benchmark, not a JMH
   substitute; its stable cases are suitable for comparing JVM and V8 and for
   catching algorithmic regressions."
  (:require [clojure.string :as str]
            [muschel.diff :as diff]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clojure.java.shell :as shell])))

(defn- now-nanos []
  #?(:clj (System/nanoTime)
     :cljs (* 1000000 (.now js/performance))))

(defn- elapsed-ms [started]
  (/ (- (now-nanos) started) 1000000.0))

(defn- changed-counts [result]
  (reduce (fn [{:keys [added deleted]} {:keys [op a-count b-count]}]
            {:added (+ added (if (= :insert op) b-count 0))
             :deleted (+ deleted (if (= :delete op) a-count 0))})
          {:added 0 :deleted 0}
          (:edits result)))

(defn- benchmark-case [{:keys [name a b]}]
  ;; Warm the exact shape once; report the best of five to reduce scheduler and
  ;; GC noise without pretending this is a statistically complete benchmark.
  (diff/diff-lines a b)
  (let [runs (for [_ (range 5)]
               (let [started (now-nanos)
                     result (diff/diff-lines a b)]
                 {:ms (elapsed-ms started) :result result}))
        best (apply min-key :ms runs)]
    (merge {:case name
            :a-lines (count a)
            :b-lines (count b)
            :best-ms (:ms best)}
           (changed-counts (:result best)))))

(defn- cases []
  (let [similar (fn [n]
                  (let [a (mapv #(str "line-" %) (range n))]
                    {:name (str "one-change-" n)
                     :a a
                     :b (assoc a (quot n 2) "changed")}))]
    [(similar 100)
     (similar 1000)
     (similar 10000)
     (similar 100000)
     {:name "unrelated-1000"
      :a (mapv #(str "old-" %) (range 1000))
      :b (mapv #(str "new-" %) (range 1000))}]))

#?(:clj
   (defn- git-oracle [{:keys [a b]}]
     (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                         "muschel-diff-oracle" (make-array java.nio.file.attribute.FileAttribute 0)))
           af (io/file dir "a")
           bf (io/file dir "b")]
       (try
         (spit af (str (str/join "\n" a) "\n"))
         (spit bf (str (str/join "\n" b) "\n"))
         (let [run-git (fn [& args]
                         (let [{:keys [exit out err]}
                               (apply shell/sh "git" "diff" "--no-index"
                                      "--no-renames" args)]
                           (when-not (#{0 1} exit)
                             (throw (ex-info "native Git oracle failed"
                                             {:exit exit :err err :args args})))
                           out))
               numstat (run-git "--numstat" (.getPath af) (.getPath bf))
               patch (run-git "--diff-algorithm=myers" "--no-indent-heuristic"
                              "--unified=1000000" (.getPath af) (.getPath bf))
               counts (if (str/blank? numstat)
                        {:added 0 :deleted 0}
                        (let [[added deleted]
                              (str/split (first (str/split-lines numstat)) #"\t")]
                          {:added (parse-long added) :deleted (parse-long deleted)}))
               body (next (drop-while #(not (str/starts-with? % "@@"))
                                      (str/split-lines patch)))
               operations (->> body
                               (remove #(str/starts-with? % "\\ No newline"))
                               (keep (fn [line]
                                       (case (first line)
                                         \space [:keep (subs line 1)]
                                         \- [:del (subs line 1)]
                                         \+ [:add (subs line 1)]
                                         nil)))
                               vec)]
           (assoc counts :operations operations))
         (finally
           (.delete af)
           (.delete bf)
           (.delete dir))))))

#?(:clj
   (defn- verify-git-counts! [case]
     (let [result (diff/diff-lines (:a case) (:b case))
           ours (assoc (changed-counts result) :operations (diff/operations result))
           git (git-oracle case)]
       (when-not (= ours git)
         (throw (ex-info "diff disagrees with native Git"
                         {:case (:name case)
                          :ours (update ours :operations #(take 20 %))
                          :git (update git :operations #(take 20 %))})))
       true)))

(defn main [& _]
  (let [inputs (cases)]
    #?(:clj (doseq [case inputs] (verify-git-counts! case)))
    (println (pr-str {:runtime #?(:clj :jvm :cljs :node)
                      :git-oracle #?(:clj :passed :cljs :not-run)
                      :results (mapv benchmark-case inputs)}))))

(defn -main [& args] (apply main args))

#?(:cljs (set! *main-cli-fn* -main))
