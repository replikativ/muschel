(ns git-corpus
  "Stream Claude Code transcripts and measure Git argv shapes.

  Usage:
    clojure -M tools/git_corpus.clj [TRANSCRIPT_ROOT] [MAX_FILES] [OUTPUT]

  The report is EDN so compatibility tooling can consume the same artifact as
  humans and CI. No transcript content is retained in the repository."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [geschichte.git.compatibility :as compatibility]
            [geschichte.git.command :as git-command]
            [jsonista.core :as json]
            [muschel.ast :as ast]
            [muschel.parse :as parse]))

(def mapper (json/object-mapper {:decode-key-fn true}))

(defn- static-part [part]
  (case (:type part)
    (:lit :squoted :escape) (:value part)
    :dquoted (let [parts (map static-part (:parts part))]
               (when (every? some? parts) (apply str parts)))
    :tilde (str "~" (:user part))
    :brace-exp (:raw part)
    :ansi-c-quoted (:raw part)
    :var-ref "<dynamic>"
    :cmd-subst "<dynamic>"
    :arith "<dynamic>"
    nil))

(defn- static-word [word]
  (let [parts (map static-part (:parts word))]
    (when (every? some? parts) (apply str parts))))

(defn- call-argv [call]
  (mapv #(or (static-word %) "<dynamic>") (:args call)))

(defn- nested-substitutions [node]
  (let [bodies (atom [])]
    (ast/walk node
              (fn [part]
                (when (= :cmd-subst (:type part))
                  (swap! bodies conj (:body part)))))
    @bodies))

(declare git-argvs)

(defn- parsed-git-argvs [source]
  (let [tree (parse/parse source)
        direct (->> (ast/leaf-calls tree)
                    (map call-argv)
                    (filter #(= "git" (first %))))
        nested (mapcat #(-> (git-argvs %) :argvs)
                       (nested-substitutions tree))]
    (into (vec direct) nested)))

(defn- fallback-git-argvs [source]
  ;; Parse failures still count against coverage. This deliberately conservative
  ;; fallback captures the subcommand and simple whitespace-delimited tail; the
  ;; report records that the shape came from fallback extraction.
  (mapv (fn [[_ tail]]
          (into ["git"] (str/split (str/trim tail) #"\s+")))
        (re-seq #"(?m)(?:^|[;&|()]\s*)git\s+([^\n;&|()]*)" source)))

(defn git-argvs [source]
  (try
    {:argvs (parsed-git-argvs source) :parsed? true}
    (catch Throwable _
      {:argvs (fallback-git-argvs source) :parsed? false})))

(defn- bash-commands [line]
  (try
    (let [entry (json/read-value line mapper)]
      (->> (get-in entry [:message :content])
           (keep (fn [content]
                   (when (= "Bash" (:name content))
                     (get-in content [:input :command]))))))
    (catch Throwable _ [])))

(defn- git-command-and-args [argv]
  (try
    (let [{:keys [args]} (git-command/parse-global (subvec argv 1))]
      [(first args) (subvec args (min 1 (count args)))])
    (catch Throwable _ [nil []])))

(def action-operands
  {"stash" #{"push" "pop" "apply" "list" "show" "drop" "clear"
             "create" "store"}
   "remote" #{"add" "remove" "rm" "rename" "get-url" "set-url"
              "show" "prune" "update"}
   "worktree" #{"add" "list" "lock" "move" "prune" "remove" "repair"
                "unlock"}
   "reflog" #{"show" "expire" "delete" "exists" "write"}
   "tag" #{"delete" "verify"}})

(defn- normalized-arg [command arg]
  (cond
    (= "--" arg) "--"
    (re-matches #"-[0-9]+" arg) "-<n>"
    (str/starts-with? arg "--")
    (if-let [index (str/index-of arg "=")]
      (str (subs arg 0 index) "=<value>") arg)
    (and (str/starts-with? arg "-") (not= "-" arg)) arg
    (contains? (get action-operands command #{}) arg) arg
    :else "<arg>"))

(defn- shape [argv]
  (let [[command args] (git-command-and-args argv)]
    (when command
      (into [command] (map #(normalized-arg command %)) args))))

(defn- transcript-files [root max-files]
  (cond->> (file-seq (io/file root))
    true (filter #(and (.isFile %) (str/ends-with? (.getName %) ".jsonl")))
    max-files (take max-files)))

(defn analyze [root max-files]
  (let [stats (atom {:files 0 :bash-inputs 0 :git-invocations 0
                     :git-input-parse-failures 0
                     :commands {} :shapes {}})]
    (doseq [file (transcript-files root max-files)]
      (swap! stats update :files inc)
      (with-open [reader (io/reader file)]
        (doseq [line (line-seq reader)
                source (bash-commands line)]
          (swap! stats update :bash-inputs inc)
          (let [{:keys [argvs parsed?]} (git-argvs source)]
            (when (and (not parsed?) (seq argvs))
              (swap! stats update :git-input-parse-failures inc))
            (doseq [argv argvs
                    :let [[command] (git-command-and-args argv)
                          shape (shape argv)]
                    :when command]
              (swap! stats update :git-invocations inc)
              (swap! stats update-in [:commands command] (fnil inc 0))
              (swap! stats update-in [:shapes shape] (fnil inc 0)))))))
    (let [result @stats]
      (let [ordered-shapes (->> (:shapes result)
                                (sort-by (juxt (comp - val) (comp pr-str key)))
                                (into []))
            weighted (reduce (fn [counts [shape frequency]]
                               (update counts
                                       (:status (compatibility/classify-shape shape))
                                       (fnil + 0) frequency))
                             {} ordered-shapes)
            unique (reduce (fn [counts [shape _]]
                             (update counts
                                     (:status (compatibility/classify-shape shape))
                                     (fnil inc 0)))
                           {} ordered-shapes)
            accounted-weighted (+ (get weighted :behavior 0)
                                    (get weighted :transparent 0))
            accounted-unique (+ (get unique :behavior 0)
                                 (get unique :transparent 0))
            percentage (fn [n total]
                         (if (zero? total) 100.0
                             (double (/ (Math/round
                                         (* 10000.0 (/ n total))) 100.0))))]
      (-> result
          (assoc :commands (->> (:commands result)
                                (sort-by (juxt (comp - val) key))
                                (into []))
                 :shapes ordered-shapes
                 :top-shapes (vec (take 250 ordered-shapes))
                 :coverage {:weighted weighted
                            :unique unique
                            :behavior-invocations-percent
                            (percentage (get weighted :behavior 0)
                                        (:git-invocations result))
                            :behavior-shapes-percent
                            (percentage (get unique :behavior 0)
                                        (count ordered-shapes))
                            :accounted-invocations-percent
                            (percentage accounted-weighted
                                        (:git-invocations result))
                            :accounted-shapes-percent
                            (percentage accounted-unique
                                        (count ordered-shapes))
                            :targets-met?
                            {:invocations (>= (percentage accounted-weighted
                                                          (:git-invocations result))
                                              99.0)
                             :argument-shapes (>= (percentage accounted-unique
                                                             (count ordered-shapes))
                                                  95.0)}}
                 :unique-shapes (count ordered-shapes)))))))

(defn -main [& [root max-files output]]
  (let [root (or root (str (System/getProperty "user.home")
                           "/.claude/projects"))
        max-files (some-> max-files parse-long)
        report (analyze root max-files)]
    (if output
      (do
        (spit output (str (pr-str report) "\n"))
        (prn (select-keys report [:files :bash-inputs :git-invocations
                                  :git-input-parse-failures :unique-shapes])))
      (prn report))))

(apply -main *command-line-args*)
