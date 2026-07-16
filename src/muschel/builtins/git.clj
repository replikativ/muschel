(ns muschel.builtins.git
  "Git-shaped builtin backed by Geschichte and a dynamic Muschel mount.

   This is deliberately a thin shell adapter. Repository operations remain in
   Geschichte; argv interpretation can move to Geschichte's shared command
   engine as that surface is filled out."
  (:require [clojure.string :as str]
            [geschichte.bytes :as bytes]
            [geschichte.content :as content]
            [geschichte.diff :as diff]
            [geschichte.repo :as repo]
            [geschichte.query :as query]
            [muschel.fs :as fs]
            [muschel.fs.geschichte :as gfs]
            [muschel.fs.mount :as mount]
            [muschel.gitignore :as ignore])
  (:import [java.nio ByteBuffer]
           [java.nio.charset CharacterCodingException CodingErrorAction
            StandardCharsets]))

(defn- ok
  ([] (ok ""))
  ([stdout] {:stdout stdout :stderr "" :exit 0}))

(defn- fail [message]
  {:stdout "" :stderr (str "fatal: " message "\n") :exit 128})

(defn- repository-context [filesystem cwd]
  (when-let [[root child] (mount/owning-mount filesystem cwd)]
    (when (instance? muschel.fs.geschichte.GeschichteFS child)
      {:root root :fs child :conn (:conn child)})))

(defn- resolve-path [cwd path]
  (let [path (if (str/starts-with? path "/") path (str cwd "/" path))]
    (when-let [segments (fs/normalize-segments (fs/split-path path))]
      (let [resolved (fs/join-path "" segments)]
        (if (str/blank? resolved) "/" resolved)))))

(defn- parent-path [path]
  (let [i (.lastIndexOf ^String path "/")]
    (if (<= i 0) "/" (subs path 0 i))))

(defn- repo-relative [root cwd path]
  (let [absolute (resolve-path cwd path)]
    (when (or (= absolute root) (str/starts-with? absolute (str root "/")))
      (str/replace (subs absolute (count root)) #"^/+" ""))))

(defn- init-args [args]
  (loop [args args, opts {}, operands []]
    (if-let [arg (first args)]
      (cond
        (or (= arg "-q") (= arg "--quiet"))
        (recur (next args) (assoc opts :quiet? true) operands)

        (or (= arg "-b") (= arg "--initial-branch"))
        (if-let [branch (second args)]
          (recur (nnext args) (assoc opts :branch branch) operands)
          (throw (ex-info (str "option requires an argument: " arg) {})))

        (str/starts-with? arg "--initial-branch=")
        (recur (next args)
               (assoc opts :branch (subs arg (count "--initial-branch=")))
               operands)

        (= arg "--bare")
        (throw (ex-info "bare repositories are not supported by the mounted worktree model" {}))

        (str/starts-with? arg "-")
        (throw (ex-info (str "unknown option `" arg "'") {}))

        :else (recur (next args) opts (conj operands arg)))
      (do
        (when (> (count operands) 1)
          (throw (ex-info "too many arguments" {})))
        (assoc opts :path (or (first operands) "."))))))

(defn- git-init [filesystem cwd create-repository! args]
  (let [{:keys [path quiet? branch]} (init-args args)
        root (resolve-path cwd path)]
    (when (and branch (not= branch "main"))
      (throw (ex-info "custom initial branches are not implemented yet"
                      {:branch branch})))
    (if (instance? muschel.fs.geschichte.GeschichteFS
                   (mount/mounted-at filesystem root))
      (ok (if quiet? ""
            (str "Reinitialized existing Geschichte repository in " root "\n")))
      (do
        (when-not (fs/exists? filesystem root)
          (loop [pending [], path root]
            (if (fs/exists? filesystem path)
              (doseq [dir (reverse pending)]
                (when-not (fs/mkdir filesystem dir)
                  (throw (ex-info "could not create work tree directory" {:path dir}))))
              (recur (conj pending path) (parent-path path)))))
        (gfs/init-and-mount! filesystem root
                             {:create-repository! create-repository!})
        (ok (if quiet? ""
              (str "Initialized empty Geschichte repository in " root "\n")))))))

(defn- short-status [{:keys [staged unstaged untracked]}]
  (str
   (apply str (map #(str "A  " % "\n") staged))
   (apply str (map #(str " M " % "\n") unstaged))
   (apply str (map #(str "?? " % "\n") untracked))))

(defn- git-status [conn args]
  (let [rules (ignore/rules conn)
        status (update (repo/status conn) :untracked
                       #(vec (ignore/filter-visible rules %)))
        status (assoc status :clean?
                      (every? empty? ((juxt :staged :unstaged :untracked)
                                      status)))
        short? (some #{"-s" "--short" "--porcelain" "--porcelain=v1"} args)
        branch (str/replace (:branch status) #"^refs/heads/" "")]
    (if short?
      (ok (short-status status))
      (ok (str "On branch " branch "\n"
               (if (:clean? status)
                 "nothing to commit, working tree clean\n"
                 (short-status status)))))))

(defn- git-add [conn root cwd args]
  (let [all? (some #{"-A" "--all"} args)
        dot? (some #{"."} args)
        force? (boolean (some #{"-f" "--force"} args))
        paths (remove #(or (= % "-A") (= % "--all") (= % "--")
                           (= % "-f") (= % "--force")) args)
        rules (ignore/rules conn)]
    (let [tracked (keys (query/stage @conn))
          worktree (repo/files conn)
          candidates (vec (distinct (concat tracked worktree)))
          cwd-relative (or (repo-relative root cwd ".") "")
          under? (fn [prefix path]
                   (or (= prefix path)
                       (str/blank? prefix)
                       (str/starts-with? path (str prefix "/"))))
          selected
          (cond
            all? candidates
            dot? (filterv #(under? cwd-relative %) candidates)
            :else
            (let [specs (mapv #(or (repo-relative root cwd %)
                                   (throw (ex-info "pathspec is outside repository"
                                                   {:path %})))
                              paths)]
              (when (empty? specs)
                (throw (ex-info "Nothing specified, nothing added" {})))
              (mapcat (fn [spec]
                        (let [matches (filterv #(under? spec %) candidates)]
                          (when (empty? matches)
                            (throw (ex-info
                                    (str "pathspec '" spec "' did not match any files")
                                    {:pathspec spec})))
                          matches))
                      specs)))
          selected (vec (distinct selected))]
      (when (and (not all?) (not dot?) (empty? paths))
          (throw (ex-info "Nothing specified, nothing added" {})))
      (let [ignored (when-not force? (filter #(ignore/ignored? rules %) selected))]
        (when (and (seq ignored) (not all?) (not dot?))
          (throw (ex-info
                  (str "The following paths are ignored by one of your .gitignore files:\n"
                       (str/join "\n" ignored)
                       "\nUse -f if you really want to add them.")
                  {:paths ignored})))
        (repo/stage! conn (if force?
                            selected
                            (vec (remove #(ignore/ignored? rules %) selected))))
        (ok)))))

(defn- option-value [args short long]
  (or (some (fn [[a b]] (when (or (= a short) (= a long)) b))
            (partition-all 2 1 args))
      (some #(when (str/starts-with? % (str long "="))
               (subs % (inc (count long)))) args)))

(defn- git-commit [conn args]
  (let [message (option-value args "-m" "--message")
        author (or (option-value args nil "--author") "unknown")
        commit (repo/commit! conn {:message message :author author})]
    (ok (str "[" (str/replace (:geschichte.ref/name commit) #"^refs/heads/" "")
             " " (:geschichte.commit/id commit) "] " message "\n"))))

(defn- present-stage [conn]
  (into (sorted-map)
        (keep (fn [[path entry]]
                (when (= :present (:state entry))
                  [path (dissoc entry :state)])))
        (query/stage @conn)))

(defn- read-tree-entry [conn entry]
  (when-let [id (:content entry)]
    (content/read-by-id conn id)))

(defn- decode-text [value]
  (try
    (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                    (.onMalformedInput CodingErrorAction/REPORT)
                    (.onUnmappableCharacter CodingErrorAction/REPORT))
          text (str (.decode decoder (ByteBuffer/wrap value)))]
      (when-not (str/includes? text "\u0000") text))
    (catch CharacterCodingException _ nil)))

(defn- pathspec-regex [spec]
  (let [expression
        (apply str
               (map (fn [character]
                      (case character
                        \* ".*"
                        \? "."
                        (java.util.regex.Pattern/quote (str character))))
                    spec))]
    (re-pattern (str "^" expression "$"))))

(defn- path-selected? [specs path]
  (or (empty? specs)
      (some (fn [spec]
              (if (re-find #"[*?]" spec)
                (or (re-matches (pathspec-regex spec) path)
                    ;; Git's basename pathspecs recurse through the tree.
                    (and (not (str/includes? spec "/"))
                         (re-matches (pathspec-regex spec)
                                     (last (str/split path #"/")))))
                (or (= spec path) (str/starts-with? path (str spec "/")))))
            specs)))

(defn- diff-args [args]
  (let [separator (.indexOf args "--")
        before (if (neg? separator) args (subvec args 0 separator))
        after (if (neg? separator) [] (subvec args (inc separator)))
        options (set (filter #(str/starts-with? % "-") before))
        operands (vec (remove #(str/starts-with? % "-") before))
        revisions (filter #{"HEAD"} operands)
        implicit-paths (remove #{"HEAD"} operands)]
    (when (some #(not (#{"HEAD"} %))
                (take (count revisions) operands))
      nil)
    {:cached? (boolean (some #{"--cached" "--staged"} before))
     :head? (boolean (some #{"HEAD"} before))
     :quiet? (boolean (some #{"--quiet" "--exit-code"} before))
     :name-only? (contains? options "--name-only")
     :stat? (contains? options "--stat")
     :check? (contains? options "--check")
     :paths (vec (concat implicit-paths after))}))

(defn- render-file-diff [conn left right path]
  (let [left-entry (get left path)
        right-entry (get right path)
        left-bytes (read-tree-entry conn left-entry)
        right-bytes (if (= ::work (:source right-entry))
                      (repo/read conn path)
                      (read-tree-entry conn right-entry))
        left-bytes (or left-bytes (bytes/empty-bytes))
        right-bytes (or right-bytes (bytes/empty-bytes))
        left-text (decode-text left-bytes)
        right-text (decode-text right-bytes)
        a-name (if left-entry (str "a/" path) "/dev/null")
        b-name (if right-entry (str "b/" path) "/dev/null")]
    (if (and left-text right-text)
      (let [result (diff/diff-text left-text right-text)]
        {:path path
         :text (str "diff --git a/" path " b/" path "\n"
                    (when-not left-entry
                      (str "new file mode " (format "%o" (:mode right-entry)) "\n"))
                    (when-not right-entry
                      (str "deleted file mode " (format "%o" (:mode left-entry)) "\n"))
                    (diff/unified result {:a-name a-name :b-name b-name}))
         :added (reduce + 0 (map (fn [{:keys [op b-count]}]
                                   (if (= :insert op) b-count 0))
                                 (:edits result)))
         :deleted (reduce + 0 (map (fn [{:keys [op a-count]}]
                                     (if (= :delete op) a-count 0))
                                   (:edits result)))
         :right-text right-text})
      {:path path
       :text (str "diff --git a/" path " b/" path "\n"
                  "Binary files " a-name " and " b-name " differ\n")
       :added 0 :deleted 0})))

(defn- git-diff [conn args]
  (let [{:keys [cached? head? quiet? name-only? stat? check? paths]}
        (diff-args args)
        head (repo/tree-at conn)
        index (present-stage conn)
        work (into (sorted-map)
                   (map (fn [[path entry]] [path (assoc entry :source ::work)]))
                   (repo/worktree conn))
        [left right] (cond cached? [head index] head? [head work] :else [index work])
        changed (->> (concat (keys left) (keys right))
                     distinct sort
                     (filter #(and (path-selected? paths %)
                                   (not= (dissoc (get left %) :source)
                                         (dissoc (get right %) :source)))))
        rendered (mapv #(render-file-diff conn left right %) changed)
        whitespace-errors
        (when check?
          (mapcat (fn [{:keys [path right-text]}]
                    (keep-indexed
                     (fn [index line]
                       (when (re-find #"[ \t]+$" line)
                         (str path ":" (inc index) ": trailing whitespace.\n")))
                     (str/split-lines (or right-text ""))))
                  rendered))
        output
        (cond
          check? (apply str whitespace-errors)
          name-only? (apply str (map #(str (:path %) "\n") rendered))
          stat? (apply str
                       (map (fn [{:keys [path added deleted]}]
                              (str " " path " | " (+ added deleted) " "
                                   (apply str (repeat added "+"))
                                   (apply str (repeat deleted "-")) "\n"))
                            rendered))
          :else (apply str (map :text rendered)))]
    {:stdout output
     :stderr ""
     :exit (cond
             (and check? (seq whitespace-errors)) 2
             (and quiet? (seq changed)) 1
             :else 0)}))

(defn- git-log [conn args]
  (let [oneline? (some #{"--oneline"} args)
        limit (some-> (or (option-value args "-n" "--max-count")
                          (some #(second (re-matches #"-([0-9]+)" %)) args))
                      parse-long)
        commits (repo/log conn (cond-> {} limit (assoc :limit limit)))]
    (ok
     (apply str
            (map (fn [commit]
                   (if oneline?
                     (str (subs (str (:geschichte.commit/id commit)) 0 8) " "
                          (:geschichte.commit/message commit) "\n")
                     (str "commit " (:geschichte.commit/id commit) "\n"
                          "Author: " (:geschichte.commit/author commit) "\n\n    "
                          (:geschichte.commit/message commit) "\n\n")))
                 commits)))))

(defn- git-ls-files [conn args]
  (let [nul? (boolean (some #{"-z"} args))
        others? (boolean (some #{"-o" "--others"} args))
        ignored? (boolean (some #{"-i" "--ignored"} args))
        rules (ignore/rules conn)
        tracked (->> (query/stage @conn)
                     (keep (fn [[path entry]]
                             (when (= :present (:state entry)) path))))
        tracked-set (set tracked)
        untracked (remove tracked-set (repo/files conn))
        selected (cond
                   ignored? (filter #(ignore/ignored? rules %) untracked)
                   others? (ignore/filter-visible rules untracked)
                   :else tracked)
        separator (if nul? "\u0000" "\n")]
    (ok (str (str/join separator (sort selected))
             (when (seq selected) separator)))))

(defn- resolve-commit [conn revision]
  (cond
    (or (nil? revision) (= revision "HEAD"))
    (some->> (repo/head-commit conn)
             :geschichte.commit/id
             (repo/commit-by-id conn))
    :else
    (some (fn [commit]
            (when (str/starts-with? (str (:geschichte.commit/id commit)) revision)
              commit))
          (repo/log conn))))

(defn- git-show [conn args]
  (let [options (filter #(str/starts-with? % "-") args)
        operand (first (remove #(str/starts-with? % "-") args))
        [revision path] (when operand (str/split operand #":" 2))
        commit (resolve-commit conn revision)]
    (when-not commit
      (throw (ex-info (str "bad object " (or revision "HEAD")) {})))
    (if path
      (if-let [value (repo/read-at conn commit path)]
        (ok (or (decode-text value)
                (throw (ex-info "binary object cannot be written to text stdout"
                                {:path path}))))
        (throw (ex-info (str "path '" path "' does not exist in '" revision "'") {})))
      (let [format-option (some #(when (str/starts-with? % "--format=")
                                   (subs % (count "--format="))) options)
            message (:geschichte.commit/message commit)
            header (cond
                     (= format-option "%H") (str (:geschichte.commit/id commit) "\n")
                     (= format-option "%s") (str message "\n")
                     (some #{"--oneline"} options)
                     (str (subs (str (:geschichte.commit/id commit)) 0 8)
                          " " message "\n")
                     :else
                     (str "commit " (:geschichte.commit/id commit) "\n"
                          "Author: " (:geschichte.commit/author commit) "\n\n    "
                          message "\n"))]
        (ok header)))))

(defn- git-branch [conn args]
  (if (some #{"--show-current"} args)
    (ok (str (str/replace (repo/current-ref conn) #"^refs/heads/" "") "\n"))
    (if-let [name (first (remove #(str/starts-with? % "-") args))]
    (do (repo/branch! conn name) (ok))
    (let [current (repo/current-ref conn)]
      (ok (apply str
                 (map (fn [[ref _]]
                        (str (if (= ref current) "* " "  ")
                             (str/replace ref #"^refs/heads/" "") "\n"))
                      (repo/refs conn))))))))

(defn- git-checkout [conn args]
  (let [force? (boolean (some #{"-f" "--force"} args))
        create? (boolean (some #{"-b" "-B" "-c" "-C"} args))
        name (last args)]
    (when (or (nil? name) (str/starts-with? name "-"))
      (throw (ex-info "you must specify a branch" {})))
    (when create? (repo/branch! conn name))
    (repo/checkout! conn name {:force? force?})
    (ok (str "Switched to branch '" name "'\n"))))

(defn- git-rev-parse [conn root args]
  (cond
    (some #{"--show-toplevel"} args) (ok (str root "\n"))
    (some #{"--is-inside-work-tree"} args) (ok "true\n")
    (and (some #{"--abbrev-ref"} args) (some #{"HEAD"} args))
    (ok (str (str/replace (repo/current-ref conn) #"^refs/heads/" "") "\n"))
    (some #{"HEAD"} args)
    (if-let [head (repo/head-commit conn)]
      (ok (str (:geschichte.commit/id head) "\n"))
      (fail "ambiguous argument 'HEAD': unknown revision or path not in the working tree"))
    :else (fail "unsupported rev-parse invocation")))

(defn make
  "Create a Muschel builtin function. `create-repository!` is injectable so
   harnesses choose memory, file, IndexedDB/projection, and lifecycle policy."
  ([] (make {}))
  ([{:keys [create-repository!] :or {create-repository! gfs/memory-repository!}}]
   (fn [argv filesystem env]
     (try
       (when-not (instance? muschel.fs.mount.MountFS filesystem)
         (throw (ex-info "Git integration requires a dynamic mount filesystem" {})))
       (let [args (vec (rest argv))
             command (first args)
             args (subvec args (min 1 (count args)))
             cwd (:cwd env)]
         (if (= command "init")
           (git-init filesystem cwd create-repository! args)
           (if-let [{:keys [root conn]} (repository-context filesystem cwd)]
             (case command
               "status" (git-status conn args)
               "add" (git-add conn root cwd args)
               "commit" (git-commit conn args)
               "diff" (git-diff conn args)
               "log" (git-log conn args)
               "show" (git-show conn args)
               "ls-files" (git-ls-files conn args)
               "branch" (git-branch conn args)
               "checkout" (git-checkout conn args)
               "switch" (git-checkout conn args)
               "rev-parse" (git-rev-parse conn root args)
               (fail (str "'" command "' is not yet implemented by Geschichte")))
             (fail "not a Geschichte repository (or any parent)"))))
       (catch Throwable error
         (fail (or (ex-message error) (str error))))))))
