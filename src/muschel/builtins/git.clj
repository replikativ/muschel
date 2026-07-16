(ns muschel.builtins.git
  "Git-shaped builtin backed by Geschichte and a dynamic Muschel mount.

   This is deliberately a thin shell adapter. Repository operations remain in
   Geschichte; argv interpretation can move to Geschichte's shared command
   engine as that surface is filled out."
  (:require [clojure.string :as str]
            [geschichte.repo :as repo]
            [muschel.fs :as fs]
            [muschel.fs.geschichte :as gfs]
            [muschel.fs.mount :as mount]))

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
  (let [status (repo/status conn)
        short? (some #{"-s" "--short" "--porcelain" "--porcelain=v1"} args)
        branch (str/replace (:branch status) #"^refs/heads/" "")]
    (if short?
      (ok (short-status status))
      (ok (str "On branch " branch "\n"
               (if (:clean? status)
                 "nothing to commit, working tree clean\n"
                 (short-status status)))))))

(defn- git-add [conn root cwd args]
  (let [all? (some #{"-A" "--all" "."} args)
        paths (remove #(or (= % "-A") (= % "--all") (= % "--")) args)]
    (if all?
      (do (repo/stage-all! conn) (ok))
      (let [paths (mapv #(or (repo-relative root cwd %)
                             (throw (ex-info "pathspec is outside repository"
                                             {:path %})))
                        paths)]
        (when (empty? paths)
          (throw (ex-info "Nothing specified, nothing added" {})))
        (repo/stage! conn paths)
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

(defn- git-branch [conn args]
  (if-let [name (first (remove #(str/starts-with? % "-") args))]
    (do (repo/branch! conn name) (ok))
    (let [current (repo/current-ref conn)]
      (ok (apply str
                 (map (fn [[ref _]]
                        (str (if (= ref current) "* " "  ")
                             (str/replace ref #"^refs/heads/" "") "\n"))
                      (repo/refs conn)))))))

(defn- git-checkout [conn args]
  (let [force? (boolean (some #{"-f" "--force"} args))
        create? (boolean (some #{"-b" "-B"} args))
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
               "log" (git-log conn args)
               "branch" (git-branch conn args)
               "checkout" (git-checkout conn args)
               "switch" (git-checkout conn args)
               "rev-parse" (git-rev-parse conn root args)
               (fail (str "'" command "' is not yet implemented by Geschichte")))
             (fail "not a Geschichte repository (or any parent)"))))
       (catch Throwable error
         (fail (or (ex-message error) (str error))))))))
