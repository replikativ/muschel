(ns muschel.builtins.git
  "Muschel mount adapter for Geschichte's shared Git command engine."
  (:require [clojure.string :as str]
            [geschichte.git.command :as command]
            [geschichte.repo :as repo]
            [muschel.fs :as fs]
            [muschel.fs.geschichte :as gfs]
            [muschel.fs.mount :as mount]))

(defn- ok [stdout]
  {:stdout stdout :stderr "" :exit 0})

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

(defn- ensure-directory! [filesystem root]
  (when-not (fs/exists? filesystem root)
    (loop [pending [], path root]
      (if (fs/exists? filesystem path)
        (doseq [dir (reverse pending)]
          (when-not (fs/mkdir filesystem dir)
            (throw (ex-info "could not create work tree directory"
                            {:path dir}))))
        (recur (conj pending path) (parent-path path))))))

(defn- repo-relative [root cwd path]
  (let [absolute (resolve-path cwd path)]
    (when (or (= absolute root)
              (str/starts-with? absolute (str root "/")))
      (str/replace (subs absolute (count root)) #"^/+" ""))))

(defn- git-init [filesystem cwd create-repository! args]
  (let [{:keys [path quiet? branch]} (command/parse-init args)
        root (resolve-path cwd path)]
    (when (and branch (not= branch "main"))
      (throw (ex-info "custom initial branches are not implemented yet"
                      {:branch branch})))
    (if (instance? muschel.fs.geschichte.GeschichteFS
                   (mount/mounted-at filesystem root))
      (ok (if quiet? ""
              (str "Reinitialized existing Geschichte repository in " root "\n")))
      (do
        (ensure-directory! filesystem root)
        (gfs/init-and-mount! filesystem root
                             {:create-repository! create-repository!})
        (ok (if quiet? ""
                (str "Initialized empty Geschichte repository in " root "\n")))))))

(defn- git-clone [filesystem cwd create-repository! clone-repository! args]
  (when-not clone-repository!
    (throw (ex-info "clone is unavailable; a permitted transport adapter is required"
                    {})))
  (let [{:keys [url path origin branch quiet?]} (command/parse-clone args)
        root (resolve-path cwd path)
        existed? (fs/exists? filesystem root)]
    (when (and existed?
               (or (not= :dir (:type (fs/stat filesystem root)))
                   (seq (fs/list-dir filesystem root))))
      (throw (ex-info (str "destination path '" path
                           "' already exists and is not an empty directory.")
                      {:path root})))
    (ensure-directory! filesystem root)
    (let [{:keys [conn close!] :as repository}
          (create-repository! {:root root :name path})]
      (try
        (repo/set-config! conn (str "remote." origin ".url") url)
        (clone-repository! {:conn conn :remote origin :url url
                            :options (cond-> {}
                                       branch (assoc :branch branch))})
        (gfs/mount-repository! filesystem root repository)
        {:stdout ""
         :stderr (if quiet? "" (str "Cloning into '" path "'...\n"))
         :exit 0}
        (catch Throwable error
          (when close! (close!))
          (when-not existed? (fs/delete filesystem root))
          (throw error))))))

(defn make
  "Create the Git builtin. Geschichte owns command semantics; this adapter owns
   dynamic mount discovery and filesystem transitions performed by init/clone.
   `clone-repository!` is an injected transport capability and is absent by
   default, preserving the sandbox's network policy."
  ([] (make {}))
  ([{:keys [create-repository! clone-repository! global-config]
     :or {create-repository! gfs/memory-repository!}}]
   (let [global-config (or global-config (atom {}))]
     (fn [argv filesystem env]
       (try
         (when-not (instance? muschel.fs.mount.MountFS filesystem)
           (throw (ex-info "Git integration requires a dynamic mount filesystem"
                           {})))
         (let [{:keys [args directories]} (command/parse-global (rest argv))
               cwd (reduce (fn [cwd directory]
                             (or (resolve-path cwd directory)
                                 (throw (ex-info "invalid -C path"
                                                 {:path directory}))))
                           (:cwd env) directories)
               command-name (first args)
               command-args (subvec args (min 1 (count args)))
               context (repository-context filesystem cwd)]
           (cond
             (= command-name "init")
             (git-init filesystem cwd create-repository! command-args)

             (= command-name "clone")
             (git-clone filesystem cwd create-repository! clone-repository!
                        command-args)

             (and (= command-name "config")
                  (some #{"--global"} command-args))
             (command/execute {:config global-config} args)

             context
             (let [{:keys [root conn fs]} context]
               (command/execute
                {:root root
                 :conn conn
                 :config (:config-atom fs)
                 :repo-relative #(repo-relative root cwd %)}
                args))

             :else
             (fail "not a Geschichte repository (or any parent)")))
         (catch Throwable error
           (fail (or (ex-message error) (str error)))))))))
