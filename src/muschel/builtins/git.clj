(ns muschel.builtins.git
  "Muschel mount adapter for Geschichte's shared Git command engine."
  (:require [clojure.string :as str]
            [geschichte.git.command :as command]
            [geschichte.git.revision :as revision]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]
            [muschel.builtins.posix :as posix]
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

(defn- repository-id [geschichte-fs]
  (get-in @(:conn geschichte-fs) [:config :store :id]))

(defn- worktree-records [filesystem source-fs]
  (let [id (repository-id source-fs)]
    (->> (mount/mount-points filesystem)
         (keep (fn [path]
                 (let [child (mount/mounted-at filesystem path)]
                   (when (and (instance? muschel.fs.geschichte.GeschichteFS child)
                              (= id (repository-id child)))
                     {:path path
                      :head (some-> (repo/head-commit (:conn child))
                                    :geschichte.commit/id str)
                      :branch (repo/current-ref (:conn child))}))))
         vec)))

(defn- prepare-worktree! [conn {:keys [target new-branch reset-branch?
                                       detach?]}]
  (when detach?
    (throw (ex-info
            "detached Geschichte workspaces are not implemented; use a named branch"
            {:requires :detached-head})))
  (let [target-commit (revision/require conn (or target "HEAD"))]
    (if new-branch
      (let [ref (str "refs/heads/" new-branch)
            existing (get (repo/refs conn) ref)]
        (cond
          (and existing (not reset-branch?))
          (throw (ex-info (str "a branch named '" new-branch
                               "' already exists") {:branch new-branch}))

          (= ref (repo/current-ref conn))
          (repo/reset! conn target-commit {:mode :hard})

          existing
          (do (repo/set-ref! conn ref target-commit)
              (repo/checkout! conn ref {:force? true}))

          :else
          (do (repo/create-ref! conn ref target-commit)
              (repo/checkout! conn ref {:force? true}))))
      (when target
        (let [ref (if (str/starts-with? target "refs/")
                    target (str "refs/heads/" target))]
          (when-not (contains? (repo/refs conn) ref)
            (throw (ex-info
                    "a commit may only be checked out in a named Geschichte workspace branch"
                    {:target target :requires :detached-head})))
          (repo/checkout! conn ref {:force? true}))))))

(defn- workspace-operations [filesystem cwd {:keys [fs conn]}]
  {:list #(worktree-records filesystem fs)
   :add (fn [{:keys [path] :as options}]
          (let [path (or (resolve-path cwd path)
                         (throw (ex-info "invalid worktree path" {:path path})))
                existed? (fs/exists? filesystem path)]
            (when (and existed?
                       (or (not= :dir (:type (fs/stat filesystem path)))
                           (seq (fs/list-dir filesystem path))))
              (throw (ex-info "worktree path already exists and is not empty"
                              {:path path})))
            (ensure-directory! filesystem path)
            (try
              (gfs/fork-and-mount-workspace!
               filesystem path fs
               {:prepare! #(prepare-worktree! % options)})
              (catch Throwable error
                (when-not existed? (fs/delete filesystem path))
                (throw error)))))
   :remove (fn [{:keys [path force?]}]
             (let [path (or (resolve-path cwd path)
                            (throw (ex-info "invalid worktree path" {:path path})))
                   child (mount/mounted-at filesystem path)
                   branch (get-in child [:repository :workspace-branch])]
               (when-not (and (instance? muschel.fs.geschichte.GeschichteFS child)
                              (= (repository-id fs) (repository-id child))
                              branch)
                 (throw (ex-info "not a removable Geschichte worktree"
                                 {:path path})))
               (let [status (repo/status (:conn child))]
                 (when (and (not force?) (not (:clean? status)))
                   (throw (ex-info "worktree contains modified or untracked files"
                                   {:path path :status status}))))
               (mount/unmount! filesystem path)
               (gfs/close! child)
               (fs/delete filesystem path)
               path))
   :prune (fn [_] nil)})

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
  (let [{:keys [url path origin branch depth quiet?]} (command/parse-clone args)
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
                                       branch (assoc :branch branch)
                                       depth (assoc :depth (parse-long depth)))})
        (gfs/mount-canonical-workspace! filesystem root repository {})
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
  ([{:keys [create-repository! clone-repository! remote-ops global-config]
     :or {create-repository! gfs/memory-repository!}}]
   (let [global-config (or global-config (atom {}))
         clone-repository! (or clone-repository! (:clone remote-ops))]
     (fn [argv filesystem env]
       (try
         (when-not (instance? muschel.fs.mount.MountFS filesystem)
           (throw (ex-info "Git integration requires a dynamic mount filesystem"
                           {})))
         (let [{:keys [args directories config]}
               (command/parse-global (rest argv))
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

             (= command-name "ls-remote")
             (command/execute {:config (atom (merge @global-config config))
                               :remote-ops remote-ops}
                              args)

             context
             (let [{:keys [root conn fs]} context]
               (command/execute
                {:root root
                 :conn conn
                 :config (atom (merge @(:config-atom fs) config))
                 :remote-ops remote-ops
                 :workspace-ops (workspace-operations filesystem cwd context)
                 :read-message
                 (fn [path]
                   (if (= path "-")
                     (posix/read-stdin env)
                     (let [absolute (or (resolve-path cwd path)
                                        (throw (ex-info "invalid message path"
                                                        {:path path})))]
                       (or (fs/read-file filesystem absolute)
                           (throw (ex-info "could not read commit message"
                                           {:path path}))))))
                 :repo-relative #(repo-relative root cwd %)}
                args))

             :else
             (fail "not a Geschichte repository (or any parent)")))
         (catch Throwable error
           (fail (or (ex-message error) (str error)))))))))
