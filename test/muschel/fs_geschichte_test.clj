(ns muschel.fs-geschichte-test
  (:require [clojure.test :refer [deftest is testing]]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]
            [muschel.fs :as fs]
            [muschel.fs.geschichte :as geschichte]
            [muschel.fs.mount :as mount]
            [muschel.fs.virtual :as vfs]))

(deftest init-imports-before-atomic-takeover
  (let [base (vfs/make {"/project" {:type :dir}
                        "/project/README.md" "hello\n"
                        "/project/src" {:type :dir}
                        "/project/src/core.clj" "(ns demo)\n"})
        routed (mount/make base {})
        {:keys [conn close! canonical-conn workspace-branch]}
        (geschichte/init-and-mount! routed "/project")
        mounted (mount/mounted-at routed "/project")]
    (try
      (testing "canonical publication state is hidden behind a workspace"
        (is (not (identical? canonical-conn conn)))
        (is (= :db (get-in @canonical-conn [:config :branch])))
        (is (= workspace-branch (get-in @conn [:config :branch])))
        (is (= #{workspace-branch} (workspace/list canonical-conn)))
        (is (empty? (repo/files canonical-conn))))
      (testing "the original files are worktree-only"
        (is (= "hello\n" (fs/read-file routed "/project/README.md")))
        (is (= #{"README.md" "src/core.clj"} (set (repo/files conn))))
        (is (= #{"README.md" "src/core.clj"}
               (set (:untracked (repo/status conn)))))
        (is (nil? (repo/head-commit conn))))
      (testing "all later writes go directly to Geschichte"
        (fs/write-string! routed "/project/new.txt" "new\n" false)
        (is (= "new\n" (fs/read-file routed "/project/new.txt")))
        (is (= "new\n" (String. ^bytes (repo/read conn "new.txt") "UTF-8"))))
      (testing "workspace commits remain private until publication"
        (repo/stage-all! conn)
        (let [commit (repo/commit! conn {:message "initial"})]
          (is (nil? (repo/head-commit canonical-conn)))
          (geschichte/publish! mounted)
          (is (= (:geschichte.commit/id commit)
                 (:geschichte.commit/id
                  (repo/head-commit canonical-conn))))))
      (testing "unmount reveals the untouched source tree"
        (mount/unmount! routed "/project")
        (is (= "hello\n" (fs/read-file routed "/project/README.md")))
        (is (nil? (fs/read-file routed "/project/new.txt"))))
      (finally (close!)))))
