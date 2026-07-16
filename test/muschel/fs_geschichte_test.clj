(ns muschel.fs-geschichte-test
  (:require [clojure.test :refer [deftest is testing]]
            [geschichte.repo :as repo]
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
        {:keys [conn close!]} (geschichte/init-and-mount! routed "/project")]
    (try
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
      (testing "unmount reveals the untouched source tree"
        (mount/unmount! routed "/project")
        (is (= "hello\n" (fs/read-file routed "/project/README.md")))
        (is (nil? (fs/read-file routed "/project/new.txt"))))
      (finally (close!)))))
