(ns muschel.git-builtin-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [muschel.builtins.posix :as posix]
            [muschel.core :as m]
            [muschel.fs.mount :as mount]
            [muschel.fs.virtual :as vfs]
            [muschel.host.builtin :as builtin]
            [muschel.test-helpers :as th]))

(defn- run [host command]
  (m/run-and-capture (m/new-env :cwd "/project") command {:host host}))

(deftest agent-git-init-takes-over-the-worktree
  (let [base (vfs/make {"/project" {:type :dir}
                        "/project/README.md" "hello\n"})
        filesystem (mount/make base {} {:cwd "/project"})
        host (builtin/make {:fs filesystem
                            :fallback-host (th/fallback-host)
                            :builtins posix/standard
                            :geschichte true})]
    (testing "init imports, verifies, and mounts"
      (let [result (run host "git init")]
        (is (= 0 (:exit result)) (:stderr result))
        (is (str/includes? (:stdout result) "Geschichte repository"))
        (is (= ["/project"] (mount/mount-points filesystem))))
      (is (str/includes? (:stdout (run host "git init")) "Reinitialized"))
      (is (= ["/project"] (mount/mount-points filesystem))))
    (testing "the familiar agent lifecycle stays Git-shaped"
      (is (= "?? README.md\n" (:stdout (run host "git status --short"))))
      (is (= 0 (:exit (run host "git add ."))))
      (is (= "A  README.md\n" (:stdout (run host "git status --short"))))
      (is (= 0 (:exit (run host "git commit -m initial"))))
      (is (str/includes? (:stdout (run host "git log --oneline")) "initial")))
    (testing "writes made after init are Geschichte worktree mutations"
      (is (= 0 (:exit (run host "echo more > more.txt"))))
      (is (= "?? more.txt\n" (:stdout (run host "git status --short")))))))
