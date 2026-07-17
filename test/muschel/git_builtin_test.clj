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
                        "/project/README.md" "hello\n"
                        "/project/.gitignore" "target/\n*.log\n"
                        "/project/target" {:type :dir}
                        "/project/target/output.js" "generated\n"
                        "/project/debug.log" "ignored\n"})
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
      (is (= 0 (:exit (run host "git config user.name 'Coding Agent'"))))
      (is (= 0 (:exit (run host "git config user.email agent@example.test"))))
      (is (= "Coding Agent\n" (:stdout (run host "git config --get user.name"))))
      (is (= "?? .gitignore\n?? README.md\n"
             (:stdout (run host "git status --short"))))
      (is (= 0 (:exit (run host "git add ."))))
      (is (= "A  .gitignore\nA  README.md\n"
             (:stdout (run host "git status --short"))))
      (is (= 0 (:exit (run host "git commit -m initial"))))
      (is (= "main\n"
             (:stdout (run host "git -C /project branch --show-current"))))
      (is (str/includes? (:stdout (run host "git log --oneline")) "initial"))
      (is (str/includes? (:stdout (run host "git status"))
                         "working tree clean"))
      (is (= 0 (:exit (run host "echo changed > README.md"))))
      (is (= " M README.md\n" (:stdout (run host "git status --short"))))
      (is (str/includes? (:stdout (run host "git diff -- README.md"))
                         "+changed"))
      (is (str/includes? (:stdout (run host "git diff -- '*.md'"))
                         "+changed"))
      (is (= 1 (:exit (run host "git diff --quiet"))))
      (is (= 0 (:exit (run host "git add README.md"))))
      (is (= "M  README.md\n" (:stdout (run host "git status --short"))))
      (is (str/includes? (:stdout (run host "git diff --cached -- README.md"))
                         "+changed"))
      (is (= 0 (:exit (run host "git commit -m update"))))
      (is (= "changed\n" (:stdout (run host "git show HEAD:README.md"))))
      (is (= "update\n" (:stdout (run host "git show --format=%s --no-patch HEAD"))))
      (is (str/includes? (:stdout (run host "git show HEAD"))
                         "Coding Agent <agent@example.test>"))
      (is (str/includes? (:stdout (run host "git ls-files")) "README.md"))
      (is (= "main\n" (:stdout (run host "git branch --show-current"))))
      (is (= 0 (:exit (run host "git switch -c feature"))))
      (is (= "feature\n" (:stdout (run host "git branch --show-current"))))
      (is (= 0 (:exit (run host "git switch main"))))
      (is (= 0 (:exit (run host "git branch -d feature"))))
      (is (= 0 (:exit (run host "echo temporary > README.md"))))
      (is (= 0 (:exit (run host "git restore README.md"))))
      (is (= "changed\n" (:stdout (run host "cat README.md"))))
      (is (= 0 (:exit (run host "echo staged > README.md"))))
      (is (= 0 (:exit (run host "git add README.md"))))
      (is (= 0 (:exit (run host "git restore --staged README.md"))))
      (is (= " M README.md\n" (:stdout (run host "git status --short"))))
      (is (= 0 (:exit (run host "git checkout -- README.md"))))
      (is (= 0 (:exit (run host "git rm README.md"))))
      (is (= "D  README.md\n" (:stdout (run host "git status --short"))))
      (is (= 0 (:exit (run host "git reset HEAD -- README.md"))))
      (is (= " D README.md\n" (:stdout (run host "git status --short"))))
      (is (= 0 (:exit (run host "git restore README.md"))))
      (is (= 0 (:exit (run host "echo discard > README.md"))))
      (is (= 0 (:exit (run host "git reset --hard HEAD"))))
      (is (= "changed\n" (:stdout (run host "cat README.md"))))
      (is (= 128 (:exit (run host "git add debug.log"))))
      (is (= 0 (:exit (run host "git add -f debug.log"))))
      (is (= 0 (:exit (run host "git commit -m ignored-file")))))
    (testing "writes made after init are Geschichte worktree mutations"
      (is (= 0 (:exit (run host "echo more > more.txt"))))
      (is (= "?? more.txt\n" (:stdout (run host "git status --short")))))))
