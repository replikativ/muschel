(ns muschel.git-builtin-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [geschichte.repo :as repo]
            [muschel.builtins.posix :as posix]
            [muschel.core :as m]
            [muschel.fs :as fs]
            [muschel.fs.geschichte :as gfs]
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

(deftest agent-git-clone-creates-an-atomic-geschichte-mount
  (let [calls (atom [])
        clone! (fn [{:keys [conn] :as request}]
                 (swap! calls conj [:clone (dissoc request :conn)])
                 (repo/write! conn "README.md" (.getBytes "from remote\n" "UTF-8"))
                 (repo/stage-all! conn)
                 (repo/commit! conn {:message "remote head" :author "Remote"}))
        fetch! (fn [request]
                 (swap! calls conj [:fetch (dissoc request :conn)])
                 {:persisted 0})
        base (vfs/make {"/project" {:type :dir}})
        filesystem (mount/make base {} {:cwd "/project"})
        host (builtin/make {:fs filesystem
                            :fallback-host (th/fallback-host)
                            :builtins posix/standard
                            :geschichte {:remote-ops {:clone clone!
                                                      :fetch fetch!}}})
        result (run host "git clone -b main https://example.test/demo.git")]
    (is (= 0 (:exit result)) (:stderr result))
    (is (= "Cloning into 'demo'...\n" (:stderr result)))
    (is (= ["/project/demo"] (mount/mount-points filesystem)))
    (let [mounted (mount/mounted-at filesystem "/project/demo")
          canonical (get-in mounted [:repository :canonical-conn])]
      (is (not (identical? canonical (:conn mounted))))
      (is (empty? (repo/files canonical)))
      (is (some? (repo/head-commit canonical))))
    (is (= "from remote\n"
           (:stdout (run host "cat /project/demo/README.md"))))
    (is (str/includes?
         (:stdout (run host "git -C /project/demo log --oneline"))
         "remote head"))
    (is (= "https://example.test/demo.git\n"
           (:stdout (run host "git -C /project/demo remote get-url origin"))))
    (is (= 0 (:exit (run host "git -C /project/demo fetch origin"))))
    (is (= [[:clone {:remote "origin" :url "https://example.test/demo.git"
                     :options {:branch "main"}}]
            [:fetch {:remote "origin" :url "https://example.test/demo.git"
                     :options {:refspec nil :prune? false :tags nil}}]]
           @calls))
    (let [failed-fs (mount/make (vfs/make {"/project" {:type :dir}}) {}
                                {:cwd "/project"})
          failed-host
          (builtin/make
           {:fs failed-fs
            :fallback-host (th/fallback-host)
            :builtins posix/standard
            :geschichte {:clone-repository!
                         (fn [_] (throw (ex-info "remote failed" {})))}})
          failed (run failed-host
                      "git clone https://example.test/broken.git")]
      (is (= 128 (:exit failed)))
      (is (str/includes? (:stderr failed) "remote failed"))
      (is (= [] (mount/mount-points failed-fs)))
      (is (not (fs/exists? failed-fs "/project/broken"))))))

(deftest git-worktrees-are-isolated-geschichte-workspaces
  (let [base (vfs/make {"/project" {:type :dir}
                        "/project/README.md" "base\n"})
        filesystem (mount/make base {} {:cwd "/project"})
        host (builtin/make {:fs filesystem
                            :fallback-host (th/fallback-host)
                            :builtins posix/standard
                            :geschichte true})]
    (is (zero? (:exit (run host "git init"))))
    (is (zero? (:exit (run host "git add ."))))
    (is (zero? (:exit (run host "git commit -m base"))))
    (gfs/publish! (mount/mounted-at filesystem "/project"))

    (testing "the same logical branch can be mounted more than once"
      (is (= 0 (:exit (run host "git worktree add /agent-two main"))))
      (is (= ["/agent-two" "/project"] (mount/mount-points filesystem)))
      (is (= "main\n"
             (:stdout (run host "git -C /agent-two branch --show-current"))))
      (is (= 0 (:exit (run host "echo isolated > /agent-two/README.md"))))
      (is (= "base\n" (:stdout (run host "cat /project/README.md"))))
      (is (= "isolated\n" (:stdout (run host "cat /agent-two/README.md"))))
      (let [listing (:stdout (run host "git worktree list --porcelain"))]
        (is (str/includes? listing "worktree /project"))
        (is (str/includes? listing "worktree /agent-two"))))

    (testing "remove protects dirty workspaces and force discards them"
      (let [refused (run host "git worktree remove /agent-two")]
        (is (= 128 (:exit refused)))
        (is (str/includes? (:stderr refused) "modified or untracked")))
      (is (= 0 (:exit (run host "git worktree remove --force /agent-two"))))
      (is (= ["/project"] (mount/mount-points filesystem)))
      (is (not (fs/exists? filesystem "/agent-two"))))

    (testing "publication and advance coordinate isolated mounts"
      (is (= 0 (:exit (run host "git worktree add /publisher main"))))
      (is (= 0 (:exit (run host "echo published > /publisher/README.md"))))
      (is (= 0 (:exit (run host "git -C /publisher add README.md"))))
      (is (= 0 (:exit (run host "git -C /publisher commit -m published"))))
      (gfs/publish! (mount/mounted-at filesystem "/publisher"))
      (is (= "base\n" (:stdout (run host "cat /project/README.md"))))
      (gfs/advance! (mount/mounted-at filesystem "/project"))
      (is (= "published\n" (:stdout (run host "cat /project/README.md"))))
      (is (= 0 (:exit (run host "git worktree remove /publisher")))))

    (testing "new logical branches stay local to their physical workspace"
      (is (= 0 (:exit (run host
                           "git worktree add -b feature /feature main"))))
      (is (= "feature\n"
             (:stdout (run host "git -C /feature branch --show-current"))))
      (is (= "main\n"
             (:stdout (run host "git -C /project branch --show-current"))))
      (is (= 0 (:exit (run host "git worktree remove --force /feature")))))))
