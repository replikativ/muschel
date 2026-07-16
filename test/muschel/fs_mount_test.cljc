(ns muschel.fs-mount-test
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.fs :as fs]
            [muschel.fs.virtual :as vfs]
            [muschel.fs.mount :as mount]))

(defn- vfs-with [entries]
  (vfs/make (merge {"/" {:type :dir :mtime-ms 0}} entries)))

(defn- write! [f path content]
  (let [sink (fs/open-sink f path false)]
    (is (some? sink) (str "open-sink " path))
    (swap! (:acc sink) str content)
    content))

(deftest routing-and-union
  (let [base  (vfs-with {"/readme.md" {:type :file :content "base" :mtime-ms 1}
                         "/src"       {:type :dir :mtime-ms 1}})
        child (vfs-with {"/notes.md"  {:type :file :content "drive!" :mtime-ms 2}})
        m     (mount/make base {"/drive" child})]
    (testing "root listing unions the mount point in"
      (is (= #{"readme.md" "src" "drive"}
             (set (map :name (fs/list-dir m "/"))))))
    (testing "mount point stats as a dir even though base lacks it"
      (is (= :dir (:type (fs/stat m "/drive"))))
      (is (fs/exists? m "/drive")))
    (testing "reads route into the child, rebased"
      (is (= "drive!" (fs/read-file m "/drive/notes.md"))))
    (testing "base paths still served by base"
      (is (= "base" (fs/read-file m "/readme.md"))))
    (testing "mount shadows base at the same path"
      (is (nil? (fs/read-file m "/drive/readme.md"))))))

(deftest nested-mount-ancestors
  (let [base  (vfs-with {})
        child (vfs-with {"/x.txt" {:type :file :content "x" :mtime-ms 1}})
        m     (mount/make base {"/data/shared" child})]
    (testing "ancestors of mounts synthesize as dirs"
      (is (= :dir (:type (fs/stat m "/data"))))
      (is (= ["shared"] (mapv :name (fs/list-dir m "/data")))))
    (testing "path through the ancestor reaches the child"
      (is (= "x" (fs/read-file m "/data/shared/x.txt"))))))

(deftest writes-and-cwd
  (let [base  (vfs-with {})
        child (vfs-with {})
        m     (mount/make base {"/drive" child})]
    (testing "write into the mount lands in the child"
      (write! m "/drive/new.txt" "hello")
      (is (= "hello" (fs/read-file m "/drive/new.txt")))
      (is (= "hello" (fs/read-file child "/new.txt"))))
    (testing "mkdir + relative paths through mount-layer cwd"
      (is (fs/mkdir m "/drive/sub"))
      (is (fs/cd! m "/drive/sub"))
      (write! m "rel.txt" "relative")
      (is (= "relative" (fs/read-file child "/sub/rel.txt"))))
    (testing "cd .. climbs back across the mount boundary"
      (is (= "/drive" (fs/cd! m "..")))
      (is (= "/" (fs/cd! m ".."))))
    (testing "write into base still works"
      (write! m "/base.txt" "b")
      (is (= "b" (fs/read-file base "/base.txt"))))))

(deftest mount-point-protection
  (let [base  (vfs-with {})
        child (vfs-with {"/f.txt" {:type :file :content "f" :mtime-ms 1}})
        m     (mount/make base {"/drive" child})]
    (is (nil? (fs/delete m "/drive")) "cannot delete a mount point")
    (is (nil? (fs/rename m "/drive" "/other")) "cannot rename a mount point")
    (is (nil? (fs/mkdir m "/drive")) "mkdir on existing mount point fails")
    (is (nil? (fs/rename m "/drive/f.txt" "/f.txt")) "cross-FS rename refused")
    (is (some? (fs/rename m "/drive/f.txt" "/drive/g.txt")) "in-child rename ok")
    (is (= "f" (fs/read-file m "/drive/g.txt")))))

(deftest containment
  (let [base  (vfs-with {"/secret.txt" {:type :file :content "s" :mtime-ms 1}})
        child (vfs-with {})
        m     (mount/make base {"/drive" child})]
    (testing "dot-dot inside the mount canonicalizes at the mount layer"
      ;; /drive/../secret.txt → /secret.txt → BASE serves it (correct:
      ;; the mount layer is one namespace; children can't be escaped
      ;; INTO from outside their prefix, and .. simply leaves the mount)
      (is (= "s" (fs/read-file m "/drive/../secret.txt"))))
    (testing "escape past root refused"
      (is (nil? (fs/resolve m "/../../etc/passwd"))))))

(deftest dynamic-mount-lifecycle
  (let [base  (vfs-with {"/project" {:type :dir :mtime-ms 1}
                         "/project/base.txt" {:type :file :content "base"}})
        child (vfs-with {"/owned.txt" {:type :file :content "owned"}})
        m     (mount/make base {})]
    (is (= [] (mount/mount-points m)))
    (mount/mount! m "/project" child)
    (is (= ["/project"] (mount/mount-points m)))
    (is (= child (mount/mounted-at m "/project")))
    (is (= ["/project" child] (mount/owning-mount m "/project/owned.txt")))
    (is (= "owned" (fs/read-file m "/project/owned.txt")))
    (is (nil? (fs/read-file m "/project/base.txt")) "mount shadows base")
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (mount/mount! m "/project/nested" (vfs-with {}))))
    (is (= child (mount/unmount! m "/project")))
    (is (= "base" (fs/read-file m "/project/base.txt")))))
