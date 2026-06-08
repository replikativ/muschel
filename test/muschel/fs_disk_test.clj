(ns muschel.fs-disk-test
  "Disk FS tests — real files, containment under tmp root.

   These tests target the generic FS-protocol behaviour (resolve,
   read, list, cd, containment, symlink rejection). To keep them
   focused on the protocol — independent of muschel's default
   `/home/agent` mount convention — they pass `:mount-at \"/\"`,
   which makes the wrapper directory itself the internal real-root.
   The integration-level mount layout is exercised by
   sandbox_escape_test and sandboxed_integration_test."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [muschel.fs :as fs]
            [muschel.fs.disk :as disk])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- mk-tmp-dir
  ([] (mk-tmp-dir "muschel-fs-test"))
  ([prefix]
   (let [p (Files/createTempDirectory prefix (make-array FileAttribute 0))]
     (str p))))

(defn- write-file! [root rel content]
  (let [f (io/file root rel)]
    (io/make-parents f)
    (spit f content)))

(def ^:private flat-opts {:mount-at "/"})

;; ============================================================================
;; Basic happy path
;; ============================================================================

(deftest disk-fs-read-and-list
  (let [root (mk-tmp-dir)
        _    (write-file! root "a.txt"    "hello\nworld")
        _    (write-file! root "sub/b.txt" "deep")
        fs   (disk/make root flat-opts)]
    (is (= "hello\nworld" (fs/read-file fs "a.txt")))
    (is (= "deep"         (fs/read-file fs "sub/b.txt")))
    (is (= ["a.txt" "sub"] (mapv :name (fs/list-dir fs "."))))))

;; ============================================================================
;; Containment — the safety guarantee
;; ============================================================================

(deftest disk-fs-absolute-paths-are-jail-relative
  (let [root (mk-tmp-dir)
        _    (write-file! root "a.txt" "ok")
        fs   (disk/make root flat-opts)]
    ;; With :mount-at "/", the sandbox is rooted at `/`, so an absolute
    ;; path is JAIL-relative: `/etc/passwd` means `<root>/etc/passwd`
    ;; (which doesn't exist) and never reaches the host's real
    ;; /etc/passwd. resolve now returns sandbox-relative; the
    ;; equivalent observable check is read/exists.
    (is (= "/etc/passwd" (fs/resolve fs "/etc/passwd"))
        "absolute path resolves to the sandbox-relative form, jailed under root")
    (is (nil? (fs/read-file fs "/etc/passwd")) "host /etc/passwd is unreachable")
    (is (not (fs/exists?    fs "/etc/passwd")))
    ;; A jail-absolute path resolves a file at the sandbox root.
    (is (= "ok" (fs/read-file fs "/a.txt")) "jail-absolute path reaches root file")))

(deftest disk-fs-rejects-traversal-past-root
  (let [root (mk-tmp-dir)
        _    (write-file! root "in/safe.txt" "ok")
        fs   (disk/make root (assoc flat-opts :cwd (str root "/in")))]
    (is (nil? (fs/resolve fs "../../../../etc/passwd"))
        "`..`-sequences that escape root return nil")
    (is (nil? (fs/read-file fs "../../../../etc/passwd")))))

(deftest disk-fs-symlink-outside-root-denied
  (testing "a symlink that points outside the root is canonicalised + rejected"
    (let [root (mk-tmp-dir)
          _    (write-file! root "decoy.txt" "innocent")
          ;; Create a symlink inside the root pointing to /etc/passwd
          link (io/file root "passwd-link")
          _    (Files/createSymbolicLink
                (.toPath link)
                (.toPath (io/file "/etc/passwd"))
                (make-array FileAttribute 0))
          fs   (disk/make root flat-opts)]
      (is (nil? (fs/resolve fs "passwd-link"))
          "symlink to outside-root resolves to nil")
      (is (nil? (fs/read-file fs "passwd-link"))))))

;; ============================================================================
;; cd
;; ============================================================================

(deftest disk-fs-cd
  (let [root (mk-tmp-dir)
        _    (write-file! root "a.txt"     "root-level")
        _    (write-file! root "sub/b.txt" "sub-level")
        fs   (disk/make root flat-opts)]
    (is (= "root-level" (fs/read-file fs "a.txt")))
    (is (some? (fs/cd! fs "sub")))
    (is (= "sub-level" (fs/read-file fs "b.txt"))
        "after cd, relative paths resolve under new cwd")
    (is (nil? (fs/cd! fs "no-such")))))

;; ============================================================================
;; Mount-at convention — the muschel default
;; ============================================================================

(deftest disk-fs-default-mount-auto-creates-home-agent
  ;; Default mount-at is /home/agent. DiskFS auto-creates
  ;; <wrapper>/home/agent on construction.
  (let [wrapper (mk-tmp-dir)
        fs (disk/make wrapper)]
    (is (.exists (io/file wrapper "home/agent"))
        "construction auto-creates <wrapper>/home/agent")
    (is (= "/home/agent" (fs/cwd fs))
        "cwd is the sandbox-relative mount path")
    (is (= (str wrapper "/home/agent")
           (fs/physical-path fs "/home/agent"))
        "-physical-path translates sandbox → real-disk for OS-spawn boundary")))

(deftest disk-fs-mount-at-paths-resolve-under-mount
  (let [wrapper (mk-tmp-dir)
        fs (disk/make wrapper)
        _  (write-file! wrapper "home/agent/note.txt" "inside")]
    (is (= "inside" (fs/read-file fs "/home/agent/note.txt"))
        "absolute under mount: re-rooted to <wrapper>/home/agent/note.txt")
    (is (= "inside" (fs/read-file fs "note.txt"))
        "relative against default cwd (mount root) reaches the file")))

(deftest disk-fs-paths-outside-mount-are-unreachable
  ;; With default mount-at /home/agent, paths under the mount resolve
  ;; normally. Ancestor paths (/, /home) resolve as virtual
  ;; directories — see disk-fs-ancestor-view below. Sibling paths
  ;; (/etc/passwd, /home/somethingelse) are outside both and resolve
  ;; to nil; reads return nil.
  (let [wrapper (mk-tmp-dir)
        fs (disk/make wrapper)]
    (is (nil? (fs/resolve fs "/etc/passwd")))
    (is (nil? (fs/resolve fs "/home/somethingelse")))
    (is (nil? (fs/read-file fs "/etc/passwd")))))

(deftest disk-fs-ancestor-view
  ;; Strict prefixes of mount-at are virtual read-only directories.
  ;; `cd /` works, `stat /` reports a dir, `list-dir /` returns just
  ;; the next segment toward the mount. Writes return nil.
  (let [wrapper (mk-tmp-dir)
        fs (disk/make wrapper)]
    (is (= "/" (fs/resolve fs "/")))
    (is (= "/home" (fs/resolve fs "/home")))
    (is (= :dir (:type (fs/stat fs "/"))))
    (is (= :dir (:type (fs/stat fs "/home"))))
    (is (true? (fs/exists? fs "/")))
    (is (= [{:name "home" :type :dir :size 0 :mtime-ms 0}]
           (fs/list-dir fs "/"))
        "ls / shows only the path toward the mount")
    (is (= [{:name "agent" :type :dir :size 0 :mtime-ms 0}]
           (fs/list-dir fs "/home")))
    (is (some? (fs/cd! fs "/")))
    (is (= "/" (fs/cwd fs)))
    (is (some? (fs/cd! fs "/home")))
    (is (= "/home" (fs/cwd fs)))
    (is (some? (fs/cd! fs "/home/agent")))
    ;; Writes anywhere above the mount are silently refused.
    (is (nil? (fs/-open-sink fs "/probe.txt" false)))
    (is (nil? (fs/-mkdir fs "/probe-dir")))))

(deftest disk-fs-rejects-bad-mount-at
  (let [wrapper (mk-tmp-dir)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (disk/make wrapper {:mount-at "home/agent"}))
        "must be absolute")
    (is (thrown? clojure.lang.ExceptionInfo
                 (disk/make wrapper {:mount-at "/home/../agent"}))
        "no `..`")
    (is (thrown? clojure.lang.ExceptionInfo
                 (disk/make wrapper {:mount-at ""})))))

(deftest disk-fs-mount-at-trailing-slash-normalised
  (let [wrapper (mk-tmp-dir)
        fs (disk/make wrapper {:mount-at "/work/"})]
    (is (= "/work" (fs/sandbox-relativize fs (fs/cwd fs)))
        "trailing slashes are stripped from mount-at")))
