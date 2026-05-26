(ns muschel.fs-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [muschel.fs :as fs]
            [muschel.fs.virtual :as vfs]))

;; ============================================================================
;; Path utilities — pure
;; ============================================================================

(deftest normalize-segments-test
  (testing "removes empties and dots"
    (is (= ["a" "b"] (fs/normalize-segments ["a" "" "." "b"]))))
  (testing "resolves .."
    (is (= ["a"] (fs/normalize-segments ["a" "b" ".."]))))
  (testing ".. past root → nil"
    (is (nil? (fs/normalize-segments [".." ".." "a"])))))

;; ============================================================================
;; Virtual FS — exists / list / read
;; ============================================================================

(defn- make-vfs []
  (vfs/make {"/work/a.txt"        "hello\nworld"
             "/work/sub/b.txt"    "deep"
             "/work/.hidden"      "shh"
             "/work/sub"          :dir
             "/work/empty-dir"    :dir}
            {:cwd "/work"}))

(deftest virtual-fs-exists-test
  (let [fs (make-vfs)]
    (is (fs/exists? fs "a.txt")    "relative-to-cwd file")
    (is (fs/exists? fs "/work/a.txt") "absolute file")
    (is (fs/exists? fs "sub")      "directory")
    (is (not (fs/exists? fs "missing.txt")) "absent file")))

(deftest virtual-fs-stat-test
  (let [fs (make-vfs)]
    (is (= :file (:type (fs/stat fs "a.txt"))))
    (is (= :dir  (:type (fs/stat fs "sub"))))
    (is (nil?    (fs/stat fs "no-such")))))

(deftest virtual-fs-list-dir-test
  (let [fs (make-vfs)
        names (mapv :name (fs/list-dir fs "."))]
    (is (= [".hidden" "a.txt" "empty-dir" "sub"] names)
        "lists direct children only, sorted (ASCII order: . < letters)")))

(deftest virtual-fs-read-test
  (let [fs (make-vfs)]
    (is (= "hello\nworld" (fs/read-file fs "a.txt")))
    (is (= "deep"         (fs/read-file fs "/work/sub/b.txt")))
    (is (nil?             (fs/read-file fs "missing")))))

;; ============================================================================
;; Containment — the key safety property
;; ============================================================================

(deftest virtual-fs-no-escape-test
  ;; The virtual FS's containment property is structural: only paths
  ;; that exist in the map are readable. Anything else (including
  ;; absolute paths like /etc/passwd, or `..`-traversal sequences) is
  ;; reported as missing without ever touching real storage.
  (let [fs (vfs/make {"/work/file" "ok"} {:cwd "/work"})]
    (is (= "/work/file" (fs/resolve fs "file"))
        "in-bounds resolves cleanly")
    (is (nil? (fs/read-file fs "/etc/passwd"))
        "absolute path not in the map → no such file")
    (is (nil? (fs/read-file fs "../../../../etc/passwd"))
        "traversal sequences resolve to paths outside the map")
    (is (nil? (fs/resolve fs "../../escape"))
        "`..` past the implicit root returns nil")))

(deftest virtual-fs-cd-test
  (let [fs (vfs/make {"/work/file"     "ok"
                      "/work/sub/file" "deep"
                      "/work/sub"      :dir}
                     {:cwd "/work"})]
    (is (= "/work/sub" (fs/cd! fs "sub")))
    (is (= "deep"      (fs/read-file fs "file"))
        "after cd, relative paths resolve under new cwd")
    (is (nil? (fs/cd! fs "no-such"))
        "cd into nonexistent returns nil; cwd unchanged")
    (is (= "/work/sub" (fs/cwd fs)))))

;; ============================================================================
;; VirtualFS snapshot / restore: send-between-instances round-trip
;; ============================================================================

(deftest vfs-snapshot-pure-data
  ;; The snapshot must be plain persistent data — no atoms, no
  ;; byte-arrays, no live host references — so it can be sent over
  ;; the wire (EDN / Transit / pr-str) and rebuilt on the other end.
  (let [fs (vfs/make {"/a.txt"   "hello"
                      "/dir"     :dir
                      "/dir/b"   "world"}
                     {:cwd "/dir"})
        snap (vfs/snapshot fs)]
    (is (map? snap))
    (is (map? (:entries snap)))
    (is (= "/dir" (:cwd snap)))
    (is (= "hello" (get-in snap [:entries "/a.txt" :content])))
    (is (= :file   (get-in snap [:entries "/a.txt" :type])))
    ;; pr-str / read-string round-trip — proves it's pure data.
    (let [restored-data (edn/read-string (pr-str snap))]
      (is (= snap restored-data))
      (let [fs2 (vfs/restore restored-data)]
        (is (= "hello" (fs/read-file fs2 "/a.txt")))
        (is (= "world" (fs/read-file fs2 "/dir/b")))
        (is (= "/dir"  (fs/cwd fs2)))))))

(deftest vfs-snapshot-forks-isolate
  ;; Writes to one fork must not affect the other.
  (let [fs1 (vfs/make {"/x" "v1"} {:cwd "/"})
        fs2 (vfs/restore (vfs/snapshot fs1))]
    (fs/write-string! fs2 "/x" "v2" false)
    (is (= "v1" (fs/read-file fs1 "/x")) "parent fork unchanged")
    (is (= "v2" (fs/read-file fs2 "/x")) "child fork has the write")))
