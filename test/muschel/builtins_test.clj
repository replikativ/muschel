(ns muschel.builtins-test
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.fs.virtual :as vfs]
            [muschel.builtins.posix :as posix]))

(defn- make-fs []
  (vfs/make {"/work/a.txt"     "alpha\nbeta\ngamma\n"
             "/work/empty.txt" ""
             "/work/.dot"      "hidden"
             "/work/sub"       :dir
             "/work/sub/b.txt" "deep\nfile\n"}
            {:cwd "/work"}))

;; ============================================================================
;; pwd
;; ============================================================================

(deftest pwd-prints-cwd
  (let [r (posix/pwd ["pwd"] (make-fs) {})]
    (is (= 0 (:exit r)))
    (is (= "/work\n" (:stdout r)))))

;; ============================================================================
;; echo
;; ============================================================================

(deftest echo-basic
  (is (= "hello world\n" (:stdout (posix/echo ["echo" "hello" "world"] nil {})))))

(deftest echo-n-no-newline
  (is (= "hello" (:stdout (posix/echo ["echo" "-n" "hello"] nil {})))))

(deftest echo-e-escapes
  (is (= "a\tb\n" (:stdout (posix/echo ["echo" "-e" "a\\tb"] nil {})))))

;; ============================================================================
;; ls
;; ============================================================================

(deftest ls-default-hides-dotfiles
  (let [r (posix/ls ["ls"] (make-fs) {})]
    (is (= 0 (:exit r)))
    (is (not (.contains ^String (:stdout r) ".dot"))
        "default ls hides .dot")
    (is (.contains ^String (:stdout r) "a.txt"))
    (is (.contains ^String (:stdout r) "sub"))))

(deftest ls-a-shows-dotfiles
  (let [r (posix/ls ["ls" "-a"] (make-fs) {})]
    (is (.contains ^String (:stdout r) ".dot"))))

(deftest ls-l-long-format
  (let [r (posix/ls ["ls" "-l"] (make-fs) {})]
    (is (re-find #"- +\d+ a\.txt" (:stdout r)) "regular file line")
    (is (re-find #"d +\d+ sub"    (:stdout r)) "directory line")))

(deftest ls-missing-target
  (let [r (posix/ls ["ls" "no-such"] (make-fs) {})]
    (is (= 2 (:exit r)))
    (is (.contains ^String (:stderr r) "No such file"))))

;; ============================================================================
;; cat
;; ============================================================================

(deftest cat-single-file
  (is (= "alpha\nbeta\ngamma\n"
         (:stdout (posix/cat ["cat" "a.txt"] (make-fs) {})))))

(deftest cat-multiple-files
  (let [r (posix/cat ["cat" "a.txt" "sub/b.txt"] (make-fs) {})]
    (is (= 0 (:exit r)))
    (is (= "alpha\nbeta\ngamma\ndeep\nfile\n" (:stdout r)))))

(deftest cat-n-numbers-lines
  (let [r (posix/cat ["cat" "-n" "a.txt"] (make-fs) {})]
    (is (.contains ^String (:stdout r) "     1\talpha"))
    (is (.contains ^String (:stdout r) "     2\tbeta"))))

(deftest cat-missing-file
  (let [r (posix/cat ["cat" "no-such"] (make-fs) {})]
    (is (= 1 (:exit r)))
    (is (.contains ^String (:stderr r) "No such file or directory"))))

(deftest cat-cannot-escape
  (let [r (posix/cat ["cat" "/etc/passwd"] (make-fs) {})]
    (is (= 1 (:exit r))
        "absolute path outside FS root cannot be read")))

(deftest cat-rejects-traversal
  (let [r (posix/cat ["cat" "../../../../etc/passwd"] (make-fs) {})]
    (is (= 1 (:exit r)))
    (is (.contains ^String (:stderr r) "No such file"))))

;; ============================================================================
;; head / tail
;; ============================================================================

(deftest head-default-10
  ;; Our file has 3 lines; head -n 10 returns all.
  (is (= "alpha\nbeta\ngamma\n"
         (:stdout (posix/head ["head" "a.txt"] (make-fs) {})))))

(deftest head-n
  (is (= "alpha\n"
         (:stdout (posix/head ["head" "-n" "1" "a.txt"] (make-fs) {})))))

(deftest head-short-form
  (is (= "alpha\n"
         (:stdout (posix/head ["head" "-1" "a.txt"] (make-fs) {})))))

(deftest tail-n
  (is (= "gamma\n"
         (:stdout (posix/tail ["tail" "-n" "1" "a.txt"] (make-fs) {})))))

(deftest tail-short-form
  (is (= "beta\ngamma\n"
         (:stdout (posix/tail ["tail" "-2" "a.txt"] (make-fs) {})))))

;; ============================================================================
;; wc
;; ============================================================================

(deftest wc-default-all-three
  (let [r (posix/wc ["wc" "a.txt"] (make-fs) {})]
    (is (= 0 (:exit r)))
    ;; "alpha\nbeta\ngamma\n" = 3 newlines, 3 words, 17 bytes
    (is (re-find #"^\s*3\s+3\s+17\s+a\.txt" (:stdout r))
        (str "got: " (pr-str (:stdout r))))))

(deftest wc-l-only
  (let [r (posix/wc ["wc" "-l" "a.txt"] (make-fs) {})]
    (is (re-find #"^\s*3\s+a\.txt" (:stdout r)))))

(deftest wc-multiple-files-total
  (let [r (posix/wc ["wc" "-l" "a.txt" "sub/b.txt"] (make-fs) {})]
    (is (.contains ^String (:stdout r) "total"))))

;; ============================================================================
;; stat
;; ============================================================================

(deftest stat-prints-metadata
  (let [r (posix/stat ["stat" "a.txt"] (make-fs) {})]
    (is (= 0 (:exit r)))
    (is (re-find #"file a\.txt" (:stdout r)))))

(deftest stat-missing
  (let [r (posix/stat ["stat" "no-such"] (make-fs) {})]
    (is (= 1 (:exit r)))
    (is (re-find #"cannot stat" (:stderr r)))))

;; ============================================================================
;; sort / uniq
;; ============================================================================

(deftest sort-basic
  (let [fs (vfs/make {"/work/lines.txt" "banana\napple\ncherry"} {:cwd "/work"})
        r  (posix/sort-fn ["sort" "lines.txt"] fs {})]
    (is (= "apple\nbanana\ncherry\n" (:stdout r)))))

(deftest sort-r-numeric
  (let [fs (vfs/make {"/work/nums.txt" "10\n2\n30\n1"} {:cwd "/work"})
        r  (posix/sort-fn ["sort" "-nr" "nums.txt"] fs {})]
    (is (= "30\n10\n2\n1\n" (:stdout r)))))

(deftest uniq-c
  (let [fs (vfs/make {"/work/dup.txt" "a\na\nb\nb\nb\nc"} {:cwd "/work"})
        r  (posix/uniq ["uniq" "-c" "dup.txt"] fs {})]
    (is (re-find #"2 a" (:stdout r)))
    (is (re-find #"3 b" (:stdout r)))
    (is (re-find #"1 c" (:stdout r)))))
