(ns muschel.host-builtin-test
  "End-to-end tests through the builtin host: dispatch, refusal,
   recursive sh -c, allowlist fallthrough."
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.core :as m]
            [muschel.fs.virtual :as vfs]
            [muschel.host.builtin :as hb]
            [muschel.host.jvm :as jvm]
            [muschel.builtins.posix :as posix]))

(defn- mk-host
  ([] (mk-host {}))
  ([{:keys [fs allowlist]
     :or {fs (vfs/make {"/work/a.txt" "alpha\nbeta\ngamma"
                        "/work/b.txt" "one\ntwo"}
                       {:cwd "/work"})
          allowlist #{}}}]
   (hb/make {:fs fs
             :fallback-host (jvm/make)
             :builtins posix/standard-read-only
             :fallback-allowlist allowlist})))

(defn- run [host cmd]
  (m/run-and-capture (m/new-env) cmd {:host host}))

;; ============================================================================
;; Dispatch — builtin wins, unknown refuses
;; ============================================================================

(deftest builtin-dispatch-pwd-handled-by-muschel-shell
  ;; pwd is a shell-builtin in muschel.exec itself (matches bash's
  ;; behaviour), so it never reaches our host -spawn override. The
  ;; muschel shell answers from the env's cwd directly. Verify it
  ;; just runs cleanly — our posix/pwd is only hit if the muschel
  ;; shell ever forwards a `pwd` to -spawn (rare).
  (let [r (run (mk-host) "pwd")]
    (is (= 0 (:exit r)))
    (is (seq (:stdout r)))))

(deftest builtin-dispatch-cat
  (let [r (run (mk-host) "cat a.txt")]
    (is (= 0 (:exit r)))
    (is (= "alpha\nbeta\ngamma\n" (:stdout r)))))

(deftest unknown-command-refused
  (let [r (run (mk-host) "rm -rf /")]
    (is (= 126 (:exit r))
        "rm is not a builtin and not allowlisted → refused")
    (is (re-find #"muschel:" (:stderr r)))))

(deftest fallback-allowlist-runs
  ;; `true` always exits 0 on any POSIX system. Allowlist it and
  ;; verify the fallback host actually executes it.
  (let [h (mk-host {:allowlist #{"true"}})
        r (run h "true")]
    (is (= 0 (:exit r)))))

;; ============================================================================
;; Recursive sh -c — the bash-of-our-own-shell trick
;; ============================================================================

(deftest sh-c-runs-via-same-host
  (let [r (run (mk-host) "sh -c \"echo hello && cat a.txt\"")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "hello"))
    (is (.contains ^String (:stdout r) "alpha"))))

(deftest sh-c-cannot-escape-builtin-set
  ;; rm isn't a builtin nor allowlisted — bash -c can't reach it.
  (let [r (run (mk-host) "sh -c \"rm -rf /\"")]
    (is (= 126 (:exit r)))
    (is (re-find #"muschel:" (:stderr r)))))

(deftest sh-c-no-script-errors
  (let [r (run (mk-host) "sh -c")]
    (is (= 2 (:exit r)))
    (is (re-find #"Missing required argument" (:stderr r)))))

(deftest bash-alias-works
  (let [r (run (mk-host) "bash -c \"echo bashing\"")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "bashing"))))

;; ============================================================================
;; which — registry visibility
;; ============================================================================

(deftest which-finds-builtin
  (let [r (run (mk-host) "which cat")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "cat"))))

(deftest which-misses-unknown
  (let [r (run (mk-host) "which nopenope")]
    (is (= 1 (:exit r)))))

;; ============================================================================
;; grep
;; ============================================================================

(deftest grep-finds-match
  (let [r (run (mk-host) "grep beta a.txt")]
    (is (= 0 (:exit r)))
    (is (= "beta\n" (:stdout r)))))

(deftest grep-no-match-exits-1
  (let [r (run (mk-host) "grep zzz a.txt")]
    (is (= 1 (:exit r)))
    (is (= "" (:stdout r)))))

(deftest grep-line-number
  (let [r (run (mk-host) "grep -n beta a.txt")]
    (is (= 0 (:exit r)))
    (is (= "2:beta\n" (:stdout r)))))

(deftest grep-invert
  (let [r (run (mk-host) "grep -v beta a.txt")]
    (is (= 0 (:exit r)))
    (is (= "alpha\ngamma\n" (:stdout r)))))

(deftest grep-count
  (let [r (run (mk-host) "grep -c beta a.txt")]
    (is (= 0 (:exit r)))
    (is (= "1\n" (:stdout r)))))

(deftest grep-fixed-string
  (let [fs (vfs/make {"/work/x.txt" "foo.bar\nfoo*bar\nfooxbar"} {:cwd "/work"})
        r  (run (mk-host {:fs fs}) "grep -F foo.bar x.txt")]
    (is (= 0 (:exit r)))
    (is (= "foo.bar\n" (:stdout r)))))

(deftest grep-pipe-stdin
  ;; grep reading from stdin via pipe
  (let [r (run (mk-host) "cat a.txt | grep beta")]
    (is (= 0 (:exit r)))
    (is (= "beta\n" (:stdout r)))))

;; ============================================================================
;; find
;; ============================================================================

(deftest find-lists-files
  (let [r (run (mk-host) "find .")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "a.txt"))
    (is (.contains ^String (:stdout r) "b.txt"))))

(deftest find-name-filter
  (let [fs (vfs/make {"/work/foo.txt" "x"
                      "/work/bar.md"  "y"
                      "/work/sub/baz.txt" "z"}
                     {:cwd "/work"})
        r (run (mk-host {:fs fs}) "find . -name '*.txt'")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "foo.txt"))
    (is (.contains ^String (:stdout r) "baz.txt"))
    (is (not (.contains ^String (:stdout r) "bar.md")))))

(deftest find-type-filter
  ;; Paths are relative to the root we hand find — `find .` → "./dir".
  (let [fs (vfs/make {"/work/a" "x" "/work/dir/b" "y"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "find . -type d")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "./dir"))
    (is (not (re-find #"\./a$" (:stdout r))))))

(deftest find-exec-cannot-escape
  ;; -exec rm should be refused — same gates apply. `\;` escapes the
  ;; statement-separator so it reaches find as the literal terminator.
  (let [r (run (mk-host) "find . -name a.txt -exec rm {} \\;")]
    (is (= 1 (:exit r)))
    (is (re-find #"muschel:" (:stderr r)))))

(deftest find-exec-builtin-works
  ;; -exec echo {} runs through builtin host → builtin echo.
  (let [r (run (mk-host) "find . -name a.txt -exec echo found {} \\;")]
    (is (= 0 (:exit r)))
    (is (re-find #"found" (:stdout r)))
    (is (re-find #"a\.txt" (:stdout r)))))

;; ============================================================================
;; tr — stdin-only character transforms
;; ============================================================================

(deftest tr-translate
  (let [r (run (mk-host) "echo hello | tr a-z A-Z")]
    (is (= 0 (:exit r)))
    (is (= "HELLO\n" (:stdout r)))))

(deftest tr-delete
  (let [r (run (mk-host) "echo abc123 | tr -d 0-9")]
    (is (= 0 (:exit r)))
    (is (= "abc\n" (:stdout r)))))

(deftest tr-squeeze
  (let [r (run (mk-host) "echo aaabbbccc | tr -s a-z")]
    (is (= 0 (:exit r)))
    (is (= "abc\n" (:stdout r)))))

;; ============================================================================
;; cut — column extraction
;; ============================================================================

(deftest cut-fields-default-tab
  (let [fs (vfs/make {"/work/tsv.txt" "a\tb\tc\nd\te\tf"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "cut -f 1,3 tsv.txt")]
    (is (= 0 (:exit r)))
    (is (= "a\tc\nd\tf\n" (:stdout r)))))

(deftest cut-fields-custom-delim
  (let [fs (vfs/make {"/work/csv.txt" "a,b,c\nd,e,f"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "cut -d , -f 2 csv.txt")]
    (is (= 0 (:exit r)))
    (is (= "b\ne\n" (:stdout r)))))

(deftest cut-characters
  (let [fs (vfs/make {"/work/x.txt" "hello\nworld"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "cut -c 1-3 x.txt")]
    (is (= 0 (:exit r)))
    (is (= "hel\nwor\n" (:stdout r)))))

;; ============================================================================
;; diff — line-level via diffit
;; ============================================================================

(deftest diff-same-exits-zero
  (let [fs (vfs/make {"/work/x" "a\nb\nc" "/work/y" "a\nb\nc"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "diff x y")]
    (is (= 0 (:exit r)))
    (is (= "" (:stdout r)))))

(deftest diff-different-exits-one-unified
  (let [fs (vfs/make {"/work/x" "a\nb\nc" "/work/y" "a\nB\nc"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "diff x y")]
    (is (= 1 (:exit r)))
    (is (.contains ^String (:stdout r) "--- x"))
    (is (.contains ^String (:stdout r) "+++ y"))
    (is (.contains ^String (:stdout r) "-b"))
    (is (.contains ^String (:stdout r) "+B"))))

(deftest diff-brief
  (let [fs (vfs/make {"/work/x" "a" "/work/y" "b"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "diff -q x y")]
    (is (= 1 (:exit r)))
    (is (.contains ^String (:stdout r) "differ"))))

;; ============================================================================
;; xargs — dispatch through same host
;; ============================================================================

(deftest xargs-echo
  (let [r (run (mk-host) "echo one two three | xargs echo")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "one"))
    (is (.contains ^String (:stdout r) "two"))
    (is (.contains ^String (:stdout r) "three"))))

(deftest xargs-cannot-escape
  ;; rm isn't a builtin → xargs can't reach it either.
  (let [r (run (mk-host) "echo a.txt | xargs rm")]
    (is (= 1 (:exit r)))
    (is (re-find #"muschel:" (:stderr r)))))

(deftest xargs-with-replace
  (let [r (run (mk-host) "echo one two | xargs -I X echo got X")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "got one"))
    (is (.contains ^String (:stdout r) "got two"))))
