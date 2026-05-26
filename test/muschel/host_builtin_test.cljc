(ns muschel.host-builtin-test
  "End-to-end tests through the builtin host: dispatch, refusal,
   recursive sh -c, allowlist fallthrough. Cross-platform: runs on
   JVM (`host.jvm`) and Node / ClojureScript (`host.browser`)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [muschel.builtins.posix :as posix]
            [muschel.core :as m]
            [muschel.fs.virtual :as vfs]
            [muschel.host.builtin :as hb]
            [muschel.test-helpers :as th]))

(defn- mk-host
  ([] (mk-host {}))
  ([{:keys [fs allowlist]
     :or {fs (vfs/make {"/work/a.txt" "alpha\nbeta\ngamma\n"
                        "/work/b.txt" "one\ntwo\n"}
                       {:cwd "/work"})
          allowlist #{}}}]
   (hb/make {:fs fs
             :fallback-host (th/fallback-host)
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
    (is (str/includes? (:stdout r) "hello"))
    (is (str/includes? (:stdout r) "alpha"))))

(deftest sh-c-cannot-escape-builtin-set
  ;; rm isn't a builtin nor allowlisted — bash -c can't reach it.
  (let [r (run (mk-host) "sh -c \"rm -rf /\"")]
    (is (= 126 (:exit r)))
    (is (re-find #"muschel:" (:stderr r)))))

(deftest sh-c-no-script-errors
  (let [r (run (mk-host) "sh -c")]
    (is (= 1 (:exit r)) "usage errors exit 1 (GNU coreutils convention)")
    (is (re-find #"Missing required argument" (:stderr r)))))

(deftest sh-script-file-runs
  ;; `sh FILE` reads the script through the FS protocol and runs it.
  ;; Used in the playground (`sh script.sh` against the seeded VFS).
  (let [h (hb/make {:fs (vfs/make {"/work/script.sh"
                                   "echo hi from script\necho line 2\n"}
                                  {:cwd "/work"})
                    :fallback-host (th/fallback-host)
                    :builtins posix/standard
                    :fallback-allowlist #{}})
        r (run h "sh script.sh")]
    (is (= 0 (:exit r)) (:stderr r))
    (is (= "hi from script\nline 2\n" (:stdout r)))))

(deftest sh-script-file-positional-args
  ;; Script args become $1, $2, …
  (let [h (hb/make {:fs (vfs/make {"/work/s.sh" "echo \"$1 then $2\"\n"}
                                  {:cwd "/work"})
                    :fallback-host (th/fallback-host)
                    :builtins posix/standard
                    :fallback-allowlist #{}})
        r (run h "sh s.sh hello world")]
    (is (= 0 (:exit r)) (:stderr r))
    (is (= "hello then world\n" (:stdout r)))))

(deftest sh-script-file-missing-127
  (let [r (run (mk-host) "sh missing.sh")]
    (is (= 127 (:exit r)))
    (is (re-find #"No such file or directory" (:stderr r)))))

(deftest bash-alias-works
  (let [r (run (mk-host) "bash -c \"echo bashing\"")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "bashing"))))

;; ============================================================================
;; which — registry visibility
;; ============================================================================

(deftest which-finds-builtin
  (let [r (run (mk-host) "which cat")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "cat"))))

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

(deftest grep-dash-e-multiple-patterns
  ;; -e PATTERN can be repeated; also lets patterns starting with `-`
  ;; through without confusing tools.cli.
  (let [r (run (mk-host) "grep -e alpha -e gamma a.txt")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "alpha"))
    (is (str/includes? (:stdout r) "gamma"))
    (is (not (str/includes? (:stdout r) "beta")))))

(deftest grep-word-regexp
  (let [fs (vfs/make {"/work/x.txt" "foobar\nfoo\nbarfoo"} {:cwd "/work"})
        r  (run (mk-host {:fs fs}) "grep -w foo x.txt")]
    (is (= 0 (:exit r)))
    (is (= "foo\n" (:stdout r)))))

(deftest grep-files-without-match-stdin
  ;; Regression: -L in stdin mode used to print `nil`.
  (let [r (run (mk-host) "echo hello | grep -L bye")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "standard input"))))

;; ============================================================================
;; find
;; ============================================================================

(deftest find-lists-files
  (let [r (run (mk-host) "find .")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "a.txt"))
    (is (str/includes? (:stdout r) "b.txt"))))

(deftest find-name-filter
  (let [fs (vfs/make {"/work/foo.txt" "x"
                      "/work/bar.md"  "y"
                      "/work/sub/baz.txt" "z"}
                     {:cwd "/work"})
        r (run (mk-host {:fs fs}) "find . -name '*.txt'")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "foo.txt"))
    (is (str/includes? (:stdout r) "baz.txt"))
    (is (not (str/includes? (:stdout r) "bar.md")))))

(deftest find-type-filter
  ;; Paths are relative to the root we hand find — `find .` → "./dir".
  (let [fs (vfs/make {"/work/a" "x" "/work/dir/b" "y"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "find . -type d")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "./dir"))
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

(deftest find-root-no-OOM
  ;; Regression: -list-dir "/" used to return a phantom "" entry which
  ;; made find /-walk recurse forever.
  (let [fs (vfs/make {"/a" "x" "/b" "y"} {:cwd "/"})
        r (run (mk-host {:fs fs}) "find /")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "/a"))
    (is (str/includes? (:stdout r) "/b"))))

(deftest find-or-operator
  (let [fs (vfs/make {"/work/a.txt" "x" "/work/b.md" "y" "/work/c.org" "z"}
                     {:cwd "/work"})
        r (run (mk-host {:fs fs}) "find . -name '*.txt' -o -name '*.md'")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "a.txt"))
    (is (str/includes? (:stdout r) "b.md"))
    (is (not (str/includes? (:stdout r) "c.org")))))

(deftest find-not-operator
  (let [fs (vfs/make {"/work/a.txt" "x" "/work/b.md" "y"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "find . -type f -not -name '*.txt'")]
    (is (= 0 (:exit r)))
    (is (not (str/includes? (:stdout r) "a.txt")))
    (is (str/includes? (:stdout r) "b.md"))))

(deftest find-mindepth
  (let [fs (vfs/make {"/work/a" "x" "/work/sub/b" "y"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "find . -mindepth 2")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "sub/b"))
    (is (not (re-find #"^\./a$" (:stdout r))))))

(deftest find-iname-case-insensitive
  (let [fs (vfs/make {"/work/README.MD" "x" "/work/notes.md" "y"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "find . -iname '*.md'")]
    (is (str/includes? (:stdout r) "README.MD"))
    (is (str/includes? (:stdout r) "notes.md"))))

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

(deftest tr-character-classes
  ;; The single most common tr idiom — used to no-op silently.
  (let [r (run (mk-host) "echo Hello | tr '[:lower:]' '[:upper:]'")]
    (is (= 0 (:exit r)))
    (is (= "HELLO\n" (:stdout r)))))

(deftest tr-rejects-extra-operand-with-d
  ;; GNU tr exits 1 on `tr -d SET1 SET2`. We used to silently drop SET2.
  (let [r (run (mk-host) "echo abc | tr -d a b")]
    (is (= 1 (:exit r)))
    (is (re-find #"extra operand" (:stderr r)))))

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
  (let [fs (vfs/make {"/work/x" "a\nb\nc\n" "/work/y" "a\nB\nc\n"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "diff x y")]
    (is (= 1 (:exit r)))
    (is (str/includes? (:stdout r) "--- x"))
    (is (str/includes? (:stdout r) "+++ y"))
    (is (str/includes? (:stdout r) "@@ -1,3 +1,3 @@"))
    (is (str/includes? (:stdout r) "-b"))
    (is (str/includes? (:stdout r) "+B"))))

(deftest diff-size-mismatch-no-IOOB
  ;; Regression: prior diff threw IndexOutOfBoundsException whenever
  ;; len(a) != len(b) because it used diffit's intermediate indices to
  ;; index into the original a-lines.
  (let [fs (vfs/make {"/work/x" "a\n" "/work/y" "a\nb\nc\n"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "diff x y")]
    (is (= 1 (:exit r)))
    (is (str/includes? (:stdout r) "+b"))
    (is (str/includes? (:stdout r) "+c"))))

(deftest diff-brief
  (let [fs (vfs/make {"/work/x" "a" "/work/y" "b"} {:cwd "/work"})
        r (run (mk-host {:fs fs}) "diff -q x y")]
    (is (= 1 (:exit r)))
    (is (str/includes? (:stdout r) "differ"))))

;; ============================================================================
;; xargs — dispatch through same host
;; ============================================================================

(deftest xargs-echo
  (let [r (run (mk-host) "echo one two three | xargs echo")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "one"))
    (is (str/includes? (:stdout r) "two"))
    (is (str/includes? (:stdout r) "three"))))

(deftest xargs-cannot-escape
  ;; rm isn't a builtin → xargs can't reach it either.
  (let [r (run (mk-host) "echo a.txt | xargs rm")]
    (is (= 1 (:exit r)))
    (is (re-find #"muschel:" (:stderr r)))))

(deftest xargs-with-replace
  ;; -I R reads a whole line at a time per GNU semantics. Use echo -e to
  ;; produce two lines on stdin so the two invocations are reachable.
  (let [r (run (mk-host) "echo -e \"one\\ntwo\" | xargs -I X echo got X")]
    (is (= 0 (:exit r)))
    (is (str/includes? (:stdout r) "got one"))
    (is (str/includes? (:stdout r) "got two"))))

(deftest xargs-no-run-if-empty
  ;; -r / --no-run-if-empty: when stdin produces no tokens, skip the
  ;; command entirely. Otherwise xargs would still run (with no args).
  (let [r (run (mk-host) "echo -n \"\" | xargs -r echo hello")]
    (is (= 0 (:exit r)))
    (is (= "" (:stdout r)))))

;; ============================================================================
;; Stdin laziness — `:in` must not be drained for builtins that don't use it
;; ============================================================================
;;
;; Regression for the System/in hang: `build-env` used to eagerly slurp the
;; stdin source for every builtin invocation, so `(run-and-capture …)` from
;; a REPL — where `System/in` lacks EOF — blocked forever inside the slurp,
;; even for programs like `grep PAT FILE` that never read stdin.
;;
;; Now: `run` doesn't default `:in` to `System/in`, and `build-env` wraps the
;; slurp in a `delay` that builtins only force via `read-stdin`.
;;
;; These tests are JVM-only because they rely on `java.io.InputStream` /
;; `System/setIn`. The contract they verify is platform-independent.

#?(:clj
   (defn- counting-stdin
     "An InputStream that records every read() call in `counter` and
      reports EOF (-1). Used to detect whether a builtin touched stdin."
     [counter]
     (proxy [java.io.InputStream] []
       (read
         ([]      (swap! counter inc) -1)
         ([_]     (swap! counter inc) -1)
         ([_ _ _] (swap! counter inc) -1))
       (available [] 0))))

#?(:clj
   (deftest builtin-stdin-not-drained-when-unused
     (let [reads (atom 0)
           src   (counting-stdin reads)
           host  (mk-host)
           r     (m/run-and-capture (m/new-env) "grep beta a.txt"
                                    {:host host :in src})]
       (is (= 0 (:exit r)))
       (is (= "beta\n" (:stdout r)))
       (is (zero? @reads)
           "grep with file args must not touch the stdin source"))))

#?(:clj
   (deftest builtin-stdin-still-drained-when-needed
     ;; The flip side: with no files, grep DOES read stdin. The lazy
     ;; delay should be forced on first access from read-stdin.
     (let [src  (java.io.ByteArrayInputStream.
                 (.getBytes "alpha\nbeta\ngamma\n" "UTF-8"))
           host (mk-host)
           r    (m/run-and-capture (m/new-env) "grep beta"
                                   {:host host :in src})]
       (is (= 0 (:exit r)))
       (is (= "beta\n" (:stdout r))))))

#?(:clj
   (deftest run-and-capture-doesnt-block-on-open-stdin
     ;; The original bug report: from a REPL where System/in has no
     ;; writer-EOF, `(m/run-and-capture env "grep beta a.txt" {:host host})`
     ;; used to hang. Simulate that with a PipedInputStream whose writer
     ;; end is held open and never closed — slurp on it would block.
     (let [piped-in (java.io.PipedInputStream.)
           _writer  (java.io.PipedOutputStream. piped-in)  ; never closed
           prev-in  System/in]
       (try
         (System/setIn piped-in)
         (let [fut (future
                     (m/run-and-capture (m/new-env) "grep beta a.txt"
                                        {:host (mk-host)}))
               r   (deref fut 5000 :TIMEOUT)]
           (when (= :TIMEOUT r) (future-cancel fut))
           (is (not= :TIMEOUT r)
               "run-and-capture must return without slurping System/in")
           (is (= 0 (:exit r)))
           (is (= "beta\n" (:stdout r))))
         (finally (System/setIn prev-in))))))
