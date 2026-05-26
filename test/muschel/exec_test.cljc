(ns muschel.exec-test
  "End-to-end execution tests. Each test runs a bash source string
   through parse → expand → exec and asserts on stdout/exit/env.

   Cross-platform — runs against a sandboxed BuiltinHost on both JVM
   and Node so the same muschel builtins (cat, tr, grep, true, false,
   echo, …) are used regardless of platform. The few tests that
   intrinsically need real-disk side effects are reader-conded
   `#?(:clj …)`."
  (:require #?(:clj [babashka.fs :as fs])
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [muschel.core :as m]
            [muschel.env :as env]
            [muschel.session :as session]
            [muschel.test-helpers :as th]))

(defn- run
  "Convenience runner. `:env` overrides default; `:session` threads
   a session through (the starting env is then the session's current
   value, not a fresh one — preserving prior cd/var state). `:fs`
   overrides the default in-memory VFS used by the host."
  [src & {:keys [env session host]}]
  (let [start-env (or env
                      (when session (session/-env session))
                      (env/new-env))
        host (or host (th/mk-host))
        opts (cond-> {:host host}
               session (assoc :session session))]
    (m/run-and-capture start-env src opts)))

(defn- stdout [src & opts]
  (:stdout (apply run src opts)))

(defn- exit [src & opts]
  (:exit (apply run src opts)))

;; ============================================================================
;; Builtins
;; ============================================================================

(deftest builtin-echo
  (is (= "hello\n" (stdout "echo hello")))
  (is (= "hello world\n" (stdout "echo hello world")))
  (is (= "no nl" (stdout "echo -n no nl"))))

(deftest builtin-true-false
  (is (zero? (exit "true")))
  (is (= 1 (exit "false")))
  (is (zero? (exit ":"))))

(deftest builtin-cd-pwd
  ;; cd to a directory that exists in the sandbox VFS.
  (let [host (th/mk-host {:files {"/tmp" :dir}})
        {:keys [env]} (run "cd /tmp && pwd" :host host)]
    (is (= "/tmp" (:cwd env)))))

(deftest cd-then-relative-path-resolves
  ;; Builtins consult (fs/cwd fs) — so the BuiltinHost must sync the
  ;; FS cwd to env's cwd at dispatch, otherwise `cd /work && cat a.txt`
  ;; fails because `cat` looks for `a.txt` under the FS's stale cwd.
  (let [host (th/mk-host {:files {"/work/a.txt" "alpha\nbeta\n"}})]
    (testing "cat reads a relative path after cd within a single run"
      (let [{:keys [exit stdout]} (run "cd /work && cat a.txt" :host host)]
        (is (zero? exit))
        (is (= "alpha\nbeta\n" stdout))))
    (testing "pwd reports the env's cwd after cd"
      (let [{:keys [exit stdout]} (run "cd /work && pwd" :host host)]
        (is (zero? exit))
        (is (= "/work\n" stdout))))))

(deftest builtin-export-changes-process-env
  (let [{:keys [env]} (run "export FOO=bar")]
    (is (env/exported? env "FOO"))
    (is (= "bar" (env/get-var env "FOO")))))

(deftest builtin-unset
  (let [e (-> (env/new-env) (env/set-var "FOO" "bar"))
        {:keys [env]} (run "unset FOO" :env e)]
    (is (not (env/declared? env "FOO")))))

(deftest builtin-set-options
  (let [{:keys [env]} (run "set -e")]
    (is (true? (env/option env :errexit)))))

;; ============================================================================
;; Sequence / logical operators
;; ============================================================================

(deftest sequence-semi
  (is (= "a\nb\nc\n" (stdout "echo a; echo b; echo c"))))

(deftest sequence-newline
  (is (= "a\nb\n" (stdout "echo a\necho b"))))

(deftest logical-and
  (is (= "yes\n" (stdout "true && echo yes")))
  (is (= "" (stdout "false && echo no")))
  (is (= 1 (exit "false && echo no"))))

(deftest logical-or
  (is (= "recovered\n" (stdout "false || echo recovered")))
  (is (zero? (exit "true || echo never"))))

(deftest logical-chain
  (is (= "ok\n" (stdout "true && true && echo ok"))))

;; ============================================================================
;; Pipelines
;; ============================================================================

(deftest pipe-builtin-to-external
  (is (= "HELLO\n" (stdout "echo hello | tr a-z A-Z"))))

(deftest pipe-multi-stage
  ;; `tac` (line reverse) isn't in our builtin set; use a 3-stage
  ;; pipeline that's equivalent for the purpose of testing pipe wiring.
  (is (= "ABC\n" (stdout "echo a b c | tr -d ' ' | tr a-z A-Z"))))

(deftest pipe-exit-code-is-last
  ;; Without pipefail, the pipeline's exit is the last cmd's
  (is (zero? (exit "false | true"))))

(deftest pipefail-uses-leftmost-nonzero
  (let [{:keys [exit]} (run "set -o pipefail; false | true"
                            :env (env/set-option (env/new-env) :pipefail true))]
    ;; pipefail option set externally — set -o pipefail support is partial,
    ;; so we exercise the env-option path directly
    (is (= 1 exit))))

;; ============================================================================
;; Variable assignment / expansion
;; ============================================================================

(deftest naked-assignment-persists
  (let [{:keys [env]} (run "FOO=bar")]
    (is (= "bar" (env/get-var env "FOO")))))

(deftest prefix-assignment-is-scoped-to-cmd
  ;; FOO=bar only applies to `env`; doesn't persist
  (let [{:keys [env stdout]} (run "FOO=bar env" :env (env/new-env))]
    (is (str/includes? stdout "FOO=bar"))
    ;; FOO not visible after the command
    (is (not (env/declared? env "FOO")))))

(deftest var-substitution-in-args
  (let [e (-> (env/new-env) (env/set-var "X" "hello"))]
    (is (= "hello\n" (stdout "echo $X" :env e)))
    (is (= "hello world\n" (stdout "echo \"$X world\"" :env e)))
    (is (= "default\n" (stdout "echo ${UNSET:-default}" :env e)))))

;; ============================================================================
;; Redirections
;; ============================================================================

(deftest redirect-stdout-to-file
  ;; In-VFS round trip: write via redirect, read via cat.
  (let [host (th/mk-host)]
    (run "echo redirected > /tmp/r.txt" :host host)
    (is (= "redirected\n" (stdout "cat /tmp/r.txt" :host host)))))

(deftest redirect-append
  (let [host (th/mk-host)]
    (run "echo first > /tmp/a.txt"  :host host)
    (run "echo second >> /tmp/a.txt" :host host)
    (is (= "first\nsecond\n" (stdout "cat /tmp/a.txt" :host host)))))

(deftest redirect-stdin-from-file
  (let [host (th/mk-host {:files {"/tmp/in.txt" "from-file\n"}})]
    (is (= "from-file\n" (stdout "cat < /tmp/in.txt" :host host)))))

(deftest heredoc-as-stdin
  (is (= "hello\nworld\n" (stdout "cat <<EOF\nhello\nworld\nEOF"))))

(deftest err-to-out
  ;; `2>&1` merges stderr into stdout
  (is (= "" (:stderr (run "sh -c 'echo err 1>&2' 2>&1"))))
  (is (re-find #"err" (:stdout (run "sh -c 'echo err 1>&2' 2>&1")))))

;; ============================================================================
;; Compound commands
;; ============================================================================

(deftest if-then-only
  (is (= "yes\n" (stdout "if true; then echo yes; fi"))))

(deftest if-then-else
  (is (= "no\n" (stdout "if false; then echo yes; else echo no; fi"))))

(deftest if-elif
  (is (= "two\n" (stdout "if false; then echo one; elif true; then echo two; else echo three; fi"))))

(deftest for-loop
  (is (= "a\nb\nc\n" (stdout "for x in a b c; do echo $x; done"))))

(deftest while-loop-bounded-by-file
  ;; Iterate until a sentinel file appears; avoids needing arithmetic.
  (let [host (th/mk-host)
        {:keys [stdout exit]}
        (run "while [ ! -f /tmp/sentinel ]; do echo tick; touch /tmp/sentinel; done"
             :host host)]
    (is (zero? exit))
    (is (= "tick\n" stdout))))

(deftest while-loop-with-arithmetic
  ;; Now that arith is wired, the canonical counter loop works.
  (is (= "0\n1\n2\n"
         (stdout "x=0; while [ $x != 3 ]; do echo $x; x=$((x+1)); done"))))

;; ============================================================================
;; Arithmetic
;; ============================================================================

(deftest arith-expansion
  (is (= "3\n"  (stdout "echo $((1+2))")))
  (is (= "10\n" (stdout "x=5; echo $((x*2))")))
  (is (= "55\n" (stdout "echo $((1+2+3+4+5+6+7+8+9+10))"))))

(deftest arith-command
  ;; `((expr))` succeeds (exit 0) iff result is non-zero
  (is (zero?  (exit "((1+1==2))")))
  (is (= 1    (exit "((1+1==3))")))
  (is (= 1    (exit "((0)) "))))

(deftest arith-mutates-env
  (let [{:keys [env]} (run "((x = 42))")]
    (is (= "42" (env/get-var env "x")))))

(deftest let-builtin
  (is (= "10\n" (stdout "let x=10; echo $x")))
  ;; let exits 0 iff the LAST expr is non-zero
  (is (zero? (exit "let x=1")))
  (is (= 1   (exit "let x=0"))))

(deftest c-style-for
  (is (= "0\n1\n2\n3\n4\n"
         (stdout "for ((i=0; i<5; i++)); do echo $i; done"))))

(deftest c-style-for-empty-cond
  ;; Empty condition means infinite — break via exit
  (is (= "0\n1\n2\n"
         (stdout "for ((i=0; ; i++)); do echo $i; ((i >= 2)) && exit 0; done"))))

;; ============================================================================
;; case
;; ============================================================================

(deftest case-exact-match
  (is (= "matched\n"
         (stdout "case foo in foo) echo matched;; *) echo other;; esac"))))

(deftest case-alternation
  (is (= "bc\n"
         (stdout "case bar in foo) echo n;; bar|baz) echo bc;; esac"))))

(deftest case-glob-pattern
  (is (= "text\n"
         (stdout "case 'file.txt' in *.txt) echo text;; *.log) echo log;; esac"))))

(deftest case-fallthrough-semi-amp
  ;; ;& falls through to the NEXT clause's body unconditionally
  (is (= "a\nb\n"
         (stdout "case a in a) echo a;& b) echo b;; c) echo c;; esac"))))

(deftest case-fallthrough-semi-semi-amp
  ;; ;;& tries the next clauses' patterns
  (is (= "a\nother\n"
         (stdout "case a in a) echo a;;& aa) echo aa;; *) echo other;; esac"))))

(deftest case-no-match
  (is (= "" (stdout "case nomatch in xyz) echo x;; esac")))
  (is (zero? (exit "case nomatch in xyz) echo x;; esac"))))

;; ============================================================================
;; Backgrounding
;; ============================================================================

(deftest background-sets-bang-and-tracks
  (let [{:keys [env session]} (run "true &")]
    (is (not= "" (env/get-var env "!")) "$! set after `&`")
    (is (= 1 (count (session/-jobs session))))))

(deftest wait-blocks-until-bg-completes
  (let [r (run "sleep 0.05 &
                wait")
        sess (:session r)
        job (first (session/-jobs sess))]
    (is (zero? (:exit r)))
    (is (false? (session/job-running? job)))
    (is (= 0 (session/job-exit job)))))

#?(:clj
   (deftest jobs-builtin-reports-state
     ;; JVM-only: background jobs land on a JVM thread; the CLJS host's
     ;; `-async` is synchronous so `&` runs inline and "Running" is never
     ;; observable. The same code paths are exercised by
     ;; `bg-shared-session-across-runs` below, which works on both.
     (let [{:keys [session]} (run "sleep 0.1 &")
           {sout1 :stdout} (run "jobs" :session session)
           _ (Thread/sleep 200)
           {sout2 :stdout} (run "jobs" :session session)]
       (is (str/includes? sout1 "Running"))
       (is (str/includes? sout2 "Done")))))

(deftest bg-shared-session-across-runs
  ;; Two run calls sharing the same session — second sees first's job.
  (let [sess (session/atom-session (env/new-env))
        _ (run "sleep 0.05 &" :session sess)
        {sout :stdout} (run "jobs" :session sess)]
    (is (str/includes? sout "Running"))
    (run "wait" :session sess)
    (let [{sout :stdout} (run "jobs" :session sess)]
      (is (str/includes? sout "Done")))))

(deftest fork-isolates-bg-jobs
  ;; New bg in parent isn't seen in fork (and vice versa).
  (let [parent (session/atom-session (env/new-env))
        _ (run "sleep 0.05 &" :session parent)
        child (session/-fork parent)]
    (is (= 1 (count (session/-jobs child)))
        "fork inherits jobs known at fork-time")
    (run "sleep 0.05 &" :session child)
    (is (= 2 (count (session/-jobs child))))
    (is (= 1 (count (session/-jobs parent)))
        "parent's job table unaffected by child's new bg")))

(deftest kill-builtin-exists
  ;; Spawn a long-running bg, then kill it. We use synthetic pids so
  ;; kill is best-effort; just exercise the code path.
  (let [sess (session/atom-session (env/new-env))
        _ (run "sleep 10 &" :session sess)
        pid (env/get-var (session/-env sess) "!")
        _ (run (str "kill " pid) :session sess)]
    (is (= 1 (count (session/-jobs sess))))))

(deftest snapshot-and-restore-time-travel
  ;; Verify: env state can be snapshotted, mutated, restored.
  (let [host (th/mk-host {:files {"/tmp" :dir "/usr" :dir}})
        sess (session/atom-session (env/new-env))
        _ (run "FOO=before; cd /tmp" :session sess :host host)
        snap (session/snapshot sess)
        _ (run "FOO=after; cd /usr" :session sess :host host)]
    (is (= "after" (env/get-var (session/-env sess) "FOO")))
    (is (= "/usr" (:cwd (session/-env sess))))
    (session/restore! sess snap)
    (is (= "before" (env/get-var (session/-env sess) "FOO")))
    (is (= "/tmp" (:cwd (session/-env sess))))))

(deftest brace-group
  (is (= "x\ny\n" (stdout "{ echo x; echo y; }"))))

(deftest subshell-isolates-cwd
  (let [{:keys [env]} (run "(cd /tmp); pwd")]
    ;; cd inside (...) doesn't leak
    (is (not= "/tmp" (:cwd env)))))

;; ============================================================================
;; Functions
;; ============================================================================

(deftest function-def-and-call
  (is (= "from-fn\n" (stdout "myfn() { echo from-fn; }\nmyfn"))))

(deftest function-with-pos-args
  (is (= "first second\n"
         (stdout "greet() { echo $1 $2; }\ngreet first second"))))

;; ============================================================================
;; Globs
;; ============================================================================

(deftest glob-expansion-files
  ;; Pre-seed the VFS with the three files and run from /work.
  (let [host (th/mk-host {:files {"/work/a.txt" ""
                                  "/work/b.txt" ""
                                  "/work/c.log" ""}
                          :cwd "/work"})
        out (stdout "echo *.txt" :host host)]
    (is (= "a.txt b.txt\n" out))))

;; ============================================================================
;; Cmd substitution
;; ============================================================================

(deftest cmd-subst-paren
  (is (= "hello\n" (stdout "echo $(echo hello)"))))

(deftest cmd-subst-backtick
  (is (= "hello\n" (stdout "echo `echo hello`"))))

(deftest cmd-subst-strips-trailing-newlines
  (is (= "no-newline\n" (stdout "echo $(echo no-newline)"))))

;; ============================================================================
;; The V1 canonical: cd && ls *glob | head
;; ============================================================================

(deftest v1-canonical-vertical
  (let [host (th/mk-host
              {:files (into {} (for [n ["a.clj" "b.clj" "c.clj"
                                        "d.clj" "e.clj" "f.txt"]]
                                 [(str "/work/" n) ""]))
               :cwd "/work"})
        r (run "ls *.clj | head -3" :host host)]
    (is (zero? (:exit r)))
    ;; head -3 emits the first three .clj files
    (is (= ["a.clj" "b.clj" "c.clj"]
           (str/split-lines (:stdout r))))))
