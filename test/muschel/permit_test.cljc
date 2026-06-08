(ns muschel.permit-test
  "Permit-layer tests: parse-time gating, runtime hook, rule
   validation. Cross-platform — uses run-and-capture with a sandboxed
   BuiltinHost instead of raw `exec/run` + JVM streams."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [muschel.core :as m]
            [muschel.parse :as parse]
            [muschel.permit :as permit]
            [muschel.test-helpers :as th]))

(defn- check
  ([src] (check src {}))
  ([src {:keys [rulesets prompter]}]
   (permit/check {:rulesets (or rulesets [permit/default-rules])
                  :ast (parse/parse src)
                  :prompter (or prompter permit/deny-all-prompter)})))

(defn- run-perm
  "Execute `src` against a sandboxed builtin host with the given
   permit config. Returns {:exit :stdout :stderr :permit :env}."
  ([src permit-cfg] (run-perm src permit-cfg {}))
  ([src permit-cfg extra-opts]
   (m/run-and-capture
    (m/new-env) src
    (merge {:host (th/mk-host)
            :permit (merge {:rulesets [permit/default-rules]
                            :prompter permit/deny-all-prompter}
                           permit-cfg)}
           extra-opts))))

;; ============================================================================
;; Defaults: auto-allow
;; ============================================================================

(deftest defaults-allow-readonly
  (doseq [src ["ls -la"
               "cat README.md"
               "grep -r foo src"
               "wc -l x.txt"
               "echo hello"
               "pwd"
               "git status"
               "git log --oneline"
               "git diff HEAD"]]
    (testing src
      (is (= :allow (:decision (check src)))))))

(deftest defaults-deny-destructive
  (doseq [src ["sudo apt install"
               "rm -rf /tmp/x"
               "eval $X"
               "source ~/.bashrc"
               "dd if=/dev/zero of=/dev/sda"
               "reboot"
               "mkfs.ext4 /dev/sda1"]]
    (testing src
      (is (= :deny (:decision (check src)))))))

(deftest run-and-capture-propagates-denied-reason
  ;; `exec/run` returns `{:exit 126 :permit … :denied-reason "…"}` on
  ;; a parse-time deny. `run-and-capture` must surface BOTH fields —
  ;; the reason is what callers want for a user-visible error message.
  (let [r (run-perm "rm -rf /tmp/x" {})]
    (is (= 126 (:exit r)))
    (is (string? (:denied-reason r))
        "run-and-capture should propagate :denied-reason from run")
    (is (= :deny (:decision (:permit r))))))

(deftest defaults-ask-for-unknown
  (let [r (check "make install" {:prompter (fn [_] {:result :allow-once})})]
    (is (= :allow (:decision r)))
    (is (= 1 (count (:prompted r))))))

(deftest defaults-argv-shape-git-push
  ;; `git push` is allowed; force-push variants deny.
  (is (= :allow (:decision (check "git push origin main"))))
  (is (= :deny  (:decision (check "git push --force origin main"))))
  (is (= :deny  (:decision (check "git push -f"))))
  (is (= :deny  (:decision (check "git push --force-with-lease"))))
  (is (= :deny  (:decision (check "git push origin --force main")))))

(deftest defaults-argv-shape-git-history-wipes
  (is (= :deny (:decision (check "git reset --hard HEAD"))))
  (is (= :deny (:decision (check "git clean -fd"))))
  (is (= :deny (:decision (check "git clean -fdx")))))

(deftest defaults-argv-shape-chmod-octals
  ;; Explicit octals: 0755 allowed, 0777 / 0666 denied.
  (is (= :allow (:decision (check "chmod 0755 script.sh"))))
  (is (= :deny  (:decision (check "chmod 0777 some.sh"))))
  (is (= :deny  (:decision (check "chmod 666 readable.txt")))))

(deftest defaults-argv-shape-rm-system-paths
  (is (= :deny (:decision (check "rm -rf /"))))
  (is (= :deny (:decision (check "rm -rf /etc"))))
  (is (= :deny (:decision (check "rm -rf /usr")))))

(deftest defaults-argv-shape-shell-c
  ;; `sh -c \"…\"` is allowed (muschel re-parses through the same gates);
  ;; bare `sh` asks.
  (is (= :allow (:decision (check "sh -c \"echo hi\""))))
  (is (= :allow (:decision (check "bash -c \"git status\""))))
  (let [r (check "sh" {:prompter (fn [_] {:result :allow-once})})]
    (is (= :allow (:decision r)))
    (is (= 1 (count (:prompted r))))))

;; ============================================================================
;; Matcher kinds
;; ============================================================================

(deftest cmd-name-matcher
  (let [rs [[{:tool :bash :pattern {:kind :cmd-name :name "mycmd"} :action :allow}]]]
    (is (= :allow (:decision (check "mycmd anything" {:rulesets rs}))))
    (is (not= :allow (:decision (check "othercmd" {:rulesets rs}))))))

(deftest argv-vec-matcher-exact-prefix
  (let [rs [[{:tool :bash :pattern {:kind :argv-vec :vec ["git" "status"]}
              :action :allow}]]]
    (is (= :allow (:decision (check "git status" {:rulesets rs}))))
    (is (= :allow (:decision (check "git status -s" {:rulesets rs}))))
    (is (not= :allow (:decision (check "git log" {:rulesets rs}))))))

(deftest argv-vec-matcher-alternatives
  (let [rs [[{:tool :bash :pattern {:kind :argv-vec
                                    :vec ["git" #{"status" "log" "diff"}]}
              :action :allow}]]]
    (is (= :allow (:decision (check "git status" {:rulesets rs}))))
    (is (= :allow (:decision (check "git log" {:rulesets rs}))))
    (is (= :allow (:decision (check "git diff" {:rulesets rs}))))
    (is (not= :allow (:decision (check "git push" {:rulesets rs}))))))

(deftest argv-glob-matcher
  (let [rs [[{:tool :bash :pattern {:kind :argv-glob :glob "npm *"}
              :action :allow}]]]
    (is (= :allow (:decision (check "npm install" {:rulesets rs}))))
    (is (= :allow (:decision (check "npm test" {:rulesets rs}))))
    (is (not= :allow (:decision (check "yarn install" {:rulesets rs}))))))

(deftest argv-shape-exact-length
  ;; Shape matches exact length only — `git push X` doesn't match
  ;; `["git" "push"]`. This is the key difference from :argv-vec.
  (let [rs [[{:tool :bash :pattern {:kind :argv-shape :shape ["git" "push"]}
              :action :allow}]]]
    (is (= :allow (:decision (check "git push" {:rulesets rs}))))
    (is (not= :allow (:decision (check "git push --force" {:rulesets rs}))))
    (is (not= :allow (:decision (check "git status" {:rulesets rs}))))))

(deftest argv-shape-allow-push-deny-force
  ;; The headline use case: allow `git push` but deny `git push --force`.
  ;; Layer broad-allow with specific-deny under last-match-wins.
  (let [rs [[{:tool :bash :pattern {:kind :argv-shape :shape ["git" "push" :**]}
              :action :allow}
             {:tool :bash :pattern {:kind :argv-shape
                                    :shape ["git" "push" #{"--force" "-f"} :**]}
              :action :deny}]]]
    (is (= :allow (:decision (check "git push"          {:rulesets rs}))))
    (is (= :allow (:decision (check "git push origin"   {:rulesets rs}))))
    (is (= :deny  (:decision (check "git push --force"  {:rulesets rs}))))
    (is (= :deny  (:decision (check "git push -f main"  {:rulesets rs}))))))

(deftest argv-shape-wildcards
  ;; :* matches exactly one slot; :** matches the rest.
  (let [rs [[{:tool :bash :pattern {:kind :argv-shape :shape ["mv" :* :*]}
              :action :allow}
             {:tool :bash :pattern {:kind :argv-shape :shape ["touch" :**]}
              :action :allow}]]]
    (is (= :allow (:decision (check "mv a b" {:rulesets rs}))))
    (is (not= :allow (:decision (check "mv a" {:rulesets rs}))))
    (is (not= :allow (:decision (check "mv a b c" {:rulesets rs}))))
    (is (= :allow (:decision (check "touch" {:rulesets rs}))))
    (is (= :allow (:decision (check "touch a" {:rulesets rs}))))
    (is (= :allow (:decision (check "touch a b c" {:rulesets rs}))))))

(deftest argv-shape-regex
  ;; A regex slot matches if re-find returns truthy.
  (let [rs [[{:tool :bash :pattern {:kind :argv-shape
                                    :shape ["docker" "run" #"^[a-z]+:[\d.]+$"]}
              :action :allow}]]]
    (is (= :allow (:decision (check "docker run nginx:1.27" {:rulesets rs}))))
    (is (not= :allow (:decision (check "docker run nginx:latest-evil" {:rulesets rs}))))))

;; Helper: matched? checks whether the user's rule actually fired (vs falling
;; through to the prompter). When no rule matches, :matched-rule is nil; we
;; use a permissive prompter so the final decision tells us nothing.
(defn- matched-action
  "Return the action of the rule that matched `src` under `rs`, or :ask
   if none matched (decided by the prompter)."
  [src rs]
  (let [r (check src {:rulesets rs
                      :prompter (fn [_] {:result :allow-once})})
        rule (some-> r :prompted first :matched-rule)]
    (cond
      rule                  (:action rule)
      (seq (:prompted r))   :ask
      :else                 (:decision r))))

(deftest argv-flags-any-of
  ;; Deny `rm` if ANY of -r/-R/--recursive shows up anywhere in argv.
  (let [rs [[{:tool :bash
              :pattern {:kind :argv-flags
                        :head ["rm"]
                        :any-of #{"-r" "-R" "--recursive"}}
              :action :deny}]]]
    (is (= :deny (matched-action "rm -r /tmp/x" rs)))
    (is (= :deny (matched-action "rm -R /tmp/x" rs)))
    (is (= :deny (matched-action "rm --recursive /tmp/x" rs)))
    (is (= :deny (matched-action "rm -rf /tmp/x" rs)))    ; cluster split
    (is (= :deny (matched-action "rm -fR /tmp/x" rs)))
    (is (= :deny (matched-action "rm /tmp/x -r" rs)))     ; flag AFTER positional
    (is (= :ask  (matched-action "rm /tmp/x" rs)))
    (is (= :ask  (matched-action "rm -f /tmp/x" rs)))))   ; -f alone, no -r

(deftest argv-flags-all-of-plus-any-of
  ;; git clean: dangerous only when -f is combined with -d or -x.
  (let [rs [[{:tool :bash
              :pattern {:kind :argv-flags
                        :head ["git" "clean"]
                        :all-of #{"-f"}
                        :any-of #{"-d" "-x"}}
              :action :deny}]]]
    (is (= :deny (matched-action "git clean -fd"     rs)))
    (is (= :deny (matched-action "git clean -df"     rs)))
    (is (= :deny (matched-action "git clean -f -d"   rs))) ; split clusters
    (is (= :deny (matched-action "git clean -d -f"   rs))) ; order-insensitive
    (is (= :deny (matched-action "git clean -fdx"    rs)))
    (is (= :ask  (matched-action "git clean -f"      rs))) ; missing :any-of
    (is (= :ask  (matched-action "git clean -d"      rs))))) ; missing :all-of

(deftest argv-flags-none-of
  (let [rs [[{:tool :bash
              :pattern {:kind :argv-flags
                        :head ["docker" "run"]
                        :none-of #{"--privileged"}}
              :action :allow}]]]
    (is (= :allow (matched-action "docker run nginx"          rs)))
    (is (= :allow (matched-action "docker run --rm -it nginx" rs)))
    (is (= :ask   (matched-action "docker run --privileged nginx" rs)))))

(deftest argv-flags-long-with-equals
  ;; `--prefix=/x` strips =/x → contributes `"--prefix"` to the set.
  (let [rs [[{:tool :bash
              :pattern {:kind :argv-flags
                        :head ["npm" "install"]
                        :any-of #{"--prefix"}}
              :action :deny}]]]
    (is (= :deny (matched-action "npm install --prefix=/tmp" rs)))
    (is (= :deny (matched-action "npm install --prefix /tmp" rs)))
    (is (= :ask  (matched-action "npm install lodash"        rs)))))

(deftest argv-flags-double-dash-terminates
  ;; After `--`, tokens are positional, not flags.
  (let [rs [[{:tool :bash
              :pattern {:kind :argv-flags
                        :head ["rm"]
                        :any-of #{"-r"}}
              :action :deny}]]]
    (is (= :deny (matched-action "rm -r foo"     rs)))
    (is (= :ask  (matched-action "rm -- -r"      rs)))    ; -r is a filename
    (is (= :ask  (matched-action "rm -- foo -r"  rs)))))

(deftest argv-flags-numeric-not-split
  ;; `-9` (kill signal) etc. should NOT be split letter-by-letter. The
  ;; cluster rule only splits letter clusters; non-letter content stays
  ;; as a single opaque flag token.
  (let [rs [[{:tool :bash
              :pattern {:kind :argv-flags
                        :head ["kill"]
                        :any-of #{"-9"}}
              :action :deny}]]]
    (is (= :deny (matched-action "kill -9 1234"  rs)))
    (is (= :ask  (matched-action "kill -15 1234" rs)))))

(deftest argv-flags-dynamic-fails-closed
  ;; If a tail token is dynamic ($cmd), the matcher can't decide. The
  ;; deny rule must NOT fire silently — call falls through to ask.
  (let [rs [[{:tool :bash
              :pattern {:kind :argv-flags
                        :head ["rm"]
                        :any-of #{"-r"}}
              :action :deny}]]]
    (is (= :ask (matched-action "rm $FLAG file" rs)))))

(deftest ast-pred-matcher
  ;; Allow any call whose first arg starts with "git" (a fake-prefix pred)
  (let [rs [[{:tool :bash
              :pattern {:kind :ast-pred
                        :pred (fn [call]
                                (let [a (some-> call :args first
                                                :parts first :value)]
                                  (and a (.startsWith ^String a "git"))))}
              :action :allow}]]]
    (is (= :allow (:decision (check "gitfoo bar" {:rulesets rs}))))
    (is (not= :allow (:decision (check "nope" {:rulesets rs}))))))

;; ============================================================================
;; Pipeline: every leaf-call must pass
;; ============================================================================

(deftest pipeline-all-leaves-checked
  ;; cat is allowed, grep is allowed, wc is allowed → overall allow
  (is (= :allow (:decision (check "cat x.txt | grep foo | wc -l"))))
  ;; But mix in a deny:
  (is (= :deny (:decision (check "cat x.txt | sudo grep root /etc/passwd")))))

;; ============================================================================
;; Prompter: allow-once / allow-always / deny variants
;; ============================================================================

(deftest prompter-allow-once-no-new-rule
  (let [r (check "make build"
                 {:prompter (fn [_] {:result :allow-once})})]
    (is (= :allow (:decision r)))
    (is (empty? (:new-rules r)))))

(deftest prompter-allow-always-generates-rule
  (let [r (check "git commit -m hi"
                 {:prompter (fn [_]
                              {:result :allow-always :scope :cmd-name})})]
    (is (= :allow (:decision r)))
    (is (= 1 (count (:new-rules r))))
    (let [rule (first (:new-rules r))]
      (is (= :allow (:action rule)))
      (is (= :cmd-name (-> rule :pattern :kind)))
      (is (= "git" (-> rule :pattern :name))))))

(deftest prompter-allow-always-argv-prefix-scope
  (let [r (check "make build"
                 {:prompter (fn [_]
                              {:result :allow-always :scope :argv-prefix})})
        rule (first (:new-rules r))]
    (is (= :argv-glob (-> rule :pattern :kind)))
    (is (= "make build *" (-> rule :pattern :glob)))))

(deftest prompter-allow-always-exact-scope
  (let [r (check "make build --release"
                 {:prompter (fn [_]
                              {:result :allow-always :scope :exact})})
        rule (first (:new-rules r))]
    (is (= :argv-vec (-> rule :pattern :kind)))
    (is (= ["make" "build" "--release"] (-> rule :pattern :vec)))))

;; ============================================================================
;; Rule ordering: later wins (per opencode semantics)
;; ============================================================================

(deftest later-rules-override-earlier
  (let [rs [;; Defaults allow ls
            permit/default-rules
            ;; User overrides: deny ls
            [{:tool :bash :pattern {:kind :cmd-name :name "ls"}
              :action :deny :reason "user override"}]]]
    (is (= :deny (:decision (check "ls" {:rulesets rs}))))))

;; ============================================================================
;; Integration with exec.run
;; ============================================================================

(deftest exec-blocks-on-deny
  (let [r (run-perm "rm -rf /tmp/test-no-real-deletion" {})]
    (is (= 126 (:exit r)) "exec returns 126 (permission denied)")
    (is (= :deny (-> r :permit :decision)))))

(deftest exec-runs-on-allow
  (let [r (run-perm "echo hi" {})]
    (is (zero? (:exit r)))
    (is (= :allow (-> r :permit :decision)))))

(deftest exec-ask-with-prompter-allow
  (let [r (run-perm "make help" {:prompter (fn [_] {:result :allow-once})})]
    ;; `make` isn't a builtin and isn't allowlisted on the sandboxed
    ;; host — the BuiltinHost refuses it after the permit gate, but
    ;; permit's own decision is what we're asserting here.
    (is (= :allow (-> r :permit :decision)))))

;; ============================================================================
;; Rule validation
;; ============================================================================

(deftest validate-good-rule
  (is (nil? (permit/validate-rule
             {:tool :bash :pattern {:kind :cmd-name :name "ls"}
              :action :allow}))))

(deftest validate-bad-rules
  (is (string? (permit/validate-rule {:tool :xx :pattern {:kind :cmd-name :name "ls"}
                                      :action :allow})))
  (is (string? (permit/validate-rule {:tool :bash :pattern {:kind :nope :name "x"}
                                      :action :allow})))
  (is (string? (permit/validate-rule {:tool :bash :pattern {:kind :cmd-name :name "x"}
                                      :action :nope}))))

;; ============================================================================
;; Runtime permit hook — catches inner cmd-subst and dynamic commands
;; ============================================================================
;;
;; Because we parse cmd-subst bodies LAZILY (matching bash), permit-at-parse
;; sees only the outer commands. The runtime hook in `exec/run-external`
;; checks every actual spawn against the same rulesets. These tests verify
;; the runtime path catches what static parse-time check can't.

(deftest runtime-hook-blocks-inner-cmd-subst-rm
  (let [r (run-perm "echo \"$(rm -rf /)\"" {})]
    ;; The outer echo runs (exit 0), but the inner rm was blocked.
    (is (zero? (:exit r)) "outer echo succeeds")))

(deftest runtime-hook-blocks-inner-sudo
  (let [r (run-perm "echo $(sudo whoami)" {})]
    (is (str/includes? (:stderr r) "runtime permit denied `sudo`"))))

(deftest runtime-hook-blocks-dynamic-command
  ;; cmd=rm; $cmd … — the literal cmd-name `rm` only resolves at runtime.
  ;; Parse-time permit can't see it (the first arg is a var-ref); the
  ;; runtime hook catches it at the spawn site.
  (let [r (run-perm "cmd=ls; $cmd /nonexistent-thing-12345"
                    {:rulesets
                     [permit/default-rules
                      ;; explicit deny on ls to demonstrate hook
                      [{:tool :bash
                        :pattern {:kind :cmd-name :name "ls"}
                        :action :deny
                        :reason "test override"}]]})]
    (is (= 126 (:exit r)) "runtime hook denied the dynamically-resolved ls")))

(deftest runtime-hook-allows-inner-when-rules-allow
  (let [r (run-perm "echo \"date is: $(date +%Y)\"" {})]
    (is (zero? (:exit r)))
    (is (str/includes? (:stdout r) "date is: 20"))))
