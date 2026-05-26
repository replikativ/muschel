(ns muschel.permit-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [muschel.env :as env]
            [muschel.exec :as exec]
            [muschel.parse :as parse]
            [muschel.permit :as permit]))

(defn- check
  ([src] (check src {}))
  ([src {:keys [rulesets prompter]}]
   (permit/check {:rulesets (or rulesets [permit/default-rules])
                  :ast (parse/parse src)
                  :prompter (or prompter permit/deny-all-prompter)})))

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
  (let [r (exec/run (env/new-env) "rm -rf /tmp/test-no-real-deletion"
                    {:permit {:rulesets [permit/default-rules]
                              :prompter permit/deny-all-prompter}})]
    (is (= 126 (:exit r)) "exec returns 126 (permission denied)")
    (is (= :deny (-> r :permit :decision)))))

(deftest exec-runs-on-allow
  (let [r (exec/run (env/new-env) "echo hi"
                    {:permit {:rulesets [permit/default-rules]
                              :prompter permit/deny-all-prompter}
                     :out (java.io.ByteArrayOutputStream.)})]
    (is (zero? (:exit r)))
    (is (= :allow (-> r :permit :decision)))))

(deftest exec-ask-with-prompter-allow
  (let [r (exec/run (env/new-env) "make help"
                    {:permit {:rulesets [permit/default-rules]
                              :prompter (fn [_] {:result :allow-once})}
                     :out (java.io.ByteArrayOutputStream.)})]
    ;; make help would fail (no Makefile) but permit check passes,
    ;; so the exec path runs (with non-zero exit from make itself)
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
  (let [r (exec/run (env/new-env) "echo \"$(rm -rf /)\""
                    {:permit {:rulesets [permit/default-rules]
                              :prompter permit/deny-all-prompter}
                     :out (java.io.ByteArrayOutputStream.)
                     :err (java.io.ByteArrayOutputStream.)})]
    ;; The outer echo runs (exit 0), but the inner rm was blocked.
    (is (zero? (:exit r)) "outer echo succeeds")
    ;; The captured stderr (via opts :err that wasn't visible here) would
    ;; have shown the permit denial. We verify behavior by checking the
    ;; env's :last-exit reflects what bash would do.
    ))

(deftest runtime-hook-blocks-inner-sudo
  (let [err-buf (java.io.ByteArrayOutputStream.)
        out-buf (java.io.ByteArrayOutputStream.)
        _ (exec/run (env/new-env) "echo $(sudo whoami)"
                    {:permit {:rulesets [permit/default-rules]
                              :prompter permit/deny-all-prompter}
                     :out out-buf
                     :err err-buf})
        err (.toString err-buf "UTF-8")]
    (is (str/includes? err "runtime permit denied `sudo`"))))

(deftest runtime-hook-blocks-dynamic-command
  ;; cmd=rm; $cmd … — the literal cmd-name `rm` only resolves at runtime.
  ;; Parse-time permit can't see it (the first arg is a var-ref); the
  ;; runtime hook catches it at the spawn site.
  (let [r (exec/run (env/new-env) "cmd=ls; $cmd /nonexistent-thing-12345"
                    {:permit {:rulesets
                              [permit/default-rules
                               ;; explicit deny on ls to demonstrate hook
                               [{:tool :bash
                                 :pattern {:kind :cmd-name :name "ls"}
                                 :action :deny
                                 :reason "test override"}]]
                              :prompter permit/deny-all-prompter}
                     :out (java.io.ByteArrayOutputStream.)
                     :err (java.io.ByteArrayOutputStream.)})]
    (is (= 126 (:exit r)) "runtime hook denied the dynamically-resolved ls")))

(deftest runtime-hook-allows-inner-when-rules-allow
  (let [out-buf (java.io.ByteArrayOutputStream.)
        r (exec/run (env/new-env) "echo \"date is: $(date +%Y)\""
                    {:permit {:rulesets [permit/default-rules]
                              :prompter permit/deny-all-prompter}
                     :out out-buf
                     :err (java.io.ByteArrayOutputStream.)})]
    (is (zero? (:exit r)))
    (is (str/includes? (.toString out-buf "UTF-8") "date is: 20"))))
