(ns muschel.permit-argv-any-test
  "`:argv-any` — match a value in ANY argument position — and the loud
   rejection of a misplaced `:**`.

   The default ruleset's \"rm on a critical system path\" rule was written as

     {:kind :argv-shape :shape [\"rm\" :** #{\"/\" \"/home\" \"/etc\" …}]}

   but `:**` only means zero-or-more when it is the LAST element; anywhere else
   `shape-elt-matches?` has no case for it, so the element matched nothing and
   the rule never fired. Nothing noticed, because the blanket recursive-delete
   deny sat in front of it and caught every `rm -rf` first. An embedder that
   relaxed that broader rule — reasonable when the workspace is versioned and
   jailed, so a recursive delete is recoverable — was left with `rm -rf /etc`
   permitted by a ruleset that appeared to forbid it.

   Two fixes, both pinned here: `:argv-any` expresses the intent directly and
   position-independently, and `:argv-shape` now THROWS on a misplaced `:**` so
   a dead rule announces itself instead of quietly passing everything."
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.parse :as parse]
            [muschel.permit :as permit]))

(defn- check
  ([src] (check src [permit/default-rules]))
  ([src rulesets]
   (permit/check {:rulesets rulesets
                  :ast (parse/parse src)
                  :prompter permit/deny-all-prompter})))

(defn- check-permissive
  "Like `check`, but unmatched calls fall through to ALLOW.

   For \"this rule must NOT fire\" assertions the default deny-all prompter is
   useless: a call that matches nothing is denied by the prompter, so the
   result is `:deny` either way and the assertion cannot tell a rule that fired
   from one that did not. Falling through to allow makes that distinction
   observable."
  [src rulesets]
  (permit/check {:rulesets rulesets
                 :ast (parse/parse src)
                 :prompter permit/allow-all-prompter}))

;; ---------------------------------------------------------------------------
;; The regression: critical paths, in every flag arrangement
;; ---------------------------------------------------------------------------

(deftest rm-on-critical-paths-is-denied-whatever-the-flag-order
  (doseq [src ["rm -rf /etc"
               "rm -r -f /etc"
               "rm -fr /etc"
               "rm --recursive /etc"
               "rm -r --force /etc"
               "rm -v -r -f /etc"
               "rm /etc"
               "rm -rf /"
               "rm -rf /home"
               "rm -rf /usr"
               "rm -rf /var/lib" ; not itself listed, but /var is
               ]]
    (testing src
      (is (= :deny (:decision (check src)))
          (str src " must be denied — this is the rule that was dead")))))

(deftest ordinary-relative-deletes-are-not-caught-by-that-rule
  (testing "the critical-path rule must not swallow normal work"
    ;; These are denied by the BLANKET recursive rule, not the path rule; the
    ;; point here is that the path rule itself does not claim them, so an
    ;; embedder relaxing the blanket rule gets working recursive deletes.
    (let [relaxed [permit/default-rules
                   [{:tool :bash
                     :pattern {:kind :argv-flags :head ["rm"]
                               :any-of #{"-r" "-R" "--recursive"}}
                     :action :allow :origin :test}
                    ;; re-assert the path guard after the allow
                    {:tool :bash
                     :pattern {:kind :argv-any :head ["rm"]
                               :any #{"/" "/home" "/usr" "/etc" "/var"}}
                     :action :deny :origin :test}]]]
      (is (= :allow (:decision (check "rm -rf build" relaxed))))
      (is (= :allow (:decision (check "rm -rf node_modules" relaxed))))
      (is (= :allow (:decision (check "rm -rf ./target" relaxed))))
      (testing "…while the critical paths stay denied through the same relaxation"
        (is (= :deny (:decision (check "rm -rf /etc" relaxed))))
        (is (= :deny (:decision (check "rm -rf /" relaxed))))))))

;; ---------------------------------------------------------------------------
;; :argv-any semantics
;; ---------------------------------------------------------------------------

(deftest argv-any-matches-at-any-position
  (let [rules [[{:tool :bash
                 :pattern {:kind :argv-any :head ["mytool"] :any #{"SECRET"}}
                 :action :deny :origin :test}]]]
    (is (= :deny (:decision (check "mytool SECRET" rules))))
    (is (= :deny (:decision (check "mytool -a SECRET" rules))))
    (is (= :deny (:decision (check "mytool -a -b -c SECRET" rules))))
    (is (= :deny (:decision (check "mytool SECRET -a" rules))))
    (testing "and does not fire when the value is absent"
      (is (= :allow (:decision (check-permissive "mytool other" rules)))))))

(deftest argv-any-does-not-match-the-command-itself
  (testing "only args AFTER the head are considered"
    (let [rules [[{:tool :bash
                   :pattern {:kind :argv-any :head ["echo"] :any #{"echo"}}
                   :action :deny :origin :test}]]]
      ;; `echo` appears as the command, not as an argument
      (is (= :allow (:decision (check-permissive "echo hello" rules)))))))

(deftest argv-any-fails-closed-on-dynamic-args
  (testing "an unresolvable arg counts as a match, so a DENY refuses"
    (let [rules [[{:tool :bash
                   :pattern {:kind :argv-any :head ["rm"] :any #{"/etc"}}
                   :action :deny :origin :test}]]]
      (is (= :deny (:decision (check "rm -rf $TARGET" rules)))
          "we cannot know what $TARGET expands to — refuse rather than guess"))))

;; ---------------------------------------------------------------------------
;; A misplaced :** is now loud
;; ---------------------------------------------------------------------------

(deftest misplaced-open-wildcard-throws
  (testing "`:**` anywhere but last is a dead rule — say so instead of passing"
    (let [rules [[{:tool :bash
                   :pattern {:kind :argv-shape :shape ["rm" :** #{"/etc"}]}
                   :action :deny :origin :test}]]]
      (is (thrown-with-msg?
           #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
           #"may only be the LAST element"
           (check "rm -rf /etc" rules))))))

(deftest trailing-open-wildcard-still-works
  (testing "the legitimate use is unaffected"
    (let [rules [[{:tool :bash
                   :pattern {:kind :argv-shape :shape ["mytool" "sub" :**]}
                   :action :deny :origin :test}]]]
      (is (= :deny (:decision (check "mytool sub" rules))))
      (is (= :deny (:decision (check "mytool sub a b c" rules))))
      (is (= :allow (:decision (check-permissive "mytool other" rules)))))))
