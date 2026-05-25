(ns muschel.session-test
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.env :as env]
            [muschel.session :as session]))

(deftest atom-session-basics
  (let [s (session/atom-session)]
    (is (some? (session/-env s)))
    (let [e (session/-env s)
          new-env (env/set-var e "X" "hi")]
      (session/-swap-env! s (constantly new-env))
      (is (= "hi" (env/get-var (session/-env s) "X"))))))

(deftest snapshot-restore-roundtrip
  (let [s (session/atom-session)
        e1 (session/-swap-env! s #(env/set-var % "X" "first"))
        snap (session/snapshot s)
        _   (session/-swap-env! s #(env/set-var % "X" "second"))]
    (is (= "second" (env/get-var (session/-env s) "X")))
    (session/restore! s snap)
    (is (= "first" (env/get-var (session/-env s) "X")))))

(deftest fork-isolates-mutations
  (let [parent (session/atom-session)
        _      (session/-swap-env! parent #(env/set-var % "X" "shared"))
        child  (session/-fork parent)]
    (is (= "shared" (env/get-var (session/-env child) "X")))
    ;; Mutate child — parent unaffected
    (session/-swap-env! child #(env/set-var % "X" "child-only"))
    (is (= "child-only" (env/get-var (session/-env child) "X")))
    (is (= "shared" (env/get-var (session/-env parent) "X")))
    ;; Mutate parent — child unaffected
    (session/-swap-env! parent #(env/set-var % "Y" "parent-only"))
    (is (= "" (env/get-var (session/-env child) "Y")))))

(deftest diff-shows-nested-changes
  ;; Diff recurses into nested maps, showing the deepest pair.
  (let [a (env/set-var (env/empty-env) "X" "1")
        b (env/set-var a "X" "2")
        d (session/diff a b)]
    (is (contains? d :vars))
    (is (= {"X" {:value ["1" "2"]}} (:vars d))
        "only :value within vars[X] changed, not the whole entry"))
  (let [a (env/empty-env)
        b (-> a (env/set-var "Y" "new") (env/cd "/tmp"))
        d (session/diff a b)]
    ;; :cwd appears as top-level [old new], :vars shows the new key Y
    (is (= "/tmp" (second (:cwd d))))
    (is (contains? (:vars d) "Y"))))

(deftest env-change-listener-fires
  (let [s (session/atom-session)
        events (atom [])
        unsub (session/-on-env-change s
                                      (fn [old new]
                                        (swap! events conj [old new])))]
    (session/-swap-env! s #(env/set-var % "X" "v1"))
    (session/-swap-env! s #(env/set-var % "X" "v2"))
    (is (= 2 (count @events)))
    (unsub)
    (session/-swap-env! s #(env/set-var % "X" "v3"))
    (is (= 2 (count @events)) "no events after unsubscribe")))

;; JobHandle internals use Clojure's `promise`/`deliver` for the
;; exit-future. cljs would need an analog (atom + watcher, or a
;; cljs.core.async chan). Tests gated to JVM until the cljs spawn
;; impl lands and we know which primitive to use.
#?(:clj
   (deftest job-tracking
     (let [s (session/atom-session)
           done (doto (promise) (deliver 0))
           job (session/->JobHandle 1 12345 {} done)]
       (session/-track-job! s job)
       (is (= 1 (count (session/-jobs s))))
       (is (false? (session/job-running? job)))
       (is (= 0 (session/job-exit job)))
       (is (= 1 (session/-purge-exited! s)))
       (is (empty? (session/-jobs s))))))

#?(:clj
   (deftest next-job-id-is-monotonic
     (let [s (session/atom-session)
           done (doto (promise) (deliver 0))]
       (is (= 1 (session/next-job-id s)))
       (session/-track-job! s (session/->JobHandle 1 100 {} done))
       (is (= 2 (session/next-job-id s)))
       (session/-track-job! s (session/->JobHandle 2 200 {} done))
       (is (= 3 (session/next-job-id s))))))
