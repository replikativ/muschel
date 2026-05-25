(ns muschel.session-spindel-test
  "Tests for the SpindelSession impl. Runs under the `:test-spindel`
   alias only — pulls spindel from `../spindel`, which has transitive
   deps not loadable in babashka. The default `:test` alias does NOT
   load this file."
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.env :as env]
            [muschel.exec :as exec]
            [muschel.session :as session]
            [muschel.session.spindel :as ss]))

(defn- with-session [body-fn]
  (let [s (ss/spindel-session)]
    (try (body-fn s)
         (finally (ss/stop-session! s)))))

(deftest snapshot-restore-via-spindel
  (with-session
    (fn [sess]
      (exec/run (env/new-env) "FOO=before; cd /tmp" {:session sess})
      (let [snap (session/snapshot sess)]
        (exec/run (env/new-env) "FOO=after; cd /usr" {:session sess})
        (is (= "after" (env/get-var (session/-env sess) "FOO")))
        (session/restore! sess snap)
        (is (= "before" (env/get-var (session/-env sess) "FOO")))
        (is (= "/tmp" (:cwd (session/-env sess))))))))

(deftest fork-isolates-via-spindel
  (with-session
    (fn [parent]
      (exec/run (env/new-env) "FOO=shared" {:session parent})
      (let [child (session/-fork parent)]
        (is (= "shared" (env/get-var (session/-env child) "FOO"))
            "fork inherits parent's env at fork-time")
        (exec/run (env/new-env) "FOO=child-only" {:session child})
        (is (= "child-only" (env/get-var (session/-env child) "FOO")))
        (is (= "shared" (env/get-var (session/-env parent) "FOO"))
            "parent unaffected by child mutations")))))

(deftest bg-tracking-via-spindel
  (with-session
    (fn [sess]
      (exec/run (env/new-env) "sleep 0.05 &" {:session sess})
      (is (= 1 (count (session/-jobs sess))))
      (Thread/sleep 100)
      (is (false? (session/job-running? (first (session/-jobs sess))))))))

(deftest env-change-listener-via-spindel
  (with-session
    (fn [sess]
      (let [events (atom [])
            unsub (session/-on-env-change sess
                                          (fn [old new]
                                            (swap! events conj [old new])))]
        (exec/run (env/new-env) "FOO=v1" {:session sess})
        (exec/run (env/new-env) "FOO=v2" {:session sess})
        (is (= 2 (count @events)))
        (unsub)
        (exec/run (env/new-env) "FOO=v3" {:session sess})
        (is (= 2 (count @events)) "no events after unsubscribe")))))
