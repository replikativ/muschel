(ns muschel.session.spindel
  "Spindel-backed Session impl. JVM-only — spindel's transitive deps
   (anglican, kabel, tools.analyzer.jvm) don't load in babashka.

   This impl gives:
     - O(1) `fork` via spindel's COW execution contexts
     - reactive `-on-env-change` via add-watch on the spindel atom
     - (future) bg jobs as spindel `spawn!`-ed spins, so pure-Clojure
       bg work forks alongside env (external Processes still aliased
       — see memory `muschel-spindel-spins`)

   For now the JobHandle structure is the same as the AtomSession.
   The next step in this layer is to launch bg compounds as spins
   when they're pure-Clojure (no external commands), gaining true
   fork semantics for that subset."
  (:require [muschel.env :as env]
            [muschel.session :as session]
            [org.replikativ.spindel.core :as s]))

(deftype SpindelSession [ctx env-atom jobs-atom]
  session/Session
  (-env [_]
    (s/with-context ctx @env-atom))
  (-swap-env! [_ f]
    (s/with-context ctx (swap! env-atom f)))
  (-fork [_]
    ;; spindel's fork-context creates a COW copy of the entire context
    ;; (including all :atoms). The same atom-id reads from the new
    ;; context's overlay — mutations diverge.
    (let [fork-ctx (s/fork-context ctx)]
      (SpindelSession. fork-ctx env-atom jobs-atom)))
  (-jobs [_]
    (s/with-context ctx @jobs-atom))
  (-track-job! [_ job]
    (s/with-context ctx (swap! jobs-atom conj job))
    job)
  (-purge-exited! [_]
    (s/with-context ctx
      (let [before (count @jobs-atom)
            _ (swap! jobs-atom
                     (fn [js]
                       (vec (remove #(some? (session/job-exit %)) js))))]
        (- before (count @jobs-atom)))))
  (-on-env-change [_ f]
    (let [k (gensym "muschel-listener-")]
      (s/with-context ctx
        (add-watch env-atom k
                   (fn [_ _ old new]
                     (try (f old new) (catch Throwable _ nil)))))
      (fn []
        (s/with-context ctx (remove-watch env-atom k))))))

(defn spindel-session
  "Build a SpindelSession starting from `initial-env`. The atoms for
   env and the job table live in a fresh spindel execution context."
  ([] (spindel-session (env/new-env)))
  ([initial-env]
   (let [ctx (s/create-execution-context)
         env-atom (s/with-context ctx (s/atom initial-env))
         jobs-atom (s/with-context ctx (s/atom []))]
     (->SpindelSession ctx env-atom jobs-atom))))

(defn spindel-session-using
  "Build a SpindelSession on an EXISTING spindel execution context.

   Useful when the caller already has a ctx whose fork lineage the
   session should join — e.g. dvergr's chat-ctx fork inherits the
   bash session for free when the chat-ctx is forked. With
   `spindel-session` the session sits on its own root ctx and does
   not branch alongside other ctx-scoped state."
  ([ctx] (spindel-session-using ctx (env/new-env)))
  ([ctx initial-env]
   (let [env-atom (s/with-context ctx (s/atom initial-env))
         jobs-atom (s/with-context ctx (s/atom []))]
     (->SpindelSession ctx env-atom jobs-atom))))

(defn stop-session!
  "Tear down the underlying spindel context, freeing background
   threads. Call when done with a session."
  [^SpindelSession sess]
  (s/stop-context! (.ctx sess)))
