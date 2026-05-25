(ns muschel.session
  "Live session state for muschel.

   The shell *env* (in `muschel.env`) is purely-data and time-travelable
   — every operation produces a new immutable value. But some shell
   state can't be expressed that way without lying:

     - **Background processes** are running on the OS; their exit codes
       arrive asynchronously. Restoring env to a past snapshot doesn't
       un-run a process.
     - **Open file descriptors / sockets** likewise.
     - **Reactive observers** (a watcher reacting to `cd` events) need
       somewhere to live.

   A `Session` is the mutable cell that owns these live concerns. The
   env value lives inside the session and can be snapshotted, diffed,
   forked, restored — exactly the time-travel story. The job table
   stays current as processes complete.

   ## Two impls

   - `AtomSession` — atom-backed; works in babashka and JVM. Default.
   - `SpindelSession` — JVM-only (`muschel.session.spindel`); env is
     a spindel signal, forking is O(1) via overlay, subscribers fire
     reactively. Load only when needed.

   ## Forking

   `(fork session)` returns a *new* session whose env starts at the
   parent's current env. Mutations don't leak in either direction. Bg
   jobs are COPIED (vec of JobHandles is duplicated) — the jobs
   themselves are shared OS processes, so either fork can `wait`/`kill`
   them. New `&` invocations land only in the fork that issues them."
  (:require [muschel.env :as env]))

;; ============================================================================
;; JobHandle — a live background process
;; ============================================================================

(defrecord JobHandle [id pid proc exit-future])

(defn job-running?
  "True if the job's exit-future is not yet realised."
  [^JobHandle job]
  (not (realized? (:exit-future job))))

(defn job-exit
  "If the job has exited, return its integer exit code; else nil."
  [^JobHandle job]
  (when (realized? (:exit-future job))
    @(:exit-future job)))

(defn await-job
  "Block until the job exits; return its exit code."
  [^JobHandle job]
  @(:exit-future job))

;; ============================================================================
;; Session protocol
;; ============================================================================

(defprotocol Session
  (-env [this]
    "Snapshot the current env value.")
  (-swap-env! [this f]
    "Atomically replace env with `(f env)`. Returns the new env.")
  (-fork [this]
    "Return a fresh session whose env value is the current one and
     whose job table is a copy. Mutations don't leak.")
  (-jobs [this]
    "Return the current vector of JobHandles. Includes both running
     and exited jobs (callers filter via `job-running?` etc.).")
  (-track-job! [this job]
    "Append `job` to the job table. Returns the JobHandle.")
  (-purge-exited! [this]
    "Remove exited jobs from the table. Returns the count removed.")
  (-on-env-change [this f]
    "Register a callback `(f old-env new-env)` to fire on every env
     update. Returns an unsubscribe fn. May be a no-op for impls that
     don't support live observation."))

;; Default implementations via plain fns over the protocol — these
;; work uniformly across impls.

(defn snapshot
  "Return the current env value. Equivalent to `-env` but reads as a
   time-travel primitive: `(def s1 (snapshot session))` captures the
   state for later restore."
  [session]
  (-env session))

(defn restore!
  "Replace the session's env with `env`. Use after `snapshot` to
   time-travel back to a prior state."
  [session env]
  (-swap-env! session (constantly env)))

(defn- map-diff [m-a m-b]
  (reduce (fn [acc k]
            (let [a (get m-a k)
                  b (get m-b k)]
              (cond
                (= a b) acc
                (and (map? a) (map? b)) (assoc acc k (map-diff a b))
                :else (assoc acc k [a b]))))
          {}
          (into (set (keys m-a)) (keys m-b))))

(defn diff
  "Diff two env values, returning `{key [before after]}` for keys that
   changed (recurses into nested maps like `:vars`). Useful for
   inspecting what an exec did. Pairs `[before after]` use `:absent`
   when one side was unset."
  [env-a env-b]
  (map-diff env-a env-b))

(defn next-job-id
  "Return the next unused job-id (1-indexed, monotonic). Examines the
   current job table; doesn't increment any counter."
  [session]
  (let [jobs (-jobs session)]
    (if (empty? jobs)
      1
      (inc (apply max (map :id jobs))))))

;; ============================================================================
;; AtomSession — the bb-compatible default
;; ============================================================================

(deftype AtomSession [^:unsynchronized-mutable state
                      ^:unsynchronized-mutable listeners]
  Session
  (-env [_] (:env @state))

  (-swap-env! [_ f]
    (let [old (volatile! nil)
          new-state (swap! state
                           (fn [s]
                             (vreset! old (:env s))
                             (update s :env f)))
          new-env (:env new-state)]
      (doseq [l @listeners]
        (try (l @old new-env)
             (catch #?(:clj Throwable :cljs :default) _ nil)))
      new-env))

  (-fork [_]
    (let [{:keys [env jobs]} @state]
      (AtomSession. (atom {:env env :jobs (vec jobs)})
                    (atom #{}))))

  (-jobs [_] (:jobs @state))

  (-track-job! [_ job]
    (swap! state update :jobs conj job)
    job)

  (-purge-exited! [_]
    (let [before (count (:jobs @state))
          _ (swap! state update :jobs
                   (fn [js] (vec (remove #(some? (job-exit %)) js))))
          after (count (:jobs @state))]
      (- before after)))

  (-on-env-change [_ f]
    (swap! listeners conj f)
    (fn [] (swap! listeners disj f))))

(defn atom-session
  "Create a new AtomSession starting from `initial-env`."
  ([] (atom-session (env/new-env)))
  ([initial-env]
   (AtomSession. (atom {:env initial-env :jobs []})
                 (atom #{}))))

;; ============================================================================
;; Backward-compat alias for default construction
;; ============================================================================

(defn new-session
  "Create a default session. Returns an AtomSession. In JVM-Clojure
   you can pass `:spindel? true` (when `muschel.session.spindel` is
   loaded) to get a SpindelSession instead — cljs gets AtomSession
   unconditionally."
  [& {:keys [initial-env spindel?]
      :or {initial-env (env/new-env)}}]
  #?(:clj
     (if spindel?
       (if-let [v (resolve 'muschel.session.spindel/spindel-session)]
         (v initial-env)
         (throw (ex-info "spindel session requested but muschel.session.spindel not loaded"
                         {:initial-env initial-env})))
       (atom-session initial-env))
     :cljs
     (atom-session initial-env)))
