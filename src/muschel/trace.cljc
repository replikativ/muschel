(ns muschel.trace
  "Introspection capture for muschel runs.

   Two-tier model:

   1. **Bounded accumulator** (default off). When run-and-capture is
      called with `:trace true` (or a map of options), env carries a
      trace-state with volatile-vectors holding tool/fs/denied events.
      The vectors are RING-BUFFERS — once a channel hits `:cap` events,
      oldest entries are dropped. Memory cannot grow unboundedly even
      under a runaway script.

   2. **Streaming hooks** (default off, unbounded by muschel). The
      same trace-state may carry `:on-tool`, `:on-fs`, `:on-deny`
      callbacks. Each event is passed to the hook **before** the
      ring-buffer is updated. Hooks decide storage:
        - dvergr persists to datahike (full history for training data)
        - a TUI logger prints
        - a test collects into an atom
      Muschel never grows past `:cap` regardless of what the hook does.

   Event shapes:

       {:type :tool   :name str :argv [str…] :exit int :duration-ms long?
        :stdout-bytes long? :stderr-bytes long?}

       {:type :fs     :op kw   :path str :ok? bool}
         where :op ∈ #{:resolve :read-file :read-bytes :open-source
                       :open-sink :list-dir :stat :exists? :mkdir
                       :delete :rename :touch :chmod :symlink :chown}

       {:type :denied :tool str :argv [str…] :reason str :rule-id any?}

   `snapshot` extracts the buffered events into a plain map suitable
   for the run() return value or JSON serialisation."
  (:require [clojure.string :as str]))

(defn make
  "Construct a fresh trace-state.

   Options:
     :cap        ring-buffer cap per channel (default 1000)
     :on-tool    (fn [event]) — fired on each tool event
     :on-fs      (fn [event]) — fired on each fs event
     :on-deny    (fn [event]) — fired on each denied event"
  ([] (make {}))
  ([{:keys [cap on-tool on-fs on-deny] :or {cap 1000}}]
   {:tools  (volatile! [])
    :fs     (volatile! [])
    :denied (volatile! [])
    :budget (atom {:steps 0 :wall-ms 0 :output-bytes 0})
    :cap    cap
    :on-tool on-tool
    :on-fs   on-fs
    :on-deny on-deny}))

(defn- push-ring!
  "Push `evt` into the volatile-vec held in trace-state under key `k`,
   trimming from the front when the vector exceeds `:cap`."
  [state k evt]
  (let [cap (:cap state)
        v   (k state)]
    (vswap! v
            (fn [xs]
              (let [xs' (conj xs evt)]
                (if (> (count xs') cap)
                  (clojure.core/vec (drop (- (count xs') cap) xs'))
                  xs'))))))

(defn record-tool!
  "Record a tool-call event. `event` is the map shape above. Fires
   `:on-tool` hook first, then appends to the ring-buffer."
  [state event]
  (when state
    (when-let [h (:on-tool state)] (try (h event) (catch #?(:clj Throwable :cljs :default) _ nil)))
    (push-ring! state :tools event)
    nil))

(defn record-fs!
  "Record a filesystem-op event."
  [state event]
  (when state
    (when-let [h (:on-fs state)] (try (h event) (catch #?(:clj Throwable :cljs :default) _ nil)))
    (push-ring! state :fs event)
    nil))

(defn record-denied!
  "Record a permit-denied event."
  [state event]
  (when state
    (when-let [h (:on-deny state)] (try (h event) (catch #?(:clj Throwable :cljs :default) _ nil)))
    (push-ring! state :denied event)
    nil))

(defn snapshot
  "Return a plain-map snapshot of the trace-state for inclusion in
   run() return values. Derives the FS reads/writes summary too."
  [state]
  (when state
    (let [tools  @(:tools state)
          fs     @(:fs state)
          denied @(:denied state)
          reads  (->> fs
                      (filter #(#{:read-file :read-bytes :open-source :stat :exists?
                                  :list-dir :resolve} (:op %)))
                      (map :path)
                      distinct
                      vec)
          writes (->> fs
                      (filter #(#{:open-sink :mkdir :delete :rename :touch :chmod
                                  :symlink :chown} (:op %)))
                      (map :path)
                      distinct
                      vec)]
      {:tools  tools
       :fs     fs
       :reads  reads
       :writes writes
       :denied denied
       :budget @(:budget state)})))

(defn coerce-options
  "Translate a user-friendly `:trace` value into a normalized state.
   Accepts:
     nil / false  → nil (no tracing)
     true         → default state (cap=1000, no hooks)
     a map        → passed straight to `make`
     an existing state map (has :tools volatile) → unchanged"
  [v]
  (cond
    (nil? v) nil
    (false? v) nil
    (true? v) (make)
    (and (map? v) (volatile? (:tools v))) v
    (map? v) (make v)
    :else
    (throw (ex-info "muschel.trace: invalid :trace option" {:value v}))))
