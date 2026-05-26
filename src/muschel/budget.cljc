(ns muschel.budget
  "Resource budgets for muschel runs. Two orthogonal mechanisms:

   1. **Cooperative interrupt-fn.** A 0-arg function read from `(:interrupt-fn env)`,
      invoked at every loop boundary (between statements, between
      pipeline stages, inside awk's record loop, inside find/grep/
      xargs iteration, inside the awk for/while/do-while bodies, etc.).
      If the function throws, execution aborts and the throw propagates
      out of `exec/run-and-capture`. If it returns truthy/false the
      caller can also use the return value as a hard abort signal —
      `check-interrupt!` honours both.

      Patterned on sci's interrupt-fn (see `../sci/src/sci/impl/interruptible.cljc`).

   2. **Wall-clock timeout / step counters.** Convenience wrappers that
      synthesise an interrupt-fn from a deadline or a max-step count.

   Output caps live in `muschel.exec` directly (the sink layer), since
   they intercept bytes before they touch a buffer."
  (:require #?(:clj [clojure.core])))

;; ============================================================================
;; The interrupt protocol — one fn, called everywhere
;; ============================================================================

(defn check-interrupt!
  "Call the env's interrupt-fn if one is installed. The fn may:
     - return truthy / nil → keep running
     - throw                → abort the run (caller catches at the
                               run-and-capture boundary)
   This is a hot-path call — keep it tiny."
  [env]
  (when-let [ifn (:interrupt-fn env)]
    (ifn))
  nil)

;; ============================================================================
;; Timeout helper
;; ============================================================================

#?(:clj
   (defn deadline-interrupt
     "Make an interrupt-fn that throws once `wall-clock-ms` have
      elapsed since this call. Cheap: each invocation is a long compare."
     [wall-clock-ms]
     (let [deadline (+ (System/currentTimeMillis) (long wall-clock-ms))]
       (fn []
         (when (> (System/currentTimeMillis) deadline)
           (throw (ex-info "muschel: wall-clock budget exceeded"
                           {:muschel/budget :timeout
                            :limit-ms wall-clock-ms})))))))

#?(:cljs
   (defn deadline-interrupt
     [wall-clock-ms]
     (let [deadline (+ (.now js/Date) wall-clock-ms)]
       (fn []
         (when (> (.now js/Date) deadline)
           (throw (ex-info "muschel: wall-clock budget exceeded"
                           {:muschel/budget :timeout
                            :limit-ms wall-clock-ms})))))))

;; ============================================================================
;; Step-counter helper
;; ============================================================================

(defn step-interrupt
  "Make an interrupt-fn that throws after `max-steps` invocations. Use
   when you want a coarse 'don't loop too many times' bound that
   doesn't depend on wall-clock (useful for CI determinism)."
  [max-steps]
  (let [counter (atom 0)]
    (fn []
      (when (> (swap! counter inc) (long max-steps))
        (throw (ex-info "muschel: step budget exceeded"
                        {:muschel/budget :steps
                         :limit-steps max-steps}))))))

;; ============================================================================
;; Combinator — both at once
;; ============================================================================

(defn combine
  "Combine several interrupt-fns into one that runs all of them in
   order. Any throw propagates out."
  [& fns]
  (let [fns (vec (remove nil? fns))]
    (cond
      (empty? fns) nil
      (= 1 (count fns)) (first fns)
      :else (fn []
              (doseq [f fns] (f))
              nil))))

;; ============================================================================
;; Budget-exceeded predicate
;; ============================================================================

(defn budget-exceeded?
  "True if `ex` is a budget-exceeded throw raised by an interrupt-fn
   from this namespace."
  [ex]
  (when (and ex
             #?(:clj (instance? clojure.lang.ExceptionInfo ex)
                :cljs (some? (ex-data ex))))
    (boolean (:muschel/budget (ex-data ex)))))
