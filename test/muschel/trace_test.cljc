(ns muschel.trace-test
  "Tests for the trace introspection layer. Cross-platform: runs on
   JVM (`host.jvm`) and Node / ClojureScript (`host.browser`)."
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.core :as m]
            [muschel.test-helpers :as th]))

(defn- mk-host
  ([] (th/mk-host))
  ([files] (th/mk-host {:files (or files {})})))

;; ============================================================================
;; Bounded accumulator
;; ============================================================================

(deftest trace-off-by-default
  (testing "without :trace opt, no trace map is on the result"
    (let [r (m/run-and-capture (m/new-env)
                               "echo hello"
                               {:host (mk-host)})]
      (is (zero? (:exit r)))
      (is (= "hello\n" (:stdout r)))
      (is (nil? (:trace r))))))

(deftest trace-records-tools
  (testing ":trace true captures every builtin invocation"
    (let [r (m/run-and-capture (m/new-env)
                               "echo a; echo b; echo c"
                               {:host (mk-host) :trace true})
          tools (get-in r [:trace :tools])
          names (mapv :name tools)]
      (is (= ["echo" "echo" "echo"] names))
      (is (every? #(= 0 (:exit %)) tools))
      (is (every? :duration-ms tools))
      (is (every? :argv tools)))))

(deftest trace-records-fs-reads-and-writes
  (testing "FS events split into :reads and :writes via path categorisation"
    (let [host (mk-host {"/seed.txt" "hello\n"})
          r (m/run-and-capture (m/new-env)
                               "cat /seed.txt > /out.txt"
                               {:host host :trace true})
          {:keys [reads writes fs]} (:trace r)]
      (is (some #(re-find #"seed" %) reads) "seed.txt should appear in reads")
      (is (some #(re-find #"out" %) writes) "out.txt should appear in writes")
      (is (some #(#{:open-source :read-bytes :read-file} (:op %)) fs)
          "expected a read-side event")
      (is (some #(= :open-sink (:op %)) fs)
          "expected an open-sink event for the redirect"))))

(deftest trace-records-permit-denials
  (testing "runtime permit denials show up in :denied"
    (let [rules [{:tool :bash
                  :pattern {:kind :cmd-name :name "rm"}
                  :action :deny
                  :reason "rm denied for test"}]
          r (m/run-and-capture (m/new-env)
                               "rm /tmp/x"
                               {:host (mk-host) :trace true
                                :permit {:rulesets [rules]
                                         :prompter (constantly :deny)}})
          {:keys [denied]} (:trace r)]
      ;; The static permit gate denies, returning exit 126 — the
      ;; per-call event has the call shape.
      (is (= 126 (:exit r)))
      (is (some #(= "rm" (:tool %)) denied)))))

;; ============================================================================
;; Streaming hooks (unbounded by muschel)
;; ============================================================================

(deftest trace-on-tool-hook-fires
  (testing ":on-tool callback fires per builtin invocation"
    (let [events (atom [])
          r (m/run-and-capture (m/new-env)
                               "echo x; echo y"
                               {:host (mk-host)
                                :trace {:on-tool (fn [e] (swap! events conj e))}})]
      (is (zero? (:exit r)))
      (is (= 2 (count @events)))
      (is (= ["echo" "echo"] (mapv :name @events))))))

;; ============================================================================
;; Ring-buffer cap
;; ============================================================================

(deftest trace-ring-buffer-cap
  (testing "cap=5 means at most 5 tool events stored even after 20 invocations"
    (let [r (m/run-and-capture
             (m/new-env)
             ;; 20 echos in a loop
             "i=0; while [ \"$i\" -lt 20 ]; do echo $i; i=$((i+1)); done"
             {:host (mk-host) :trace {:cap 5}})
          tools (get-in r [:trace :tools])]
      (is (<= (count tools) 5)
          (str "ring-buffer should have capped at 5, has " (count tools))))))
