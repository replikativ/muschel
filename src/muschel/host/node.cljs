(ns muschel.host.node
  "Node.js impl of `muschel.host/Host`.

   Uses Node's `child_process.spawnSync` for external commands and
   `fs`/`fs/promises` for file I/O. Pipelines are sequential
   (each stage runs to completion before the next sees its stdin)
   — node is single-threaded JS, so true concurrency would require
   `child_process.spawn` + streams + Promises, which is more
   work. Sequential is fine for shell-as-agent usage where output
   is finite.

   ## Usage

       (require '[muschel.host.node :as h]
                '[muschel.env :as env]
                '[muschel.exec :as exec])
       (def host (h/make))
       (exec/run (env/new-env) \"echo hello | tr a-z A-Z\" {:host host})"
  (:require [clojure.string :as str]
            [muschel.host :as host]))

(def ^:private cp (js/require "child_process"))
(def ^:private fs (js/require "fs"))
(def ^:private path-mod (js/require "path"))

;; Reuse the buffer model from the browser host — strings + atoms.
(defn- ->sink [] {::buf :sink :acc (atom "")})
(defn- ->source [s] {::buf :source :remaining (atom (str s))})
(defn- sink? [x] (and (map? x) (= :sink (::buf x))))
(defn- source? [x] (and (map? x) (= :source (::buf x))))

(deftype NodeHost []
  host/Host
  (-write-string! [_ sink s]
    (when (and sink s)
      (cond
        (sink? sink) (swap! (:acc sink) str s)
        ;; Allow callers to pass a writable Node stream
        (and sink (.-write sink)) (.write sink s))))

  (-read-all-string [_ source]
    (cond
      (source? source) (let [s @(:remaining source)]
                         (reset! (:remaining source) "")
                         s)
      (string? source) source
      :else ""))

  (-close! [_ _] nil)
  (-string-sink [_] (->sink))
  (-sink->string [_ sink] (cond (sink? sink) @(:acc sink) :else ""))
  (-string-source [_ s] (->source s))

  (-open-file-sink [_ path append?]
    (let [acc (atom (if append?
                      (try (.readFileSync fs path "utf8")
                           (catch :default _ ""))
                      ""))
          sink {::buf :sink :acc acc ::path path}]
      (add-watch acc ::node-flush
                 (fn [_ _ _ new]
                   (.writeFileSync fs path new "utf8")))
      sink))

  (-open-file-source [_ path]
    (->source (.readFileSync fs path "utf8")))

  (-file-info [_ p]
    (try
      (let [st (.statSync fs p)
            consts (aget fs "constants")
            x-ok (aget consts "X_OK")]
        {:exists?     true
         :file?       (.call (aget st "isFile") st)
         :dir?        (.call (aget st "isDirectory") st)
         :readable?   true
         :writable?   true
         :executable? (try (.accessSync fs p x-ok)
                           true
                           (catch :default _ false))
         :symlink?    (try (let [lst (.lstatSync fs p)]
                             (.call (aget lst "isSymbolicLink") lst))
                           (catch :default _ false))
         :size        (aget st "size")
         :mtime-ms    (.getTime (aget st "mtime"))})
      (catch :default _
        {:exists? false})))

  (-read-file [_ path] (.readFileSync fs path "utf8"))

  (-make-pipe [_]
    ;; Sequential pipes: shared atom buffer between stages.
    (let [shared (atom "")
          source {::buf :source :remaining shared}
          sink   {::buf :sink :acc shared}]
      [source sink]))

  (-spawn [this {:keys [cmd args dir extra-env in out err]}]
    (let [stdin (cond
                  (source? in) @(:remaining in)
                  (string? in) in
                  :else "")
          env-obj (js/Object.assign #js {} (.-env js/process))
          _ (doseq [[k v] extra-env]
              (aset env-obj k v))
          spawn-opts (doto #js {}
                       (aset "cwd" dir)
                       (aset "input" stdin)
                       (aset "env" env-obj)
                       (aset "encoding" "utf8"))
          args-arr (to-array (vec args))
          r (try (.spawnSync cp cmd args-arr spawn-opts)
                 (catch :default e
                   (doto #js {}
                     (aset "status" 127)
                     (aset "stdout" "")
                     (aset "stderr" (.-message e)))))
          r-stdout (aget r "stdout")
          r-stderr (aget r "stderr")
          r-status (aget r "status")
          r-err    (aget r "error")]
      (when out (host/-write-string! this out (or r-stdout "")))
      (when err (host/-write-string! this err (or r-stderr "")))
      {:handle r
       :wait (fn [] (or r-status (if r-err 127 0)))}))

  (-async [_ thunk]
    ;; Node JS is single-threaded — run inline.
    {::handle :sync :value (try (thunk) (catch :default _ nil))})

  (-await [_ h] (:value h)))

(defn make [] (->NodeHost))
