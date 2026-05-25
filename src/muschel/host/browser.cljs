(ns muschel.host.browser
  "Browser-side `muschel.host/Host` impl.

   Provides:
     - **String-buffer sinks/sources** — `:sink` and `:source` values
       are plain Clojure atoms holding string accumulators.
     - **Sequential pipes** — `host/async` is synchronous; pipelines
       collect each stage's stdout before running the next. Different
       semantics from JVM for infinite output, but matches what
       agents actually emit (finite, complete responses).
     - **Virtual fs** — files live in an atom `{path → content}`.
       Browser tabs don't have a real fs; we simulate one. Persist
       to localStorage / OPFS as a follow-up if desired.
     - **Virtual tool registry** — `:tools` is a map
       `{cmd-name → (fn [argv stdin env-map] => {:stdout :stderr :exit})}`.
       External commands look up here; unknown names exit 127.

   ## Construction

       (require '[muschel.host.browser :as h])
       (def host (h/make
                    {:tools {\"git\"  fake-git
                             \"curl\" fake-curl}
                     :files {\"/README.md\" \"# muschel\\n\"}}))

   Then thread `:host host` into `muschel.exec/run` / `run-and-capture`."
  (:require [clojure.string :as str]
            [muschel.host :as host]))

;; ============================================================================
;; Tagged buffer types
;; ============================================================================
;;
;; Sinks and sources are plain maps with a :type tag and an atom
;; holding their content. Keeping them simple maps (not deftypes) lets
;; cljs printing and devtools inspection stay friendly.

(defn- ->sink [] {::buf :sink :acc (atom "")})
(defn- ->source [s] {::buf :source :remaining (atom (str s))})

(defn- sink? [x] (and (map? x) (= :sink (::buf x))))
(defn- source? [x] (and (map? x) (= :source (::buf x))))

;; ============================================================================
;; Virtual filesystem
;; ============================================================================
;;
;; A flat map path → string content for files; directories are
;; marked by a trailing slash entry or inferred from prefixes.
;; Path semantics: pure-string, no resolution beyond what
;; `host/resolve-path` does.

(defn- normalize-path [^String p]
  (str/replace p #"/+" "/"))

(defn- vfs-get [vfs path]
  (get @vfs (normalize-path path)))

(defn- vfs-exists? [vfs path]
  (let [np (normalize-path path)]
    (or (contains? @vfs np)
        ;; A path is "a directory" if any entry starts with it + "/"
        (some #(str/starts-with? % (str np "/"))
              (keys @vfs)))))

(defn- vfs-dir? [vfs path]
  (let [np (normalize-path path)]
    (some #(str/starts-with? % (str np "/")) (keys @vfs))))

(defn- vfs-file? [vfs path]
  (string? (vfs-get vfs path)))

(defn- vfs-write! [vfs path content append?]
  (let [np (normalize-path path)
        prev (when append? (or (vfs-get vfs np) ""))]
    (swap! vfs assoc np (str prev content))))

;; ============================================================================
;; Host impl
;; ============================================================================

(deftype BrowserHost [tools vfs]
  host/Host
  (-write-string! [_ sink s]
    (when (and sink s)
      (cond
        (sink? sink) (swap! (:acc sink) str s)
        ;; Allow callers to pass a JS console as a sink for the
        ;; playground UI — write via .log.
        (and (some? sink) (some? (.-log sink)))
        (.log sink s)
        :else nil)))

  (-read-all-string [_ source]
    (cond
      (source? source) (let [s @(:remaining source)]
                         (reset! (:remaining source) "")
                         s)
      (string? source) source
      :else ""))

  (-close! [_ _] nil)

  (-string-sink [_] (->sink))
  (-sink->string [_ sink]
    (cond
      (sink? sink) @(:acc sink)
      :else ""))
  (-string-source [_ s] (->source s))

  (-open-file-sink [_ path append?]
    ;; Return a sink that writes to vfs on close. Simpler: return a
    ;; sink wrapping the vfs atom directly so writes show up
    ;; immediately.
    (let [acc (atom (if append? (or (vfs-get vfs path) "") ""))
          sink {::buf :sink :acc acc ::path path ::vfs vfs ::append? append?}]
      ;; Eagerly flush on every write — we'd add explicit close
      ;; semantics if we wanted batched writes.
      (add-watch acc ::vfs-flush
                 (fn [_ _ _ new] (vfs-write! vfs path new false)))
      sink))

  (-open-file-source [_ path]
    (->source (or (vfs-get vfs path) "")))

  (-file-info [_ path]
    (let [np (normalize-path path)
          exists? (vfs-exists? vfs np)
          file?   (vfs-file? vfs np)
          dir?    (vfs-dir? vfs np)
          content (when file? (vfs-get vfs np))]
      {:exists?     exists?
       :file?       file?
       :dir?        dir?
       :readable?   exists?
       :writable?   true                 ; vfs always writable
       :executable? false                ; no real exec bit
       :symlink?    false
       :size        (when content (count content))
       :mtime-ms    nil}))

  (-read-file [_ path]
    (or (vfs-get vfs path)
        (throw (ex-info (str "file not found: " path)
                        {:type ::no-file :path path}))))

  (-make-pipe [this]
    ;; A pipe = a sink that's also readable. We return [source sink]
    ;; pointing at shared state: writes accumulate, the source reads
    ;; whatever's accumulated.
    (let [shared (atom "")
          sink   {::buf :sink :acc shared}
          source {::buf :source :remaining shared}]
      [source sink]))

  (-spawn [_ {:keys [cmd args extra-env in out err]}]
    (let [tool-fn (get tools cmd)]
      (cond
        (nil? tool-fn)
        (do (host/-write-string! _ err (str cmd ": command not found\n"))
            {:handle nil :wait (fn [] 127)})

        :else
        (let [stdin-str (cond
                          (source? in) @(:remaining in)
                          (string? in) in
                          :else "")
              result (try (tool-fn (vec args) stdin-str (or extra-env {}))
                          (catch :default e
                            {:stderr (str cmd ": " (.-message e) "\n")
                             :exit 1}))]
          (host/-write-string! _ out (or (:stdout result) ""))
          (host/-write-string! _ err (or (:stderr result) ""))
          {:handle nil :wait (fn [] (or (:exit result) 0))}))))

  (-async [_ thunk]
    ;; Browser is single-threaded — run inline, capture result.
    {::handle :sync :value (try (thunk) (catch :default e
                                          (.warn js/console
                                                 (str "async thunk threw: " e))
                                          nil))})

  (-await [_ h] (:value h)))

(defn make
  "Build a BrowserHost.
   `tools` is a map of `name → (fn [argv stdin env-map] result)` where
   result is `{:stdout :stderr :exit}`.
   `files` is the initial vfs `{path → content}`."
  [& {:keys [tools files]
      :or   {tools {}
             files {}}}]
  (->BrowserHost tools (atom files)))

;; ============================================================================
;; Stock tools — minimal set so the playground demo works without
;; users having to register everything.
;; ============================================================================

(defn stock-tools
  "Returns a basic tool map covering: cat, ls, true, false, env-style
   helpers. Most actual shell builtins (cd, echo, test, etc.) are
   already covered by muschel's own builtin dispatch — these are the
   externally-spawned commands the playground demo might need."
  []
  {"cat"  (fn [args stdin _]
            (if (empty? args)
              {:stdout stdin :exit 0}
              ;; Can't read files here without host access — leave
              ;; that to specialized impls. For now: error.
              {:stderr "cat: filenames not supported in stock-tools\n"
               :exit 1}))

   "wc"   (fn [_args stdin _]
            (let [lines (count (str/split-lines (str stdin)))
                  words (count (str/split (str stdin) #"\s+"))
                  bytes (count stdin)]
              {:stdout (str " " lines " " words " " bytes "\n")
               :exit 0}))

   "grep" (fn [args stdin _]
            (let [pat (first args)
                  rx (when pat (re-pattern pat))
                  matched (when rx
                            (->> (str/split-lines (str stdin))
                                 (filter #(re-find rx %))))]
              {:stdout (str/join "\n" matched)
               :exit (if (seq matched) 0 1)}))

   "head" (fn [args stdin _]
            (let [n (or (some-> (re-find #"^-(\d+)$" (str (first args)))
                                second
                                js/parseInt)
                        10)]
              {:stdout (str/join "\n" (take n (str/split-lines (str stdin))))
               :exit 0}))})
