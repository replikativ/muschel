(ns muschel.playground
  "Browser playground entry point.

   Renders a tiny shell UI: input box, history pane, $? + cwd in
   prompt. Talks to `muschel.exec` via `muschel.host.browser`.

   Pre-seeds the virtual fs with a small example tree and registers
   a few stock tools so common demos (`ls`, `cat`, `grep`, `wc`,
   `head`) work without further setup.

   Production playground would polish with CodeMirror for input,
   syntax highlighting, AST/permit panes alongside output. This
   file is the minimal viable demo."
  (:require [clojure.string :as str]
            [muschel.env :as env]
            [muschel.exec :as exec]
            [muschel.host :as host]
            [muschel.host.browser :as bh]
            [muschel.permit :as permit]))

(defonce host-atom (atom nil))
(defonce env-atom  (atom nil))

(defn- abs-path
  "Resolve `p` against the env's PWD if it's relative. PWD is in the
   process-env map muschel hands to tools."
  [env p]
  (cond
    (str/starts-with? p "/") p
    :else (let [pwd (or (get env "PWD") "/")
                pwd (if (str/ends-with? pwd "/") pwd (str pwd "/"))]
            (str pwd p))))

(defn- demo-tools [host*]
  (merge (bh/stock-tools)
         {;; A `cat` that reads from the vfs (resolves relative paths
          ;; against PWD).
          "cat" (fn [args stdin env]
                  (cond
                    (empty? args) {:stdout stdin :exit 0}
                    :else
                    (reduce (fn [acc raw]
                              (let [p (abs-path env raw)]
                                (if (host/file-exists? @host* p)
                                  (update acc :stdout str
                                          (host/read-file @host* p))
                                  (-> acc
                                      (assoc :exit 1)
                                      (update :stderr str
                                              (str "cat: " raw ": No such file\n"))))))
                            {:stdout "" :stderr "" :exit 0}
                            args)))

          ;; `ls` lists vfs entries under the (absolute or relative) dir.
          "ls"  (fn [args _stdin env]
                  (let [raw    (or (first args) ".")
                        target (abs-path env raw)
                        prefix (str (str/replace target #"/$" "") "/")
                        vfs    @(.-vfs ^js @host*)
                        matches (->> (keys vfs)
                                     (filter #(str/starts-with? % prefix))
                                     (map #(subs % (count prefix)))
                                     (map #(first (str/split % #"/")))
                                     distinct
                                     sort)]
                    {:stdout (str (str/join "\n" matches)
                                  (when (seq matches) "\n"))
                     :exit (if (seq matches) 0 1)}))}))

(defn- init! []
  (let [host-box (atom nil)
        h (bh/make
           :tools (demo-tools host-box)
           :files {"/README.md"  "# muschel playground\n\nTry: cat /README.md | wc -l\n"
                   "/etc/issue"  "muschel browser demo\n"
                   "/tmp/.keep"  ""})]
    (reset! host-box h)
    (reset! host-atom h)
    (reset! env-atom  (env/new-env :cwd "/"))))

(defn run-line!
  "Public API: run one bash line in the playground state. Returns
   {:stdout :stderr :exit :env}."
  [src]
  (when-not @host-atom (init!))
  (let [{:keys [env exit stdout stderr]}
        (exec/run-and-capture
         @env-atom
         src
         {:host  @host-atom
          :permit {:rulesets [permit/default-rules]
                   :prompter permit/allow-all-prompter}})]
    (reset! env-atom env)
    {:stdout stdout :stderr stderr :exit exit
     :cwd   (:cwd env)
     :last-exit (:last-exit env)}))

(defn- $ [sel] (.querySelector js/document sel))

(defn- append-output! [{:keys [stdout stderr exit cwd]}]
  (let [log ($ "#log")]
    (set! (.-textContent log)
          (str (.-textContent log)
               (when (seq stdout) stdout)
               (when (seq stderr) (str "[stderr] " stderr))))
    (set! (.-scrollTop log) (.-scrollHeight log))
    (set! (.-textContent ($ "#prompt"))
          (str cwd " $? " exit " > "))))

(defn ^:export start
  "Wires up the DOM event handlers. Call this from the browser
   entry script after the page loads."
  []
  (init!)
  (let [input ($ "#input")]
    (set! (.-textContent ($ "#prompt"))
          (str (:cwd @env-atom) " > "))
    (.addEventListener
     input "keydown"
     (fn [e]
       (when (= "Enter" (.-key e))
         (.preventDefault e)
         (let [src (.-value input)]
           (set! (.-value input) "")
           (let [echoed (str (.-textContent ($ "#prompt")) src "\n")]
             (set! (.-textContent ($ "#log"))
                   (str (.-textContent ($ "#log")) echoed)))
           (try
             (append-output! (run-line! src))
             (catch :default e
               (append-output!
                {:stderr (str (.-message e) "\n")
                 :exit 1
                 :cwd (:cwd @env-atom)})))))))))
