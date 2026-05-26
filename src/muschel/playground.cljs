(ns muschel.playground
  "Browser playground entry point.

   Wires a tiny shell UI to a full muschel sandbox:

     - `BuiltinHost` (cross-platform builtin dispatch + fallback gate)
       over
     - `BrowserHost` (in-memory buffers, virtual tool registry, no spawn),
       backed by
     - `VirtualFS` (containment-aware, no real-disk access).

   The agent (well, the human typing at the prompt) sees the full
   ~50-builtin POSIX surface — `cat`, `ls`, `grep`, `find`, `sed`,
   `awk`, `cp`, `mv`, `tee`, redirects, pipes, nested `sh -c`, … —
   running against a virtual filesystem that persists in
   `localStorage` so reloads keep state.

   UX:
     - Up/Down arrow scroll back through command history.
     - Tab completes builtin names (when the word is the cmd) or
       VFS paths (otherwise).
     - Ctrl-L clears the log.
     - A `Files` pane on the right shows the current VFS tree, refreshed
       after every command.
     - `Reset` button wipes localStorage and rebuilds the default tree."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [muschel.builtins.posix :as posix]
            [muschel.core :as m]
            [muschel.env :as env]
            [muschel.fs :as fs]
            [muschel.fs.virtual :as vfs]
            [muschel.host.browser :as bh]
            [muschel.host.builtin :as hb]
            [muschel.permit :as permit]
            [muschel.session :as session]))

;; ============================================================================
;; Defaults — what the agent sees on first load / after Reset
;; ============================================================================

(def default-tree
  "Initial VFS contents. Designed to give the user enough to exercise
   the full builtin set immediately (grep, sed, awk, find, redirects,
   pipes) without typing a tutorial first."
  {"/etc/passwd"        "root:x:0:0:root:/root:/bin/sh\nuser:x:1000:1000:user:/home/user:/bin/sh\n"
   "/etc/issue"         "muschel browser demo — try `cat /etc/issue`\n"
   "/home/user"         :dir
   "/home/user/README.md"
   (str "# Welcome to muschel\n\n"
        "This is a real bash shell running in your browser. Every "
        "command goes through muschel's `BuiltinHost`, which routes "
        "through ~50 POSIX builtins implemented in Clojure.\n\n"
        "Try:\n"
        "  cat README.md\n"
        "  grep bash README.md\n"
        "  find . -type f\n"
        "  echo greetings > /tmp/note.txt && cat /tmp/note.txt\n"
        "  awk -F : '{print $1}' /etc/passwd\n"
        "  for f in *.md; do echo \"-- $f --\"; head -3 $f; done\n"
        "  sh -c 'echo nested && pwd'\n")
   "/home/user/notes.md"
   "# Notes\n\nMuschel parses bash, gates effects, and runs builtins.\n"
   "/home/user/data.csv"
   "name,age,role\nalice,30,engineer\nbob,25,designer\ncarol,42,manager\n"
   "/home/user/script.sh"
   "#!/bin/sh\nfor i in 1 2 3; do echo step $i; done\n"
   "/tmp"                :dir
   "/var/log/app.log"    "[2026-05-26 09:00] INFO  starting\n[2026-05-26 09:01] WARN  retry\n[2026-05-26 09:02] ERROR boom\n"})

(def default-cwd "/home/user")

(def ^:private storage-key "muschel/playground/v1")

;; ============================================================================
;; State — single host atom; the underlying FS is mutable, env / session
;; are reset-able values.
;; ============================================================================

(defonce !host    (atom nil))
(defonce !fs      (atom nil))   ; ref into the VirtualFS for snapshot/list
(defonce !session (atom nil))
(defonce !history (atom {:items [] :idx nil}))  ; idx=nil means "not in history"

;; ============================================================================
;; localStorage — survive page reloads
;; ============================================================================

(defn- ls-get []
  (try
    (when-let [s (.getItem js/localStorage storage-key)]
      (edn/read-string s))
    (catch :default _ nil)))

(defn- ls-put! [snapshot history-items]
  (try
    (.setItem js/localStorage storage-key
              (pr-str {:vfs snapshot
                       :history (vec (take-last 100 history-items))}))
    (catch :default _ nil)))

(defn- ls-clear! []
  (try (.removeItem js/localStorage storage-key)
       (catch :default _ nil)))

;; ============================================================================
;; Sandbox construction
;; ============================================================================

(defn- build-host
  "Construct a fresh sandboxed host from a VFS snapshot (or the
   defaults). Returns [fs host]; both should be stashed in the atoms."
  [snapshot]
  (let [fs   (if snapshot
               (vfs/restore snapshot)
               (vfs/make default-tree {:cwd default-cwd}))
        host (hb/make {:fs fs
                       :fallback-host (bh/make)
                       :builtins posix/standard})]
    [fs host]))

(defn- reset-state!
  "Rebuild the sandbox from defaults (or, with `:keep-vfs? true`,
   from the current localStorage snapshot). Resets the session so
   `cd`, `export`, `set -o` start clean."
  [{:keys [keep-vfs?] :or {keep-vfs? false}}]
  (let [snapshot (when keep-vfs? (:vfs (ls-get)))
        [fs host] (build-host snapshot)]
    (reset! !fs fs)
    (reset! !host host)
    (reset! !session (session/atom-session
                      (env/new-env :cwd (or (and snapshot (:cwd snapshot))
                                            default-cwd))))))

;; ============================================================================
;; One-line execute. Run, persist, sync cwd.
;; ============================================================================

(defn run-line!
  "Run one bash line against the playground state. Returns
   `{:stdout :stderr :exit :cwd :last-exit}`. Persists the VFS after
   the run so the next reload sees it."
  [src]
  (when (nil? @!host) (reset-state! {:keep-vfs? true}))
  (let [sess @!session
        host @!host
        result
        (try
          (m/run-and-capture
           (session/-env sess) src
           {:host host
            :session sess
            :permit {:rulesets [permit/default-rules]
                     :prompter permit/allow-all-prompter}})
          (catch :default e
            {:stdout "" :stderr (str (.-message e) "\n")
             :exit 1 :env (session/-env sess)}))
        env' (:env result)]
    ;; Keep the FS's own cwd in sync with the env's cwd so the next
    ;; command's relative-path resolution sees the latest `cd`.
    (when-let [cwd (:cwd env')]
      (try (fs/cd! @!fs cwd) (catch :default _ nil)))
    ;; Persist to localStorage.
    (ls-put! (vfs/snapshot @!fs) (:items @!history))
    {:stdout (:stdout result)
     :stderr (:stderr result)
     :exit (:exit result)
     :cwd (:cwd env')
     :last-exit (:last-exit env')}))

;; ============================================================================
;; History
;; ============================================================================

(defn- push-history! [src]
  (let [src (str/trim src)]
    (when (seq src)
      (swap! !history
             (fn [{:keys [items]}]
               {:items (if (= src (last items))
                         items                       ; dedup consecutive
                         (vec (take-last 200 (conj items src))))
                :idx nil})))))

(defn- history-up
  "Replace `current` with the previous history entry (toward older).
   Returns [new-text new-state] suitable for input-field replacement."
  [current]
  (let [{:keys [items idx]} @!history
        n (count items)]
    (when (pos? n)
      (let [new-idx (cond
                      (nil? idx) (dec n)
                      (zero? idx) 0
                      :else (dec idx))]
        (swap! !history assoc :idx new-idx
               :pinned (or (:pinned @!history) current))
        (nth items new-idx)))))

(defn- history-down
  "Move toward newer history. Returns the next stored line, or the
   pinned 'live' text if we walk past the end."
  [_current]
  (let [{:keys [items idx pinned]} @!history
        n (count items)]
    (cond
      (nil? idx) nil
      (>= (inc idx) n) (do (swap! !history assoc :idx nil :pinned nil)
                           (or pinned ""))
      :else (let [new-idx (inc idx)]
              (swap! !history assoc :idx new-idx)
              (nth items new-idx)))))

;; ============================================================================
;; Tab completion
;; ============================================================================

(def ^:private builtin-names
  (delay (vec (sort (keys posix/standard)))))

(defn- abs-path [cwd p]
  (cond
    (str/starts-with? p "/") p
    :else (let [cwd (if (str/ends-with? cwd "/") cwd (str cwd "/"))]
            (str cwd p))))

(defn- list-dir-safe
  "List the entries of `dir` in the VFS, sorted, with trailing slashes
   on directories. Returns [] on failure / outside-root."
  [fs dir]
  (try
    (->> (or (fs/list-dir fs dir) [])
         (map (fn [e]
                (str (:name e) (when (= :dir (:type e)) "/"))))
         sort
         vec)
    (catch :default _ [])))

(defn- complete-current-word
  "Given the current input text + cursor, return a vector of
   completions for the word under the cursor. The first call after a
   string change returns up to 8 candidates so the UI can render them;
   subsequent identical-prefix calls cycle. Pure — no UI work here."
  [text caret]
  (let [pre  (subs text 0 caret)
        post (subs text caret)
        ;; Split on whitespace to find the current word.
        last-ws (or (some (fn [i]
                            (when (or (= " " (subs pre i (inc i)))
                                      (= "\t" (subs pre i (inc i))))
                              (inc i)))
                          (range (dec (count pre)) -1 -1))
                    0)
        word (subs pre last-ws)
        first? (zero? last-ws)
        fs @!fs
        cwd (:cwd (session/-env @!session))]
    (cond
      ;; First word: complete builtin names.
      first?
      (filterv #(str/starts-with? % word) @builtin-names)

      ;; Path-like: split into dir + leaf, list dir, filter by leaf prefix.
      :else
      (let [slash (str/last-index-of word "/")
            [dir-part leaf]
            (cond
              (nil? slash) ["" word]
              (zero? slash) ["/" (subs word 1)]
              :else [(subs word 0 (inc slash)) (subs word (inc slash))])
            base (cond
                   (str/starts-with? dir-part "/") dir-part
                   (= "" dir-part) cwd
                   :else (abs-path cwd dir-part))
            entries (list-dir-safe fs base)
            matches (filterv #(str/starts-with? % leaf) entries)]
        (mapv (fn [m] (str dir-part m)) matches)))))

(defn- apply-completion
  "Apply `candidate` over the current word in `text` at `caret`. Returns
   [new-text new-caret]."
  [text caret candidate]
  (let [pre  (subs text 0 caret)
        post (subs text caret)
        last-ws (or (some (fn [i]
                            (when (or (= " " (subs pre i (inc i)))
                                      (= "\t" (subs pre i (inc i))))
                              (inc i)))
                          (range (dec (count pre)) -1 -1))
                    0)
        before-word (subs pre 0 last-ws)
        new-text (str before-word candidate post)]
    [new-text (+ (count before-word) (count candidate))]))

;; ============================================================================
;; DOM helpers
;; ============================================================================

(defn- $ [sel] (.querySelector js/document sel))

(defn- update-prompt! []
  (let [env (session/-env @!session)]
    (set! (.-textContent ($ "#prompt"))
          (str (or (:cwd env) "/") " $? " (or (:last-exit env) 0) " > "))))

(defn- append-output!
  "Append a result block to the log, with stderr in a dimmer / accent
   color via inline spans."
  [{:keys [stdout stderr]}]
  (let [log ($ "#log")]
    (when (seq stdout)
      (let [span (.createElement js/document "span")]
        (set! (.-textContent span) stdout)
        (.appendChild log span)))
    (when (seq stderr)
      (let [span (.createElement js/document "span")]
        (set! (.-className span) "stderr")
        (set! (.-textContent span) stderr)
        (.appendChild log span)))
    (set! (.-scrollTop log) (.-scrollHeight log))))

(defn- echo-cmd! [src]
  (let [log ($ "#log")
        line (.createElement js/document "span")]
    (set! (.-className line) "echo")
    (set! (.-textContent line)
          (str (.-textContent ($ "#prompt")) src "\n"))
    (.appendChild log line)))

;; --- Files pane -----------------------------------------------------------

(defn- entries-as-tree
  "Group all VFS entries into a simple flat list, sorted, with the
   directory leading slash trimmed and types tagged. Suitable for the
   side pane."
  [fs]
  (let [entries @(:entries-atom fs)]
    (->> entries
         (sort-by key)
         (mapv (fn [[path entry]]
                 {:path path
                  :type (:type entry)
                  :size (count (or (:content entry) ""))})))))

(defn- refresh-files-pane! []
  (when-let [pane ($ "#files-list")]
    (set! (.-innerHTML pane) "")
    (doseq [{:keys [path type size]} (entries-as-tree @!fs)]
      (let [row (.createElement js/document "div")]
        (set! (.-className row) (str "file-row " (name type)))
        (set! (.-textContent row)
              (str (if (= :dir type) "📁 " "📄 ")
                   path
                   (when (= :file type) (str "  (" size " B)"))))
        (.appendChild pane row)))))

;; ============================================================================
;; Keyboard handling
;; ============================================================================

(defn- clear-log! []
  (set! (.-textContent ($ "#log")) ""))

(defonce ^:private !completion-cycle
  ;; {:text "input as of last tab" :candidates [...] :idx N}
  (atom nil))

(defn- on-tab! [input e]
  (.preventDefault e)
  (let [text (.-value input)
        caret (.-selectionStart input)
        st @!completion-cycle]
    (cond
      ;; Re-tab through prior candidates.
      (and st (= (:text st) text) (seq (:candidates st)))
      (let [next-idx (mod (inc (:idx st)) (count (:candidates st)))
            cand (nth (:candidates st) next-idx)
            [new-text new-caret] (apply-completion (:base-text st)
                                                   (:base-caret st)
                                                   cand)]
        (swap! !completion-cycle assoc :idx next-idx :text new-text)
        (set! (.-value input) new-text)
        (.setSelectionRange input new-caret new-caret))

      :else
      (let [candidates (complete-current-word text caret)]
        (cond
          (empty? candidates) (reset! !completion-cycle nil)

          (= 1 (count candidates))
          (let [[new-text new-caret] (apply-completion text caret (first candidates))]
            (set! (.-value input) new-text)
            (.setSelectionRange input new-caret new-caret)
            (reset! !completion-cycle nil))

          :else
          (let [first-cand (first candidates)
                [new-text new-caret] (apply-completion text caret first-cand)]
            (reset! !completion-cycle
                    {:base-text text :base-caret caret
                     :candidates candidates :idx 0 :text new-text})
            (set! (.-value input) new-text)
            (.setSelectionRange input new-caret new-caret)
            ;; Show all candidates in the log so the user knows the list.
            (let [log ($ "#log")
                  span (.createElement js/document "span")]
              (set! (.-className span) "dim")
              (set! (.-textContent span)
                    (str (str/join "  " candidates) "\n"))
              (.appendChild log span)
              (set! (.-scrollTop log) (.-scrollHeight log)))))))))

(defn- on-enter! [input]
  (let [src (.-value input)]
    (set! (.-value input) "")
    (reset! !completion-cycle nil)
    (when (seq (str/trim src))
      (echo-cmd! src)
      (push-history! src)
      (try
        (append-output! (run-line! src))
        (catch :default e
          (append-output! {:stderr (str (.-message e) "\n")})))
      (update-prompt!)
      (refresh-files-pane!))))

(defn- on-keydown [input e]
  (case (.-key e)
    "Enter"
    (do (.preventDefault e) (on-enter! input))

    "Tab"
    (on-tab! input e)

    "ArrowUp"
    (when-let [line (history-up (.-value input))]
      (.preventDefault e)
      (set! (.-value input) line)
      (let [n (count line)]
        (.setSelectionRange input n n)))

    "ArrowDown"
    (when-let [line (history-down (.-value input))]
      (.preventDefault e)
      (set! (.-value input) line)
      (let [n (count line)]
        (.setSelectionRange input n n)))

    ;; Ctrl-L = clear screen, Ctrl-C = abort current input.
    "l" (when (.-ctrlKey e) (.preventDefault e) (clear-log!))
    "c" (when (.-ctrlKey e)
          (.preventDefault e)
          (set! (.-value input) "")
          (reset! !completion-cycle nil)
          (echo-cmd! "^C"))

    ;; Reset completion-cycle whenever the input changes naturally.
    (reset! !completion-cycle nil)))

;; ============================================================================
;; Entry point
;; ============================================================================

(defn ^:export start
  "Wires up DOM handlers. Call from the page after document load."
  []
  (let [stored (ls-get)]
    (reset-state! {:keep-vfs? (some? stored)})
    (when-let [hist (:history stored)]
      (swap! !history assoc :items (vec hist))))
  (update-prompt!)
  (refresh-files-pane!)

  (let [input ($ "#input")]
    (.addEventListener input "keydown" (fn [e] (on-keydown input e))))

  (when-let [btn ($ "#reset-btn")]
    (.addEventListener
     btn "click"
     (fn [_]
       (ls-clear!)
       (reset! !history {:items [] :idx nil})
       (reset-state! {:keep-vfs? false})
       (clear-log!)
       (update-prompt!)
       (refresh-files-pane!)))))
