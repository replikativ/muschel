(ns muschel.cli
  "Command-line entry for muschel — bash-shaped invocation surface, plus
   a small set of muschel-specific extensions (sandbox flags, analysis
   subcommands).

   ## Invocation forms

     muschel [opts]                            interactive shell from a tty
     muschel [opts] -c COMMAND [$0 [args...]]  run inline source
     muschel [opts] script.sh [args...]        run a script file
     muschel [opts] -s [args...]               read script from stdin
     muschel translate [-f file | src]         bash → Clojure
     muschel check     [-f file | src]         permit dry-run report
     muschel parse     [-f file | src]         pretty-print AST

   ## Bash-compatible flags

     -c              inline command string (next arg is the source)
     -s              read script from stdin
     -n              parse-only; don't execute (validate syntax)
     -v              verbose (echo input lines as read)
     -x              xtrace (echo each command before exec)
     -o OPT          set shell option (errexit|pipefail|nounset|noglob)
     --              end of options
     --help          show help
     --version       show version

   ## Muschel extensions

     --sandbox       BuiltinHost + permit gate (requires --root or --virtual)
     --root DIR      DiskFS pinned to DIR
     --virtual [F]   in-memory VFS (empty, or seeded from edn file F)
     --permit FILE   append permit rules on top of the default ruleset
     --allow CMDS    comma-separated fallback-allowlist (git,clojure,...)
     --trace         emit a trace report to stderr on exit
     --budget N      cap to N executor steps

   ## OS-level sandbox (Linux only; requires --sandbox --root)

     --os-sandbox KIND   off (default) | bwrap
     --net MODE          off (default) | on — sandbox network policy
     --mem-max VAL       cgroup MemoryMax (e.g. 2G, 512M)
     --cpu-quota VAL     cgroup CPUQuota (e.g. 100% = 1 core, 200% = 2)
     --tasks-max N       cgroup TasksMax

   ## Bash-style positional semantics

   Bash treats anything after the script-file (or after `-c CMD`, or
   after `-s`) as positional ($0 / $1 ...), NOT as options. So
   `muschel script.sh -x` runs the script with $1 = \"-x\", not with
   xtrace on. We honour that — see `split-mode`."
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.tools.cli :as tools.cli]
            [muschel.builtins.posix :as posix]
            [muschel.core :as m]
            [muschel.emit :as emit]
            [muschel.env :as env]
            [muschel.exec :as exec]
            [muschel.fs.disk :as fs.disk]
            [muschel.fs.virtual :as fs.virtual]
            [muschel.host.sandboxed :as host.sandboxed]
            [muschel.parse :as parse]
            [muschel.permit :as permit]
            [muschel.session :as session]))

;; ============================================================================
;; Version
;; ============================================================================

(def ^:private version
  (or (some-> (io/resource "muschel/MUSCHEL_VERSION") slurp str/trim)
      "dev"))

;; ============================================================================
;; Subcommand verbs (recognised as the first non-flag positional)
;; ============================================================================

(def ^:private verbs #{"translate" "check" "parse"})

;; ============================================================================
;; Argv splitter — bash-style positional semantics
;; ============================================================================
;;
;; tools.cli is good at named flags but doesn't know that `bash script.sh
;; -x` should pass `-x` as $1. We pre-walk argv ourselves: collect named
;; flags + their values into a flag-argv vec, then stop at the first
;; mode-switching token (`-c`, `-s`, `--`, a verb, or any other bare
;; positional). Everything after is positional.

(def ^:private value-flags
  "Long/short flags that consume the next argv slot as their value."
  #{"-o" "--root" "--permit" "--allow" "--budget" "--virtual"
    "--os-sandbox" "--net" "--mem-max" "--cpu-quota" "--tasks-max"})

(def ^:private boolean-flags
  "Flags that DON'T consume the next slot."
  #{"-n" "-v" "-x" "-i" "--sandbox" "--trace" "--help" "-h" "--version"})

(defn- value-flag?    [s] (contains? value-flags s))
(defn- boolean-flag?  [s] (contains? boolean-flags s))
(defn- looks-like-flag? [s] (and s (str/starts-with? s "-") (not= s "-") (not= s "--")))

(defn- split-mode
  "Walk argv. Stop at the first mode-switching token; return a parse.

   Returns a map:
     :flag-argv  pre-mode flags + values, ready for tools.cli
     :mode       :interactive | :command | :stdin | :script | :verb | :end-of-opts
     :payload    inline source string (for :command)
     :verb       \"translate\" | \"check\" | \"parse\"  (for :verb)
     :rest       positional tail vec (for :command, :stdin, :script, :end-of-opts);
                 OR full argv tail for :verb (subcommand re-parses).
     :error      usage error string (mutually exclusive with the above)

   `--virtual` is a special case: it MAY take a path or be standalone.
   We treat it as boolean here (no value consumed) and let the flag
   parser look at the next arg in `:arguments` to decide. That's a tiny
   bit of fiddliness for one flag."
  [argv]
  (loop [[a & more :as remaining] argv
         flag-argv []]
    (cond
      (nil? a)
      {:flag-argv flag-argv :mode :interactive :rest []}

      ;; Subcommand verb? Only valid as the very first non-flag arg.
      ;; (We allow flags before, but bash-style flags don't really make
      ;; sense for translate/check/parse anyway — the subcommand has
      ;; its own option parser.)
      (and (empty? flag-argv) (verbs a))
      {:flag-argv [] :mode :verb :verb a :rest (vec more)}

      ;; -c CMD: consume one slot for CMD; everything after is
      ;; positional ([$0 [args...]]).
      (= a "-c")
      (if (empty? more)
        {:error "-c: option requires a value"}
        {:flag-argv flag-argv :mode :command :payload (first more) :rest (vec (rest more))})

      ;; -s: stdin mode. Rest is positional ([args...]).
      (= a "-s")
      {:flag-argv flag-argv :mode :stdin :rest (vec more)}

      ;; -- : end of options. Rest is positional ([script [args...]]),
      ;; treated the same way as a bare positional below.
      (= a "--")
      (if-let [first-pos (first more)]
        {:flag-argv flag-argv :mode :script :script first-pos :rest (vec (rest more))}
        {:flag-argv flag-argv :mode :interactive :rest []})

      ;; --virtual may be standalone (empty VFS) or followed by a seed
      ;; FILE. Always emit it with a value into flag-argv so tools.cli
      ;; (which sees it spec'd as `--virtual FILE`) doesn't error;
      ;; empty string == "no seed".
      (= a "--virtual")
      (let [nxt (first more)
            takes? (and (some? nxt)
                        (not (looks-like-flag? nxt))
                        (not (verbs nxt))
                        (not (contains? #{"-c" "-s" "--"} nxt)))]
        (if takes?
          (recur (rest more) (conj flag-argv a nxt))
          (recur more         (conj flag-argv a ""))))

      ;; Other value-taking flag.
      (value-flag? a)
      (if (empty? more)
        {:error (str a ": option requires a value")}
        (recur (rest more) (conj flag-argv a (first more))))

      ;; Boolean flag.
      (boolean-flag? a)
      (recur more (conj flag-argv a))

      ;; Combined short flags (-xv → -x -v).
      (and (looks-like-flag? a) (re-matches #"-[a-zA-Z]{2,}" a))
      (let [chars (rest a)
            expanded (mapv #(str "-" %) chars)]
        ;; If any expanded char is a value-flag, that's a usage error
        ;; (-no would mean -n -o, but -o needs a value).
        (if-let [bad (some #(when (value-flag? %) %) expanded)]
          {:error (str a ": cannot combine value-taking flag " bad
                       " in a cluster")}
          (recur more (into flag-argv expanded))))

      ;; Unknown flag — bash exits 2. We do the same.
      (looks-like-flag? a)
      {:error (str a ": unknown option")}

      ;; Bare positional: it's the script-file. Rest is $1..
      :else
      {:flag-argv flag-argv :mode :script :script a :rest (vec more)})))

;; ============================================================================
;; tools.cli spec for the flag-argv part
;; ============================================================================

(def ^:private cli-options
  [;; bash-compatible
   ["-n" nil "Parse-only; don't execute."        :id :parse-only]
   ["-v" nil "Verbose: echo input lines as read." :id :verbose]
   ["-x" nil "Xtrace: echo each command before exec." :id :xtrace]
   ["-i" nil "Force interactive (currently a no-op; tty detection wins)." :id :interactive]
   ["-o" "--option OPT" "Set a shell option (errexit|pipefail|nounset|noglob)."
    :id :options
    :multi true
    :default []
    :update-fn conj]
   ["-h" "--help"]
   [nil  "--version"]
   ;; muschel extensions
   [nil "--sandbox"           "Enable BuiltinHost sandbox."]
   [nil "--root DIR"          "DiskFS pinned to DIR (requires --sandbox)."]
   [nil "--virtual FILE"      "In-memory VFS, optionally seeded from FILE. Use --virtual '' for empty."
    :default :unset]
   [nil "--permit FILE"       "Append permit rules from FILE."]
   [nil "--allow CMDS"        "Comma-separated fallback-allowlist."]
   [nil "--trace"             "Emit a trace report to stderr on exit."]
   [nil "--budget N"          "Cap to N executor steps."
    :parse-fn #(Long/parseLong %)]
   ;; OS sandbox (Linux, requires --sandbox --root)
   [nil "--os-sandbox KIND"   "OS-layer sandbox: off (default) | bwrap. Requires --sandbox --root."
    :default "off"
    :validate [#{"off" "bwrap"} "must be off or bwrap"]]
   [nil "--net MODE"          "Sandbox network: off (default with --os-sandbox) | on."
    :default "off"
    :validate [#{"off" "on"} "must be off or on"]]
   [nil "--mem-max VAL"       "cgroup MemoryMax (e.g. 2G, 512M). Implies systemd-run."]
   [nil "--cpu-quota VAL"     "cgroup CPUQuota (e.g. 100%, 200%). Implies systemd-run."]
   [nil "--tasks-max N"       "cgroup TasksMax."
    :parse-fn #(Long/parseLong %)]])

(defn- print-help! []
  (println
   (str/join "\n"
             ["muschel — a contained bash shell."
              ""
              "USAGE"
              "  muschel [opts]                            interactive shell from a tty"
              "  muschel [opts] -c COMMAND [$0 [args...]]  run inline source"
              "  muschel [opts] script.sh [args...]        run a script file"
              "  muschel [opts] -s [args...]               read script from stdin"
              "  muschel translate [-f file | src]         bash → Clojure"
              "  muschel check     [-f file | src]         permit dry-run report"
              "  muschel parse     [-f file | src]         pretty-print AST"
              ""
              "BASH-COMPATIBLE FLAGS"
              "  -c              inline command string"
              "  -s              read script from stdin"
              "  -n              parse-only; don't execute"
              "  -v              verbose (echo input lines as read)"
              "  -x              xtrace (echo each command before exec)"
              "  -o OPT          set shell option (errexit|pipefail|nounset|noglob)"
              "  --              end of options"
              "  --help          show this help"
              "  --version       show version"
              ""
              "MUSCHEL EXTENSIONS"
              "  --sandbox       enable BuiltinHost + permit gate (needs --root or --virtual)"
              "  --root DIR      DiskFS pinned to DIR"
              "  --virtual [F]   in-memory VFS (empty, or seeded from edn file F)"
              "  --permit FILE   append permit rules on top of the default ruleset"
              "  --allow CMDS    comma-separated fallback-allowlist (e.g. git,clojure)"
              "  --trace         emit a trace report to stderr on exit"
              "  --budget N      cap to N executor steps"
              ""
              "OS SANDBOX (Linux only; requires --sandbox --root)"
              "  --os-sandbox KIND   off (default) | bwrap"
              "  --net MODE          off (default with --os-sandbox) | on"
              "  --mem-max VAL       cgroup MemoryMax (e.g. 2G, 512M)"
              "  --cpu-quota VAL     cgroup CPUQuota (e.g. 100%, 200%)"
              "  --tasks-max N       cgroup TasksMax"
              ""
              "EXAMPLES"
              "  muschel                                          # interactive shell"
              "  muschel -c 'echo hi | grep h'                    # one-shot"
              "  muschel script.sh foo bar                        # script with $1=foo $2=bar"
              "  muschel -n script.sh                             # validate syntax only"
              "  muschel --sandbox --root . script.sh             # sandboxed run"
              "  muschel --sandbox --virtual ./seed.edn -c 'ls'   # in-memory sandbox"
              "  muschel --sandbox --root . --os-sandbox bwrap \\"
              "          --mem-max 2G --cpu-quota 200% -c 'npm test'    # OS-jailed run"])))

;; ============================================================================
;; Sandbox / host construction
;; ============================================================================

(defn- read-edn-file [path]
  (try (edn/read-string (slurp path))
       (catch Exception e
         (binding [*out* *err*]
           (println (str "muschel: cannot read " path ": " (.getMessage e))))
         (System/exit 1))))

(defn- build-permit-cfg
  "Compose the rulesets vector for permit/check. Default first, then
   the user-supplied --permit file appended."
  [opts]
  (let [extra (when-let [p (:permit opts)] (read-edn-file p))]
    {:rulesets (cond-> [permit/default-rules]
                 (seq extra) (conj extra))
     :prompter permit/deny-all-prompter}))

(defn- maybe-wrap-os-sandbox
  "If --os-sandbox=bwrap, decorate the fallback host with
   SandboxedHost so allowlisted system commands run inside bwrap.

   Wraps the FALLBACK, not the BuiltinHost: muschel's own builtins
   (cat/ls/grep/…) execute in-process against the FS protocol and
   are already FS-jailed; only allowlisted system tools (git, npm,
   python, …) need OS-level isolation."
  [fallback-host opts]
  (if (not= "bwrap" (:os-sandbox opts))
    fallback-host
    (let [bind-root (.getCanonicalPath (java.io.File. ^String (:root opts)))]
      (host.sandboxed/make
       (cond-> {:wrapped   fallback-host
                :bind-root bind-root
                :net       (keyword (or (:net opts) "off"))}
         (:mem-max opts)    (assoc :mem-max    (:mem-max opts))
         (:cpu-quota opts)  (assoc :cpu-quota  (:cpu-quota opts))
         (:tasks-max opts)  (assoc :tasks-max  (:tasks-max opts)))))))

(defn- build-host
  "Returns {:host h :permit cfg-or-nil}.

   Without --sandbox             → JvmHost, no permit.
   With --sandbox                → BuiltinHost over DiskFS or VirtualFS.
   With --sandbox --os-sandbox=… → BuiltinHost whose fallback-host is
                                    wrapped in SandboxedHost (bwrap +
                                    optional cgroup limits).

   Permit cfg is built from defaults + --permit overlay."
  [opts]
  (if-not (:sandbox opts)
    {:host (m/jvm-host)}
    (let [fs    (cond
                  (:root opts)
                  (fs.disk/make (:root opts))

                  (not= :unset (:virtual opts))
                  (let [seed (when-not (str/blank? (:virtual opts))
                               (read-edn-file (:virtual opts)))]
                    (fs.virtual/make (or seed {}) {:cwd "/"})))
          allow (when-let [s (:allow opts)]
                  (set (map str/trim (str/split s #","))))
          fallback (maybe-wrap-os-sandbox (m/jvm-host) opts)
          host  (m/builtin-host
                 {:fs fs
                  :fallback-host fallback
                  :fallback-allowlist (or allow #{})
                  :builtins posix/standard})]
      {:host host :permit (build-permit-cfg opts)})))

(defn- validate-sandbox-flags
  "Return a usage-error string, or nil if OK."
  [opts]
  (let [has-root?    (some? (:root opts))
        has-virtual? (not= :unset (:virtual opts))
        has-sandbox? (:sandbox opts)
        os-sandbox   (:os-sandbox opts)
        cgroup-flag? (or (:mem-max opts) (:cpu-quota opts) (:tasks-max opts))
        net-set?     (and (:net opts) (not= "off" (:net opts)))]
    (cond
      (and has-sandbox? (not (or has-root? has-virtual?)))
      "--sandbox requires --root DIR or --virtual [FILE]"

      (and has-sandbox? has-root? has-virtual?)
      "--root and --virtual are mutually exclusive"

      (and (not has-sandbox?) (or has-root? has-virtual?))
      "--root / --virtual require --sandbox (otherwise the FS choice has no effect)"

      ;; --os-sandbox composition rules
      (and (= "bwrap" os-sandbox) (not has-sandbox?))
      "--os-sandbox=bwrap requires --sandbox --root DIR"

      (and (= "bwrap" os-sandbox) (not has-root?))
      "--os-sandbox=bwrap requires --root DIR (VirtualFS has no real path to bind)"

      (and (or cgroup-flag? net-set?) (not= "bwrap" os-sandbox))
      "--net / --mem-max / --cpu-quota / --tasks-max require --os-sandbox=bwrap"

      :else nil)))

(defn- apply-set-options
  "Apply -o OPTION flags to env's :options. Returns env or exits 2 on
   unknown option name."
  [env opt-names]
  (reduce
   (fn [e opt]
     (let [k (keyword opt)]
       (if (contains? (:options e) k)
         (assoc-in e [:options k] true)
         (do (binding [*out* *err*]
               (println (str "muschel: -o " opt ": unknown shell option")))
             (System/exit 2)))))
   env
   opt-names))

(defn- apply-bash-options [env opts]
  ;; Note: -v (`:verbose`) is currently a no-op; verbose echoing of input
  ;; lines isn't implemented yet. Accepted at the CLI so scripts that
  ;; passed it to bash don't break, but it has no runtime effect.
  (cond-> env
    (:xtrace opts)        (assoc-in [:options :xtrace] true)
    (seq (:options opts)) (apply-set-options (:options opts))))

;; ============================================================================
;; Running source
;; ============================================================================

(defn- write-permit-denial!
  "Parse-time permit denials exit 126 but `exec/run` doesn't itself
   write a stderr message for that path (runtime denials inside
   `run-external` do). Surface the per-call reasons here so users
   see why the run was refused."
  [result]
  (when-let [per-call (some-> result :permit :per-call)]
    (binding [*out* *err*]
      (doseq [pc per-call
              :when (= :deny (:decision pc))]
        (let [argv (->> (:call pc) :args
                        (map #(some-> % :parts first :value))
                        (str/join " "))]
          (println (str "muschel: permit denied `" argv "`: "
                        (:reason pc "?"))))))))

(defn- run-source
  "Run a bash source string. Streams stdout/stderr to System/out / System/err.
   Returns the exec result map (with :exit)."
  [src env-base host run-opts]
  (try
    (let [result (exec/run env-base src
                           (merge {:host host
                                   :out System/out
                                   :err System/err
                                   :in  System/in}
                                  run-opts))]
      (when (:denied-reason result)
        (write-permit-denial! result))
      result)
    (catch clojure.lang.ExceptionInfo e
      (binding [*out* *err*]
        (println (str "muschel: " (.getMessage e)))
        (when-let [pos (some-> e ex-data :pos)]
          (println (str "  at " pos))))
      {:exit 1})))

(defn- parse-only!
  "Implements `-n`: parse the source, exit 0 on success, 2 on syntax error."
  [src]
  (try
    (parse/parse src)
    (System/exit 0)
    (catch clojure.lang.ExceptionInfo e
      (binding [*out* *err*]
        (println (str "muschel: " (.getMessage e))))
      (System/exit 2))))

;; ============================================================================
;; Default mode: shell / -c / -s / script
;; ============================================================================

(defn- new-env-for [{:keys [sandbox]} {:keys [script pos-args]}]
  (env/new-env :script    (or script "muschel")
               :pos-args  (or pos-args [])
               :host-env? (not sandbox)))

(defn- run-command-mode
  "muschel [opts] -c CMD [$0 [args...]]"
  [opts payload rest-args]
  (let [{:keys [host permit]} (build-host opts)
        env-base (-> (new-env-for opts {:script   (or (first rest-args) "muschel")
                                        :pos-args (vec (rest rest-args))})
                     (apply-bash-options opts))
        run-opts (cond-> {} permit (assoc :permit permit))]
    (if (:parse-only opts)
      (parse-only! payload)
      (let [{:keys [exit]} (run-source payload env-base host run-opts)]
        (System/exit (or exit 0))))))

(defn- run-stdin-mode
  "muschel [opts] -s [args...]"
  [opts rest-args]
  (let [{:keys [host permit]} (build-host opts)
        src (slurp System/in)
        env-base (-> (new-env-for opts {:script   "muschel"
                                        :pos-args (vec rest-args)})
                     (apply-bash-options opts))
        run-opts (cond-> {} permit (assoc :permit permit))]
    (if (:parse-only opts)
      (parse-only! src)
      (let [{:keys [exit]} (run-source src env-base host run-opts)]
        (System/exit (or exit 0))))))

(defn- run-script-mode
  "muschel [opts] script.sh [args...]"
  [opts script-path rest-args]
  (let [src (try (slurp script-path)
                 (catch Exception _
                   (binding [*out* *err*]
                     (println (str "muschel: " script-path
                                   ": No such file or directory")))
                   (System/exit 127)))
        {:keys [host permit]} (build-host opts)
        env-base (-> (new-env-for opts {:script script-path :pos-args (vec rest-args)})
                     (apply-bash-options opts))
        run-opts (cond-> {} permit (assoc :permit permit))]
    (if (:parse-only opts)
      (parse-only! src)
      (let [{:keys [exit]} (run-source src env-base host run-opts)]
        (System/exit (or exit 0))))))

(defn- run-interactive-mode
  "muschel [opts]  — interactive REPL loop. Mirrors `bb sh`'s shape but
   threads --sandbox/--permit/etc. through `build-host`."
  [opts]
  (let [{:keys [host permit]} (build-host opts)
        sess (m/atom-session
              (-> (new-env-for opts {:script "muschel" :pos-args []})
                  (apply-bash-options opts)))
        run-opts (cond-> {:host host :session sess
                          :out System/out :err System/err :in System/in}
                   permit (assoc :permit permit))]
    (println "muschel" version
             "—" (if (:sandbox opts) "sandboxed" "unsandboxed")
             "— Ctrl-D or 'exit' to quit")
    (loop []
      (print (str (:cwd (session/-env sess))
                  " $? " (:last-exit (session/-env sess)) " > "))
      (flush)
      (when-let [line (read-line)]
        (let [trimmed (str/trim line)]
          (when-not (contains? #{"exit" "quit"} trimmed)
            (when (seq trimmed)
              (try
                (exec/run (session/-env sess) line run-opts)
                (catch clojure.lang.ExceptionInfo e
                  (binding [*out* *err*]
                    (println (str "muschel: " (.getMessage e)))))))
            (recur)))))))

;; ============================================================================
;; Subcommands: translate / parse / check
;; ============================================================================

(def ^:private sub-cli-options
  [["-f" "--file FILE" "Read source from FILE instead of positional arg."]
   ["-h" "--help"]
   [nil  "--permit FILE" "(check only) append permit rules from FILE."]])

(defn- read-sub-source
  "translate/parse/check accept either -f FILE or a positional source
   string. Returns the source, or exits 2 with usage error."
  [{:keys [file]} positional verb]
  (cond
    file (try (slurp file)
              (catch Exception e
                (binding [*out* *err*]
                  (println (str "muschel " verb ": " (.getMessage e))))
                (System/exit 1)))
    (seq positional) (first positional)
    :else (do (binding [*out* *err*]
                (println (str "usage: muschel " verb " [-f FILE | <bash-src>]")))
              (System/exit 2))))

(defn- run-translate [args]
  (let [{:keys [options arguments errors]}
        (tools.cli/parse-opts args sub-cli-options)]
    (cond
      errors      (do (binding [*out* *err*] (println (str/join \newline errors))) (System/exit 2))
      (:help options) (do (println "muschel translate [-f FILE | <bash-src>]") (System/exit 0))
      :else
      (let [src (read-sub-source options arguments "translate")]
        (try (pprint (emit/translate src))
             (System/exit 0)
             (catch clojure.lang.ExceptionInfo e
               (binding [*out* *err*]
                 (println (str "muschel translate: " (.getMessage e))))
               (System/exit 1)))))))

(defn- run-parse [args]
  (let [{:keys [options arguments errors]}
        (tools.cli/parse-opts args sub-cli-options)]
    (cond
      errors      (do (binding [*out* *err*] (println (str/join \newline errors))) (System/exit 2))
      (:help options) (do (println "muschel parse [-f FILE | <bash-src>]") (System/exit 0))
      :else
      (let [src (read-sub-source options arguments "parse")]
        (try (pprint (parse/parse src))
             (System/exit 0)
             (catch clojure.lang.ExceptionInfo e
               (binding [*out* *err*]
                 (println (str "muschel parse: " (.getMessage e))))
               (System/exit 1)))))))

(defn- print-check-report [{:keys [decision per-call]}]
  (println "Decision:" (str/upper-case (name decision)))
  (println)
  (println "Calls:")
  (doseq [{:keys [call] :as pc} per-call]
    (let [cmd (or (some-> call :args first :parts first :value) "?")
          argv (->> (:args call)
                    (map #(some-> % :parts first :value))
                    (str/join " "))
          d (name (:decision pc))
          tag (format "[%-5s]" (str/upper-case d))]
      (println (str "  " tag "  " argv
                    (when-let [r (:reason pc)] (str "  — " r)))))))

(defn- run-check [args]
  (let [{:keys [options arguments errors]}
        (tools.cli/parse-opts args sub-cli-options)]
    (cond
      errors      (do (binding [*out* *err*] (println (str/join \newline errors))) (System/exit 2))
      (:help options) (do (println "muschel check [-f FILE | <bash-src>] [--permit FILE]") (System/exit 0))
      :else
      (let [src (read-sub-source options arguments "check")
            extra (when-let [p (:permit options)] (read-edn-file p))
            ast (try (parse/parse src)
                     (catch clojure.lang.ExceptionInfo e
                       (binding [*out* *err*]
                         (println (str "muschel check: " (.getMessage e))))
                       (System/exit 1)))
            rulesets (cond-> [permit/default-rules] (seq extra) (conj extra))
            result (permit/check {:rulesets rulesets :ast ast
                                  :prompter permit/deny-all-prompter})]
        (print-check-report result)
        (System/exit (case (:decision result) :allow 0 :deny 1))))))

;; ============================================================================
;; Main
;; ============================================================================

(defn- exit-usage! [msg]
  (binding [*out* *err*] (println (str "muschel: " msg)))
  (System/exit 2))

(defn -main [& argv]
  (let [{:keys [flag-argv mode payload verb script error]
         pos-tail :rest}
        (split-mode (vec argv))]
    (when error (exit-usage! error))
    (let [{:keys [options errors]}
          (tools.cli/parse-opts flag-argv cli-options)]
      (when (seq errors)
        (exit-usage! (str/join "; " errors)))
      (cond
        (:help options)    (do (print-help!) (System/exit 0))
        (:version options) (do (println "muschel" version) (System/exit 0)))

      (when (not= :verb mode)
        (when-let [err (validate-sandbox-flags options)]
          (exit-usage! err)))

      (case mode
        :verb (case verb
                "translate" (run-translate pos-tail)
                "parse"     (run-parse pos-tail)
                "check"     (run-check pos-tail))
        :command     (run-command-mode     options payload pos-tail)
        :stdin       (run-stdin-mode       options pos-tail)
        :script      (run-script-mode      options script pos-tail)
        :interactive (run-interactive-mode options)))))
