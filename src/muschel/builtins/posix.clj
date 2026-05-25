(ns muschel.builtins.posix
  "Pure-Clojure implementations of a useful subset of POSIX coreutils.

   Each fn has the same shape:

       (cmd-fn argv fs env) → {:stdout str :stderr str :exit int}

   - `argv` is the post-expansion argv (a vector of strings, including
     the command name as the first element — caller doesn't strip it).
   - `fs` is a muschel.fs/FS handle (containment + read-file etc.).
   - `env` is the muschel env value (for $PWD, $HOME etc.) plus
     `:stdin` (string, optional) injected by the host wrapper for
     builtins that read from stdin (xargs, tr, cut, …).

   Coverage in v1 (read-only):

     pwd echo ls cat head tail wc stat which sort uniq sh bash dash
     grep find tr cut diff xargs

   Option parsing uses `clojure.tools.cli` so every builtin gets the
   same POSIX(-ish) flag handling: short-flag combos (`-la`), short
   with value (`-n10`), long with `=` or space, and `--` terminator.

   Reference: uutils-coreutils. Long GNU options not in our spec
   surface as parse errors with a clear message."
  (:refer-clojure :exclude [cat])
  (:require [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [diffit.vec :as dv]
            [muschel.fs :as fs]
            [muschel.host :as host]))

;; ============================================================================
;; Dynamic context — set by muschel.host.builtin around every dispatch
;;
;; Most builtins only care about (argv, fs, env). Builtins like `sh`,
;; `find -exec`, and `xargs` need recursive access to the dispatching
;; host so they can re-enter through the same gates. Dynamic vars keep
;; the common signature unchanged while letting the few that need it
;; reach further.
;; ============================================================================

(def ^:dynamic *host* nil)
(def ^:dynamic *session* nil)
(def ^:dynamic *depth* 0)

(def ^:private max-shell-depth
  "Cap on nested `sh -c …` / `find -exec` / `xargs` chains."
  32)

;; ============================================================================
;; Result builders
;; ============================================================================

(defn- ok
  ([stdout]        {:stdout stdout :stderr ""     :exit 0})
  ([stdout stderr] {:stdout stdout :stderr stderr :exit 0}))

(defn- err
  ([msg]      {:stdout "" :stderr (str msg "\n") :exit 1})
  ([msg code] {:stdout "" :stderr (str msg "\n") :exit code}))

(defn- usage-err [cmd msg]
  (err (str cmd ": " msg) 2))

;; ============================================================================
;; tools.cli wrapper — every builtin's argv goes through this.
;; ============================================================================

(defn- cli-parse
  "Run `clojure.tools.cli/parse-opts` on (rest argv).

   Returns `{:opts {} :pos [] :err nil|string}`.

   `spec` follows tools.cli's vector-of-vectors format. `:in-order true`
   is the default so flags that look like options but belong to a nested
   command (e.g. `xargs cmd -x`, `sh -c 'echo' '-x'`) pass through as
   positionals."
  ([argv spec] (cli-parse argv spec {:in-order true}))
  ([argv spec opts]
   (let [args (rest argv)
         {:keys [options arguments errors]}
         (apply cli/parse-opts args spec (mapcat identity opts))]
     {:opts options
      :pos  (vec arguments)
      :err  (when (seq errors) (str/join "; " errors))})))

;; ============================================================================
;; pwd
;; ============================================================================

(defn pwd
  "POSIX pwd. -L and -P are accepted but currently treat the env's
   logical cwd as both — the muschel session tracks logical cwd."
  [_argv fs _env]
  (ok (str (fs/cwd fs) "\n")))

;; ============================================================================
;; echo
;; ============================================================================

(def ^:private echo-spec
  [["-n" "--no-newline"]
   ["-e" "--escapes"]
   ["-E" "--no-escapes"]])

(defn echo
  "POSIX echo. Honours -n (no trailing newline) and -e (interpret
   simple backslash escapes: \\n \\t \\r \\\\). -E is the default."
  [argv _fs _env]
  (let [{:keys [opts pos err]} (cli-parse argv echo-spec)]
    (if err
      (usage-err "echo" err)
      (let [escapes? (and (:escapes opts) (not (:no-escapes opts)))
            text     (str/join " " pos)
            rendered (if escapes?
                       (-> text
                           (str/replace "\\n" "\n")
                           (str/replace "\\t" "\t")
                           (str/replace "\\r" "\r")
                           (str/replace "\\\\" "\\"))
                       text)]
        (ok (if (:no-newline opts) rendered (str rendered "\n")))))))

;; ============================================================================
;; ls
;; ============================================================================

(def ^:private ls-spec
  [["-a" "--all"]
   ["-A" "--almost-all"]
   ["-l" "--long"]
   ["-1" "--one-per-line"]
   ["-h" "--human-readable"]
   ["-F" "--classify"]])

(defn- format-ls-long [entry]
  (let [t (case (:type entry)
            :dir     "d"
            :file    "-"
            :symlink "l"
            "?")
        size (or (:size entry) 0)]
    (format "%s %10d %s" t size (:name entry))))

(defn ls
  "POSIX ls, subset: -a (show dotfiles), -l (long format), -1
   (one-per-line). Default is one-per-line; we don't emulate
   column layout. Missing targets go to stderr with exit 2."
  [argv fs _env]
  (let [{:keys [opts pos err]} (cli-parse argv ls-spec)]
    (if err
      (usage-err "ls" err)
      (let [all?    (or (:all opts) (:almost-all opts))
            long?   (:long opts)
            targets (if (seq pos) pos ["."])
            multi?  (> (count targets) 1)
            stdout  (volatile! [])
            stderr  (volatile! [])
            any-err? (volatile! false)]
        (doseq [target targets]
          (let [stat (fs/stat fs target)]
            (cond
              (nil? stat)
              (do (vswap! stderr conj
                          (str "ls: cannot access '" target "': No such file or directory"))
                  (vreset! any-err? true))

              (= :dir (:type stat))
              (let [entries (->> (fs/list-dir fs target)
                                 (filter (fn [{:keys [name]}]
                                           (or all?
                                               (not (str/starts-with? name "."))))))
                    rows    (if long?
                              (mapv format-ls-long entries)
                              (mapv :name entries))]
                (when multi? (vswap! stdout conj (str target ":")))
                (vswap! stdout into rows)
                (when multi? (vswap! stdout conj "")))

              :else
              (vswap! stdout conj
                      (if long?
                        (format-ls-long {:type (:type stat) :size (:size stat) :name target})
                        target)))))
        {:stdout (cond-> (str/join "\n" @stdout)
                   (seq @stdout) (str "\n"))
         :stderr (cond-> (str/join "\n" @stderr)
                   (seq @stderr) (str "\n"))
         :exit (if @any-err? 2 0)}))))

;; ============================================================================
;; cat
;; ============================================================================

(def ^:private cat-spec
  [["-n" "--number"]
   ["-b" "--number-nonblank"]
   ["-s" "--squeeze-blank"]
   ["-E" "--show-ends"]
   ["-v" "--show-nonprinting"]
   ["-T" "--show-tabs"]
   ["-A" "--show-all"]])

(defn cat
  "POSIX cat, subset: -n (number all lines), -b (number non-empty),
   -s (squeeze blank lines), -E (show $ at line end). No -A (we
   don't implement TAB/control-char display)."
  [argv fs env]
  (let [{:keys [opts pos err]} (cli-parse argv cat-spec)]
    (if err
      (usage-err "cat" err)
      (let [num-all?    (:number opts)
            num-nb?     (:number-nonblank opts)
            squeeze?    (:squeeze-blank opts)
            show-end?   (or (:show-ends opts) (:show-all opts))
            stdin       (or (:stdin env) "")
            files       (if (seq pos) pos ["-"])
            stderr      (volatile! "")
            any-err?    (volatile! false)
            n           (volatile! 0)
            last-blank? (volatile! false)
            process-line
            (fn [line]
              (let [is-blank? (= "" line)]
                (cond
                  (and squeeze? @last-blank? is-blank?) nil
                  :else
                  (let [_ (vreset! last-blank? is-blank?)
                        line' (if show-end? (str line "$") line)]
                    (cond
                      num-nb?
                      (if is-blank?
                        line'
                        (do (vswap! n inc)
                            (format "%6d\t%s" @n line')))
                      num-all?
                      (do (vswap! n inc)
                          (format "%6d\t%s" @n line'))
                      :else line')))))
            contents
            (mapcat (fn [f]
                      (if (= "-" f)
                        (str/split-lines stdin)
                        (let [bytes (fs/read-bytes fs f)]
                          (if (nil? bytes)
                            (let [stat (fs/stat fs f)
                                  msg  (if (= :dir (:type stat))
                                         (str "cat: " f ": Is a directory")
                                         (str "cat: " f ": No such file or directory"))]
                              (vswap! stderr str msg "\n")
                              (vreset! any-err? true)
                              nil)
                            (let [s (if (string? bytes) bytes (String. ^bytes bytes "UTF-8"))]
                              (str/split-lines s))))))
                    files)
            processed (keep process-line contents)]
        {:stdout (str (str/join "\n" processed)
                      (when (seq processed) "\n"))
         :stderr @stderr
         :exit (if @any-err? 1 0)}))))

;; ============================================================================
;; head / tail — POSIX -N shorthand for -n N supported via a small pre-pass
;; ============================================================================

(defn- expand-numeric-n
  "POSIX head/tail accept `-5` as shorthand for `-n 5`. tools.cli
   doesn't speak this form, so rewrite the first matching arg before
   handing it off. Only rewrites a single `-\\d+` arg."
  [argv]
  (let [cmd (first argv)
        rest-args (rest argv)
        out (atom [])
        rewritten? (atom false)]
    (doseq [a rest-args]
      (if (and (not @rewritten?)
               (re-matches #"-\d+" a))
        (do (swap! out conj "-n" (subs a 1))
            (reset! rewritten? true))
        (swap! out conj a)))
    (into [cmd] @out)))

(def ^:private head-tail-spec
  [["-n" "--lines N"
    :default 10
    :parse-fn #(Long/parseLong %)
    :validate [#(>= % 0) "must be non-negative"]]])

(defn head
  "POSIX head. -n N (or -N) lines, default 10."
  [argv fs env]
  (let [{:keys [opts pos err]} (cli-parse (expand-numeric-n argv) head-tail-spec)]
    (if err
      (usage-err "head" err)
      (let [n     (:lines opts)
            stdin (or (:stdin env) "")
            files (if (seq pos) pos ["-"])
            stderr (volatile! "")
            any-err? (volatile! false)
            results
            (mapv (fn [f]
                    (let [content (if (= "-" f) stdin (fs/read-file fs f))]
                      (if (nil? content)
                        (do (vswap! stderr str "head: cannot open '" f "' for reading: No such file or directory\n")
                            (vreset! any-err? true)
                            "")
                        (->> (str/split-lines content)
                             (take n)
                             (str/join "\n")
                             (#(if (str/blank? %) "" (str % "\n")))))))
                  files)]
        {:stdout (str/join "\n" results)
         :stderr @stderr
         :exit (if @any-err? 1 0)}))))

(defn tail
  "POSIX tail. -n N (or -N), default 10. No -f."
  [argv fs env]
  (let [{:keys [opts pos err]} (cli-parse (expand-numeric-n argv) head-tail-spec)]
    (if err
      (usage-err "tail" err)
      (let [n     (:lines opts)
            stdin (or (:stdin env) "")
            files (if (seq pos) pos ["-"])
            stderr (volatile! "")
            any-err? (volatile! false)
            results
            (mapv (fn [f]
                    (let [content (if (= "-" f) stdin (fs/read-file fs f))]
                      (if (nil? content)
                        (do (vswap! stderr str "tail: cannot open '" f "' for reading: No such file or directory\n")
                            (vreset! any-err? true)
                            "")
                        (->> (str/split-lines content)
                             (take-last n)
                             (str/join "\n")
                             (#(if (str/blank? %) "" (str % "\n")))))))
                  files)]
        {:stdout (str/join "\n" results)
         :stderr @stderr
         :exit (if @any-err? 1 0)}))))

;; ============================================================================
;; wc
;; ============================================================================

(def ^:private wc-spec
  [["-l" "--lines"]
   ["-w" "--words"]
   ["-c" "--bytes"]
   ["-m" "--chars"]])

(defn- count-stats [s]
  (let [byte-count (count (.getBytes ^String s "UTF-8"))
        word-count (count (re-seq #"\S+" s))
        line-count (if (= "" s) 0 (count (str/split-lines s)))]
    {:lines line-count :words word-count :bytes byte-count}))

(defn wc
  "POSIX wc. -l (lines), -w (words), -c (bytes). Default: all three.
   With multiple files, prints a final 'total' row."
  [argv fs env]
  (let [{:keys [opts pos err]} (cli-parse argv wc-spec)]
    (if err
      (usage-err "wc" err)
      (let [explicit? (some opts [:lines :words :bytes])
            show-l?   (or (not explicit?) (boolean (:lines opts)))
            show-w?   (or (not explicit?) (boolean (:words opts)))
            show-c?   (or (not explicit?) (boolean (:bytes opts)))
            stdin     (or (:stdin env) "")
            files     (if (seq pos) pos ["-"])
            stderr    (volatile! "")
            any-err?  (volatile! false)
            rows (keep
                  (fn [f]
                    (let [content (if (= "-" f) stdin (fs/read-file fs f))]
                      (if (and (not= "-" f) (nil? content))
                        (do
                          (vswap! stderr str "wc: " f ": No such file or directory\n")
                          (vreset! any-err? true)
                          nil)
                        (assoc (count-stats (or content "")) :name f))))
                  files)
            fmt-row (fn [{:keys [lines words bytes name]}]
                      (str/trim
                       (str (when show-l? (format "%8d " lines))
                            (when show-w? (format "%8d " words))
                            (when show-c? (format "%8d " bytes))
                            (when (and name (not= "-" name)) name))))
            total (when (> (count rows) 1)
                    (reduce (fn [acc r]
                              (-> acc
                                  (update :lines + (:lines r))
                                  (update :words + (:words r))
                                  (update :bytes + (:bytes r))))
                            {:lines 0 :words 0 :bytes 0 :name "total"}
                            rows))
            lines (mapv fmt-row (if total (concat rows [total]) rows))]
        {:stdout (str (str/join "\n" lines)
                      (when (seq lines) "\n"))
         :stderr @stderr
         :exit (if @any-err? 1 0)}))))

;; ============================================================================
;; sh / bash — recursive shell-of-our-own-shell
;; ============================================================================

(def ^:private sh-spec
  [["-c" "--command CMD"]
   ["-i" "--interactive"]
   ["-l" "--login"]
   ["-s" "--stdin"]])

(defn sh
  "Builtin sh / bash. Supports -c SCRIPT — the only mode we need.
   Parses SCRIPT with muschel.parse and runs it through the same
   host the outer invocation is on."
  [argv _fs env]
  (let [{:keys [opts err]} (cli-parse argv sh-spec)]
    (cond
      err
      (usage-err "sh" err)

      (>= *depth* max-shell-depth)
      (err (str "sh: too many nested shell invocations (depth >= "
                max-shell-depth ")")
           2)

      (:command opts)
      (let [script (:command opts)
            host   *host*]
        (if-not host
          (err "sh: no host available for nested dispatch" 1)
          (let [_         (require 'muschel.parse 'muschel.exec 'muschel.env)
                parse-fn  (resolve 'muschel.parse/parse)
                run-fn    (resolve 'muschel.exec/run-and-capture)
                new-env   (resolve 'muschel.env/new-env)
                e0        (or env (new-env))
                ast       (parse-fn script)
                result    (binding [*depth* (inc *depth*)]
                            (run-fn e0 ast
                                    (cond-> {:host host}
                                      *session* (assoc :session *session*))))]
            {:stdout (or (:stdout result) "")
             :stderr (or (:stderr result) "")
             :exit   (or (:exit result) 0)})))

      :else
      (err "sh: only -c SCRIPT mode is supported in muschel builtins" 2))))

;; ============================================================================
;; stat — file metadata as text
;; ============================================================================

(defn stat
  "POSIX-ish stat. Prints `<size> <type> <name>` per file. Multiple
   files allowed."
  [argv fs _env]
  (let [files (rest argv)
        stderr (volatile! "")
        any-err? (volatile! false)
        lines
        (mapv (fn [f]
                (if-let [s (fs/stat fs f)]
                  (format "%10d %s %s" (:size s) (name (or (:type s) :unknown)) f)
                  (do
                    (vswap! stderr str "stat: cannot stat '" f "': No such file or directory\n")
                    (vreset! any-err? true)
                    nil)))
              files)
        valid (remove nil? lines)]
    {:stdout (str (str/join "\n" valid) (when (seq valid) "\n"))
     :stderr @stderr
     :exit (if @any-err? 1 0)}))

;; ============================================================================
;; which — is a command available?
;; ============================================================================

(defn which
  "Report which names are builtins or allowlisted-fallback. Reads
   the dispatch registry off *host*; without one, says nothing is
   available."
  [argv _fs _env]
  (let [names (rest argv)
        host  *host*
        registry (when host
                   (set (concat (keys (or (:builtins host) {}))
                                (or (:fallback-allowlist host) #{}))))
        stderr (volatile! "")
        any-err? (volatile! false)
        lines
        (mapv (fn [n]
                (if (and registry (contains? registry n))
                  (str "(muschel) " n)
                  (do (vswap! stderr str "which: no " n " in muschel registry\n")
                      (vreset! any-err? true)
                      nil)))
              names)
        valid (remove nil? lines)]
    {:stdout (str (str/join "\n" valid) (when (seq valid) "\n"))
     :stderr @stderr
     :exit (if @any-err? 1 0)}))

;; ============================================================================
;; gather-input — read positional files (and stdin) into one string
;; ============================================================================

(defn- gather-input
  "Concatenate stdin (when `-` or no args) and named files. Returns
   `[content err? stderr]`."
  [files fs env cmd]
  (let [stdin    (or (:stdin env) "")
        targets  (if (seq files) files ["-"])
        stderr   (volatile! "")
        any-err? (volatile! false)
        parts
        (mapv (fn [f]
                (if (= "-" f)
                  stdin
                  (or (fs/read-file fs f)
                      (do (vswap! stderr str cmd ": " f ": No such file or directory\n")
                          (vreset! any-err? true)
                          ""))))
              targets)]
    [(str/join "" parts) @any-err? @stderr]))

;; ============================================================================
;; sort / uniq — line-oriented transforms
;; ============================================================================

(def ^:private sort-spec
  [["-r" "--reverse"]
   ["-n" "--numeric-sort"]
   ["-u" "--unique"]
   ["-f" "--ignore-case"]])

(defn sort-fn
  "POSIX sort, subset: -r reverse, -n numeric, -u unique, -f ignore-case."
  [argv fs env]
  (let [{:keys [opts pos err]} (cli-parse argv sort-spec)]
    (if err
      (usage-err "sort" err)
      (let [[content err? stderr] (gather-input pos fs env "sort")
            lines (str/split-lines content)
            key-fn (if (:ignore-case opts) str/lower-case identity)
            cmp (if (:numeric-sort opts)
                  (fn [a b]
                    (let [pa (try (Long/parseLong (str/trim a))
                                  (catch Exception _ Long/MAX_VALUE))
                          pb (try (Long/parseLong (str/trim b))
                                  (catch Exception _ Long/MAX_VALUE))]
                      (compare pa pb)))
                  (fn [a b] (compare (key-fn a) (key-fn b))))
            sorted (sort cmp lines)
            sorted (if (:reverse opts) (reverse sorted) sorted)
            sorted (if (:unique opts) (distinct sorted) sorted)]
        {:stdout (str (str/join "\n" sorted)
                      (when (seq sorted) "\n"))
         :stderr stderr
         :exit (if err? 1 0)}))))

(def ^:private uniq-spec
  [["-c" "--count"]
   ["-d" "--repeated"]
   ["-u" "--unique"]
   ["-i" "--ignore-case"]])

(defn uniq
  "POSIX uniq, subset: -c count, -d only-dupes, -u only-uniques.
   Adjacent only (matches GNU)."
  [argv fs env]
  (let [{:keys [opts pos err]} (cli-parse argv uniq-spec)]
    (if err
      (usage-err "uniq" err)
      (let [[content err? stderr] (gather-input pos fs env "uniq")
            lines (str/split-lines content)
            key-fn (if (:ignore-case opts) str/lower-case identity)
            grouped (->> lines
                         (partition-by key-fn)
                         (map (fn [grp]
                                {:line (first grp) :n (count grp)})))
            filtered (cond
                       (:repeated opts) (filter #(> (:n %) 1) grouped)
                       (:unique opts)   (filter #(= 1 (:n %)) grouped)
                       :else grouped)
            rendered (mapv (fn [{:keys [line n]}]
                             (if (:count opts)
                               (format "%7d %s" n line)
                               line))
                           filtered)]
        {:stdout (str (str/join "\n" rendered)
                      (when (seq rendered) "\n"))
         :stderr stderr
         :exit (if err? 1 0)}))))

;; ============================================================================
;; grep — regex line search
;; ============================================================================

(def ^:private grep-spec
  [["-E" "--extended-regexp"]
   ["-F" "--fixed-strings"]
   ["-i" "--ignore-case"]
   ["-n" "--line-number"]
   ["-v" "--invert-match"]
   ["-c" "--count"]
   ["-l" "--files-with-matches"]
   ["-L" "--files-without-match"]
   ["-H" "--with-filename"]
   ["-h" "--no-filename"]
   ["-r" "--recursive"]
   ["-q" "--quiet"]])

(defn- compile-pattern
  "Build a java.util.regex.Pattern from the grep pattern, honouring
   -F (literal) and -i. We don't distinguish BRE vs ERE — Java's
   regex engine is closer to ERE, which is what -E selects and what
   most modern users expect by default."
  [pattern {:keys [fixed-strings ignore-case]}]
  (let [body (if fixed-strings (java.util.regex.Pattern/quote pattern) pattern)
        flags (cond-> 0
                ignore-case (bit-or java.util.regex.Pattern/CASE_INSENSITIVE))]
    (java.util.regex.Pattern/compile body flags)))

(defn- walk-files
  "Recursively yield {:path … :type :file} maps under target. Uses the
   FS handle so containment is honoured. Skips dotfiles only at the
   user's request — grep -r matches GNU behaviour and follows in."
  [fs target]
  (let [stat (fs/stat fs target)]
    (cond
      (nil? stat) []
      (= :file (:type stat)) [{:path target}]
      (= :dir (:type stat))
      (loop [pending [target]
             acc []]
        (if-let [p (peek pending)]
          (let [pending (pop pending)
                children (or (fs/list-dir fs p) [])]
            (recur
             (into pending
                   (->> children
                        (filter #(= :dir (:type %)))
                        (map #(str p "/" (:name %)))))
             (into acc
                   (->> children
                        (filter #(= :file (:type %)))
                        (map #(hash-map :path (str p "/" (:name %))))))))
          acc))
      :else [])))

(defn grep
  "POSIX grep, subset: -E -F -i -n -v -c -l -L -H -h -r -q.
   With no files and stdin available in env, reads stdin. Exit 0 if
   any match, 1 if no match, 2 on usage error."
  [argv fs env]
  (let [{:keys [opts pos err]} (cli-parse argv grep-spec)]
    (cond
      err (usage-err "grep" err)
      (empty? pos) (usage-err "grep" "missing pattern")
      :else
      (let [pattern (first pos)
            files   (rest pos)
            re      (compile-pattern pattern opts)
            targets (cond
                      (and (empty? files) (contains? env :stdin))
                      [{:path nil :stdin-content (:stdin env)}]
                      (:recursive opts)
                      (mapcat (fn [t] (walk-files fs t)) (or (seq files) ["."]))
                      :else
                      (mapv (fn [f] {:path f}) (or (seq files) ["-"])))
            multi?  (> (count targets) 1)
            show-name? (or (:with-filename opts)
                           (and multi? (not (:no-filename opts))))
            stderr  (volatile! "")
            any-err? (volatile! false)
            any-match? (volatile! false)
            stdout  (volatile! [])
            invert? (:invert-match opts)
            matches?
            (fn [line]
              (let [m (.find (.matcher re line))]
                (if invert? (not m) m)))]
        (doseq [{:keys [path stdin-content]} targets]
          (let [content
                (cond
                  (some? stdin-content) stdin-content
                  (= "-" path) (or (:stdin env) "")
                  :else
                  (let [c (fs/read-file fs path)]
                    (when (nil? c)
                      (vswap! stderr str "grep: " path ": No such file or directory\n")
                      (vreset! any-err? true))
                    c))]
            (when content
              (let [lines (str/split-lines content)
                    hits (keep-indexed
                          (fn [i ln] (when (matches? ln) {:i (inc i) :line ln}))
                          lines)]
                (cond
                  (:files-with-matches opts)
                  (when (seq hits)
                    (vreset! any-match? true)
                    (vswap! stdout conj path))

                  (:files-without-match opts)
                  (when (empty? hits)
                    (vreset! any-match? true)
                    (vswap! stdout conj path))

                  (:count opts)
                  (let [n (count hits)]
                    (when (pos? n) (vreset! any-match? true))
                    (vswap! stdout conj (if show-name? (str path ":" n) (str n))))

                  :else
                  (doseq [{:keys [i line]} hits]
                    (vreset! any-match? true)
                    (let [pieces (cond-> []
                                   show-name? (conj path)
                                   (:line-number opts) (conj (str i))
                                   true (conj line))]
                      (vswap! stdout conj (str/join ":" pieces)))))))))
        (cond
          (:quiet opts)
          {:stdout "" :stderr "" :exit (if @any-match? 0 1)}

          :else
          {:stdout (str (str/join "\n" @stdout)
                       (when (seq @stdout) "\n"))
           :stderr @stderr
           :exit (cond
                   @any-err? 2
                   @any-match? 0
                   :else 1)})))))

;; ============================================================================
;; find — path walker with safe -exec via builtin host
;; ============================================================================

(defn- glob->re
  "Translate a POSIX glob (-name) to a Java regex. * → .*, ? → .,
   [abc] passes through. Escapes other regex metacharacters."
  [glob]
  (let [sb (StringBuilder.)]
    (doseq [c glob]
      (case c
        \* (.append sb ".*")
        \? (.append sb ".")
        \. (.append sb "\\.")
        \( (.append sb "\\(")
        \) (.append sb "\\)")
        \+ (.append sb "\\+")
        \| (.append sb "\\|")
        \^ (.append sb "\\^")
        \$ (.append sb "\\$")
        \{ (.append sb "\\{")
        \} (.append sb "\\}")
        (.append sb c)))
    (java.util.regex.Pattern/compile (.toString sb))))

(defn- parse-find-expr
  "Split argv (after positional paths) into a vector of predicate
   maps. Recognises -name GLOB, -type c, -maxdepth N, -mindepth N,
   -print, -exec CMD … ;. Returns {:paths [..] :preds [..] :exec
   [argv-prefix-with-{}] :err nil|str}."
  [args]
  (let [paths (transient [])
        preds (transient [])
        exec  (atom nil)
        err   (atom nil)
        scan  (volatile! args)]
    (loop []
      (when (and (seq @scan) (nil? @err))
        (let [a (first @scan)
              tail (rest @scan)]
          (cond
            (= "-name" a)
            (if (seq tail)
              (do (conj! preds {:kind :name :pat (first tail)})
                  (vreset! scan (rest tail))
                  (recur))
              (reset! err "find: -name requires an argument"))

            (= "-type" a)
            (if (seq tail)
              (do (conj! preds {:kind :type :t (first tail)})
                  (vreset! scan (rest tail))
                  (recur))
              (reset! err "find: -type requires an argument"))

            (= "-maxdepth" a)
            (if (seq tail)
              (do (conj! preds {:kind :maxdepth :n (Long/parseLong (first tail))})
                  (vreset! scan (rest tail))
                  (recur))
              (reset! err "find: -maxdepth requires an argument"))

            (= "-print" a)
            (do (vreset! scan tail) (recur))

            (= "-exec" a)
            (let [;; Collect args until literal `;`
                  collected (transient [])]
              (loop [t tail]
                (cond
                  (empty? t) (reset! err "find: -exec requires terminating ';'")
                  (= ";" (first t))
                  (do (reset! exec (persistent! collected))
                      (vreset! scan (rest t)))
                  :else
                  (do (conj! collected (first t)) (recur (rest t)))))
              (when (nil? @err) (recur)))

            (str/starts-with? a "-")
            (reset! err (str "find: unsupported predicate " a))

            :else
            (do (conj! paths a)
                (vreset! scan tail)
                (recur))))))
    {:paths (persistent! paths)
     :preds (persistent! preds)
     :exec  @exec
     :err   @err}))

(defn- find-walk
  "BFS walk under `root`, depth-aware. Yields {:path :type :depth}."
  [fs root]
  (let [stat (fs/stat fs root)]
    (cond
      (nil? stat) []
      (not= :dir (:type stat)) [{:path root :type (:type stat) :depth 0}]
      :else
      (loop [pending (conj clojure.lang.PersistentQueue/EMPTY {:path root :depth 0})
             acc     [{:path root :type :dir :depth 0}]]
        (if-let [{:keys [path depth]} (peek pending)]
          (let [pending  (pop pending)
                children (or (fs/list-dir fs path) [])
                child-recs (mapv (fn [c]
                                   {:path (str path "/" (:name c))
                                    :type (:type c)
                                    :depth (inc depth)})
                                 children)
                next-pending (reduce (fn [q r]
                                       (if (= :dir (:type r))
                                         (conj q {:path (:path r) :depth (:depth r)})
                                         q))
                                     pending
                                     child-recs)]
            (recur next-pending (into acc child-recs)))
          acc)))))

(defn- pred-match? [{:keys [kind pat t n]} entry]
  (case kind
    :name (let [re (glob->re pat)
                base (last (str/split (:path entry) #"/"))]
            (.matches (.matcher re (or base ""))))
    :type (= (:type entry)
             (case t "f" :file "d" :dir "l" :symlink (keyword t)))
    :maxdepth (<= (:depth entry) n)
    true))

(defn find-fn
  "POSIX-ish find. Supports paths + -name GLOB, -type {f,d,l},
   -maxdepth N, -print, -exec CMD [args...] {} \\;.

   -exec dispatches CMD through *host* — the same builtins + allowlist
   gates apply to whatever -exec invokes, so it can't escape to system
   binaries. {} is substituted with the file path."
  [argv _fs _env]
  (let [args (rest argv)
        {:keys [paths preds exec err]} (parse-find-expr args)]
    (cond
      err (usage-err "find" err)
      (and exec (nil? *host*)) (err "find: -exec needs a host" 1)
      (>= *depth* max-shell-depth)
      (err "find: too many nested -exec invocations" 2)
      :else
      (let [fs-handle  (or (:fs *host*)
                           (throw (ex-info "find: no fs in host" {})))
            roots      (if (seq paths) paths ["."])
            stdout     (volatile! [])
            stderr     (volatile! "")
            any-err?   (volatile! false)
            entries    (mapcat (fn [r] (find-walk fs-handle r)) roots)
            matched    (filter (fn [e] (every? #(pred-match? % e) preds)) entries)]
        (doseq [{:keys [path]} matched]
          (if exec
            (let [substituted (mapv (fn [a] (if (= "{}" a) path a)) exec)
                  out-sink (host/-string-sink *host*)
                  err-sink (host/-string-sink *host*)
                  proc (binding [*depth* (inc *depth*)]
                         (host/-spawn *host*
                                      {:cmd (first substituted)
                                       :args (vec (rest substituted))
                                       :dir (fs/cwd fs-handle)
                                       :session *session*
                                       :out out-sink
                                       :err err-sink}))
                  exit ((:wait proc))
                  out-s (host/-sink->string *host* out-sink)
                  err-s (host/-sink->string *host* err-sink)]
              (when (seq out-s) (vswap! stdout conj (str/trim-newline out-s)))
              (when (seq err-s) (vswap! stderr str err-s))
              (when (not (zero? exit)) (vreset! any-err? true)))
            (vswap! stdout conj path)))
        {:stdout (str (str/join "\n" @stdout) (when (seq @stdout) "\n"))
         :stderr @stderr
         :exit (if @any-err? 1 0)}))))

;; ============================================================================
;; tr — character-level transliteration
;; ============================================================================

(def ^:private tr-spec
  [["-d" "--delete"]
   ["-s" "--squeeze-repeats"]
   ["-c" "--complement"]])

(defn- expand-set
  "Expand a tr SET string: supports `a-z` ranges and literal chars.
   Does NOT support character classes like [:alpha:] in v1."
  [s]
  (let [sb (StringBuilder.)
        cs (vec s)
        n  (count cs)]
    (loop [i 0]
      (when (< i n)
        (let [c (cs i)]
          (cond
            (and (< (+ i 2) n) (= \- (cs (inc i))))
            (let [start (int c) end (int (cs (+ i 2)))]
              (doseq [k (range start (inc end))] (.append sb (char k)))
              (recur (+ i 3)))
            :else
            (do (.append sb c) (recur (inc i)))))))
    (.toString sb)))

(defn tr
  "POSIX tr, subset: SET1 SET2 transliteration; -d delete SET1;
   -s squeeze runs in SET1 (or SET2 in non-delete mode); -c
   complement SET1. Reads stdin only."
  [argv _fs env]
  (let [{:keys [opts pos err]} (cli-parse argv tr-spec)]
    (cond
      err (usage-err "tr" err)
      (empty? pos) (usage-err "tr" "missing operand")
      :else
      (let [set1-raw (first pos)
            set2-raw (second pos)
            set1     (expand-set set1-raw)
            set2     (when set2-raw (expand-set set2-raw))
            input    (or (:stdin env) "")
            in-set1? (if (:complement opts)
                       (fn [c] (not (str/includes? set1 (str c))))
                       (fn [c] (str/includes? set1 (str c))))
            ;; Mapping table for translate mode
            xlate (when (and set2 (not (:delete opts)))
                    (let [n2 (count set2)]
                      (persistent!
                       (reduce (fn [m i]
                                 (assoc! m (nth set1 i) (nth set2 (min i (dec n2)))))
                               (transient {})
                               (range (count set1))))))
            stage1
            (cond
              (:delete opts)
              (apply str (remove in-set1? input))
              xlate
              (apply str (map (fn [c] (if (in-set1? c) (get xlate c c) c)) input))
              :else
              input)
            squeeze-set (when (:squeeze-repeats opts)
                          (or set2 set1))
            stage2
            (if squeeze-set
              (let [in-sq? (fn [c] (str/includes? squeeze-set (str c)))]
                (apply str
                       (reduce (fn [acc c]
                                 (let [prev (peek acc)]
                                   (if (and prev (= prev c) (in-sq? c))
                                     acc
                                     (conj acc c))))
                               []
                               stage1)))
              stage1)]
        (ok stage2)))))

;; ============================================================================
;; cut — column extraction
;; ============================================================================

(def ^:private cut-spec
  [["-d" "--delimiter D" :default "\t"]
   ["-f" "--fields F"]
   ["-c" "--characters R"]
   ["-s" "--only-delimited"]])

(defn- parse-range-spec
  "Parse a comma-separated range list like `1,3-5,7-` into a fn
   `(in? n)` for 1-based indexing. `7-` means open-ended."
  [s]
  (let [parts (str/split s #",")
        ranges (mapv
                (fn [p]
                  (cond
                    (re-matches #"\d+" p)
                    (let [n (Long/parseLong p)] [n n])
                    (re-matches #"\d+-\d+" p)
                    (let [[a b] (str/split p #"-")]
                      [(Long/parseLong a) (Long/parseLong b)])
                    (re-matches #"\d+-" p)
                    [(Long/parseLong (subs p 0 (dec (count p)))) Long/MAX_VALUE]
                    (re-matches #"-\d+" p)
                    [1 (Long/parseLong (subs p 1))]
                    :else
                    (throw (ex-info "bad range" {:part p}))))
                parts)]
    (fn [n] (some (fn [[a b]] (<= a n b)) ranges))))

(defn cut
  "POSIX cut, subset: -d D -f F (fields) | -c R (chars) | -s.
   Reads stdin only."
  [argv fs env]
  (let [{:keys [opts pos err]} (cli-parse argv cut-spec)]
    (cond
      err (usage-err "cut" err)
      (and (nil? (:fields opts)) (nil? (:characters opts)))
      (usage-err "cut" "you must specify a list of bytes, characters, or fields")
      :else
      (try
        (let [delim (:delimiter opts)
              [content err? stderr] (gather-input pos fs env "cut")
              in? (parse-range-spec (or (:fields opts) (:characters opts)))
              lines (str/split-lines content)
              out
              (mapv
               (fn [line]
                 (cond
                   (:characters opts)
                   (apply str (keep-indexed (fn [i ch] (when (in? (inc i)) ch)) line))
                   (:only-delimited opts)
                   (if-not (str/includes? line delim)
                     nil
                     (let [parts (str/split line (re-pattern (java.util.regex.Pattern/quote delim)))]
                       (->> parts
                            (keep-indexed (fn [i s] (when (in? (inc i)) s)))
                            (str/join delim))))
                   :else
                   (let [parts (str/split line (re-pattern (java.util.regex.Pattern/quote delim)))]
                     (->> parts
                          (keep-indexed (fn [i s] (when (in? (inc i)) s)))
                          (str/join delim)))))
               lines)
              kept (remove nil? out)]
          {:stdout (str (str/join "\n" kept) (when (seq kept) "\n"))
           :stderr stderr
           :exit (if err? 1 0)})
        (catch Throwable t
          (usage-err "cut" (.getMessage t)))))))

;; ============================================================================
;; diff — line-level edit script via diffit, rendered as unified diff
;; ============================================================================

(def ^:private diff-spec
  [["-u" "--unified"]
   ["-q" "--brief"]
   ["-i" "--ignore-case"]
   ["-w" "--ignore-all-space"]])

(defn- normalise-line [opts line]
  (cond-> line
    (:ignore-case opts)       str/lower-case
    (:ignore-all-space opts)  (str/replace #"\s+" "")))

(defn- render-unified [a-name b-name a-lines b-lines script]
  "Render a diffit edit-script as a minimal unified-diff. We don't
   coalesce hunks — each op becomes its own line. Good enough for
   eyeballing and for tests."
  (let [header (str "--- " a-name "\n+++ " b-name "\n")
        body (str/join
              "\n"
              (for [op script]
                (case (first op)
                  :- (let [[_ i n] op]
                       (str/join "\n"
                                 (for [k (range n)]
                                   (str "-" (nth a-lines (+ i k))))))
                  :+ (let [[_ _ items] op]
                       (str/join "\n"
                                 (for [it items]
                                   (str "+" it)))))))]
    (str header body (when (seq body) "\n"))))

(defn diff
  "POSIX diff, subset: -u (unified), -q (brief), -i (case),
   -w (ignore whitespace). Exit 0 = same, 1 = different, 2 = error."
  [argv fs _env]
  (let [{:keys [opts pos err]} (cli-parse argv diff-spec)]
    (cond
      err (usage-err "diff" err)
      (not= 2 (count pos))
      (usage-err "diff" "two files required")
      :else
      (let [[fa fb] pos
            ca (fs/read-file fs fa)
            cb (fs/read-file fs fb)]
        (cond
          (nil? ca) (err (str "diff: " fa ": No such file or directory") 2)
          (nil? cb) (err (str "diff: " fb ": No such file or directory") 2)
          :else
          (let [la (mapv (partial normalise-line opts) (str/split-lines ca))
                lb (mapv (partial normalise-line opts) (str/split-lines cb))
                [dist script] (dv/diff la lb)]
            (cond
              (zero? dist) {:stdout "" :stderr "" :exit 0}
              (:brief opts)
              {:stdout (str "Files " fa " and " fb " differ\n")
               :stderr "" :exit 1}
              :else
              {:stdout (render-unified fa fb la lb script)
               :stderr "" :exit 1})))))))

;; ============================================================================
;; xargs — read stdin, dispatch CMD with substituted args via host
;; ============================================================================

(def ^:private xargs-spec
  [["-0" "--null"]
   ["-n" "--max-args N" :parse-fn #(Long/parseLong %)]
   ["-I" "--replace R"]
   ["-d" "--delimiter D"]])

(defn xargs
  "POSIX xargs, subset: -0 NUL-separated, -n N args per call,
   -I R per-call substitution, -d D explicit delimiter.

   Dispatches CMD through *host* — same gates apply, so xargs
   can't escape the builtin/allowlist set."
  [argv _fs env]
  (let [{:keys [opts pos err]} (cli-parse argv xargs-spec)]
    (cond
      err (usage-err "xargs" err)
      (nil? *host*) (err "xargs: no host available for dispatch" 1)
      (>= *depth* max-shell-depth)
      (err "xargs: too many nested invocations" 2)
      :else
      (let [stdin    (or (:stdin env) "")
            sep      (cond
                       (:null opts) "\0"
                       (:delimiter opts) (:delimiter opts)
                       :else nil)
            tokens   (->> (if sep
                            (str/split stdin (re-pattern (java.util.regex.Pattern/quote sep)))
                            (str/split stdin #"\s+"))
                          (remove str/blank?))
            cmd-argv (if (seq pos) pos ["echo"])
            cmd      (first cmd-argv)
            base     (vec (rest cmd-argv))
            replace  (:replace opts)
            max-n    (:max-args opts)
            stdout   (volatile! [])
            stderr   (volatile! "")
            any-err? (volatile! false)
            batches  (cond
                       replace (mapv vector tokens)
                       max-n   (vec (partition-all max-n tokens))
                       :else   [tokens])]
        (doseq [batch batches]
          (when-not @any-err?
            (let [args (if replace
                         (mapv (fn [a]
                                 (if (= replace a)
                                   (first batch)
                                   (str/replace a (re-pattern (java.util.regex.Pattern/quote replace))
                                                (first batch))))
                               base)
                         (into base batch))
                  out-sink (host/-string-sink *host*)
                  err-sink (host/-string-sink *host*)
                  proc (binding [*depth* (inc *depth*)]
                         (host/-spawn *host*
                                      {:cmd cmd
                                       :args args
                                       :session *session*
                                       :out out-sink
                                       :err err-sink}))
                  exit ((:wait proc))
                  out-s (host/-sink->string *host* out-sink)
                  err-s (host/-sink->string *host* err-sink)]
              (when (seq out-s) (vswap! stdout conj (str/trim-newline out-s)))
              (when (seq err-s) (vswap! stderr str err-s))
              (when (not (zero? exit)) (vreset! any-err? true)))))
        {:stdout (str (str/join "\n" @stdout) (when (seq @stdout) "\n"))
         :stderr @stderr
         :exit (if @any-err? 1 0)}))))

;; ============================================================================
;; Registry — the canonical builtin map
;; ============================================================================

(def standard-read-only
  "All read-only builtins shipping in v1. Suitable as :builtins for
   muschel.host.builtin/make."
  {"pwd"   pwd
   "echo"  echo
   "ls"    ls
   "cat"   cat
   "head"  head
   "tail"  tail
   "wc"    wc
   "stat"  stat
   "which" which
   "sort"  sort-fn
   "uniq"  uniq
   "grep"  grep
   "find"  find-fn
   "tr"    tr
   "cut"   cut
   "diff"  diff
   "xargs" xargs
   "sh"    sh
   "bash"  sh
   "dash"  sh})
