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
   -s (squeeze blank lines), -E (show $ at line end). Preserves the
   byte-level trailing-newline property of the source content."
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
            out         (StringBuilder.)
            n           (volatile! 0)
            last-blank? (volatile! false)
            emit-line!
            (fn [line trailing-nl?]
              (let [is-blank? (= "" line)
                    skip?     (and squeeze? @last-blank? is-blank?)]
                (when-not skip?
                  (vreset! last-blank? is-blank?)
                  (let [shown   (if show-end? (str line "$") line)
                        labeled (cond
                                  num-nb?
                                  (if is-blank? shown
                                      (do (vswap! n inc)
                                          (format "%6d\t%s" @n shown)))
                                  num-all?
                                  (do (vswap! n inc)
                                      (format "%6d\t%s" @n shown))
                                  :else shown)]
                    (.append out ^String labeled)
                    (when trailing-nl? (.append out "\n"))))))
            process-content
            (fn [content]
              (let [lines     (str/split-lines content)
                    last-idx  (dec (count lines))
                    has-trailing-nl? (and (pos? (count content))
                                          (= \newline (.charAt ^String content
                                                               (dec (count content)))))]
                (dotimes [i (count lines)]
                  (emit-line! (nth lines i)
                              (or (< i last-idx) has-trailing-nl?)))))]
        (doseq [f files]
          (if (= "-" f)
            (process-content stdin)
            (let [bytes (fs/read-bytes fs f)]
              (if (nil? bytes)
                (let [stat (fs/stat fs f)
                      msg  (if (= :dir (:type stat))
                             (str "cat: " f ": Is a directory")
                             (str "cat: " f ": No such file or directory"))]
                  (vswap! stderr str msg "\n")
                  (vreset! any-err? true))
                (process-content (if (string? bytes)
                                   bytes
                                   (String. ^bytes bytes "UTF-8")))))))
        {:stdout (.toString out)
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

(defn- finish-lines
  "Take a sequence of line strings; emit them joined by `\\n`, plus a
   trailing `\\n` iff there were any lines. Empty seq → empty string."
  [lines]
  (if (seq lines)
    (str (str/join "\n" lines) "\n")
    ""))

(defn- header-line
  "GNU-style multi-file header: `==> name <==`."
  [name]
  (str "==> " name " <=="))

(defn head
  "POSIX head. -n N (or -N) lines, default 10. With multiple files,
   each block is preceded by `==> name <==` (use -q to suppress)."
  [argv fs env]
  (let [{:keys [opts pos err]} (cli-parse (expand-numeric-n argv)
                                          (conj head-tail-spec
                                                ["-q" "--quiet"]
                                                ["-v" "--verbose"]))]
    (if err
      (usage-err "head" err)
      (let [n        (:lines opts)
            stdin    (or (:stdin env) "")
            files    (if (seq pos) pos ["-"])
            multi?   (> (count files) 1)
            show-h?  (or (:verbose opts)
                         (and multi? (not (:quiet opts))))
            stderr   (volatile! "")
            any-err? (volatile! false)
            blocks
            (keep
             (fn [[idx f]]
               (let [content (if (= "-" f) stdin (fs/read-file fs f))]
                 (if (nil? content)
                   (do (vswap! stderr str "head: cannot open '" f "' for reading: No such file or directory\n")
                       (vreset! any-err? true)
                       nil)
                   (let [head-text (->> (str/split-lines content)
                                        (take n)
                                        finish-lines)]
                     (if show-h?
                       (str (when (pos? idx) "\n")
                            (header-line (if (= "-" f) "standard input" f))
                            "\n"
                            head-text)
                       head-text)))))
             (map-indexed vector files))]
        {:stdout (apply str blocks)
         :stderr @stderr
         :exit (if @any-err? 1 0)}))))

(defn tail
  "POSIX tail. -n N (or -N), default 10. No -f. Multi-file headers
   like head."
  [argv fs env]
  (let [{:keys [opts pos err]} (cli-parse (expand-numeric-n argv)
                                          (conj head-tail-spec
                                                ["-q" "--quiet"]
                                                ["-v" "--verbose"]))]
    (if err
      (usage-err "tail" err)
      (let [n        (:lines opts)
            stdin    (or (:stdin env) "")
            files    (if (seq pos) pos ["-"])
            multi?   (> (count files) 1)
            show-h?  (or (:verbose opts)
                         (and multi? (not (:quiet opts))))
            stderr   (volatile! "")
            any-err? (volatile! false)
            blocks
            (keep
             (fn [[idx f]]
               (let [content (if (= "-" f) stdin (fs/read-file fs f))]
                 (if (nil? content)
                   (do (vswap! stderr str "tail: cannot open '" f "' for reading: No such file or directory\n")
                       (vreset! any-err? true)
                       nil)
                   (let [tail-text (->> (str/split-lines content)
                                        (take-last n)
                                        finish-lines)]
                     (if show-h?
                       (str (when (pos? idx) "\n")
                            (header-line (if (= "-" f) "standard input" f))
                            "\n"
                            tail-text)
                       tail-text)))))
             (map-indexed vector files))]
        {:stdout (apply str blocks)
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

(defn- count-stats
  "Count lines (newline occurrences — GNU semantics, so trailing-nl-less
   content yields one fewer line than chunks), words (whitespace-separated
   runs), bytes (UTF-8), and chars (codepoints / String length)."
  [^String s]
  (let [byte-count (count (.getBytes s "UTF-8"))
        word-count (count (re-seq #"\S+" s))
        line-count (count (filter #(= \newline %) s))
        char-count (.length s)]
    {:lines line-count :words word-count :bytes byte-count :chars char-count}))

(defn wc
  "POSIX wc. -l (lines = `\\n` occurrences), -w (words), -c (bytes),
   -m (chars). Default when no flags: -l -w -c. With multiple files,
   prints a final 'total' row."
  [argv fs env]
  (let [{:keys [opts pos err]} (cli-parse argv wc-spec)]
    (if err
      (usage-err "wc" err)
      (let [explicit? (some opts [:lines :words :bytes :chars])
            show-l?   (or (not explicit?) (boolean (:lines opts)))
            show-w?   (or (not explicit?) (boolean (:words opts)))
            show-c?   (or (not explicit?) (boolean (:bytes opts)))
            show-m?   (boolean (:chars opts))
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
            fmt-row (fn [{:keys [lines words bytes chars name]}]
                      (str/trim
                       (str (when show-l? (format "%8d " lines))
                            (when show-w? (format "%8d " words))
                            (when show-c? (format "%8d " bytes))
                            (when show-m? (format "%8d " chars))
                            (when (and name (not= "-" name)) name))))
            total (when (> (count rows) 1)
                    (reduce (fn [acc r]
                              (-> acc
                                  (update :lines + (:lines r))
                                  (update :words + (:words r))
                                  (update :bytes + (:bytes r))
                                  (update :chars + (:chars r))))
                            {:lines 0 :words 0 :bytes 0 :chars 0 :name "total"}
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
   ["-q" "--quiet"]
   ["-w" "--word-regexp"]
   ["-e" "--regexp PATTERN"
    :assoc-fn (fn [m k v] (update m k (fnil conj []) v))
    :default  []]])

(defn- compile-pattern
  "Build a java.util.regex.Pattern from a single grep pattern,
   honouring -F (literal), -i (case-insensitive), and -w (word-bound).
   Java's regex engine is ERE-ish, which matches -E and what most
   modern users expect by default."
  [pattern {:keys [fixed-strings ignore-case word-regexp]}]
  (let [body  (if fixed-strings (java.util.regex.Pattern/quote pattern) pattern)
        body  (if word-regexp (str "\\b(?:" body ")\\b") body)
        flags (cond-> 0
                ignore-case (bit-or java.util.regex.Pattern/CASE_INSENSITIVE))]
    (java.util.regex.Pattern/compile body flags)))

(defn- collect-patterns
  "Combine -e PATTERN (possibly repeated) with the positional pattern.
   Returns [patterns positional-files]."
  [{:keys [regexp]} pos]
  (let [e-patterns (or (seq regexp) nil)]
    (if e-patterns
      [(vec e-patterns) (vec pos)]
      (if (seq pos)
        [[(first pos)] (vec (rest pos))]
        [nil []]))))

(defn- any-pattern-matches?
  "True if any compiled pattern matches the line."
  [^java.util.regex.Pattern compiled-or-coll line]
  (if (coll? compiled-or-coll)
    (some #(.find (.matcher ^java.util.regex.Pattern % ^String line)) compiled-or-coll)
    (.find (.matcher compiled-or-coll ^String line))))

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
  "POSIX grep, subset: -E -F -i -n -v -c -l -L -H -h -r -q -w -e.
   With no files and stdin available in env, reads stdin. Exit 0 if
   any match, 1 if no match, 2 on usage error."
  [argv fs env]
  (let [{:keys [opts pos err]} (cli-parse argv grep-spec)]
    (cond
      err (usage-err "grep" err)
      :else
      (let [[patterns files] (collect-patterns opts pos)]
        (cond
          (empty? patterns) (usage-err "grep" "missing pattern")
          :else
          (let [compiled (mapv #(compile-pattern % opts) patterns)
                stdin-mode? (and (empty? files) (contains? env :stdin))
                targets (cond
                          stdin-mode?
                          [{:path "(standard input)" :show-name? false}]
                          (:recursive opts)
                          (mapcat (fn [t] (walk-files fs t)) (or (seq files) ["."]))
                          :else
                          (mapv (fn [f] {:path f}) (or (seq files) ["-"])))
                multi?  (> (count targets) 1)
                show-name? (or (:with-filename opts)
                               (and multi? (not (:no-filename opts))
                                    (not stdin-mode?)))
                stderr     (volatile! "")
                any-err?   (volatile! false)
                any-match? (volatile! false)
                stdout     (volatile! [])
                invert?    (:invert-match opts)
                matches?
                (fn [line]
                  (let [m (any-pattern-matches? compiled line)]
                    (if invert? (not m) (boolean m))))]
            (doseq [{:keys [path] :as target} targets]
              (let [content
                    (cond
                      stdin-mode? (:stdin env)
                      (= "-" path) (or (:stdin env) "")
                      :else
                      (let [c (fs/read-file fs path)]
                        (when (nil? c)
                          (vswap! stderr str "grep: " path ": No such file or directory\n")
                          (vreset! any-err? true))
                        c))]
                (when content
                  (let [lines (str/split-lines content)
                        hits  (keep-indexed
                               (fn [i ln] (when (matches? ln) {:i (inc i) :line ln}))
                               lines)
                        ;; In stdin mode -l/-L print "(standard input)";
                        ;; with explicit "-" file, ditto.
                        display-path (if stdin-mode? "(standard input)" path)]
                    (cond
                      (:files-with-matches opts)
                      (when (seq hits)
                        (vreset! any-match? true)
                        (vswap! stdout conj display-path))

                      (:files-without-match opts)
                      (when (empty? hits)
                        (vswap! stdout conj display-path))

                      (:count opts)
                      (let [n (count hits)]
                        (when (pos? n) (vreset! any-match? true))
                        (vswap! stdout conj
                                (if show-name? (str display-path ":" n) (str n))))

                      :else
                      (doseq [{:keys [i line]} hits]
                        (vreset! any-match? true)
                        (let [pieces (cond-> []
                                       show-name? (conj display-path)
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
                       ;; -L: success when SOME file had no matches.
                       (:files-without-match opts)
                       (if (seq @stdout) 0 1)
                       @any-match? 0
                       :else 1)})))))))

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

;; -- find expression parser -------------------------------------------------
;;
;; Grammar (recursive descent, POSIX precedence — `()` > `-not`/`!` > `-a`
;; (implicit) > `-o`):
;;
;;   top   := PATH* expr?
;;   expr  := and (-o and)*
;;   and   := not (-a? not)*       ; -a optional / implicit
;;   not   := (-not | !) not | atom
;;   atom  := '(' expr ')' | pred
;;   pred  := -name GLOB | -iname GLOB | -type T | -maxdepth N | -mindepth N
;;          | -print | -exec ARGS ; | -exec ARGS +
;;
;; Tree shape:
;;   {:kind :pred ...}            — predicate
;;   {:kind :action :action ...}  — side-effecting action (-print, -exec)
;;   {:kind :and :a t :b t}
;;   {:kind :or  :a t :b t}
;;   {:kind :not :inner t}
;;   nil                          — empty expression (matches everything,
;;                                  default action `-print`)

(defn- find-pred?
  "True if token introduces a predicate/action (starts with `-` or is
   one of the bool/group tokens)."
  [t]
  (or (#{"(" ")" "!" "-not" "-a" "-and" "-o" "-or"} t)
      (and (string? t) (str/starts-with? t "-"))))

(declare find-parse-or)

(defn- find-parse-pred
  "Consume one predicate or action from `tokens` at `pos`. Returns
   [node new-pos err]."
  [tokens pos]
  (let [t (nth tokens pos nil)
        nxt (fn [] (nth tokens (inc pos) nil))]
    (case t
      "-name"     (if-let [g (nxt)]
                    [{:kind :pred :pred :name :pat g :ci? false} (+ pos 2) nil]
                    [nil pos "find: -name requires an argument"])
      "-iname"    (if-let [g (nxt)]
                    [{:kind :pred :pred :name :pat g :ci? true} (+ pos 2) nil]
                    [nil pos "find: -iname requires an argument"])
      "-type"     (if-let [v (nxt)]
                    [{:kind :pred :pred :type :t v} (+ pos 2) nil]
                    [nil pos "find: -type requires an argument"])
      "-maxdepth" (if-let [v (nxt)]
                    [{:kind :pred :pred :maxdepth :n (Long/parseLong v)} (+ pos 2) nil]
                    [nil pos "find: -maxdepth requires an argument"])
      "-mindepth" (if-let [v (nxt)]
                    [{:kind :pred :pred :mindepth :n (Long/parseLong v)} (+ pos 2) nil]
                    [nil pos "find: -mindepth requires an argument"])
      "-print"    [{:kind :action :action :print} (inc pos) nil]
      "-exec"     (loop [p (inc pos) collected (transient [])]
                    (let [a (nth tokens p nil)]
                      (cond
                        (nil? a)
                        [nil p "find: -exec requires terminating ';' or '+'"]
                        (or (= ";" a) (= "+" a))
                        [{:kind :action :action :exec
                          :argv (persistent! collected)
                          :batch? (= "+" a)}
                         (inc p) nil]
                        :else
                        (recur (inc p) (conj! collected a)))))
      [nil pos (str "find: unsupported predicate '" t "'")])))

(defn- find-parse-atom [tokens pos]
  (let [t (nth tokens pos nil)]
    (cond
      (= "(" t)
      (let [[inner p' err] (find-parse-or tokens (inc pos))]
        (cond
          err [nil p' err]
          (not= ")" (nth tokens p' nil))
          [nil p' "find: missing ')'"]
          :else [inner (inc p') nil]))
      :else
      (find-parse-pred tokens pos))))

(defn- find-parse-not [tokens pos]
  (if (#{"-not" "!"} (nth tokens pos nil))
    (let [[inner p' err] (find-parse-not tokens (inc pos))]
      (if err [nil p' err]
          [{:kind :not :inner inner} p' nil]))
    (find-parse-atom tokens pos)))

(defn- find-parse-and [tokens pos]
  (loop [[left p err] (find-parse-not tokens pos)]
    (if err
      [nil p err]
      (let [t (nth tokens p nil)]
        (cond
          (#{"-a" "-and"} t)
          (let [[right p' err'] (find-parse-not tokens (inc p))]
            (if err' [nil p' err']
                (recur [{:kind :and :a left :b right} p' nil])))
          (and (some? t) (not (#{"-o" "-or" ")"} t)))
          ;; Implicit AND when another predicate follows.
          (let [[right p' err'] (find-parse-not tokens p)]
            (if err' [nil p' err']
                (recur [{:kind :and :a left :b right} p' nil])))
          :else
          [left p nil])))))

(defn- find-parse-or [tokens pos]
  (loop [[left p err] (find-parse-and tokens pos)]
    (if err
      [nil p err]
      (let [t (nth tokens p nil)]
        (if (#{"-o" "-or"} t)
          (let [[right p' err'] (find-parse-and tokens (inc p))]
            (if err' [nil p' err']
                (recur [{:kind :or :a left :b right} p' nil])))
          [left p nil])))))

(defn- parse-find-args
  "Top-level: split leading paths from the expression tokens; parse
   the expression. Returns {:paths [..] :tree expr-tree :err nil|str}."
  [args]
  (let [[paths exprs] (split-with #(not (find-pred? %)) args)]
    (if (empty? exprs)
      {:paths (vec paths) :tree nil :err nil}
      (let [tokens (vec exprs)
            [tree pos err] (find-parse-or tokens 0)]
        (cond
          err
          {:paths (vec paths) :tree nil :err err}
          (< pos (count tokens))
          {:paths (vec paths) :tree nil
           :err (str "find: stray token '" (nth tokens pos) "'")}
          :else
          {:paths (vec paths) :tree tree :err nil})))))

(defn- tree-has-action?
  "Walk the parsed expr; true if any node is an action (`:print`,
   `:exec`). Used to decide whether to inject an implicit `-print`."
  [tree]
  (cond
    (nil? tree) false
    (= :action (:kind tree)) true
    (#{:and :or} (:kind tree)) (or (tree-has-action? (:a tree))
                                   (tree-has-action? (:b tree)))
    (= :not (:kind tree)) (tree-has-action? (:inner tree))
    :else false))

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

(defn- pred-match? [{:keys [pred pat ci? t n]} entry]
  (case pred
    :name (let [pat'  (if ci? (str/lower-case pat) pat)
                re    (glob->re pat')
                base  (or (last (str/split (:path entry) #"/")) "")
                base' (if ci? (str/lower-case base) base)]
            (.matches (.matcher re base')))
    :type (= (:type entry)
             (case t "f" :file "d" :dir "l" :symlink (keyword t)))
    :maxdepth (<= (:depth entry) n)
    :mindepth (>= (:depth entry) n)
    false))

(defn- spawn-exec-action!
  "Run an -exec action (single-mode `\\;`) against `path` via *host*.
   Appends output to the stdout-sb / stderr-sb StringBuilders. Returns
   the exit code."
  [{:keys [argv]} path fs-handle stdout-sb stderr-sb]
  (let [substituted (mapv (fn [a] (str/replace a "{}" path)) argv)
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
    (.append stdout-sb ^String out-s)
    (.append stderr-sb ^String err-s)
    exit))

(defn- spawn-exec-batch!
  "Run an -exec action (`+` batched mode): substitute paths in for the
   sole `{}` arg and pass *all* matching paths in one invocation."
  [{:keys [argv]} paths fs-handle stdout-sb stderr-sb]
  (let [;; The {} placeholder becomes all the paths concatenated; in
        ;; GNU `find -exec ... {} +` semantics they're individual argv
        ;; elements, not one joined string.
        substituted (mapcat (fn [a]
                              (if (= "{}" a) paths [a]))
                            argv)
        argv-vec    (vec substituted)
        out-sink (host/-string-sink *host*)
        err-sink (host/-string-sink *host*)
        proc (binding [*depth* (inc *depth*)]
               (host/-spawn *host*
                            {:cmd (first argv-vec)
                             :args (vec (rest argv-vec))
                             :dir (fs/cwd fs-handle)
                             :session *session*
                             :out out-sink
                             :err err-sink}))
        exit ((:wait proc))
        out-s (host/-sink->string *host* out-sink)
        err-s (host/-sink->string *host* err-sink)]
    (.append stdout-sb ^String out-s)
    (.append stderr-sb ^String err-s)
    exit))

(defn- eval-find-expr
  "Walk the expression tree against `entry`. Actions have side-effects
   on the captured state (stdout-sb, stderr-sb, any-err? volatile,
   batch-paths volatile for `-exec ... +`). Returns boolean — actions
   always return true so they don't short-circuit downstream `-and`."
  [tree entry {:keys [fs-handle stdout-sb stderr-sb any-err? batch-paths]}]
  (cond
    (nil? tree) true

    (= :pred (:kind tree))
    (pred-match? tree entry)

    (= :and (:kind tree))
    (let [l (eval-find-expr (:a tree) entry
                            {:fs-handle fs-handle :stdout-sb stdout-sb
                             :stderr-sb stderr-sb :any-err? any-err?
                             :batch-paths batch-paths})]
      (if l
        (eval-find-expr (:b tree) entry
                        {:fs-handle fs-handle :stdout-sb stdout-sb
                         :stderr-sb stderr-sb :any-err? any-err?
                         :batch-paths batch-paths})
        false))

    (= :or (:kind tree))
    (or (eval-find-expr (:a tree) entry
                        {:fs-handle fs-handle :stdout-sb stdout-sb
                         :stderr-sb stderr-sb :any-err? any-err?
                         :batch-paths batch-paths})
        (eval-find-expr (:b tree) entry
                        {:fs-handle fs-handle :stdout-sb stdout-sb
                         :stderr-sb stderr-sb :any-err? any-err?
                         :batch-paths batch-paths}))

    (= :not (:kind tree))
    (not (eval-find-expr (:inner tree) entry
                         {:fs-handle fs-handle :stdout-sb stdout-sb
                          :stderr-sb stderr-sb :any-err? any-err?
                          :batch-paths batch-paths}))

    (= :action (:kind tree))
    (case (:action tree)
      :print (do (.append stdout-sb (str (:path entry) "\n")) true)
      :exec  (if (:batch? tree)
               ;; Defer to end: accumulate paths under this action's
               ;; argv signature (we only support one batched -exec).
               (do (vswap! batch-paths update :argv (fn [a]
                                                     (or a (:argv tree))))
                   (vswap! batch-paths update :paths (fnil conj []) (:path entry))
                   true)
               (let [exit (spawn-exec-action! tree (:path entry)
                                              fs-handle stdout-sb stderr-sb)]
                 (when (not (zero? exit)) (vreset! any-err? true))
                 (zero? exit))))

    :else false))

(defn find-fn
  "POSIX find. Supports paths + -name/-iname GLOB, -type {f,d,l},
   -maxdepth N, -mindepth N, -print, -exec CMD [args] {} \\;,
   -exec CMD {} +, boolean operators -a (implicit) / -o / -not / !,
   and grouping with `(` `)`. Default action is `-print`.

   -exec dispatches CMD through *host* — same gates apply, can't
   escape the builtin/allowlist set."
  [argv _fs _env]
  (let [{:keys [paths tree err]} (parse-find-args (rest argv))]
    (cond
      err (usage-err "find" err)
      (and (tree-has-action? tree) (nil? *host*))
      (err "find: -exec / -print needs a host" 1)
      (>= *depth* max-shell-depth)
      (err "find: too many nested -exec invocations" 2)
      :else
      (let [fs-handle (or (:fs *host*)
                          (throw (ex-info "find: no fs in host" {})))
            ;; Default action: implicit -print when tree has none.
            tree (if (tree-has-action? tree)
                   tree
                   (let [pr {:kind :action :action :print}]
                     (if (nil? tree) pr
                         {:kind :and :a tree :b pr})))
            roots     (if (seq paths) paths ["."])
            stdout-sb (StringBuilder.)
            stderr-sb (StringBuilder.)
            any-err?  (volatile! false)
            batch-paths (volatile! {})
            entries   (mapcat (fn [r] (find-walk fs-handle r)) roots)]
        (doseq [entry entries]
          (eval-find-expr tree entry
                          {:fs-handle fs-handle
                           :stdout-sb stdout-sb
                           :stderr-sb stderr-sb
                           :any-err? any-err?
                           :batch-paths batch-paths}))
        ;; Flush deferred `-exec ... +` batch.
        (when-let [bp (and (seq (:paths @batch-paths)) @batch-paths)]
          (let [exit (spawn-exec-batch! {:argv (:argv bp)} (:paths bp)
                                        fs-handle stdout-sb stderr-sb)]
            (when (not (zero? exit)) (vreset! any-err? true))))
        {:stdout (.toString stdout-sb)
         :stderr (.toString stderr-sb)
         :exit (if @any-err? 1 0)}))))

;; ============================================================================
;; tr — character-level transliteration
;; ============================================================================

(def ^:private tr-spec
  [["-d" "--delete"]
   ["-s" "--squeeze-repeats"]
   ["-c" "--complement"]])

(def ^:private tr-char-classes
  "POSIX character classes recognised in tr's SETs. Mapped to char
   sequences. `[:upper:]` and `[:lower:]` are the workhorses; we ship
   the common ones."
  {"alpha"  (concat (map char (range (int \a) (inc (int \z))))
                    (map char (range (int \A) (inc (int \Z)))))
   "alnum"  (concat (map char (range (int \a) (inc (int \z))))
                    (map char (range (int \A) (inc (int \Z))))
                    (map char (range (int \0) (inc (int \9)))))
   "digit"  (map char (range (int \0) (inc (int \9))))
   "lower"  (map char (range (int \a) (inc (int \z))))
   "upper"  (map char (range (int \A) (inc (int \Z))))
   "space"  [\space \tab \newline \return \formfeed (char 11)]
   "blank"  [\space \tab]
   "punct"  (seq "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
   "xdigit" (concat (map char (range (int \0) (inc (int \9))))
                    (map char (range (int \a) (inc (int \f))))
                    (map char (range (int \A) (inc (int \F)))))
   "cntrl"  (concat (map char (range 0 32)) [(char 127)])
   "print"  (map char (range 32 127))
   "graph"  (map char (range 33 127))})

(defn- expand-escape
  "Translate the two-char escape starting with `\\` and the next char.
   Returns [char-or-nil consumed-count]; nil if not a known escape."
  [c2]
  (case c2
    \n [\newline 2]
    \t [\tab 2]
    \r [\return 2]
    \\ [\\ 2]
    \/ [\/ 2]
    \a [(char 7) 2]
    \b [\backspace 2]
    \f [\formfeed 2]
    \v [(char 11) 2]
    \0 [(char 0) 2]
    nil))

(defn- expand-set
  "Expand a tr SET string. Supports:
   - `a-z` ranges (start ≤ end ASCII)
   - `\\n \\t \\r \\\\ \\a \\b \\f \\v \\0` C-style escapes
   - `[:upper:] [:lower:] [:digit:] [:alpha:] [:alnum:] [:space:]
      [:blank:] [:punct:] [:xdigit:] [:cntrl:] [:print:] [:graph:]`
      POSIX character classes
   - literal chars otherwise"
  [^String s]
  (let [sb (StringBuilder.)
        n  (.length s)]
    (loop [i 0]
      (when (< i n)
        (let [c (.charAt s i)]
          (cond
            ;; POSIX char class [:name:]
            (and (= \[ c)
                 (< (+ i 1) n)
                 (= \: (.charAt s (inc i))))
            (let [end (.indexOf s ":]" (+ i 2))]
              (if (neg? end)
                (do (.append sb c) (recur (inc i)))
                (let [cls-name (subs s (+ i 2) end)]
                  (if-let [chars (get tr-char-classes cls-name)]
                    (do (doseq [ch chars] (.append sb ^char ch))
                        (recur (+ end 2)))
                    (do (.append sb c) (recur (inc i)))))))

            ;; Escape sequence
            (and (= \\ c) (< (inc i) n))
            (if-let [[ch n-consumed] (expand-escape (.charAt s (inc i)))]
              (do (.append sb ^char ch) (recur (+ i n-consumed)))
              (do (.append sb c) (recur (inc i))))

            ;; Range a-z
            (and (< (+ i 2) n) (= \- (.charAt s (inc i))))
            (let [start (int c) end (int (.charAt s (+ i 2)))]
              (if (<= start end)
                (do (doseq [k (range start (inc end))] (.append sb (char k)))
                    (recur (+ i 3)))
                (do (.append sb c) (recur (inc i)))))

            :else
            (do (.append sb c) (recur (inc i))))))
      )
    (.toString sb)))

(defn tr
  "POSIX tr, subset: SET1 SET2 transliteration; -d delete SET1;
   -s squeeze runs in SET1 (or SET2 in non-delete mode); -c
   complement SET1. Character classes `[:upper:]` `[:lower:]` etc.
   and the common escape sequences are supported. Reads stdin only."
  [argv _fs env]
  (let [{:keys [opts pos err]} (cli-parse argv tr-spec)]
    (cond
      err (usage-err "tr" err)
      (empty? pos) (usage-err "tr" "missing operand")
      (and (:delete opts)
           (not (:squeeze-repeats opts))
           (> (count pos) 1))
      (usage-err "tr" (str "extra operand '" (second pos) "' (only one SET allowed with -d)"))
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

(def ^:private diff-default-context 3)

(defn- normalise-line [opts line]
  (cond-> line
    (:ignore-case opts)       str/lower-case
    (:ignore-all-space opts)  (str/replace #"\s+" "")))

;; ---- LCS-based line diff ---------------------------------------------------
;;
;; We compute the standard O(n·m) LCS DP table, then trace back to emit a
;; sequence of [:keep|:del|:add line] ops in source order. From those we
;; build standard unified-diff hunks (with `@@ -a,A +b,B @@` headers and
;; 3 lines of context around each change run). The output is patch(1) /
;; git apply parseable.
;;
;; n, m are file lengths; our agent-facing files are small enough that
;; O(n·m) memory (an int array of (n+1)·(m+1)) is acceptable.

(defn- lcs-table ^ints [a b]
  (let [n    (count a)
        m    (count b)
        cols (inc m)
        t    (int-array (* (inc n) cols))]
    (dotimes [i n]
      (dotimes [j m]
        (let [idx (+ (* (inc i) cols) (inc j))
              up   (aget t (+ (* i cols) (inc j)))
              left (aget t (+ (* (inc i) cols) j))]
          (if (= (nth a i) (nth b j))
            (aset-int t idx (inc (aget t (+ (* i cols) j))))
            (aset-int t idx (max up left))))))
    t))

(defn- diff-ops
  "Return a vector of [op line] pairs (`:keep`/`:del`/`:add`) tracing
   the LCS table for vectors `a` and `b`. Source order."
  [a b]
  (let [t    (lcs-table a b)
        n    (count a)
        m    (count b)
        cols (inc m)
        get-t (fn [i j] (aget ^ints t (+ (* i cols) j)))
        out  (java.util.ArrayList.)]
    (loop [i n j m]
      (cond
        (and (pos? i) (pos? j) (= (nth a (dec i)) (nth b (dec j))))
        (do (.add out [:keep (nth a (dec i))])
            (recur (dec i) (dec j)))

        (and (pos? j) (or (zero? i) (>= (get-t i (dec j)) (get-t (dec i) j))))
        (do (.add out [:add (nth b (dec j))])
            (recur i (dec j)))

        (pos? i)
        (do (.add out [:del (nth a (dec i))])
            (recur (dec i) j))

        :else nil))
    (vec (reverse out))))

(defn- group-hunks
  "Bundle the linear op stream into unified-diff hunks: runs of
   non-`:keep` ops with up to `ctx` lines of context above and below
   each change run, merging adjacent runs whose context regions
   overlap. Returns a vec of hunks; each hunk has `:a-start` /
   `:a-len` / `:b-start` / `:b-len` (1-indexed, GNU semantics where a
   zero-length range has start 0) and `:lines` (the [op line] pairs)."
  [ops ctx]
  (let [changed? (fn [[op _]] (not= op :keep))
        consumes-a? (fn [[op _]] (or (= op :keep) (= op :del)))
        consumes-b? (fn [[op _]] (or (= op :keep) (= op :add)))
        n (count ops)
        ;; Find every change-region — a contiguous run of :del/:add
        ;; ops. Each region grows to absorb at most `ctx` :keep ops on
        ;; either side. Overlapping windows merge.
        regions
        (loop [i 0 acc (transient [])]
          (cond
            (>= i n) (persistent! acc)
            (changed? (nth ops i))
            (let [start (loop [s i k 0]
                          (cond
                            (zero? s) 0
                            (changed? (nth ops (dec s))) (recur (dec s) 0)
                            (>= k ctx) s
                            :else (recur (dec s) (inc k))))
                  end (loop [e (inc i) k 0]
                        (cond
                          (>= e n) e
                          (changed? (nth ops e)) (recur (inc e) 0)
                          (>= k ctx) e
                          :else (recur (inc e) (inc k))))]
              (recur end (conj! acc [start end])))
            :else (recur (inc i) acc)))
        ;; Merge overlapping regions.
        merged
        (reduce (fn [acc [s e]]
                  (if (and (seq acc) (<= s (peek (peek acc))))
                    (conj (pop acc) [(first (peek acc)) (max e (peek (peek acc)))])
                    (conj acc [s e])))
                []
                regions)]
    (mapv (fn [[s e]]
            (let [hunk-lines (subvec ops s e)
                  a-prefix   (count (filter consumes-a? (subvec ops 0 s)))
                  b-prefix   (count (filter consumes-b? (subvec ops 0 s)))
                  a-len      (count (filter consumes-a? hunk-lines))
                  b-len      (count (filter consumes-b? hunk-lines))]
              {:a-start (if (zero? a-len) 0 (inc a-prefix))
               :a-len   a-len
               :b-start (if (zero? b-len) 0 (inc b-prefix))
               :b-len   b-len
               :lines   hunk-lines}))
          merged)))

(defn- render-unified
  "Format hunks as unified-diff bytes. Empty hunks → empty string."
  [a-name b-name hunks]
  (if (empty? hunks)
    ""
    (let [sb (StringBuilder.)]
      (.append sb (str "--- " a-name "\n+++ " b-name "\n"))
      (doseq [{:keys [a-start a-len b-start b-len lines]} hunks]
        (.append sb (format "@@ -%d,%d +%d,%d @@%n" a-start a-len b-start b-len))
        (doseq [[op line] lines]
          (.append sb (case op :keep " " :del "-" :add "+"))
          (.append sb ^String line)
          (.append sb "\n")))
      (.toString sb))))

(defn diff
  "POSIX diff. Subset: -u (unified, the default), -q (brief),
   -i (ignore case), -w (ignore all whitespace). Exit 0 = same,
   1 = different, 2 = error.

   Output is a real unified diff (with `@@ -a,A +b,B @@` hunk
   headers and 3 lines of context) so `patch` / `git apply` can
   consume it."
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
                ops (diff-ops la lb)
                same? (every? (fn [[op _]] (= op :keep)) ops)]
            (cond
              same? {:stdout "" :stderr "" :exit 0}
              (:brief opts)
              {:stdout (str "Files " fa " and " fb " differ\n")
               :stderr "" :exit 1}
              :else
              (let [hunks (group-hunks ops diff-default-context)]
                {:stdout (render-unified fa fb hunks)
                 :stderr "" :exit 1}))))))))

;; ============================================================================
;; xargs — read stdin, dispatch CMD with substituted args via host
;; ============================================================================

(def ^:private xargs-spec
  [["-0" "--null"]
   ["-n" "--max-args N" :parse-fn #(Long/parseLong %)]
   ["-I" "--replace R"]
   ["-d" "--delimiter D"]
   ["-r" "--no-run-if-empty"]])

(defn- xargs-tokens
  "Split stdin into tokens per xargs's rules:
   - -0 / --null: NUL-separated
   - -d D: explicit delimiter D
   - -I R: each LINE is a token (not whitespace-split)
   - default: whitespace-split

   Empty tokens are dropped."
  [stdin {:keys [null delimiter replace]}]
  (let [raw (cond
              null      (str/split stdin (re-pattern (java.util.regex.Pattern/quote "\0")))
              delimiter (str/split stdin (re-pattern (java.util.regex.Pattern/quote delimiter)))
              replace   (str/split-lines stdin)
              :else     (str/split stdin #"\s+"))]
    (->> raw (remove str/blank?) vec)))

(defn xargs
  "POSIX xargs, subset: -0 NUL-separated, -n N args per call,
   -I R per-call substitution (one whole line per invocation),
   -d D explicit delimiter, -r / --no-run-if-empty.

   Dispatches CMD through *host* — same gates apply, so xargs
   can't escape the builtin/allowlist set. Subprocess stdout is
   passed through verbatim (newlines preserved)."
  [argv _fs env]
  (let [{:keys [opts pos err]} (cli-parse argv xargs-spec)]
    (cond
      err (usage-err "xargs" err)
      (nil? *host*) (err "xargs: no host available for dispatch" 1)
      (>= *depth* max-shell-depth)
      (err "xargs: too many nested invocations" 2)
      :else
      (let [stdin    (or (:stdin env) "")
            tokens   (xargs-tokens stdin opts)
            cmd-argv (if (seq pos) pos ["echo"])
            cmd      (first cmd-argv)
            base     (vec (rest cmd-argv))
            replace  (:replace opts)
            max-n    (:max-args opts)
            stdout-sb (StringBuilder.)
            stderr-sb (StringBuilder.)
            any-err? (volatile! false)
            batches  (cond
                       replace (mapv vector tokens)
                       max-n   (vec (partition-all max-n tokens))
                       :else   [tokens])]
        (cond
          ;; -r and nothing on stdin → noop
          (and (:no-run-if-empty opts) (empty? tokens))
          (ok "")

          :else
          (do
            (doseq [batch batches]
              (when-not @any-err?
                (let [args (if replace
                             (mapv (fn [a]
                                     (str/replace a
                                                  (re-pattern (java.util.regex.Pattern/quote replace))
                                                  (first batch)))
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
                  (.append stdout-sb ^String out-s)
                  (.append stderr-sb ^String err-s)
                  (when (not (zero? exit)) (vreset! any-err? true)))))
            {:stdout (.toString stdout-sb)
             :stderr (.toString stderr-sb)
             :exit (if @any-err? 1 0)}))))))

;; ============================================================================
;; Write builtins — touch, mkdir, rmdir, rm, cp, mv, chmod, ln, tee
;;
;; Each goes through the muschel.fs protocol so containment is enforced.
;; None touches the fallback host's file methods directly.
;; ============================================================================

(defn touch
  "POSIX touch. Create empty file if missing, update mtime if existing.
   -c skips creation. -a / -m selective time-axes treated as no-ops."
  [argv fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-a" "--access-only"]
                         ["-c" "--no-create"]
                         ["-m" "--mtime-only"]])]
    (cond
      err (usage-err "touch" err)
      (empty? pos) (usage-err "touch" "missing file operand")
      :else
      (let [stderr (volatile! "")
            any-err? (volatile! false)]
        (doseq [f pos]
          (let [existing (fs/exists? fs f)]
            (cond
              (and (:no-create opts) (not existing)) nil
              :else
              (when-not (fs/touch fs f)
                (vswap! stderr str "touch: " f ": cannot touch (outside root?)\n")
                (vreset! any-err? true)))))
        {:stdout "" :stderr @stderr :exit (if @any-err? 1 0)}))))

(defn- mkdir-p
  "Create every missing ancestor of `path` (including path itself).
   Returns truthy only when the final path is a directory."
  [fs ^String path]
  (let [absolute? (str/starts-with? path "/")
        segs      (->> (str/split path #"/") (filter seq) vec)
        start     (if absolute? "/" (fs/cwd fs))
        steps     (rest (reductions (fn [acc s]
                                      (str (str/replace acc #"/+$" "") "/" s))
                                    start
                                    segs))]
    (every?
     (fn [p]
       (if (fs/exists? fs p)
         (= :dir (:type (fs/stat fs p)))
         (fs/mkdir fs p)))
     steps)))

(defn mkdir
  "POSIX mkdir. -p creates parents (and is idempotent on existing dirs)."
  [argv fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-p" "--parents"]
                         ["-m" "--mode MODE"]])]
    (cond
      err (usage-err "mkdir" err)
      (empty? pos) (usage-err "mkdir" "missing operand")
      :else
      (let [stderr (volatile! "")
            any-err? (volatile! false)]
        (doseq [d pos]
          (cond
            (:parents opts)
            (when-not (mkdir-p fs d)
              (vswap! stderr str "mkdir: cannot create directory '" d "'\n")
              (vreset! any-err? true))
            (fs/exists? fs d)
            (do (vswap! stderr str "mkdir: cannot create directory '" d "': File exists\n")
                (vreset! any-err? true))
            :else
            (when-not (fs/mkdir fs d)
              (vswap! stderr str "mkdir: cannot create directory '" d "'\n")
              (vreset! any-err? true))))
        {:stdout "" :stderr @stderr :exit (if @any-err? 1 0)}))))

(defn rmdir
  "POSIX rmdir — empty directories only."
  [argv fs _env]
  (let [{:keys [pos err]} (cli-parse argv [["-p" "--parents"]])]
    (cond
      err (usage-err "rmdir" err)
      (empty? pos) (usage-err "rmdir" "missing operand")
      :else
      (let [stderr (volatile! "")
            any-err? (volatile! false)]
        (doseq [d pos]
          (let [s (fs/stat fs d)]
            (cond
              (nil? s)
              (do (vswap! stderr str "rmdir: failed to remove '" d "': No such file or directory\n")
                  (vreset! any-err? true))
              (not= :dir (:type s))
              (do (vswap! stderr str "rmdir: failed to remove '" d "': Not a directory\n")
                  (vreset! any-err? true))
              (seq (fs/list-dir fs d))
              (do (vswap! stderr str "rmdir: failed to remove '" d "': Directory not empty\n")
                  (vreset! any-err? true))
              :else
              (when-not (fs/delete fs d)
                (vswap! stderr str "rmdir: failed to remove '" d "'\n")
                (vreset! any-err? true)))))
        {:stdout "" :stderr @stderr :exit (if @any-err? 1 0)}))))

(defn- delete-recursive!
  "Depth-first walk: delete every leaf first, then the dir."
  [fs path]
  (let [s (fs/stat fs path)]
    (cond
      (nil? s) false
      (= :dir (:type s))
      (let [children (fs/list-dir fs path)]
        (doseq [{:keys [name]} children]
          (delete-recursive! fs (str (str/replace path #"/+$" "") "/" name)))
        (boolean (fs/delete fs path)))
      :else
      (boolean (fs/delete fs path)))))

(defn rm
  "POSIX rm. -r/-R recursive; -f silences missing-file errors. -i not
   implemented — refusing dangerous patterns is the permit layer's job."
  [argv fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-r" "--recursive"]
                         ["-R" "--recursive-cap"]
                         ["-f" "--force"]
                         ["-v" "--verbose"]])]
    (cond
      err (usage-err "rm" err)
      (empty? pos) (if (:force opts) (ok "") (usage-err "rm" "missing operand"))
      :else
      (let [recursive? (or (:recursive opts) (:recursive-cap opts))
            stdout    (volatile! "")
            stderr    (volatile! "")
            any-err?  (volatile! false)]
        (doseq [f pos]
          (let [s (fs/stat fs f)]
            (cond
              (nil? s)
              (when-not (:force opts)
                (vswap! stderr str "rm: cannot remove '" f "': No such file or directory\n")
                (vreset! any-err? true))
              (and (= :dir (:type s)) (not recursive?))
              (do (vswap! stderr str "rm: cannot remove '" f "': Is a directory\n")
                  (vreset! any-err? true))
              :else
              (if (delete-recursive! fs f)
                (when (:verbose opts) (vswap! stdout str "removed '" f "'\n"))
                (do (vswap! stderr str "rm: cannot remove '" f "'\n")
                    (vreset! any-err? true))))))
        {:stdout @stdout :stderr @stderr :exit (if @any-err? 1 0)}))))

(defn- copy-file! [fs src dst]
  (let [bytes (fs/read-bytes fs src)
        out   (fs/open-sink fs dst false)]
    (if (and bytes out)
      (with-open [^java.io.OutputStream o out]
        (cond
          (bytes? bytes)  (.write o ^bytes bytes)
          (string? bytes) (.write o (.getBytes ^String bytes "UTF-8")))
        true)
      false)))

(defn- copy-tree! [fs src dst]
  (let [s (fs/stat fs src)]
    (cond
      (nil? s) false
      (= :dir (:type s))
      (do (or (fs/exists? fs dst) (fs/mkdir fs dst))
          (doseq [{:keys [name]} (fs/list-dir fs src)]
            (copy-tree! fs
                        (str (str/replace src #"/+$" "") "/" name)
                        (str (str/replace dst #"/+$" "") "/" name)))
          true)
      :else
      (copy-file! fs src dst))))

(defn- cp-target
  "If `dst` is an existing dir, append SRC's basename; otherwise use
   DST as the final filename."
  [fs src dst]
  (let [s (fs/stat fs dst)]
    (if (and s (= :dir (:type s)))
      (let [base (last (str/split src #"/"))]
        (str (str/replace dst #"/+$" "") "/" base))
      dst)))

(defn cp
  "POSIX cp. -r/-R for recursive."
  [argv fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-r" "--recursive"]
                         ["-R" "--recursive-cap"]
                         ["-f" "--force"]
                         ["-v" "--verbose"]])]
    (cond
      err (usage-err "cp" err)
      (< (count pos) 2) (usage-err "cp" "missing destination file operand")
      :else
      (let [recursive? (or (:recursive opts) (:recursive-cap opts))
            dst       (last pos)
            srcs      (butlast pos)
            stderr    (volatile! "")
            any-err?  (volatile! false)]
        (doseq [src srcs]
          (let [s (fs/stat fs src)]
            (cond
              (nil? s)
              (do (vswap! stderr str "cp: cannot stat '" src "': No such file or directory\n")
                  (vreset! any-err? true))
              (and (= :dir (:type s)) (not recursive?))
              (do (vswap! stderr str "cp: -r not specified; omitting directory '" src "'\n")
                  (vreset! any-err? true))
              :else
              (let [final (cp-target fs src dst)]
                (when-not (copy-tree! fs src final)
                  (vswap! stderr str "cp: failed to copy '" src "' to '" final "'\n")
                  (vreset! any-err? true))))))
        {:stdout "" :stderr @stderr :exit (if @any-err? 1 0)}))))

(defn mv
  "POSIX mv. Same final-arg semantics as cp."
  [argv fs _env]
  (let [{:keys [pos err]} (cli-parse argv [["-f" "--force"]
                                           ["-v" "--verbose"]])]
    (cond
      err (usage-err "mv" err)
      (< (count pos) 2) (usage-err "mv" "missing destination file operand")
      :else
      (let [dst       (last pos)
            srcs      (butlast pos)
            stderr    (volatile! "")
            any-err?  (volatile! false)]
        (doseq [src srcs]
          (let [final (cp-target fs src dst)]
            (when-not (fs/rename fs src final)
              (vswap! stderr str "mv: cannot move '" src "' to '" final "'\n")
              (vreset! any-err? true))))
        {:stdout "" :stderr @stderr :exit (if @any-err? 1 0)}))))

(defn- parse-mode
  "Parse an octal mode string (`755`, `0755`) into an int. Returns
   nil on malformed input. Symbolic modes (`u+x`) are not supported."
  [^String s]
  (try (Long/parseLong s 8) (catch Throwable _ nil)))

(defn chmod
  "POSIX chmod, octal modes only."
  [argv fs _env]
  (let [args (vec (rest argv))]
    (cond
      (< (count args) 2) (usage-err "chmod" "missing operand")
      :else
      (let [mode-s (first args)
            files  (rest args)
            mode   (parse-mode mode-s)]
        (if (nil? mode)
          (usage-err "chmod" (str "invalid mode: " mode-s))
          (let [stderr   (volatile! "")
                any-err? (volatile! false)]
            (doseq [f files]
              (when-not (fs/chmod fs f mode)
                (vswap! stderr str "chmod: cannot chmod '" f "'\n")
                (vreset! any-err? true)))
            {:stdout "" :stderr @stderr :exit (if @any-err? 1 0)}))))))

(defn ln
  "Symbolic link via `-s` only. Hard links aren't meaningful inside
   the sandbox; refusing them keeps the surface tight."
  [argv fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-s" "--symbolic"]
                         ["-f" "--force"]])]
    (cond
      err                       (usage-err "ln" err)
      (not (:symbolic opts))    (usage-err "ln" "only -s (symbolic) is supported")
      (not= 2 (count pos))      (usage-err "ln" "exactly TARGET LINK_NAME required")
      :else
      (let [[target link-path] pos]
        (when (and (:force opts) (fs/exists? fs link-path))
          (fs/delete fs link-path))
        (if (fs/symlink fs target link-path)
          {:stdout "" :stderr "" :exit 0}
          (err (str "ln: failed to create symbolic link '" link-path "'") 1))))))

(defn tee
  "Read stdin, write to stdout AND each FILE. -a appends."
  [argv fs env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-a" "--append"]
                         ["-i" "--ignore-interrupts"]])]
    (cond
      err (usage-err "tee" err)
      :else
      (let [stdin    (or (:stdin env) "")
            stderr   (volatile! "")
            any-err? (volatile! false)]
        (doseq [f pos]
          (try
            (let [out (fs/open-sink fs f (boolean (:append opts)))]
              (if out
                (with-open [^java.io.OutputStream o out]
                  (.write o (.getBytes ^String stdin "UTF-8")))
                (do (vswap! stderr str "tee: " f ": cannot open for writing\n")
                    (vreset! any-err? true))))
            (catch Throwable t
              (vswap! stderr str "tee: " f ": " (.getMessage t) "\n")
              (vreset! any-err? true))))
        {:stdout stdin :stderr @stderr :exit (if @any-err? 1 0)}))))

;; ============================================================================
;; Registry — the canonical builtin map
;; ============================================================================

(def standard-read-only
  "Strict read-only builtin set. No filesystem mutations possible
   through this map. Use as :builtins on a BuiltinHost that should
   never be able to alter its FS."
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

(def standard
  "Full standard set: read builtins plus write builtins (touch, mkdir,
   rmdir, rm, cp, mv, chmod, ln, tee). All routed through the FS for
   containment. Use as :builtins on a BuiltinHost when the agent
   should be able to author files in the sandbox."
  (merge standard-read-only
         {"touch" touch
          "mkdir" mkdir
          "rmdir" rmdir
          "rm"    rm
          "cp"    cp
          "mv"    mv
          "chmod" chmod
          "ln"    ln
          "tee"   tee}))
