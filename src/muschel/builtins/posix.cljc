(ns muschel.builtins.posix
  "Pure-Clojure implementations of a useful subset of POSIX coreutils.

   ## Cross-platform port — WORK IN PROGRESS

   This file was renamed `.clj` → `.cljc` to start the cross-platform
   port (JVM + CLJS + babashka). The body is still JVM-only today;
   the port is staged so the JVM tests keep passing while we
   incrementally swap Java interop for `muschel.builtins.awk-compat`
   shims or `#?` reader conditionals. See the TODOs at the bottom of
   this docstring for the work map.

   No CLJS namespace requires this file yet, so the unported Java
   interop is unreached by the CLJS compiler. To enable Node /
   browser dispatch, a follow-up will:

     1. Replace the dynamic `(require 'muschel.parse 'muschel.exec …)`
        in `sh` with a registry pattern (exec sets the fn refs at
        load time).
     2. Convert ~8 `java.util.regex.Pattern` uses to `awk-compat`
        `re-compile` / `re-quote`.
     3. Convert `java.io.OutputStream` with-open blocks to shims
        (or quarantine behind :clj).
     4. Convert ~13 `(catch #?(:clj Throwable :cljs :default) …)` to
        `#?(:clj Throwable :cljs :default)`.
     5. Replace `java.util.ArrayList` with `(transient [])`.
     6. Shim `java.util.Calendar`/`Date` in the `date` builtin.
     7. Shim `java.util.Base64` using `js/Buffer` on CLJS.
     8. Quarantine `curl` (java.net.http) behind
        `#?(:clj … :bb babashka.http-client :cljs refuse-stub)`.

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
  (:refer-clojure :exclude [cat printf])
  (:require [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [muschel.builtins.awk :as awk-impl]
            [muschel.builtins.awk-compat :as cc]
            [muschel.fs :as fs]
            [muschel.host :as host]
            [muschel.runtime :as rt]))

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

(defn- usage-err
  "Usage errors exit 1 (matches GNU coreutils + POSIX). 2 is
   reserved for higher-tier protocol errors (some greps + diffs)."
  [cmd msg]
  (err (str cmd ": " msg) 1))

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
   logical cwd as both — the muschel session tracks logical cwd.
   Output is sandbox-relativised (so a disk-backed FS doesn't leak the
   host mount prefix)."
  [_argv fs _env]
  (ok (str (fs/sandbox-relativize fs (fs/cwd fs)) "\n")))

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
    (cc/fmt-many "%s %10d %s" [t size (:name entry)])))

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
            out         (cc/sbuf)
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
                                          (cc/fmt-many "%6d\t%s" [@n shown])))
                                  num-all?
                                  (do (vswap! n inc)
                                      (cc/fmt-many "%6d\t%s" [@n shown]))
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
                                   #?(:clj (String. ^bytes bytes "UTF-8")
                                      :cljs (str bytes))))))))
        {:stdout (cc/sbstr out)
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
    :parse-fn #(parse-long %)
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

(defn- utf8-byte-count
  "Byte length of `s` when encoded as UTF-8. JVM uses java.lang.String#getBytes;
   CLJS uses a portable Unicode walk (1-byte for U+0000..7F, 2-byte to U+07FF,
   3-byte to U+FFFF, 4-byte for surrogate pairs)."
  [^String s]
  #?(:clj  (alength (.getBytes s "UTF-8"))
     :cljs (let [n (.-length s)]
             (loop [i 0, acc 0]
               (if (>= i n)
                 acc
                 (let [c (.charCodeAt s i)]
                   (cond
                     (< c 0x80)   (recur (inc i) (+ acc 1))
                     (< c 0x800)  (recur (inc i) (+ acc 2))
                     ;; high surrogate — pair codes a 4-byte codepoint.
                     (and (>= c 0xD800) (<= c 0xDBFF))
                     (recur (+ i 2) (+ acc 4))
                     :else        (recur (inc i) (+ acc 3)))))))))

(defn- count-stats
  "Count lines (newline occurrences — GNU semantics, so trailing-nl-less
   content yields one fewer line than chunks), words (whitespace-separated
   runs), bytes (UTF-8), and chars (codepoints / String length)."
  [^String s]
  (let [byte-count (utf8-byte-count s)
        word-count (count (re-seq #"\S+" s))
        line-count (count (filter #(= \newline %) s))
        char-count #?(:clj (.length s) :cljs (.-length s))]
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
                       (str (when show-l? (cc/fmt1 "%8d " lines))
                            (when show-w? (cc/fmt1 "%8d " words))
                            (when show-c? (cc/fmt1 "%8d " bytes))
                            (when show-m? (cc/fmt1 "%8d " chars))
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

(defn- run-nested-script
  "Parse `script` and run it through the same host as the outer
   invocation. `script-args` becomes the nested shell's positional
   parameters ($1, $2, …). Returns the builtin result shape."
  [script script-args env]
  (let [host     *host*
        parse-fn @rt/parse-fn
        run-fn   @rt/run-fn
        new-env  @rt/new-env-fn]
    (cond
      (not host)
      (err "sh: no host available for nested dispatch" 1)

      (or (nil? parse-fn) (nil? run-fn) (nil? new-env))
      (err (str "sh: muschel.exec is not loaded — recursive shell "
                "dispatch requires (require 'muschel.exec)")
           1)

      :else
      (let [e0     (cond-> (or env (new-env))
                     (seq script-args) (assoc :pos-args (vec script-args)))
            ast    (parse-fn script)
            result (binding [*depth* (inc *depth*)]
                     (run-fn e0 ast
                             (cond-> {:host host}
                               *session* (assoc :session *session*))))]
        {:stdout (or (:stdout result) "")
         :stderr (or (:stderr result) "")
         :exit   (or (:exit result) 0)}))))

(defn sh
  "Builtin sh / bash. Two forms:
     - `sh -c SCRIPT`   — run SCRIPT inline (`bash -c` convention).
     - `sh FILE [args]` — read FILE through the FS protocol and run
                          it; `[args]` become the script's `$1`, `$2`, …

   Both parse with muschel.parse and execute through the same host
   the outer invocation is on (so containment, permits, and tracing
   carry through into the sub-shell)."
  [argv fs env]
  ;; Don't shadow the local `err` helper with the cli-parse `:err`
  ;; field — `cond`'s `:else` branch needs to call `(err …)`.
  (let [{:keys [opts pos] cli-err :err} (cli-parse argv sh-spec)]
    (cond
      cli-err
      (usage-err "sh" cli-err)

      (>= *depth* max-shell-depth)
      (err (str "sh: too many nested shell invocations (depth >= "
                max-shell-depth ")")
           2)

      (:command opts)
      (run-nested-script (:command opts) (rest pos) env)

      (seq pos)
      (let [script-path (first pos)
            script-args (rest pos)
            content     (fs/read-file fs script-path)]
        (if (nil? content)
          (err (str "sh: " script-path ": No such file or directory") 127)
          (run-nested-script content script-args env)))

      :else
      (err "sh: missing operand — expected `sh -c SCRIPT` or `sh FILE`" 2))))

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
                  (cc/fmt-many "%10d %s %s" [(:size s) (name (or (:type s) :unknown)) f])
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
                    (let [pa (try (parse-long (str/trim a))
                                  (catch #?(:clj Exception :cljs :default) _ #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER)))
                          pb (try (parse-long (str/trim b))
                                  (catch #?(:clj Exception :cljs :default) _ #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER)))]
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
                               (cc/fmt-many "%7d %s" [n line])
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
  "Build a platform regex from a single grep pattern, honouring
   -F (literal), -i (case-insensitive), and -w (word-bound). Uses
   `(?i)` inline flag for case-insensitivity so the same code works
   on Java regex and JS regex."
  [pattern {:keys [fixed-strings ignore-case word-regexp]}]
  (let [body  (if fixed-strings (cc/re-quote pattern) pattern)
        body  (if word-regexp (str "\\b(?:" body ")\\b") body)
        body  (if ignore-case  (str "(?i)" body) body)]
    (cc/re-compile body)))

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
  [compiled-or-coll line]
  (if (coll? compiled-or-coll)
    (some #(cc/re-find-any? % line) compiled-or-coll)
    (cc/re-find-any? compiled-or-coll line)))

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
  "Translate a POSIX glob (-name) to a regex. * → .*, ? → .,
   [abc] passes through. Escapes other regex metacharacters."
  [glob]
  (let [sb (cc/sbuf)]
    (doseq [c glob]
      (case c
        \* (cc/sappend! sb ".*")
        \? (cc/sappend! sb ".")
        \. (cc/sappend! sb "\\.")
        \( (cc/sappend! sb "\\(")
        \) (cc/sappend! sb "\\)")
        \+ (cc/sappend! sb "\\+")
        \| (cc/sappend! sb "\\|")
        \^ (cc/sappend! sb "\\^")
        \$ (cc/sappend! sb "\\$")
        \{ (cc/sappend! sb "\\{")
        \} (cc/sappend! sb "\\}")
        (cc/sappend! sb c)))
    (cc/re-compile (cc/sbstr sb))))

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
                    [{:kind :pred :pred :maxdepth :n (parse-long v)} (+ pos 2) nil]
                    [nil pos "find: -maxdepth requires an argument"])
      "-mindepth" (if-let [v (nxt)]
                    [{:kind :pred :pred :mindepth :n (parse-long v)} (+ pos 2) nil]
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
      (loop [pending (conj #?(:clj clojure.lang.PersistentQueue/EMPTY
                              :cljs cljs.core/PersistentQueue.EMPTY)
                           {:path root :depth 0})
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
            (cc/re-full-match? re base'))
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
            stdout-sb (cc/sbuf)
            stderr-sb (cc/sbuf)
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
  {"alpha"  (concat (map char (range 97 123))
                    (map char (range 65 91)))
   "alnum"  (concat (map char (range 97 123))
                    (map char (range 65 91))
                    (map char (range 48 58)))
   "digit"  (map char (range 48 58))
   "lower"  (map char (range 97 123))
   "upper"  (map char (range 65 91))
   "space"  [\space \tab \newline \return \formfeed (char 11)]
   "blank"  [\space \tab]
   "punct"  (seq "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
   "xdigit" (concat (map char (range 48 58))
                    (map char (range 97 103))
                    (map char (range 65 71)))
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
  (let [sb (cc/sbuf)
        n  (count s)]
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
                    (do (doseq [ch chars] (.append sb ch))
                        (recur (+ end 2)))
                    (do (.append sb c) (recur (inc i)))))))

            ;; Escape sequence
            (and (= \\ c) (< (inc i) n))
            (if-let [[ch n-consumed] (expand-escape (.charAt s (inc i)))]
              (do (.append sb ch) (recur (+ i n-consumed)))
              (do (.append sb c) (recur (inc i))))

            ;; Range a-z
            (and (< (+ i 2) n) (= \- (.charAt s (inc i))))
            (let [start (cc/char-code c)
                  end   (cc/char-code (.charAt s (+ i 2)))]
              (if (<= start end)
                (do (doseq [k (range start (inc end))] (.append sb (char k)))
                    (recur (+ i 3)))
                (do (.append sb c) (recur (inc i)))))

            :else
            (do (.append sb c) (recur (inc i)))))))
    (cc/sbstr sb)))

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
                    (let [n (parse-long p)] [n n])
                    (re-matches #"\d+-\d+" p)
                    (let [[a b] (str/split p #"-")]
                      [(parse-long a) (parse-long b)])
                    (re-matches #"\d+-" p)
                    [(parse-long (subs p 0 (dec (count p)))) #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER)]
                    (re-matches #"-\d+" p)
                    [1 (parse-long (subs p 1))]
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
                     (let [parts (str/split line (re-pattern (cc/re-quote delim)))]
                       (->> parts
                            (keep-indexed (fn [i s] (when (in? (inc i)) s)))
                            (str/join delim))))
                   :else
                   (let [parts (str/split line (re-pattern (cc/re-quote delim)))]
                     (->> parts
                          (keep-indexed (fn [i s] (when (in? (inc i)) s)))
                          (str/join delim)))))
               lines)
              kept (remove nil? out)]
          {:stdout (str (str/join "\n" kept) (when (seq kept) "\n"))
           :stderr stderr
           :exit (if err? 1 0)})
        (catch #?(:clj Throwable :cljs :default) t
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

(defn- lcs-table
  "Compute the LCS DP table for vectors `a` and `b`. Returns a
   transient vector treated as a flat (n+1)·(m+1) row-major grid;
   cross-platform (no int-array)."
  [a b]
  (let [n    (count a)
        m    (count b)
        cols (inc m)]
    (loop [i 0
           t (let [empty-row (vec (repeat (* (inc n) cols) 0))]
               (transient empty-row))]
      (if (= i n)
        (persistent! t)
        (recur (inc i)
               (loop [j 0 t t]
                 (if (= j m)
                   t
                   (let [idx (+ (* (inc i) cols) (inc j))
                         up   (nth t (+ (* i cols) (inc j)))
                         left (nth t (+ (* (inc i) cols) j))
                         val  (if (= (nth a i) (nth b j))
                                (inc (nth t (+ (* i cols) j)))
                                (max up left))]
                     (recur (inc j) (assoc! t idx val))))))))))

(defn- diff-ops
  "Return a vector of [op line] pairs (`:keep`/`:del`/`:add`) tracing
   the LCS table for vectors `a` and `b`. Source order."
  [a b]
  (let [t    (lcs-table a b)
        n    (count a)
        m    (count b)
        cols (inc m)
        get-t (fn [i j] (nth t (+ (* i cols) j)))]
    (loop [i n j m acc (transient [])]
      (cond
        (and (pos? i) (pos? j) (= (nth a (dec i)) (nth b (dec j))))
        (recur (dec i) (dec j) (conj! acc [:keep (nth a (dec i))]))

        (and (pos? j) (or (zero? i) (>= (get-t i (dec j)) (get-t (dec i) j))))
        (recur i (dec j) (conj! acc [:add (nth b (dec j))]))

        (pos? i)
        (recur (dec i) j (conj! acc [:del (nth a (dec i))]))

        :else (vec (reverse (persistent! acc)))))))

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
    (let [sb (cc/sbuf)]
      (.append sb (str "--- " a-name "\n+++ " b-name "\n"))
      (doseq [{:keys [a-start a-len b-start b-len lines]} hunks]
        (.append sb (cc/fmt-many "@@ -%d,%d +%d,%d @@\n" [a-start a-len b-start b-len]))
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
   ["-n" "--max-args N" :parse-fn #(parse-long %)]
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
              null      (str/split stdin (re-pattern (cc/re-quote "\0")))
              delimiter (str/split stdin (re-pattern (cc/re-quote delimiter)))
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
            stdout-sb (cc/sbuf)
            stderr-sb (cc/sbuf)
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
                                                  (re-pattern (cc/re-quote replace))
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

(declare parse-mode)

(defn mkdir
  "POSIX mkdir. -p creates parents (idempotent on existing dirs).
   -m MODE applies an octal or symbolic mode to created dirs."
  [argv fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-p" "--parents"]
                         ["-m" "--mode MODE"]
                         ["-v" "--verbose"]])]
    (cond
      err (usage-err "mkdir" err)
      (empty? pos) (usage-err "mkdir" "missing operand")
      :else
      (let [stderr (volatile! "")
            stdout (volatile! "")
            any-err? (volatile! false)
            mode-val (when (:mode opts)
                       (parse-mode (:mode opts) 0755))
            note-creation (fn [d]
                            (when (:verbose opts)
                              (vswap! stdout str "mkdir: created directory '" d "'\n"))
                            (when mode-val
                              (fs/chmod fs d mode-val)))]
        (doseq [d pos]
          (cond
            (:parents opts)
            (if (mkdir-p fs d)
              (note-creation d)
              (do (vswap! stderr str "mkdir: cannot create directory '" d "'\n")
                  (vreset! any-err? true)))

            (fs/exists? fs d)
            (do (vswap! stderr str "mkdir: cannot create directory '" d "': File exists\n")
                (vreset! any-err? true))

            :else
            (if (fs/mkdir fs d)
              (note-creation d)
              (do (vswap! stderr str "mkdir: cannot create directory '" d "'\n")
                  (vreset! any-err? true)))))
        {:stdout @stdout :stderr @stderr :exit (if @any-err? 1 0)}))))

(defn rmdir
  "POSIX rmdir — empty directories only. -p also removes empty
   parent directories on the way up."
  [argv fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-p" "--parents"]])]
    (cond
      err (usage-err "rmdir" err)
      (empty? pos) (usage-err "rmdir" "missing operand")
      :else
      (let [stderr (volatile! "")
            any-err? (volatile! false)
            remove-one!
            (fn [d]
              (let [s (fs/stat fs d)]
                (cond
                  (nil? s)
                  (do (vswap! stderr str "rmdir: failed to remove '" d "': No such file or directory\n")
                      (vreset! any-err? true)
                      false)
                  (not= :dir (:type s))
                  (do (vswap! stderr str "rmdir: failed to remove '" d "': Not a directory\n")
                      (vreset! any-err? true)
                      false)
                  (seq (fs/list-dir fs d))
                  (do (vswap! stderr str "rmdir: failed to remove '" d "': Directory not empty\n")
                      (vreset! any-err? true)
                      false)
                  :else
                  (if (fs/delete fs d)
                    true
                    (do (vswap! stderr str "rmdir: failed to remove '" d "'\n")
                        (vreset! any-err? true)
                        false)))))]
        (doseq [d pos]
          (when (remove-one! d)
            (when (:parents opts)
              ;; Walk up; stop when a parent is non-empty or removal fails.
              (loop [path d]
                (let [idx (.lastIndexOf ^String path "/")
                      parent (cond
                               (neg? idx)  nil
                               (zero? idx) nil           ; "/a" → "/", stop
                               :else       (subs path 0 idx))]
                  (when (and parent (not= "" parent) (not= "." parent))
                    (when (remove-one! parent)
                      (recur parent))))))))
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
      ;; Native: `rm` always requires at least one operand. -f silences
      ;; per-file 'no such file' errors, not the missing-operand error.
      (empty? pos) (usage-err "rm" "missing operand")
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
  (let [bytes (fs/read-bytes fs src)]
    (when bytes
      (let [s (cond
                (string? bytes) bytes
                #?@(:clj  [(bytes? bytes) (String. ^bytes bytes "UTF-8")]
                    :cljs [(string? bytes) bytes])
                :else (str bytes))]
        (boolean (fs/write-string! fs dst s false))))))

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
   DST as the final filename. SRC's trailing slashes are stripped so
   `cp d/ d2` resolves to `d2` (the dir's contents), matching native
   cp behaviour."
  [fs src dst]
  (let [s (fs/stat fs dst)]
    (if (and s (= :dir (:type s)))
      (let [src-trimmed (str/replace src #"/+$" "")
            base (last (str/split src-trimmed #"/"))]
        (str (str/replace dst #"/+$" "") "/" base))
      dst)))

(defn cp
  "POSIX cp. -r/-R for recursive. -p (preserve attributes) is a no-op
   here since the VFS already preserves mtime through stat round-trip.
   -n (no-clobber) refuses to overwrite an existing destination."
  [argv fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-r" "--recursive"]
                         ["-R" "--recursive-cap"]
                         ["-f" "--force"]
                         ["-v" "--verbose"]
                         ["-p" "--preserve"]
                         ["-n" "--no-clobber"]
                         ["-i" "--interactive"]])]
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
                (cond
                  (and (:no-clobber opts) (fs/exists? fs final))
                  nil  ; silently skip per GNU `cp -n`
                  :else
                  (when-not (copy-tree! fs src final)
                    (vswap! stderr str "cp: failed to copy '" src "' to '" final "'\n")
                    (vreset! any-err? true)))))))
        {:stdout "" :stderr @stderr :exit (if @any-err? 1 0)}))))

(defn mv
  "POSIX mv. Same final-arg semantics as cp. -n refuses to overwrite
   an existing destination; -f silences errors; -i (interactive) is
   accepted but always behaves like -f (we have no TTY for prompting)."
  [argv fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-f" "--force"]
                         ["-v" "--verbose"]
                         ["-n" "--no-clobber"]
                         ["-i" "--interactive"]])]
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
            (cond
              (and (:no-clobber opts) (fs/exists? fs final))
              nil  ; skip
              :else
              (when-not (fs/rename fs src final)
                (vswap! stderr str "mv: cannot move '" src "' to '" final "'\n")
                (vreset! any-err? true)))))
        {:stdout "" :stderr @stderr :exit (if @any-err? 1 0)}))))

(defn- parse-mode-octal
  "Parse `755`, `0755`, `0o755` → integer. Returns nil on malformed."
  [^String s]
  (try (cc/parse-long-radix (str/replace s #"^0o?" "") 8)
       (catch #?(:clj Throwable :cljs :default) _ nil)))

(defn- apply-symbolic-mode
  "Apply one symbolic mode clause (e.g. `u+x`, `go-w`, `a=r`) to an
   octal `base`. Returns the new mode. Supports who={u,g,o,a}, op={
   +,-,=}, perms={r,w,x,X,s,t}."
  [^long base ^String clause]
  (if-let [[_ who op perms] (re-matches #"([ugoa]*)([+\-=])([rwxX]+)" clause)]
    (let [who-bits (cond
                     (or (= "" who) (= "a" who)) 0777
                     :else
                     (reduce + 0
                             (for [c who]
                               (case c
                                 \u 0700
                                 \g 0070
                                 \o 0007
                                 0))))
          ;; Build the perm-bit triplet from perms (`rwx` → 7).
          perm-triplet (reduce + 0 (for [c perms]
                                     (case c
                                       \r 4
                                       \w 2
                                       (\x \X) 1
                                       0)))
          ;; Expand triplet across each who-octet.
          new-bits (cond-> 0
                     (pos? (bit-and who-bits 0700)) (bit-or (* perm-triplet 64))
                     (pos? (bit-and who-bits 0070)) (bit-or (* perm-triplet 8))
                     (pos? (bit-and who-bits 0007)) (bit-or perm-triplet))]
      (case op
        "+" (bit-or base new-bits)
        "-" (bit-and base (bit-not new-bits))
        "=" (bit-or (bit-and base (bit-not who-bits)) new-bits)))
    nil))

(defn- parse-mode
  "Parse either an octal mode (`755`, `0755`) or a symbolic mode
   (`u+x`, `a=r`, `go-w`, `u+x,g-w`). Returns a function that takes
   the current mode and returns the new one — symbolic modes can
   compose, octal is a constant override."
  [^String s base]
  (cond
    (re-matches #"0?o?[0-7]+" s)
    (parse-mode-octal s)

    :else
    ;; Comma-separated clauses, each acting on the running mode.
    (let [clauses (str/split s #",")]
      (reduce (fn [m clause]
                (when m
                  (apply-symbolic-mode m clause)))
              (or base 0644)
              clauses))))

(defn chmod
  "POSIX chmod. Octal (`0755`, `755`) or symbolic (`u+x`, `a=r`,
   `go-w`, comma-separated clauses). -R recursive."
  [argv fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-R" "--recursive"]
                         ["-v" "--verbose"]
                         ["-c" "--changes"]
                         ["-f" "--silent"]])]
    (cond
      err (usage-err "chmod" err)
      (< (count pos) 2) (usage-err "chmod" "missing operand")
      :else
      (let [mode-s (first pos)
            files  (rest pos)
            stderr   (volatile! "")
            any-err? (volatile! false)
            chmod-one!
            (fn [path]
              (let [cur (or (:perms-mode (fs/stat fs path)) 0644)
                    new-mode (parse-mode mode-s cur)]
                (cond
                  (nil? new-mode)
                  (do (vswap! stderr str "chmod: invalid mode: '" mode-s "'\n")
                      (vreset! any-err? true))
                  (not (fs/chmod fs path new-mode))
                  (when-not (:silent opts)
                    (vswap! stderr str "chmod: cannot operate on '" path "'\n")
                    (vreset! any-err? true)))))]
        (doseq [f files]
          (chmod-one! f)
          (when (:recursive opts)
            (let [s (fs/stat fs f)]
              (when (and s (= :dir (:type s)))
                (let [walk (fn walk [p]
                             (doseq [{:keys [name type]} (or (fs/list-dir fs p) [])]
                               (let [child (str (str/replace p #"/+$" "") "/" name)]
                                 (chmod-one! child)
                                 (when (= :dir type) (walk child)))))]
                  (walk f))))))
        {:stdout "" :stderr @stderr :exit (if @any-err? 1 0)}))))

(defn chown
  "POSIX chown OWNER[:GROUP] FILE...

   In the sandbox, ownership writes are best-effort: real-disk chown
   needs root for cross-user changes, and the virtual FS just stores
   the strings for stat round-trip. The agent gets non-zero exit when
   the underlying FS rejects."
  [argv fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-R" "--recursive"]
                         ["-v" "--verbose"]
                         ["-f" "--silent"]])]
    (cond
      err (usage-err "chown" err)
      (< (count pos) 2) (usage-err "chown" "missing operand")
      :else
      (let [[owner-spec & files] pos
            [owner group] (if (str/includes? owner-spec ":")
                            (str/split owner-spec #":" 2)
                            [owner-spec nil])
            owner (when (and owner (not= "" owner)) owner)
            stderr   (volatile! "")
            any-err? (volatile! false)
            chown-one!
            (fn [path]
              (when-not (fs/chown fs path owner group)
                (when-not (:silent opts)
                  (vswap! stderr str "chown: cannot change ownership of '" path "'\n")
                  (vreset! any-err? true))))]
        (doseq [f files]
          (chown-one! f)
          (when (:recursive opts)
            (let [s (fs/stat fs f)]
              (when (and s (= :dir (:type s)))
                (let [walk (fn walk [p]
                             (doseq [{:keys [name type]} (or (fs/list-dir fs p) [])]
                               (let [child (str (str/replace p #"/+$" "") "/" name)]
                                 (chown-one! child)
                                 (when (= :dir type) (walk child)))))]
                  (walk f))))))
        {:stdout "" :stderr @stderr :exit (if @any-err? 1 0)}))))

(defn ln
  "Symbolic link via `-s` only. Hard links aren't meaningful inside
   the sandbox; refusing them keeps the surface tight."
  [argv fs _env]
  (let [{:keys [opts pos] parse-err :err}
        (cli-parse argv [["-s" "--symbolic"]
                         ["-f" "--force"]])]
    (cond
      parse-err                 (usage-err "ln" parse-err)
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
            (if (fs/write-string! fs f stdin (boolean (:append opts)))
              nil
              (do (vswap! stderr str "tee: " f ": cannot open for writing\n")
                  (vreset! any-err? true)))
            (catch #?(:clj Throwable :cljs :default) t
              (vswap! stderr str "tee: " f ": "
                      #?(:clj (.getMessage t) :cljs (.-message t)) "\n")
              (vreset! any-err? true))))
        {:stdout stdin :stderr @stderr :exit (if @any-err? 1 0)}))))

;; ============================================================================
;; Path tools: basename, dirname, realpath
;; ============================================================================

(defn- basename-one [path suffix]
  (let [base
        (cond
          (re-matches #"/+" path) "/"  ; POSIX: basename / → /
          :else
          (let [trimmed (str/replace path #"/+$" "")
                last-seg (last (str/split trimmed #"/"))]
            (or last-seg trimmed)))]
    (if (and suffix
             (not= base suffix)
             (str/ends-with? base suffix))
      (subs base 0 (- (count base) (count suffix)))
      base)))

(defn basename
  "POSIX basename PATH [SUFFIX]. -a treats every arg as a path (no
   SUFFIX consumed); -s SUFFIX strips a common suffix from each;
   -z separates with NUL instead of newline."
  [argv _fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-a" "--multiple"]
                         ["-s" "--suffix SUFFIX"]
                         ["-z" "--zero"]])]
    (cond
      err (usage-err "basename" err)
      (empty? pos) (usage-err "basename" "missing operand")
      :else
      (let [sep (if (:zero opts) "\0" "\n")
            paths (if (or (:multiple opts) (:suffix opts))
                    pos
                    [(first pos)])
            suffix (or (:suffix opts)
                       (when (and (not (:multiple opts))
                                  (= 2 (count pos)))
                         (second pos)))
            outs (mapv #(basename-one % suffix) paths)]
        (ok (str (str/join sep outs) sep))))))

(defn dirname
  "POSIX dirname PATH...  -z separates outputs with NUL."
  [argv _fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-z" "--zero"]])]
    (cond
      err (usage-err "dirname" err)
      (empty? pos) (usage-err "dirname" "missing operand")
      :else
      (let [sep (if (:zero opts) "\0" "\n")
            outs (for [p pos]
                   (cond
                     (re-matches #"/+" p) "/"  ; POSIX: dirname / → /
                     :else
                     (let [trimmed (str/replace p #"/+$" "")
                           idx     (.lastIndexOf ^String trimmed "/")]
                       (cond
                         (neg? idx)  "."
                         (zero? idx) "/"
                         :else       (subs trimmed 0 idx)))))]
        (ok (str (str/join sep outs) sep))))))

(defn realpath
  "Canonicalise a path through the FS. -m (canonicalize-missing) lets
   missing paths through; default mode errors on missing (matches
   GNU). -e requires the full path to exist (same as default for us)."
  [argv fs _env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-e" "--canonicalize-existing"]
                         ["-m" "--canonicalize-missing"]
                         ["-s" "--strip"]
                         ["-z" "--zero"]])]
    (cond
      err (usage-err "realpath" err)
      (empty? pos) (usage-err "realpath" "missing operand")
      :else
      (let [stderr   (volatile! "")
            any-err? (volatile! false)
            sep      (if (:zero opts) "\0" "\n")
            lines
            (mapv (fn [p]
                    (if-let [resolved (fs/resolve fs p)]
                      (let [sandbox (fs/sandbox-relativize fs resolved)]
                        (cond
                          ;; -m: accept whatever resolves, even if missing.
                          (:canonicalize-missing opts) sandbox
                          ;; Default + -e: refuse if it doesn't exist.
                          (fs/exists? fs p) sandbox
                          :else
                          (do (vswap! stderr str "realpath: " p ": No such file or directory\n")
                              (vreset! any-err? true)
                              nil)))
                      (do (vswap! stderr str "realpath: " p ": No such file or directory\n")
                          (vreset! any-err? true)
                          nil)))
                  pos)]
        {:stdout (str/join "" (for [l lines :when l] (str l sep)))
         :stderr @stderr
         :exit (if @any-err? 1 0)}))))

;; ============================================================================
;; printf
;; ============================================================================

(defn- printf-process-escapes
  "Translate the `\\n \\t \\r \\\\ \\\"` escapes printf interprets in
   both format and arg strings."
  [^String s]
  (-> s
      (str/replace "\\n" "\n")
      (str/replace "\\t" "\t")
      (str/replace "\\r" "\r")
      (str/replace "\\\\" "\\")))

(defn printf
  "POSIX printf FORMAT [ARG]...

   Recognised specifiers: %s %d %i %x %o %c %% with width/precision.
   Format string is reused if there are more args than placeholders;
   if the spec count is zero, the format prints once (no reuse)."
  [argv _fs _env]
  (let [args (rest argv)]
    (cond
      (empty? args) (usage-err "printf" "usage: printf FORMAT [ARG]...")
      :else
      (let [fmt   (printf-process-escapes (first args))
            xs    (vec (rest args))
            specs (vec (re-seq #"%[-+# 0]*\d*(?:\.\d+)?[sdiouxXc%]" fmt))
            n     (count specs)
            coerce
            (fn [^String spec ^String arg]
              (let [last-ch (.charAt spec (dec (count spec)))]
                (case last-ch
                  (\d \i)
                  (try (parse-long arg) (catch #?(:clj Throwable :cljs :default) _ 0))
                  (\x \X \o)
                  (try (parse-long arg)
                       (catch #?(:clj Throwable :cljs :default) _
                         (try (cc/parse-long-radix arg 16)
                              (catch #?(:clj Throwable :cljs :default) _ 0))))
                  arg)))
            out (cc/sbuf)]
        (cond
          (zero? n)
          ;; No specifiers: emit format once, ignore extra args.
          (.append out fmt)
          :else
          ;; Walk args in groups of `n`, emit format per group. When
          ;; the last group is short, pad with empty strings so each
          ;; %s slot fills cleanly (matches GNU printf).
          (let [groups (partition-all n xs)
                groups (if (empty? groups) [(repeat n "")] groups)]
            (doseq [grp groups]
              (let [batch (vec (take n (concat grp (repeat ""))))]
                (try
                  (.append out (cc/fmt-many fmt
                                            (map-indexed
                                             (fn [i a] (coerce (nth specs i) (str a)))
                                             batch)))
                  (catch #?(:clj Throwable :cljs :default) _
                    (.append out fmt)))))))
        (ok (.toString out))))))

;; ============================================================================
;; env, date, seq
;; ============================================================================

(defn env-fn
  "Print environment as `KEY=VAL` lines. Honours `-i` (clear env
   before printing) and `-u VAR` (unset VAR). v1 doesn't implement
   the `env VAR=val CMD` form (would need a process spawn through
   host)."
  [argv _fs env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-i" "--ignore-environment"]
                         ["-u" "--unset NAME"
                          :assoc-fn (fn [m k v] (update m k (fnil conj #{}) v))
                          :default #{}]])
        ignore? (:ignore-environment opts)
        unsets  (or (:unset opts) #{})]
    (cond
      err (usage-err "env" err)
      ;; Refuse `env VAR=val CMD` form — pos args after flags imply
      ;; a command to execute, which we'd need to spawn through host.
      (seq pos) (usage-err "env" "running commands not supported (use the command directly)")
      :else
      (let [src-vars (if ignore? {} (or (:vars env) {}))
            vars    (apply dissoc src-vars unsets)
            out     (str/join "\n"
                              (for [[k v] (sort-by key vars)]
                                (str k "=" v)))]
        (ok (if (seq out) (str out "\n") ""))))))

(defn- date-parts-now
  "Return a `{:y :mo :d :h :mi :s :j :epoch-ms}` snapshot of the
   current local time. Cross-platform: JVM + babashka use
   `java.time.LocalDateTime` (bb refuses `java.util.Calendar`);
   CLJS uses js/Date."
  []
  #?(:clj
     (let [now (java.time.LocalDateTime/now)
           inst (.. now (atZone (java.time.ZoneId/systemDefault)) toInstant)]
       {:y  (.getYear now)
        :mo (.getMonthValue now)
        :d  (.getDayOfMonth now)
        :h  (.getHour now)
        :mi (.getMinute now)
        :s  (.getSecond now)
        :j  (.getDayOfYear now)
        :epoch-ms (.toEpochMilli inst)})
     :cljs
     (let [now (js/Date.)
           jan-1 (js/Date. (.getFullYear now) 0 1)
           ms-since-jan-1 (- (.getTime now) (.getTime jan-1))]
       {:y  (.getFullYear now)
        :mo (inc (.getMonth now))
        :d  (.getDate now)
        :h  (.getHours now)
        :mi (.getMinutes now)
        :s  (.getSeconds now)
        :j  (inc (Math/floor (/ ms-since-jan-1 (* 1000 60 60 24))))
        :epoch-ms (.getTime now)})))

(defn- date-translate
  "Walk the format string once, emitting each substitution. Left-to-
   right pass — `%%` can't collide with `%S`/`%T` etc. because we
   consume two chars per directive. `parts` is a date-parts-now map."
  [^String fmt parts]
  (let [{:keys [y mo d h mi s j epoch-ms]} parts
        n (count fmt)
        out (cc/sbuf)
        d2 (fn [v] (cc/fmt1 "%02d" v))
        d3 (fn [v] (cc/fmt1 "%03d" v))]
    (loop [i 0]
      (when (< i n)
        (let [c (.charAt fmt i)]
          (cond
            (and (= c \%) (< (inc i) n))
            (let [d2c (.charAt fmt (inc i))]
              (case d2c
                \%   (do (cc/sappend! out \%) (recur (+ i 2)))
                \Y   (do (cc/sappend! out (str y))      (recur (+ i 2)))
                \m   (do (cc/sappend! out (d2 mo))      (recur (+ i 2)))
                \d   (do (cc/sappend! out (d2 d))       (recur (+ i 2)))
                \H   (do (cc/sappend! out (d2 h))       (recur (+ i 2)))
                \M   (do (cc/sappend! out (d2 mi))      (recur (+ i 2)))
                \S   (do (cc/sappend! out (d2 s))       (recur (+ i 2)))
                \j   (do (cc/sappend! out (d3 j))       (recur (+ i 2)))
                \F   (do (cc/sappend! out (cc/fmt-many "%04d-%02d-%02d" [y mo d]))
                         (recur (+ i 2)))
                \T   (do (cc/sappend! out (cc/fmt-many "%02d:%02d:%02d" [h mi s]))
                         (recur (+ i 2)))
                \s   (do (cc/sappend! out (str (quot epoch-ms 1000)))
                         (recur (+ i 2)))
                \n   (do (cc/sappend! out \newline) (recur (+ i 2)))
                \t   (do (cc/sappend! out \tab)     (recur (+ i 2)))
                ;; unknown directive: pass through literally
                (do (cc/sappend! out c) (cc/sappend! out d2c) (recur (+ i 2)))))
            :else
            (do (cc/sappend! out c) (recur (inc i)))))))
    (cc/sbstr out)))

(defn date-fn
  "POSIX date. With +FORMAT prints the current time per format
   string; without args, prints a default \"%F %T\" form.

   Format directives supported: %Y %m %d %H %M %S %j %F %T %s %% %n %t
   (a pragmatic subset; unknown directives pass through literally)."
  [argv _fs _env]
  (let [args (rest argv)
        fmt-arg (first args)
        parts (date-parts-now)]
    (cond
      (and fmt-arg (str/starts-with? fmt-arg "+"))
      (ok (str (date-translate (subs fmt-arg 1) parts) "\n"))
      :else
      (ok (str (date-translate "%F %T" parts) "\n")))))

(defn- seq-format-int? [n]
  ;; Whole number that's representable losslessly as a long.
  (and (== n (long n))
       #?(:clj  (Double/isFinite (double n))
          :cljs (js/Number.isFinite n))))

(defn seq-fn
  "POSIX seq:
     seq LAST                start=1, step=1
     seq FIRST LAST          step=1
     seq FIRST STEP LAST

   Numbers may be integer or decimal (`seq 0.5 0.5 2.0`). -w pads
   to equal width with leading zeros. Calls the env's `:interrupt-fn`
   (if any) every 256 iterations so a runaway `seq 1 1000000000` is
   bounded by the caller's budget instead of OOMing the host."
  [argv _fs env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-s" "--separator S" :default "\n"]
                         ["-w" "--equal-width"]
                         ["-f" "--format FMT"]])]
    (cond
      err (usage-err "seq" err)
      (empty? pos) (usage-err "seq" "missing operand")
      :else
      (try
        (let [nums (mapv #(parse-double %) pos)
              [start step end] (case (count nums)
                                 1 [1.0 1.0 (first nums)]
                                 2 [(first nums) 1.0 (second nums)]
                                 3 [(first nums) (second nums) (nth nums 2)]
                                 (throw (ex-info "too many operands" {})))
              sep (:separator opts)
              positive-step? (pos? step)
              ifn (:interrupt-fn env)
              ;; Use a small epsilon to defend against float drift.
              eps 1e-9
              values (loop [v start acc (transient []) i 0]
                       (cond
                         (zero? step) (persistent! acc)
                         (and positive-step? (> v (+ end eps))) (persistent! acc)
                         (and (not positive-step?) (< v (- end eps))) (persistent! acc)
                         :else
                         (do
                           (when (and ifn (zero? (bit-and i 255))) (ifn))
                           (recur (+ v step) (conj! acc v) (inc i)))))
              all-int? (every? seq-format-int? values)
              fmt-one (fn [v]
                        (if all-int?
                          (str (long v))
                          ;; Strip trailing zeros for tidy output
                          (let [s (cc/fmt1 "%.10f" (double v))
                                s (str/replace s #"0+$" "")
                                s (str/replace s #"\.$" "")]
                            s)))
              rendered (mapv fmt-one values)
              padded (if (:equal-width opts)
                       (let [width (apply max 0 (map count rendered))]
                         (mapv (fn [s]
                                 ;; Java's %s spec doesn't zero-pad
                                 ;; strings (only numerics). Manual.
                                 (let [pad (- width (count s))]
                                   (str (apply str (repeat (max 0 pad) "0")) s)))
                               rendered))
                       rendered)]
          (ok (str (str/join sep padded) (when (seq padded) "\n"))))
        (catch #?(:clj Throwable :cljs :default) t
          (usage-err "seq" (str "invalid floating point argument: "
                                (.getMessage t))))))))

;; ============================================================================
;; test / [ — file-and-string predicates
;; ============================================================================

(defn- test-eval
  "Evaluate a parsed test/[ argv. Returns true/false. Throws on
   malformed expression."
  [args fs]
  (let [n (count args)]
    (cond
      (zero? n) false
      ;; Single arg: true iff non-empty
      (= 1 n)
      (not (= "" (first args)))

      ;; Two args: -OP operand
      (= 2 n)
      (let [[op a] args]
        (case op
          "!"  (not (test-eval [a] fs))
          "-z" (= "" a)
          "-n" (not (= "" a))
          "-e" (boolean (fs/exists? fs a))
          "-f" (= :file (:type (fs/stat fs a)))
          "-d" (= :dir  (:type (fs/stat fs a)))
          "-h" (= :symlink (:type (fs/stat fs a)))
          "-L" (= :symlink (:type (fs/stat fs a)))
          "-r" (boolean (fs/exists? fs a))   ;; perms ignored in v1
          "-w" (boolean (fs/exists? fs a))
          "-x" (boolean (fs/exists? fs a))
          "-s" (let [s (fs/stat fs a)]
                 (and s (pos? (or (:size s) 0))))
          (throw (ex-info (str "test: unary op required, got: " op) {}))))

      ;; Three args: a OP b
      (= 3 n)
      (let [[a op b] args]
        (case op
          "="   (= a b)
          "=="  (= a b)
          "!="  (not= a b)
          "-eq" (= (parse-long a) (parse-long b))
          "-ne" (not= (parse-long a) (parse-long b))
          "-lt" (< (parse-long a) (parse-long b))
          "-le" (<= (parse-long a) (parse-long b))
          "-gt" (> (parse-long a) (parse-long b))
          "-ge" (>= (parse-long a) (parse-long b))
          "-a"  (and (test-eval [a] fs) (test-eval [b] fs))
          "-o"  (or (test-eval [a] fs) (test-eval [b] fs))
          (throw (ex-info (str "test: unknown binary op: " op) {}))))

      :else
      (throw (ex-info (str "test: too many arguments (got " n ")") {})))))

(defn test-fn
  "POSIX test / `[`. Exit 0 = true, 1 = false, 2 = malformed.

   Supports the common subset:
     -z STR / -n STR
     -e/-f/-d/-h/-r/-w/-x/-s FILE
     STR = STR  / STR == STR / STR != STR
     INT -eq -ne -lt -le -gt -ge INT
     EXPR -a EXPR / EXPR -o EXPR / ! EXPR

   Invoked as `[ ... ]` muschel.exec strips the closing `]` for us."
  [argv fs _env]
  (let [args (vec (rest argv))
        ;; `[ ... ]` form: drop the trailing literal `]`
        args (if (and (= "[" (first argv)) (= "]" (peek args)))
               (pop args)
               args)]
    (try
      (if (test-eval args fs)
        {:stdout "" :stderr "" :exit 0}
        {:stdout "" :stderr "" :exit 1})
      (catch #?(:clj Throwable :cljs :default) t
        (usage-err (or (first argv) "test") (.getMessage t))))))

;; ============================================================================
;; sed — basic substitution + simple addresses
;; ============================================================================

(defn- sed-parse-single-addr
  "Parse one address token from the start of `s`. Returns `[addr rest]`
   where `addr` is `{:type :line :n N}`, `{:type :last}`, or
   `{:type :regex :pat P}`. If no address matches, returns `[nil s]`."
  [^String s]
  (cond
    (re-find #"^(\d+)" s)
    (let [[_ n] (re-find #"^(\d+)" s)]
      [{:type :line :n (parse-long n)} (subs s (count n))])
    (str/starts-with? s "$")
    [{:type :last} (subs s 1)]
    (str/starts-with? s "/")
    (let [end (.indexOf s "/" 1)]
      (if (neg? end)
        [nil s]
        [{:type :regex :pat (subs s 1 end)} (subs s (inc end))]))
    :else [nil s]))

(defn- parse-sed-script
  "Translate a sed script string into a vector of `{:addr ... :op ...}`
   commands. Supported ops: s (substitute), d (delete), p (print).
   Addresses can be:
     N           line N
     $           last line
     /REGEX/     pattern match
     N,M / N,$ / /PAT/,/PAT/  range (inclusive)
     (none)      always"
  [^String script]
  (let [scripts (str/split script #";")]
    (mapv
     (fn [s]
       (let [s (str/triml s)
             [from rest-s] (sed-parse-single-addr s)
             ;; If a comma follows, parse the second address.
             [addr rest-s]
             (if (and from (str/starts-with? rest-s ","))
               (let [[to rest-s'] (sed-parse-single-addr (subs rest-s 1))]
                 [{:type :range :from from :to to} (str/triml rest-s')])
               [from (str/triml rest-s)])
             op (first rest-s)]
         (case op
           \s
           (let [sep (.charAt ^String rest-s 1)
                 ;; Split on the separator (which is char-2). e.g. `s/A/B/g`
                 parts (str/split (subs rest-s 2) (re-pattern (cc/re-quote (str sep))) 3)]
             {:addr addr :op :s
              :pat   (first parts)
              :repl  (second parts)
              :flags (or (nth parts 2 nil) "")})
           \d {:addr addr :op :d}
           \p {:addr addr :op :p}
           {:addr addr :op :unknown :raw rest-s})))
     scripts)))

(defn- sed-single-addr-match? [{:keys [type n pat]} line line-idx total]
  (case type
    nil      true
    :line    (= n line-idx)
    :last    (= line-idx total)
    :regex   (boolean (re-find (re-pattern pat) line))))

(defn- sed-addr-match?
  "True if `addr` (single or range) matches the current line.

   Range matching is *stateful* for pattern ranges (`/a/,/b/`) — once
   `/a/` matches we're inside the range until `/b/` matches (inclusive).
   Numeric ranges (`N,M`) are stateless. The caller passes an atom
   `in-range?-state` keyed by command index for stateful tracking."
  [addr line line-idx total cmd-idx in-range-state]
  (case (:type addr)
    nil      true
    :line    (= (:n addr) line-idx)
    :last    (= line-idx total)
    :regex   (boolean (re-find (re-pattern (:pat addr)) line))
    :range
    (let [{:keys [from to]} addr]
      ;; Numeric: simple bounds check.
      (cond
        (and (= :line (:type from)) (= :line (:type to)))
        (and (>= line-idx (:n from)) (<= line-idx (:n to)))

        (and (= :line (:type from)) (= :last (:type to)))
        (>= line-idx (:n from))

        :else
        ;; Pattern-based or mixed: track per-cmd state.
        (let [active? (get @in-range-state cmd-idx false)
              start?  (sed-single-addr-match? from line line-idx total)
              end?    (sed-single-addr-match? to   line line-idx total)]
          (cond
            (and (not active?) start?)
            (do (swap! in-range-state assoc cmd-idx (not end?))
                true)
            active?
            (do (when end? (swap! in-range-state assoc cmd-idx false))
                true)
            :else false))))))

(defn- sed-do-substitute [line {:keys [pat repl flags]}]
  (let [flags (or flags "")
        case-insensitive? (str/includes? flags "i")
        global?           (str/includes? flags "g")
        ;; `s/PAT/REPL/N` (N a digit) replaces only the Nth match.
        nth-target (when-let [m (re-find #"\d+" flags)]
                     #?(:clj (parse-long m) :cljs (js/parseInt m 10)))
        ;; Case-insensitivity via `(?i)` inline flag (cross-platform).
        body (if case-insensitive? (str "(?i)" pat) pat)
        re   (cc/re-compile body)]
    (cond
      nth-target
      ;; Nth-only substitution: walk matches, replace the nth.
      (let [[out _] (cc/re-replace re line
                                   (let [cnt (volatile! 0)]
                                     (fn [matched]
                                       (let [n (vswap! cnt inc)]
                                         (if (= n nth-target) repl matched))))
                                   true)]
        out)

      global?  (first (cc/re-replace re line (fn [_] repl) true))
      :else    (first (cc/re-replace re line (fn [_] repl) false)))))

(defn sed
  "POSIX sed, subset:
     s/PAT/REPL/[gi]            substitute
     /PAT/d   N d   $ d         delete line
     /PAT/p   N p   $ p         print line (with -n: only matches)
   Flags: -n quiet, -e SCRIPT (repeatable), -i (in-place)."
  [argv fs env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-n" "--quiet"]
                         ["-e" "--expression SCRIPT"
                          :assoc-fn (fn [m k v] (update m k (fnil conj []) v))
                          :default  []]
                         ["-i" "--in-place"]])]
    (cond
      err (usage-err "sed" err)
      :else
      (let [e-scripts (:expression opts)
            [script-s files]
            (if (seq e-scripts)
              [(str/join ";" e-scripts) pos]
              (if (seq pos)
                [(first pos) (rest pos)]
                ["" []]))
            cmds (parse-sed-script script-s)
            stdin (or (:stdin env) "")
            stderr (volatile! "")
            any-err? (volatile! false)
            process-text
            (fn [^String content]
              (let [lines (str/split-lines content)
                    total (count lines)
                    out (cc/sbuf)
                    ;; Per-command range state for pattern ranges
                    ;; (/a/,/b/). Keyed by cmd-idx.
                    range-state (atom {})]
                (loop [i 0]
                  (when (< i total)
                    (let [orig (nth lines i)
                          line (volatile! orig)
                          deleted? (volatile! false)
                          force-print? (volatile! false)]
                      (doseq [[cmd-idx cmd] (map-indexed vector cmds)
                              :when (not @deleted?)]
                        (when (sed-addr-match? (:addr cmd) @line (inc i)
                                               total cmd-idx range-state)
                          (case (:op cmd)
                            :s (vreset! line (sed-do-substitute @line cmd))
                            :d (vreset! deleted? true)
                            :p (vreset! force-print? true)
                            nil)))
                      (when (or @force-print?
                                (and (not (:quiet opts)) (not @deleted?)))
                        (.append out ^String @line)
                        (.append out "\n")))
                    (recur (inc i))))
                (.toString out)))]
        (if (seq files)
          (let [results
                (mapv
                 (fn [f]
                   (if-let [c (fs/read-file fs f)]
                     (let [out (process-text c)]
                       (when (:in-place opts)
                         (fs/write-string! fs f out false))
                       (if (:in-place opts) nil out))
                     (do (vswap! stderr str "sed: " f ": No such file or directory\n")
                         (vreset! any-err? true)
                         nil)))
                 files)]
            {:stdout (apply str (remove nil? results))
             :stderr @stderr
             :exit (if @any-err? 1 0)})
          (ok (process-text stdin)))))))

;; ============================================================================
;; awk — minimal subset: `{print …}` / `/PAT/ {…}` / `NR==N`
;; ============================================================================

(defn- awk-split-line [^String line ^String fs-pat]
  (cond
    (= "" line) [""]
    (= " " fs-pat)
    (vec (str/split (str/trim line) #"\s+"))
    :else
    (vec (str/split line (re-pattern (cc/re-quote fs-pat))))))

(defn- awk-field [fields ^long n]
  (cond
    (zero? n) (str/join " " fields)
    (or (neg? n) (> n (count fields))) ""
    :else (nth fields (dec n))))

(defn- awk-eval-action
  "Tiny action evaluator. Supports `print` with comma-separated
   $N | $0 | NR | NF | string-literal | int-literal args."
  [action {:keys [fields nr nf out]}]
  (let [action (str/trim action)]
    (when (str/starts-with? action "print")
      (let [args-s (str/trim (subs action 5))
            args   (if (= "" args-s)
                     [(str/join " " fields)]
                     (mapv str/trim (str/split args-s #",")))
            tokens
            (mapv (fn [a]
                    (cond
                      (re-matches #"\$\d+" a)
                      (awk-field fields (parse-long (subs a 1)))
                      (= "NR" a) (str nr)
                      (= "NF" a) (str nf)
                      (re-matches #"\".*\"" a) (subs a 1 (dec (count a)))
                      (re-matches #"\d+" a) a
                      :else a))
                  args)]
        (cc/sappend! out (str/join " " tokens))
        (cc/sappend! out "\n")))))

(defn- awk-parse-program [^String prog]
  "Split program into pattern/action rules. Rule shape:
     {:pattern :all | {:regex ...} | {:nr-eq N} | nil
      :action  STRING (the inside-{}-text)}
   With no `{ }` the implicit action is `{print}`."
  (let [prog (str/trim prog)
        rules
        (loop [s prog acc []]
          (let [s (str/triml s)]
            (cond
              (empty? s) acc

              (str/starts-with? s "/")
              (let [end (.indexOf s "/" 1)]
                (if (neg? end)
                  acc
                  (let [pat (subs s 1 end)
                        rest-s (str/triml (subs s (inc end)))]
                    (if (str/starts-with? rest-s "{")
                      (let [close (.indexOf rest-s "}")]
                        (recur (subs rest-s (inc close))
                               (conj acc {:pattern {:regex pat}
                                          :action (subs rest-s 1 close)})))
                      (recur rest-s
                             (conj acc {:pattern {:regex pat}
                                        :action "print"}))))))

              (str/starts-with? s "{")
              (let [close (.indexOf s "}")]
                (recur (subs s (inc close))
                       (conj acc {:pattern :all
                                  :action (subs s 1 close)})))

              (re-find #"^NR==\d+" s)
              (let [[_ n] (re-find #"^NR==(\d+)" s)
                    rest-s (str/triml (subs s (count (str "NR==" n))))]
                (if (str/starts-with? rest-s "{")
                  (let [close (.indexOf rest-s "}")]
                    (recur (subs rest-s (inc close))
                           (conj acc {:pattern {:nr-eq (parse-long n)}
                                      :action (subs rest-s 1 close)})))
                  (recur rest-s
                         (conj acc {:pattern {:nr-eq (parse-long n)}
                                    :action "print"}))))

              :else
              ;; Skip unrecognised input rather than crash.
              (recur "" acc))))]
    rules))

(defn- awk-pattern-match? [pattern line nr]
  (cond
    (= :all pattern) true
    (nil? pattern) true
    (:regex pattern) (boolean (re-find (re-pattern (:regex pattern)) line))
    (:nr-eq pattern) (= (:nr-eq pattern) nr)
    :else false))

(defn awk
  "POSIX awk, bounded subset. Dispatches to muschel.builtins.awk.

   Supported:
     BEGIN / END, pattern { action } rules incl. pat1,pat2 ranges
     $0 $N NR NF FS OFS ORS RS RSTART RLENGTH FILENAME
     arithmetic + - * / % ^ **; comparison == != < <= > >=
     logical && || !; string concat by juxtaposition
     regex match ~ !~; assignment = += -= *= /= %= ^= **=
     pre/post inc/dec; ternary ?:
     if/else, while, do/while, for(;;), for(var in array)
     break / continue / next / exit; delete a[k] / delete a
     print (OFS-joined, ORS-terminated), printf (%d %s %x %o %c %% etc.)
     length(), substr(), index(), split(), sub(), gsub(),
     sprintf(), match(), tolower(), toupper(), int()
     sqrt, exp, log, sin, cos, atan2
     single-dim associative arrays
     -v VAR=val, -F SEP, -f FILE

   Not supported: user-defined functions, getline, system(),
   redirects (> >> |), multi-dim arrays via SUBSEP."
  [argv fs env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-F" "--field-separator SEP" :default " "]
                         ["-v" "--var KV"
                          :assoc-fn (fn [m k v] (update m k (fnil conj []) v))
                          :default []]
                         ["-f" "--file FILE"]])]
    (cond
      err (usage-err "awk" err)
      :else
      (let [;; -f FILE → program from file; otherwise first positional.
            program (cond
                      (:file opts) (or (fs/read-file fs (:file opts))
                                       (throw (ex-info (str "awk: " (:file opts)
                                                            ": No such file or directory") {})))
                      (seq pos)    (first pos)
                      :else        nil)
            files   (if (:file opts) pos (rest pos))
            stdin   (or (:stdin env) "")
            ;; Pass raw text — awk-impl/run splits it internally by
            ;; the current RS (which may have been changed in BEGIN).
            raw     (cond
                      (seq files)
                      (str/join "\n"
                                (keep (fn [f] (fs/read-file fs f)) files))
                      :else stdin)
            kv-vars (into {}
                          (for [kv (:var opts)
                                :let [idx (.indexOf ^String kv "=")]
                                :when (pos? idx)]
                            [(subs kv 0 idx) (subs kv (inc idx))]))]
        (cond
          (nil? program) (usage-err "awk" "missing program")
          :else
          (try
            (let [result (awk-impl/run {:program      program
                                        :raw-input    raw
                                        :fs           (:field-separator opts)
                                        :vars         kv-vars
                                        :interrupt-fn (:interrupt-fn env)})]
              {:stdout (:stdout result)
               :stderr ""
               :exit   (:exit result)})
            (catch #?(:clj Throwable :cljs :default) t
              ;; Let budget interrupts propagate up to run-and-capture.
              (when (and (instance? #?(:clj clojure.lang.ExceptionInfo
                                       :cljs cljs.core/ExceptionInfo) t)
                         (:muschel/budget (ex-data t)))
                (throw t))
              (err (str "awk: " #?(:clj (.getMessage t)
                                   :cljs (or (ex-message t) (str t)))) 2))))))))

;; ============================================================================
;; jq — JSON projection & filtering
;;
;; A practical subset of jq, useful for what agents actually do with
;; JSON output (project a field out of an API response, iterate an
;; array, count length). Built on org.clojure/data.json so containment
;; is preserved (no real-disk reads except through fs/read-file).
;;
;; Supported filters:
;;   .                    identity
;;   .key                 object field
;;   .key.nested          chained
;;   .[N]                 array index
;;   .[]                  iterate (each elem becomes a separate output)
;;   .[K:M]               array slice
;;   length               array / object / string length
;;   keys                 object keys (sorted)
;;   values               object values
;;   type                 \"string\" \"number\" \"object\" \"array\" \"boolean\" \"null\"
;;   first / last         array first / last element
;;   |                    pipe (compose)
;; ============================================================================

(declare jq-apply-filter)

(defn- jq-tokenize-pipeline
  "Split a jq filter expression on `|` at top level (not inside `[...]`
   or `(...)`). Returns a vector of substring filters."
  [^String s]
  (let [out (cc/sbuf)
        result (transient [])
        n (count s)
        depth (volatile! 0)]
    (loop [i 0]
      (when (< i n)
        (let [c (.charAt s i)]
          (cond
            (or (= c \[) (= c \()) (do (cc/sappend! out c) (vswap! depth inc))
            (or (= c \]) (= c \))) (do (cc/sappend! out c) (vswap! depth dec))
            (and (= c \|) (zero? @depth))
            (do (conj! result (str/trim (cc/sbstr out)))
                (cc/sbclear! out))
            :else (cc/sappend! out c)))
        (recur (inc i))))
    (conj! result (str/trim (cc/sbstr out)))
    (vec (persistent! result))))

(defn- jq-step
  "Apply one chained step (e.g. `.field` or `[N]` or `[]`) to a value.
   Returns either a single value, or `[:multi v1 v2 …]` for `[]` which
   fans out."
  [v ^String step]
  (cond
    ;; Bare `.` is the identity-step that appears whenever a filter
    ;; starts with `.` (the chain splitter emits it as its first
    ;; element). Pass through.
    (= step ".") v

    (= step "[]")
    (cond
      (sequential? v) (into [:multi] v)
      (map? v)        (into [:multi] (vals v))
      :else           nil)

    (re-matches #"\[\d+\]" step)
    (let [n (parse-long (subs step 1 (dec (count step))))]
      (when (sequential? v) (nth v n nil)))

    ;; Full slice `.[a:b]` plus open-ended `.[a:]` / `.[:b]`.
    (re-matches #"\[\d*:\d*\]" step)
    (let [[_ a b] (re-find #"\[(\d*):(\d*)\]" step)
          n  (when (sequential? v) (count v))
          aa (if (str/blank? a) 0 (parse-long a))
          bb (if (str/blank? b) (or n 0) (parse-long b))]
      (when n
        (subvec (vec v) (min aa n) (min bb n))))

    ;; .field — strip the leading dot if present
    :else
    (let [k (cond-> step (str/starts-with? step ".") (subs 1))]
      (when (and v (map? v)) (get v k)))))

(defn- jq-split-steps
  "Break `.foo.bar[0].baz[]` into [\".foo\" \".bar\" \"[0]\" \".baz\" \"[]\"]."
  [^String s]
  (loop [i 0 acc (transient []) buf (cc/sbuf)]
    (cond
      (>= i (count s))
      (let [tail (cc/sbstr buf)]
        (cond-> (persistent! acc)
          (seq tail) (conj tail)))

      (= \. (.charAt s i))
      (let [tail (cc/sbstr buf)
            acc' (cond-> acc (seq tail) (conj! tail))]
        (cc/sbclear! buf)
        (cc/sappend! buf \.)
        (recur (inc i) acc' buf))

      (= \[ (.charAt s i))
      (let [tail (cc/sbstr buf)
            acc' (cond-> acc (seq tail) (conj! tail))
            close (.indexOf s "]" i)
            chunk (subs s i (inc close))]
        (cc/sbclear! buf)
        (recur (inc close) (conj! acc' chunk) buf))

      :else
      (do (cc/sappend! buf (.charAt s i))
          (recur (inc i) acc buf)))))

(defn- jq-builtin-fn [^String fname]
  (case fname
    "length" (fn [v]
               (cond (string? v) (count v)
                     (sequential? v) (count v)
                     (map? v) (count v)
                     :else 0))
    "keys"   (fn [v] (when (map? v) (vec (sort (map name (keys v))))))
    "values" (fn [v] (when (map? v) (vec (vals v))))
    "type"   (fn [v]
               (cond (nil? v) "null"
                     (boolean? v) "boolean"
                     (number? v) "number"
                     (string? v) "string"
                     (sequential? v) "array"
                     (map? v) "object"
                     :else "object"))
    "first"  (fn [v] (when (sequential? v) (first v)))
    "last"   (fn [v] (when (sequential? v) (last v)))
    nil))

(defn- jq-apply-filter
  "Apply a single (no-pipe) filter string to v. Returns a vector of
   results (jq can produce multiple outputs from a single input)."
  [v ^String f]
  (let [f (str/trim f)]
    (cond
      (= "." f) [v]

      ;; Builtin function name
      (jq-builtin-fn f)
      [((jq-builtin-fn f) v)]

      ;; Chain of .field / [N] / [] steps
      :else
      (let [steps (jq-split-steps f)]
        (loop [vs [v] [step & more] steps]
          (if (nil? step)
            vs
            (let [vs' (->> vs
                           (mapcat (fn [v]
                                     (let [r (jq-step v step)]
                                       (cond
                                         (and (vector? r) (= :multi (first r))) (rest r)
                                         :else [r])))))]
              (recur (vec vs') more))))))))

(defn- jq-run-pipeline
  "Apply a `|`-piped filter expression to a parsed JSON value."
  [v ^String expr]
  (let [filters (jq-tokenize-pipeline expr)]
    (reduce
     (fn [vs f]
       (vec (mapcat #(jq-apply-filter % f) vs)))
     [v]
     filters)))

(defn- jq-format
  "Render a value as JSON output. Honours -c (compact) and -r (raw —
   strip surrounding quotes on string outputs).

   JVM uses `clojure.data.json` (resolved at call-time to avoid a
   compile-time require cycle); babashka uses its built-in
   `cheshire.core`; CLJS uses `JSON.stringify`."
  [v {:keys [compact raw]}]
  #?(:bb
     (do (require 'cheshire.core)
         (let [gen (resolve 'cheshire.core/generate-string)]
           (cond
             (and raw (string? v)) v
             compact (gen v)
             :else (gen v {:pretty true}))))
     :clj
     (do (require 'clojure.data.json)
         (let [json-write (resolve 'clojure.data.json/write-str)]
           (cond
             (and raw (string? v)) v
             compact (json-write v)
             :else (json-write v :indent true))))
     :cljs
     (cond
       (and raw (string? v)) v
       compact (.stringify js/JSON (clj->js v))
       :else (.stringify js/JSON (clj->js v) nil 2))))

(defn- jq-parse
  "Parse the input text into a vector of JSON values. JVM uses
   `clojure.data.json` (handles multi-value whitespace-separated
   streams); babashka uses `cheshire.core/parse-string` (single
   value); CLJS uses `js/JSON.parse` (single value)."
  [^String input-text]
  (if (str/blank? input-text)
    []
    #?(:bb
       (do (require 'cheshire.core)
           (let [parse (resolve 'cheshire.core/parse-string)]
             [(parse input-text)]))
       :clj
       (do (require 'clojure.data.json)
           (let [read-fn (resolve 'clojure.data.json/read)
                 pbr (java.io.PushbackReader.
                      (java.io.StringReader. input-text) 64)]
             (loop [acc []]
               (let [v (read-fn pbr :eof-error? false :eof-value ::eof)]
                 (if (= ::eof v) acc (recur (conj acc v)))))))
       :cljs
       [(js->clj (.parse js/JSON input-text) :keywordize-keys false)])))

(defn jq
  "Practical jq subset: `.`, `.field`, `.[N]`, `.[]`, `.[A:B]`,
   `length`, `keys`, `values`, `type`, `first`, `last`, and `|`
   composition. Reads JSON from stdin or files."
  [argv fs env]
  (let [{:keys [opts pos err]}
        (cli-parse argv [["-c" "--compact-output"]
                         ["-r" "--raw-output"]
                         ["-s" "--slurp"]
                         ["-n" "--null-input"]])]
    (cond
      err (usage-err "jq" err)
      (empty? pos) (usage-err "jq" "missing filter")
      :else
      (let [expr  (first pos)
            files (rest pos)
            stdin (or (:stdin env) "")
            input-text (cond
                         (:null-input opts) "null"
                         (seq files)
                         (str/join "\n" (for [f files] (or (fs/read-file fs f) "")))
                         :else stdin)
            parsed (try (jq-parse input-text)
                        (catch #?(:clj Throwable :cljs :default) t
                          (throw (ex-info (str "jq: parse error: "
                                               #?(:clj (.getMessage t)
                                                  :cljs (.-message t))) {}))))
            parsed (if (:slurp opts) [parsed] parsed)
            fmt-opts {:compact (:compact-output opts)
                      :raw     (:raw-output opts)}
            outs (mapcat #(jq-run-pipeline % expr) parsed)
            sb (cc/sbuf)]
        (doseq [o outs]
          (cc/sappend! sb (jq-format o fmt-opts))
          (cc/sappend! sb "\n"))
        (ok (cc/sbstr sb))))))

;; ============================================================================
;; Network: curl
;;
;; A pragmatic curl built on java.net.http (JDK 11+). Output goes
;; through the FS handle when -o FILE is requested, so writes stay
;; contained inside the muschel root — even though the network call
;; itself reaches the open internet.
;; ============================================================================

(defn- curl-method
  "Pick the HTTP method from flags. Defaults to GET; -X overrides;
   -d / --data implies POST unless -X was given."
  [{:keys [request data]}]
  (cond
    request                    request
    data                       "POST"
    :else                      "GET"))

#?(:clj
   (defn- curl-build-request
     ^java.net.http.HttpRequest
     [^String url {:keys [request data headers user] :as opts}]
     (let [b (.. (java.net.http.HttpRequest/newBuilder)
                 (uri (java.net.URI/create url)))
           method (curl-method opts)
           body-publisher (if data
                            (java.net.http.HttpRequest$BodyPublishers/ofString
                             ^String data)
                            (java.net.http.HttpRequest$BodyPublishers/noBody))]
       (.method b ^String method body-publisher)
       (doseq [h (or headers [])]
         (let [idx (.indexOf ^String h ":")]
           (when (pos? idx)
             (.header b (str/trim (subs h 0 idx)) (str/trim (subs h (inc idx)))))))
       (when user
         (let [enc (.encodeToString (java.util.Base64/getEncoder)
                                    (.getBytes ^String user "UTF-8"))]
           (.header b "Authorization" (str "Basic " enc))))
       (.build b))))

(defn curl
  "POSIX-ish curl. Subset:
     -X METHOD                custom HTTP method
     -d DATA / --data DATA    request body (implies POST)
     -H 'Header: value'       custom header (repeatable)
     -o FILE                  write body to FILE (via FS handle)
     -O                       use the URL's basename as FILE
     -L / --location          follow redirects (on by default)
     -s / --silent            no progress (always on; we don't print one)
     -i / --include           include status + response headers
     -f / --fail              non-zero exit on HTTP error status
     -u USER:PASS             basic auth

   The URL is the last positional argument. Output goes to stdout
   unless -o / -O is given.

   Currently JVM-only. CLJS port to come (Node fetch / browser fetch);
   bb port via `babashka.http-client`."
  [argv fs _env]
  #?(:clj
     (let [{:keys [opts pos err]}
           (cli-parse argv [["-X" "--request METHOD"]
                            ["-d" "--data DATA"]
                            ["-H" "--header H"
                             :assoc-fn (fn [m k v] (update m k (fnil conj []) v))
                             :default []]
                            ["-o" "--output FILE"]
                            ["-O" "--remote-name"]
                            ["-L" "--location"]
                            ["-s" "--silent"]
                            ["-i" "--include"]
                            ["-f" "--fail"]
                            ["-u" "--user USER:PASS"]])]
       (cond
         err (usage-err "curl" err)
         (empty? pos) (usage-err "curl" "no URL specified")
         :else
         (let [url     (last pos)
               client  (.. (java.net.http.HttpClient/newBuilder)
                           (followRedirects java.net.http.HttpClient$Redirect/NORMAL)
                           build)
               req     (curl-build-request url opts)
               resp    (try
                         (.send client req
                                (java.net.http.HttpResponse$BodyHandlers/ofByteArray))
                         (catch Throwable t
                           (vary-meta {} assoc ::err (.getMessage t))))]
           (if-let [errmsg (::err (meta resp))]
             (err (str "curl: (6) " errmsg) 6)
             (let [status (.statusCode ^java.net.http.HttpResponse resp)
                   body   ^bytes (.body ^java.net.http.HttpResponse resp)
                   hdr-out (when (:include opts)
                             (let [hs (.headers ^java.net.http.HttpResponse resp)
                                   version (.version ^java.net.http.HttpResponse resp)
                                   v-str (case (.name ^java.net.http.HttpClient$Version version)
                                           "HTTP_1_1" "HTTP/1.1"
                                           "HTTP_2"   "HTTP/2"
                                           "HTTP/1.1")]
                               (str v-str " " status "\r\n"
                                    (str/join
                                     ""
                                     (for [[k vs] (.map hs)
                                           v vs]
                                       (str k ": " v "\r\n")))
                                    "\r\n")))
                   target (or (:output opts)
                              (when (:remote-name opts)
                                (last (str/split url #"/"))))
                   fail?  (and (:fail opts) (>= status 400))]
               (cond
                 fail?
                 (err (str "curl: (22) The requested URL returned error: " status) 22)

                 target
                 (if (fs/write-string! fs target (String. body "UTF-8") false)
                   {:stdout (or hdr-out "") :stderr "" :exit 0}
                   (err (str "curl: cannot write to '" target "'") 23))

                 :else
                 {:stdout (str (or hdr-out "")
                               (String. body "UTF-8"))
                  :stderr ""
                  :exit 0}))))))
     :cljs
     (err "curl: HTTP client not yet ported to CLJS" 1)))

;; ============================================================================
;; sleep
;; ============================================================================

(defn- parse-duration
  "Parse `5m`, `1h`, `100ms`, `2.5s`, plain `30` (seconds) into ms.
   Returns nil on malformed input."
  [^String s]
  (try
    (let [[_ num-s unit] (re-matches #"(?i)^\s*([0-9.]+)\s*(ms|s|m|h|d)?\s*$" s)]
      (when num-s
        (let [n (parse-double num-s)
              mul (case (str/lower-case (or unit "s"))
                    "ms" 1
                    "s"  1000
                    "m"  60000
                    "h"  3600000
                    "d"  86400000)]
          (long (* n mul)))))
    (catch #?(:clj Throwable :cljs :default) _ nil)))

(defn sleep
  "POSIX sleep. Accepts integer or decimal seconds; GNU-style suffix
   durations (`5m`, `1h`, `100ms`, `2.5s`, `1d`); and multiple args
   which sum together (per GNU)."
  [argv _fs _env]
  (let [args (rest argv)]
    (cond
      (empty? args) (usage-err "sleep" "missing operand")
      :else
      (let [parsed (mapv parse-duration args)]
        (if (some nil? parsed)
          (usage-err "sleep" (str "invalid time interval: " (nth args (.indexOf parsed nil))))
          (let [ms (long (reduce + 0 parsed))]
            #?(:clj (Thread/sleep ms)
               :cljs
               ;; CLJS lacks synchronous sleep. We busy-wait — only
               ;; acceptable for the tiny budgets agent scripts use
               ;; (typically `sleep 0.1` between polls). Callers
               ;; needing real async should not use this builtin.
               (let [deadline (+ (.now js/Date) ms)]
                 (while (< (.now js/Date) deadline) nil)))
            {:stdout "" :stderr "" :exit 0}))))))

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
   rmdir, rm, cp, mv, chmod, ln, tee), plus text/path tools (sed, awk,
   printf, env, date, seq, basename, dirname, realpath, test/[).
   All routed through the FS for containment."
  (merge standard-read-only
         {"touch"    touch
          "mkdir"    mkdir
          "rmdir"    rmdir
          "rm"       rm
          "cp"       cp
          "mv"       mv
          "chmod"    chmod
          "chown"    chown
          "ln"       ln
          "tee"      tee
          ;; text + path
          "sed"      sed
          "awk"      awk
          "printf"   printf
          "env"      env-fn
          "date"     date-fn
          "seq"      seq-fn
          "basename" basename
          "dirname"  dirname
          "realpath" realpath
          ;; data
          "jq"       jq
          ;; network + timing
          "curl"     curl
          "sleep"    sleep}))
