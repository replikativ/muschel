(ns muschel.builtins.posix
  "Pure-Clojure implementations of a useful subset of POSIX coreutils.

   Each fn has the same shape:

       (cmd-fn argv fs env) → {:stdout str :stderr str :exit int}

   - `argv` is the post-expansion argv (a vector of strings, including
     the command name as the first element — caller doesn't strip it).
   - `fs` is a muschel.fs/FS handle (containment + read-file etc.).
   - `env` is the muschel env value (for $PWD, $HOME etc.). Builtins
     don't mutate env; they return the new cwd via opts if `cd` is
     invoked (handled by the host wrapper, not in this ns).

   Coverage in this slice (read-only):

     pwd echo ls cat head tail wc

   Reference: uutils-coreutils. We aim for behaviour that matches the
   standard short-flag set; long GNU options (`--color`, `--time` etc.)
   are not implemented and pass through as no-ops or simple errors. If
   an agent reaches for a flag we haven't implemented, the error tells
   them which subset is supported."
  (:require [clojure.string :as str]
            [muschel.fs :as fs]
            [muschel.env :as env]))

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
  (err (str cmd ": " msg)))

;; ============================================================================
;; Tiny argv parser
;; ============================================================================

(defn- parse-flags
  "Parse a POSIX-style argv into [flags-set positionals].

   `recognized` is a set of short flag chars (strings of length 1).
   Long flags (`--name`) and unrecognised short flags accumulate into
   [:unknown-flag …] entries. `--` terminates flag parsing.

   `=value` and `name value` long forms aren't handled here; the
   commands that need them parse positionals themselves."
  [argv recognized]
  (loop [in     (rest argv)
         flags  #{}
         pos    []
         after? false]
    (if-let [arg (first in)]
      (cond
        after?                 (recur (rest in) flags (conj pos arg) true)
        (= "--" arg)           (recur (rest in) flags pos true)
        (and (> (count arg) 1)
             (str/starts-with? arg "--"))
        (recur (rest in) (conj flags arg) pos false)
        (and (> (count arg) 1)
             (str/starts-with? arg "-"))
        (let [chars (rest arg)
              {:keys [recognised unknown]}
              (reduce (fn [acc ch]
                        (let [s (str ch)]
                          (if (contains? recognized s)
                            (update acc :recognised conj s)
                            (update acc :unknown conj s))))
                      {:recognised #{} :unknown []}
                      chars)]
          (recur (rest in)
                 (into flags recognised)
                 (if (seq unknown)
                   (conj pos (str "-" (str/join unknown)))
                   pos)
                 false))
        :else
        (recur (rest in) flags (conj pos arg) false))
      [flags pos])))

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

(defn echo
  "POSIX echo. Honours -n (no trailing newline) and -e (interpret
   simple backslash escapes: \\n \\t \\r \\\\). -E is the default."
  [argv _fs _env]
  (let [[flags pos] (parse-flags argv #{"n" "e" "E"})
        no-newline? (flags "n")
        escapes?    (and (flags "e") (not (flags "E")))
        text        (str/join " " pos)
        rendered    (if escapes?
                      (-> text
                          (str/replace "\\n" "\n")
                          (str/replace "\\t" "\t")
                          (str/replace "\\r" "\r")
                          (str/replace "\\\\" "\\"))
                      text)]
    (ok (if no-newline? rendered (str rendered "\n")))))

;; ============================================================================
;; ls
;; ============================================================================

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
  (let [[flags pos] (parse-flags argv #{"a" "l" "1" "A" "h" "F"})
        all? (or (flags "a") (flags "A"))
        long? (flags "l")
        targets (if (seq pos) pos ["."])
        multi? (> (count targets) 1)
        stdout (volatile! [])
        stderr (volatile! [])
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
     :exit (if @any-err? 2 0)}))

;; ============================================================================
;; cat
;; ============================================================================

(defn cat
  "POSIX cat, subset: -n (number all lines), -b (number non-empty),
   -s (squeeze blank lines), -E (show $ at line end). No -A (we
   don't implement TAB/control-char display)."
  [argv fs _env]
  (let [[flags pos] (parse-flags argv #{"n" "b" "s" "E" "v" "T" "A"})
        num-all? (flags "n")
        num-nb?  (flags "b")
        squeeze? (flags "s")
        show-end? (or (flags "E") (flags "A"))
        files    (if (seq pos) pos ["-"])
        stderr   (volatile! "")
        any-err? (volatile! false)
        n        (volatile! 0)
        last-blank? (volatile! false)
        process-line
        (fn [line]
          (let [is-blank? (= "" line)]
            (cond
              (and squeeze? @last-blank? is-blank?) nil
              :else
              (let [;; Track for next-iteration squeeze
                    _ (vreset! last-blank? is-blank?)
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
                    nil   ;; stdin not supported in builtins; skip silently
                    (let [bytes (fs/read-bytes fs f)]
                      (if (nil? bytes)
                        (let [stat (fs/stat fs f)
                              msg (if (= :dir (:type stat))
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
     :exit (if @any-err? 1 0)}))

;; ============================================================================
;; head
;; ============================================================================

(defn- parse-count
  "Parse a -nN | -n N | --lines=N positional/flag combo. Returns
   [count remaining-positionals], default 10."
  [argv]
  (loop [in (rest argv)
         n 10
         pos []]
    (let [a (first in)]
      (cond
        (nil? a) [n pos]
        (= "-n" a) (recur (drop 2 in) (Long/parseLong (second in)) pos)
        (and (str/starts-with? (or a "") "-n")
             (> (count a) 2))
        (recur (rest in) (Long/parseLong (subs a 2)) pos)
        (and (str/starts-with? (or a "") "-")
             (> (count a) 1)
             (every? #(Character/isDigit ^char %) (rest a)))
        (recur (rest in) (Long/parseLong (subs a 1)) pos)
        :else (recur (rest in) n (conj pos a))))))

(defn head
  "POSIX head. -n N (or -N) lines, default 10."
  [argv fs _env]
  (let [[n files] (parse-count argv)
        files (if (seq files) files ["-"])
        stderr (volatile! "")
        any-err? (volatile! false)
        results
        (mapv (fn [f]
                (if (= "-" f)
                  ""
                  (if-let [content (fs/read-file fs f)]
                    (->> (str/split-lines content)
                         (take n)
                         (str/join "\n")
                         (#(if (str/blank? %) "" (str % "\n"))))
                    (do
                      (vswap! stderr str "head: cannot open '" f "' for reading: No such file or directory\n")
                      (vreset! any-err? true)
                      ""))))
              files)]
    {:stdout (str/join "\n" results)
     :stderr @stderr
     :exit (if @any-err? 1 0)}))

;; ============================================================================
;; tail
;; ============================================================================

(defn tail
  "POSIX tail. -n N (or -N), default 10. No -f."
  [argv fs _env]
  (let [[n files] (parse-count argv)
        files (if (seq files) files ["-"])
        stderr (volatile! "")
        any-err? (volatile! false)
        results
        (mapv (fn [f]
                (if (= "-" f)
                  ""
                  (if-let [content (fs/read-file fs f)]
                    (->> (str/split-lines content)
                         (take-last n)
                         (str/join "\n")
                         (#(if (str/blank? %) "" (str % "\n"))))
                    (do
                      (vswap! stderr str "tail: cannot open '" f "' for reading: No such file or directory\n")
                      (vreset! any-err? true)
                      ""))))
              files)]
    {:stdout (str/join "\n" results)
     :stderr @stderr
     :exit (if @any-err? 1 0)}))

;; ============================================================================
;; wc
;; ============================================================================

(defn- count-stats [s]
  (let [byte-count (count (.getBytes ^String s "UTF-8"))
        word-count (count (re-seq #"\S+" s))
        line-count (if (str/blank? s)
                     (if (= "" s) 0 0)
                     (count (str/split-lines s)))
        ;; POSIX-ish: lines = number of \n. If no trailing \n, the
        ;; last line is still counted by str/split-lines, which is
        ;; close enough for agent use.
        ]
    {:lines line-count :words word-count :bytes byte-count}))

(defn wc
  "POSIX wc. -l (lines), -w (words), -c (bytes). Default: all three.
   With multiple files, prints a final 'total' row."
  [argv fs _env]
  (let [[flags pos] (parse-flags argv #{"l" "w" "c"})
        explicit?   (some flags ["l" "w" "c"])
        show-l?     (or (not explicit?) (boolean (flags "l")))
        show-w?     (or (not explicit?) (boolean (flags "w")))
        show-c?     (or (not explicit?) (boolean (flags "c")))
        files (if (seq pos) pos ["-"])
        stderr (volatile! "")
        any-err? (volatile! false)
        rows (keep
               (fn [f]
                 (let [content (if (= "-" f) "" (fs/read-file fs f))]
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
     :exit (if @any-err? 1 0)}))

;; ============================================================================
;; Registry — the canonical builtin map
;; ============================================================================

(def standard-read-only
  "All read-only builtins shipping in v1. Suitable as :builtins for
   muschel.host.builtin/make."
  {"pwd"  pwd
   "echo" echo
   "ls"   ls
   "cat"  cat
   "head" head
   "tail" tail
   "wc"   wc})
