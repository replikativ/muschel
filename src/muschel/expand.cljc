(ns muschel.expand
  "POSIX expansion order for muschel.

   Per POSIX 2018 §2.6, every word goes through these passes:

     1. brace expansion       `{a,b,c}` `{1..10}`     [bash extension]
     2. tilde expansion       `~` `~/x` `~user`
     3. parameter expansion   `$VAR` `${VAR:-x}` etc.
     4. command substitution  `$(cmd)` `\\`cmd\\``
     5. arithmetic expansion  `$((expr))`             [bash extension]
     6. word splitting        unquoted results split on IFS
     7. pathname expansion    glob `*` `?` `[abc]`
     8. quote removal         strip the surviving quote chars

   Quoting changes the rules:
     - inside `'...'` no expansion runs and quote removal strips the quotes
     - inside `\"...\"` steps 3-5 run, 6-7 do NOT
     - unquoted: all steps

   `expand-words` is the entry point most callers want: it takes the
   word vector from a `:call` AST and returns the field list to pass
   to the executor. `expand-word` returns one or more fields (a word
   can expand into multiple via brace expansion or word splitting).

   ## Recursion into exec

   Command substitution requires running bash code, which lives in
   `muschel.exec`. We avoid a circular require by taking `cmd-subst`
   as a config callback:

     (expand-words env words {:cmd-subst (fn [env src] ...)
                              :arith     (fn [env expr] ...)})"
  (:require [clojure.string :as str]
            #?(:cljs [goog.string :as gstr])
            #?(:cljs [goog.string.format])
            [muschel.env :as env]
            [muschel.errors :as err]
            [muschel.lex :as lex]
            #?(:clj [babashka.fs :as fs])
            #?(:clj [clojure.java.shell :as csh])))

(defn- parse-int*
  "Portable Integer/parseInt. Throws if not a valid int (caller handles)."
  ([^String s] (parse-int* s 10))
  ([^String s base]
   #?(:clj  (Integer/parseInt s base)
      :cljs (let [n (js/parseInt s base)]
              (if (js/isNaN n)
                (throw (ex-info (str "not a number: " s) {}))
                n)))))

(defn- fmt
  "Portable format: cljs uses goog.string/format."
  [fmt-str & args]
  #?(:clj  (apply format fmt-str args)
     :cljs (apply goog.string/format fmt-str args)))

(defn- letter? [c]
  (and c (let [n #?(:clj (int ^Character c)
                    :cljs (.charCodeAt (str c) 0))]
           (or (and (>= n 65) (<= n 90))
               (and (>= n 97) (<= n 122))))))

;; ============================================================================
;; ANSI-C quoting decoder ($'...')
;; ============================================================================
;;
;; Mirrors bash's $'…' table: \n \t \r \\ \' \" \? \a \b \e \E \f \v \0
;; plus \xHH (hex byte), \uHHHH (unicode), \UHHHHHHHH (long unicode),
;; \nnn (octal). Unknown escapes are passed through verbatim (with
;; the backslash retained), matching bash.

(defn decode-ansi-c
  "Decode bash $'...'-style escape sequences in `s`. Portable across
   JVM and cljs (uses a vec accumulator instead of StringBuilder)."
  [^String s]
  (let [n (count s)
        hex-digit? (fn [c] (and c (re-find #"[0-9a-fA-F]" (str c))))
        oct-digit? (fn [c] (and c (re-find #"[0-7]" (str c))))
        parse-hex (fn [^String t] #?(:clj  (Long/parseLong t 16)
                                     :cljs (js/parseInt t 16)))
        parse-oct (fn [^String t] #?(:clj  (parse-int* t 8)
                                     :cljs (js/parseInt t 8)))]
    (loop [i 0 acc []]
      (cond
        (>= i n) (apply str acc)

        (and (= \\ (.charAt s i)) (< (inc i) n))
        (let [e (.charAt s (inc i))]
          (case e
            \n (recur (+ i 2) (conj acc "\n"))
            \t (recur (+ i 2) (conj acc "\t"))
            \r (recur (+ i 2) (conj acc "\r"))
            \\ (recur (+ i 2) (conj acc "\\"))
            \' (recur (+ i 2) (conj acc "'"))
            \" (recur (+ i 2) (conj acc "\""))
            \? (recur (+ i 2) (conj acc "?"))
            \a (recur (+ i 2) (conj acc (str (char 7))))
            \b (recur (+ i 2) (conj acc "\b"))
            \e (recur (+ i 2) (conj acc (str (char 27))))
            \E (recur (+ i 2) (conj acc (str (char 27))))
            \f (recur (+ i 2) (conj acc "\f"))
            \v (recur (+ i 2) (conj acc (str (char 11))))
            \0 (recur (+ i 2) (conj acc (str (char 0))))

            \x (let [h1 (when (< (+ i 2) n) (.charAt s (+ i 2)))
                     h2 (when (< (+ i 3) n) (.charAt s (+ i 3)))]
                 (cond
                   (and (hex-digit? h1) (hex-digit? h2))
                   (recur (+ i 4) (conj acc (str (char (parse-hex (str h1 h2))))))
                   (hex-digit? h1)
                   (recur (+ i 3) (conj acc (str (char (parse-hex (str h1))))))
                   :else
                   (recur (+ i 2) (conj acc "\\x"))))

            \u (let [end (loop [j (+ i 2) k 0]
                           (if (and (< k 4) (< j n) (hex-digit? (.charAt s j)))
                             (recur (inc j) (inc k)) j))]
                 (if (> end (+ i 2))
                   (recur end (conj acc (str (char (parse-hex (subs s (+ i 2) end))))))
                   (recur (+ i 2) (conj acc "\\u"))))

            \U (let [end (loop [j (+ i 2) k 0]
                           (if (and (< k 8) (< j n) (hex-digit? (.charAt s j)))
                             (recur (inc j) (inc k)) j))]
                 (if (> end (+ i 2))
                   (let [cp (parse-hex (subs s (+ i 2) end))]
                     (recur end (conj acc (str (char (min cp 0xFFFF))))))
                   (recur (+ i 2) (conj acc "\\U"))))

            (if (oct-digit? e)
              ;; bash truncates octal escapes to a single byte (\\777 → 0xFF),
              ;; matching what an 8-bit terminal would see.
              (let [end (loop [j (+ i 1) k 0]
                          (if (and (< k 3) (< j n) (oct-digit? (.charAt s j)))
                            (recur (inc j) (inc k)) j))
                    v (bit-and (parse-oct (subs s (+ i 1) end)) 0xFF)]
                (recur end (conj acc (str (char v)))))
              (recur (+ i 2) (conj acc (str "\\" e))))))

        :else
        (recur (inc i) (conj acc (str (.charAt s i))))))))

;; ============================================================================
;; Brace expansion (pass 1)
;; ============================================================================
;;
;; The lexer already split braced parts off as `:brace-exp` word-parts
;; with `:raw` (the inside) and `:kind :list | :range`. We reconstruct
;; the alternates and produce a vector of "alternate-words" — each of
;; which is itself a word with the same `:parts` shape but with the
;; brace-exp part replaced by a single `:lit`.

(defn- range-from-raw
  "Parse `A..B` or `A..B..STEP` into a sequence of strings.
   Supports integer ranges (zero-padded if either endpoint is) and
   single-char letter ranges."
  [^String raw]
  (let [parts (str/split raw #"\.\." 3)
        n (count parts)]
    (cond
      (not (#{2 3} n)) nil

      ;; integer range
      (and (re-matches #"-?\d+" (nth parts 0))
           (re-matches #"-?\d+" (nth parts 1)))
      (let [a (parse-int* (nth parts 0))
            b (parse-int* (nth parts 1))
            step (or (when (= 3 n)
                       (try (parse-int* (nth parts 2))
                            (catch #?(:clj Exception :cljs :default) _ nil)))
                     (if (<= a b) 1 -1))
            pad? (or (str/starts-with? (nth parts 0) "0")
                     (str/starts-with? (nth parts 1) "0"))
            width (max (count (nth parts 0)) (count (nth parts 1)))
            fmt-str (if pad? (str "%0" width "d") "%d")
            xs (if (<= a b)
                 (range a (inc b) (if (pos? step) step 1))
                 (range a (dec b) (if (neg? step) step -1)))]
        (mapv #(fmt fmt-str %) xs))

      ;; single-letter range
      (and (= 1 (count (nth parts 0)))
           (= 1 (count (nth parts 1)))
           (letter? (first (nth parts 0)))
           (letter? (first (nth parts 1))))
      (let [a (int (first (nth parts 0)))
            b (int (first (nth parts 1)))]
        (mapv (comp str char)
              (if (<= a b) (range a (inc b)) (range a (dec b) -1))))

      :else nil)))

(defn- split-brace-list
  "Split the raw inside of `{a,b,{c,d}}` on top-level commas (tracking
   nested braces). Returns a vector of alternative strings. Portable
   — uses a vec accumulator instead of StringBuilder."
  [^String raw]
  (let [out (volatile! [])
        cur (volatile! [])
        depth (volatile! 0)
        n (count raw)
        flush! (fn []
                 (vswap! out conj (apply str @cur))
                 (vreset! cur []))]
    (loop [i 0]
      (when (< i n)
        (let [c (.charAt raw i)]
          (cond
            (= c \{) (do (vswap! cur conj c) (vswap! depth inc) (recur (inc i)))
            (= c \}) (do (vswap! cur conj c) (vswap! depth dec) (recur (inc i)))
            (and (= c \,) (zero? @depth)) (do (flush!) (recur (inc i)))
            :else (do (vswap! cur conj c) (recur (inc i)))))))
    (flush!)
    @out))

(defn- expand-brace-alternates
  "Given the brace-exp part's :raw and :kind, return a vector of
   alternate-string-vectors (each alternate is itself a vector
   because nested brace expansion can recurse). For simplicity we
   first expand to a vector of strings, then re-expand any nested
   `{` recursively."
  [{:keys [raw kind]}]
  (let [seeds (case kind
                :range (or (range-from-raw raw)
                           ;; Range invalid (e.g. `{1..a}`); bash treats as literal
                           [(str "{" raw "}")])
                :list (split-brace-list raw))]
    seeds))

(defn- brace-expand-word
  "Expand any `:brace-exp` parts in `word`. Returns a vector of
   words (each with parts unchanged except brace-exps replaced by
   :lit alternates).

   `pre{a,b}suf` → two words: prea, preb. Brace expansion happens at
   the word level (not the part level) and produces multiple words."
  [word]
  (loop [acc [word]
         done? false]
    (if done?
      acc
      (let [next (mapcat
                  (fn [w]
                    (let [parts (:parts w)
                          i (some (fn [[idx p]]
                                    (when (= :brace-exp (:type p)) idx))
                                  (map-indexed vector parts))]
                      (if (nil? i)
                        [w]
                        (let [bp (nth parts i)
                              alts (expand-brace-alternates bp)]
                          (mapv (fn [alt]
                                  (assoc w :parts
                                         (-> (vec parts)
                                             (assoc i {:type :lit :value alt}))))
                                alts)))))
                  acc)
            still-brace? (some (fn [w] (some #(= :brace-exp (:type %)) (:parts w))) next)]
        (recur (vec next) (not still-brace?))))))

;; ============================================================================
;; Tilde expansion (pass 2)
;; ============================================================================

(defn- home-of-user [user]
  #?(:clj
     (try (let [r (csh/sh "getent" "passwd" user)
                line (-> r :out (str/split #"\n") first)]
            (when line
              (-> line (str/split #":") (nth 5 nil))))
          (catch #?(:clj Throwable :cljs :default) _ nil))
     :cljs
     ;; cljs: no portable way to look up other users' homes. Leave
     ;; the literal `~user` for bash's "user not found" behavior.
     nil))

(defn- tilde-replacement [env {:keys [user]}]
  (cond
    (or (nil? user) (= "" user)) (or (env/get-var* env "HOME") "")
    (= "+" user) (:cwd env)
    (= "-" user) (or (:prev-cwd env) (:cwd env))
    :else (or (home-of-user user)
              ;; bash leaves `~user` unchanged if user not found
              (str "~" user))))

(defn- expand-tilde-part [env part]
  {:type :lit :value (tilde-replacement env part)})

;; ============================================================================
;; Parameter expansion (pass 3)
;; ============================================================================
;;
;; The lexer captured `${VAR ...}` bodies as a raw string. We dissect
;; them here. The grammar is roughly:
;;
;;   ${NAME}
;;   ${#NAME}                        — length
;;   ${NAME:-WORD} ${NAME-WORD}      — default
;;   ${NAME:=WORD} ${NAME=WORD}      — assign-default (mutates env!)
;;   ${NAME:?WORD} ${NAME?WORD}      — error-if-unset
;;   ${NAME:+WORD} ${NAME+WORD}      — alternate-if-set
;;   ${NAME#PAT}  ${NAME##PAT}       — strip prefix
;;   ${NAME%PAT}  ${NAME%%PAT}       — strip suffix
;;   ${NAME/PAT/REPL}  ${NAME//PAT/REPL}  — replace
;;   ${NAME:OFFSET}  ${NAME:OFFSET:LEN}    — substring
;;   ${!NAME}                        — indirect (one extra lookup)

(def ^:private posix-char-class->regex
  "Translation table for `[[:class:]]` POSIX bracket-expressions to
   Java regex escapes."
  {"alpha"  "a-zA-Z"
   "alnum"  "a-zA-Z0-9"
   "digit"  "0-9"
   "lower"  "a-z"
   "upper"  "A-Z"
   "space"  "\\s"
   "blank"  " \\t"
   "punct"  "\\p{Punct}"
   "cntrl"  "\\p{Cntrl}"
   "print"  "\\p{Print}"
   "graph"  "\\p{Graph}"
   "xdigit" "0-9A-Fa-f"})

(defn- translate-bracket-body
  "Inside `[...]` of a glob: expand POSIX classes `[:digit:]` etc and
   regex-escape characters Java treats specially inside a char class
   (`[` for nested-class intersection, `&` for intersection, `\\` for
   escape). Leading `!` already stripped + `^` substituted by the
   caller."
  [^String body]
  (let [n (count body)
        esc? #{\[ \\ \&}]
    (loop [i 0 acc []]
      (if (>= i n)
        (apply str acc)
        (cond
          ;; [:class:]
          (and (<= (+ i 7) n)
               (= "[:" (subs body i (+ i 2)))
               (let [close (.indexOf body ":]" (+ i 2))]
                 (and (pos? close) (< close n))))
          (let [close (.indexOf body ":]" (+ i 2))
                class-name (subs body (+ i 2) close)
                rgx (get posix-char-class->regex class-name)]
            (if rgx
              (recur (+ close 2) (conj acc rgx))
              (recur (inc i) (conj acc (str (.charAt body i))))))

          :else
          (let [c (.charAt body i)]
            (recur (inc i)
                   (if (esc? c)
                     (conj acc "\\" (str c))
                     (conj acc (str c))))))))))

(defn glob->regex
  "Translate a bash glob pattern (`*` `?` `[abc]` `[!abc]`,
   `[[:digit:]]`, etc.) to a regex body (NOT anchored — the caller
   adds ^…$ if needed)."
  [^String pat]
  (let [n (count pat)]
    (loop [i 0 acc []]
      (if (>= i n)
        (apply str acc)
        (let [c (.charAt pat i)]
          (case c
            \* (recur (inc i) (conj acc ".*"))
            \? (recur (inc i) (conj acc "."))
            \[ (let [;; Where to start looking for closing `]`. bash
                     ;; quirk: a `]` as the FIRST char of the class
                     ;; body is literal — skip it. Same for `!]` / `^]`.
                     ;; This is for finding `]` (start position), NOT a
                     ;; minimum-length guard.
                     search-from
                     (cond
                       (>= (inc i) n) (inc i)
                       (= \] (.charAt pat (inc i))) (+ i 2)
                       (and (#{\! \^} (.charAt pat (inc i)))
                            (< (+ i 2) n)
                            (= \] (.charAt pat (+ i 2))))
                       (+ i 3)
                       :else (+ i 2))
                     end (.indexOf pat "]" search-from)]
                 (if (neg? end)
                   (recur (inc i) (conj acc "\\["))
                   (let [raw (subs pat (inc i) end)
                         raw (if (str/starts-with? raw "!")
                               (str "^" (subs raw 1))
                               raw)
                         body (translate-bracket-body raw)]
                     (recur (inc end)
                            (conj acc "[" body "]")))))
            \\ (if (< (inc i) n)
                 (recur (+ i 2)
                        (conj acc "\\" (str (.charAt pat (inc i)))))
                 (recur (inc i) (conj acc "\\\\")))
            (recur (inc i)
                   (conj acc
                         (if (re-find #"[.+(){}|^$]" (str c))
                           (str "\\" c)
                           (str c))))))))))

(defn- ->non-greedy [^String regex-body]
  ;; Make `*` matches reluctant: `.*` → `.*?`, `.+` → `.+?`.
  (-> regex-body
      (str/replace ".*" ".*?")
      (str/replace ".+" ".+?")))

(defn- match-glob-prefix
  "Return the matched prefix of `s` matching glob `pat` — `longest?`
   true returns the greediest match, false the shortest. nil if no
   match."
  [^String s ^String pat longest?]
  (let [rx-src (glob->regex pat)
        rx (re-pattern (str "^(" (if longest? rx-src (->non-greedy rx-src)) ")"))]
    (when-let [m (re-find rx s)]
      (second m))))

(defn- strip-prefix [s pat longest?]
  (if-let [pre (match-glob-prefix s pat longest?)]
    (subs s (count pre))
    s))

(defn- match-glob-suffix
  "Greedy: matches the LONGEST suffix. For shortest we walk from the
   end of `s` and try shorter suffixes first."
  [^String s ^String pat longest?]
  (let [rx-greedy (re-pattern (str "(" (glob->regex pat) ")$"))]
    (if longest?
      (when-let [m (re-find rx-greedy s)] (second m))
      ;; Shortest: try suffixes starting from len=0 up.
      (let [n (count s)
            anchored (re-pattern (str "^(" (glob->regex pat) ")$"))]
        (loop [k 0]
          (cond
            (> k n) nil
            (re-find anchored (subs s (- n k))) (subs s (- n k))
            :else (recur (inc k))))))))

(defn- strip-suffix [s pat longest?]
  (if-let [suf (match-glob-suffix s pat longest?)]
    (subs s 0 (- (count s) (count suf)))
    s))

(defn- replace-glob
  "Apply `${var/pat/repl}` / `${var//pat/repl}`. `repl` is passed
   through as a LITERAL replacement (bash semantics): regex
   metacharacters like `$` and `\\` in repl should not be treated
   specially. Java's `Matcher#quoteReplacement` does this.

   `(?s)` makes `.*` (from `*` in the glob) match across newlines,
   matching bash's pattern-in-replacement semantics.

   Empty pattern is a no-op (bash drops the operation)."
  [^String s ^String pat ^String repl all?]
  (cond
    (or (nil? pat) (= "" pat)) s
    :else
    (let [rx (re-pattern (str "(?s)" (glob->regex pat)))
          repl' #?(:clj  (java.util.regex.Matcher/quoteReplacement (or repl ""))
                   :cljs (or repl ""))]
      (if all?
        (str/replace s rx repl')
        (str/replace-first s rx repl')))))

(declare expand-string-in-env)

(defn- parse-param-body
  "Dissect the raw inside of `${...}`. Returns one of:
     {:op :plain  :name <str>}
     {:op :length :name <str>}
     {:op :indirect :name <str>}
     {:op :default | :default-unset | :assign | :assign-unset
          | :error | :error-unset | :alt | :alt-unset
          :name <str> :word-src <str>}
     {:op :strip-prefix-short | :strip-prefix-long
          | :strip-suffix-short | :strip-suffix-long
          :name <str> :pat-src <str>}
     {:op :replace | :replace-all  :name <str> :pat-src <str> :repl-src <str>}
     {:op :substring :name <str> :offset <int> :length <int|nil>}"
  [^String raw]
  (cond
    ;; ${#NAME}  (must be the FIRST char and there's no `:` after the #)
    (and (str/starts-with? raw "#") (> (count raw) 1)
         (not (#{\: \- \= \? \+ \% \#} (.charAt raw 1))))
    {:op :length :name (subs raw 1)}

    ;; ${!NAME} indirect
    (str/starts-with? raw "!")
    {:op :indirect :name (subs raw 1)}

    :else
    (let [n (count raw)
          ;; find end of NAME — first non-name char
          name-end (loop [i 0]
                     (if (< i n)
                       (let [c (.charAt raw i)]
                         (if (or (and (>= (int c) 48) (<= (int c) 57))
                                 (and (>= (int c) 65) (<= (int c) 90))
                                 (and (>= (int c) 97) (<= (int c) 122))
                                 (= c \_))
                           (recur (inc i))
                           i))
                       i))
          name (subs raw 0 name-end)]
      (if (= name-end n)
        {:op :plain :name name}
        (let [rest (subs raw name-end)]
          (cond
            (str/starts-with? rest ":-") {:op :default      :name name :word-src (subs rest 2)}
            (str/starts-with? rest "-")  {:op :default-unset :name name :word-src (subs rest 1)}
            (str/starts-with? rest ":=") {:op :assign       :name name :word-src (subs rest 2)}
            (str/starts-with? rest "=")  {:op :assign-unset :name name :word-src (subs rest 1)}
            (str/starts-with? rest ":?") {:op :error        :name name :word-src (subs rest 2)}
            (str/starts-with? rest "?")  {:op :error-unset  :name name :word-src (subs rest 1)}
            (str/starts-with? rest ":+") {:op :alt          :name name :word-src (subs rest 2)}
            (str/starts-with? rest "+")  {:op :alt-unset    :name name :word-src (subs rest 1)}
            (str/starts-with? rest "##") {:op :strip-prefix-long :name name :pat-src (subs rest 2)}
            (str/starts-with? rest "#")  {:op :strip-prefix-short :name name :pat-src (subs rest 1)}
            (str/starts-with? rest "%%") {:op :strip-suffix-long :name name :pat-src (subs rest 2)}
            (str/starts-with? rest "%")  {:op :strip-suffix-short :name name :pat-src (subs rest 1)}
            (str/starts-with? rest "//")
            (let [body (subs rest 2)
                  slash (.indexOf body "/")
                  [pat repl] (if (neg? slash) [body ""]
                                 [(subs body 0 slash) (subs body (inc slash))])]
              {:op :replace-all :name name :pat-src pat :repl-src repl})
            (str/starts-with? rest "/")
            (let [body (subs rest 1)
                  slash (.indexOf body "/")
                  [pat repl] (if (neg? slash) [body ""]
                                 [(subs body 0 slash) (subs body (inc slash))])]
              {:op :replace :name name :pat-src pat :repl-src repl})
            (str/starts-with? rest ":")
            ;; ${var:OFFSET[:LENGTH]} substring. bash quirk: a negative
            ;; offset must be written with a leading space (`${a: -1}`)
            ;; to avoid colliding with the `${var:-default}` syntax —
            ;; we trim here so " -1" parses as -1. Empty operands
            ;; default to 0 (`${a::2}` = `${a:0:2}`).
            (let [body (subs rest 1)
                  cc (.indexOf body ":")
                  pi (fn [s] (let [s (str/trim (or s ""))]
                               (if (empty? s) 0 (parse-int* s))))]
              (if (neg? cc)
                {:op :substring :name name
                 :offset (pi body) :length nil}
                {:op :substring :name name
                 :offset (pi (subs body 0 cc))
                 :length (pi (subs body (inc cc)))}))

            ;; ${var^^[pat]} / ${var^[pat]} — uppercase all / first
            ;; ${var,,[pat]} / ${var,[pat]} — lowercase all / first
            ;; pat is currently ignored (always applies to all chars).
            (str/starts-with? rest "^^") {:op :upcase-all   :name name}
            (str/starts-with? rest "^")  {:op :upcase-first :name name}
            (str/starts-with? rest ",,") {:op :downcase-all  :name name}
            (str/starts-with? rest ",")  {:op :downcase-first :name name}

            ;; ${var@OP} — transform operators.
            ;;   @U upper-case      @u upper-first    @L lower-case
            ;;   @Q shell-quote     @E expand escapes
            (str/starts-with? rest "@U") {:op :upcase-all    :name name}
            (str/starts-with? rest "@u") {:op :upcase-first  :name name}
            (str/starts-with? rest "@L") {:op :downcase-all  :name name}
            (str/starts-with? rest "@Q") {:op :shell-quote   :name name}
            (str/starts-with? rest "@E") {:op :expand-esc    :name name}
            (str/starts-with? rest "@a") {:op :attrs         :name name}
            (str/starts-with? rest "@A") {:op :decl-stmt     :name name}

            :else
            ;; Unknown form — return as plain to be lenient
            {:op :plain :name name}))))))

(defn- truthy-set?
  "POSIX `:` operator: var is 'set and non-empty'. With `:`, both
   unset and empty count as 'unset'. Without `:`, only truly-unset
   counts."
  [env name colon?]
  (let [v (env/get-var* env name)]
    (if colon?
      (and (some? v) (not= v ""))
      (some? v))))

(defn- nounset-trip?
  "True when `set -u` is on and reading `name` would touch an unset
   variable (and we're not using a default-providing op like `:-`).
   `get-var*` returns nil for genuinely-unset positional params even
   though `declared?` claims they're 'declared' as special vars; we
   prefer get-var* here."
  [env name op]
  (and (env/option env :nounset)
       (nil? (env/get-var* env name))
       (not (#{:default :default-unset :alt :alt-unset
               :assign :assign-unset :error :error-unset} op))))

(defn- apply-param-op
  "Apply a parsed param expansion. Returns [new-env result-string]
   since some ops (`:=`) mutate the env."
  [env op opts]
  (let [{:keys [name]} opts
        v (or (env/get-var* env name) "")
        word-of (fn [src] (when src (expand-string-in-env env src opts)))]
    (when (nounset-trip? env name op)
      (err/error! (str name ": unbound variable")
                  {:type ::param-error :source (:src opts)}))
    (case op
      :plain    [env v]
      :length   [env (str (count v))]
      :indirect (let [target (env/get-var* env v)]
                  [env (or target "")])

      :default       (if (truthy-set? env name true)  [env v] [env (or (word-of (:word-src opts)) "")])
      :default-unset (if (truthy-set? env name false) [env v] [env (or (word-of (:word-src opts)) "")])

      :assign        (if (truthy-set? env name true)  [env v]
                         (let [val (or (word-of (:word-src opts)) "")]
                           [(env/set-var env name val) val]))
      :assign-unset  (if (truthy-set? env name false) [env v]
                         (let [val (or (word-of (:word-src opts)) "")]
                           [(env/set-var env name val) val]))

      :error         (if (truthy-set? env name true) [env v]
                         (err/error! (str name ": "
                                          (or (word-of (:word-src opts))
                                              "parameter null or not set"))
                                     {:type ::param-error :source (:src opts)}))
      :error-unset   (if (truthy-set? env name false) [env v]
                         (err/error! (str name ": "
                                          (or (word-of (:word-src opts))
                                              "parameter not set"))
                                     {:type ::param-error :source (:src opts)}))

      :alt           (if (truthy-set? env name true)  [env (or (word-of (:word-src opts)) "")] [env ""])
      :alt-unset     (if (truthy-set? env name false) [env (or (word-of (:word-src opts)) "")] [env ""])

      :strip-prefix-short [env (strip-prefix v (or (word-of (:pat-src opts)) "") false)]
      :strip-prefix-long  [env (strip-prefix v (or (word-of (:pat-src opts)) "") true)]
      :strip-suffix-short [env (strip-suffix v (or (word-of (:pat-src opts)) "") false)]
      :strip-suffix-long  [env (strip-suffix v (or (word-of (:pat-src opts)) "") true)]

      :replace      [env (replace-glob v (or (word-of (:pat-src opts)) "")
                                       (or (word-of (:repl-src opts)) "") false)]
      :replace-all  [env (replace-glob v (or (word-of (:pat-src opts)) "")
                                       (or (word-of (:repl-src opts)) "") true)]

      :upcase-all    [env (str/upper-case v)]
      :upcase-first  [env (if (empty? v) v (str (str/upper-case (subs v 0 1)) (subs v 1)))]
      :downcase-all  [env (str/lower-case v)]
      :downcase-first [env (if (empty? v) v (str (str/lower-case (subs v 0 1)) (subs v 1)))]
      :shell-quote   [env (str "'" (str/replace v "'" "'\\''") "'")]
      :expand-esc    [env (decode-ansi-c v)]
      :attrs         ;; bash flags: 'x' exported, 'r' readonly, 'a' array,
      ;; 'i' integer, 'l' lower, 'u' upper. Unset vars → empty.
      (let [meta (get-in env [:vars name])
            flags (str (when (:exported? meta) "x")
                       (when (:readonly? meta) "r"))]
        (if (nil? meta) [env ""] [env flags]))
      :decl-stmt
      (let [meta (get-in env [:vars name])]
        (if (nil? meta)
          [env ""]
          (let [decl (cond
                       (and (:exported? meta) (:readonly? meta)) "declare -rx"
                       (:exported? meta) "declare -x"
                       (:readonly? meta) "declare -r"
                       :else "declare --")]
            [env (str decl " " name "=\""
                      (str/replace v "\"" "\\\"") "\"")])))

      :substring    (let [{:keys [offset length]} opts
                          n   (count v)
                          ;; bash: negative offset = from end (requires a
                          ;; leading space, ${a: -1}, which the lexer
                          ;; preserves verbatim in the operand).
                          off (cond
                                (nil? offset) 0
                                (neg? offset) (max 0 (+ n offset))
                                :else         (min offset n))
                          ;; bash: negative length = absolute position
                          ;; from end (not relative to offset).
                          end (cond
                                (nil? length) n
                                (neg? length) (max off (+ n length))
                                :else         (max off (min (+ off length) n)))]
                      [env (subs v off end)]))))

;; ============================================================================
;; Word part expansion
;; ============================================================================

(declare expand-word*)

(defn- expand-cmd-subst [env body opts]
  (let [f (:cmd-subst opts)]
    (when-not f
      (err/error! "command substitution requested but no :cmd-subst handler provided"
                  {:type ::expand-error}))
    ;; `f` returns [env' captured-stdout-string]
    (let [[env' out] (f env body)
          ;; bash strips trailing newlines from cmd-subst output
          out (str/replace out #"\n+$" "")]
      [env' out])))

(defn- expand-arith [env expr opts]
  (let [f (:arith opts)]
    (if f
      (let [[env' result] (f env expr)]
        [env' (str result)])
      ;; If no handler was provided (rare — expand is typically called
      ;; through exec which always wires one), surface explicitly.
      (err/error! "arithmetic expansion requested but no :arith handler"
                  {:type ::expand-error}))))

(defn- expand-part
  "Expand one word-part. Returns [env' [fragment ...]]:
     - env' may have mutations from cmd-subst, assign-param-op
     - fragments are tagged with their quoting (for word-splitting later)
   Each fragment is {:s <str> :quoted? bool}."
  [env part opts]
  (case (:type part)
    :lit      [env [{:s (:value part) :quoted? false}]]
    :escape   [env [{:s (:value part) :quoted? true}]]   ; escaped chars are 'quoted' for splitting
    :squoted  [env [{:s (:value part) :quoted? true}]]
    :tilde    [env [{:s (tilde-replacement env part) :quoted? false}]]
    :ansi-c-quoted
    [env [{:s (decode-ansi-c (:raw part)) :quoted? true}]]

    :var-ref
    (let [name (:name part)]
      (when (and name (nounset-trip? env name :plain))
        (err/error! (str name ": unbound variable")
                    {:type ::param-error :source (:offset part)}))
      (cond
        (:special? part)
        [env [{:s (env/get-var env name) :quoted? false}]]
        (:positional? part)
        [env [{:s (env/get-var env name) :quoted? false}]]
        (not (:braced part))
        [env [{:s (env/get-var env name) :quoted? false}]]
        :else
        (let [parsed (parse-param-body (:raw part))
              [env' v] (apply-param-op env (:op parsed) parsed)]
          [env' [{:s v :quoted? false}]])))

    :cmd-subst
    ;; Cmd-subst body is the raw bash source (lazy parsing — matches
    ;; bash). The :cmd-subst handler in `exec.cljc` re-parses + runs it,
    ;; threading the permit config so the runtime hook catches any
    ;; spawn-points inside.
    (let [[env' v] (expand-cmd-subst env (:body part) opts)]
      [env' [{:s v :quoted? false}]])

    :arith
    (let [[env' v] (expand-arith env (:expr part) opts)]
      [env' [{:s v :quoted? false}]])

    :dquoted
    ;; Inside "...", expand sub-parts but mark all output as :quoted?
    ;; true. Adjacent fragments concatenate (no field splitting).
    ;;
    ;; Special case: `"$@"` (or `"prefix$@suffix"`) preserves the
    ;; positional split — `"$@"` with N positionals produces N fields,
    ;; not one. The first field glues to anything before $@ in the
    ;; same dquoted, the last to anything after. Zero positionals
    ;; produce zero fields (which is what makes `count "$@"` work
    ;; when $# is 0).
    (let [parts (:parts part)
          dollar-at? (fn [p]
                       (and (= :var-ref (:type p))
                            (= "@" (:name p))
                            (not (:braced p))))]
      (if-let [at-idx (some (fn [[i p]] (when (dollar-at? p) i))
                            (map-indexed vector parts))]
        (let [before (subvec (vec parts) 0 at-idx)
              after  (subvec (vec parts) (inc at-idx))
              expand-side (fn [env subs]
                            (reduce (fn [[env acc] sub]
                                      (let [[env' fs] (expand-part env sub opts)]
                                        [env' (str acc (apply str (map :s fs)))]))
                                    [env ""]
                                    subs))
              [env1 pre-str] (expand-side env before)
              [env2 post-str] (expand-side env1 after)
              pos (vec (:pos-args env2))]
          (cond
            ;; Zero positionals → zero fields if there's also no
            ;; surrounding text (i.e. just "$@"). With surrounding
            ;; text the empty $@ collapses and pre+post fuse to one
            ;; field.
            (and (empty? pos) (= "" pre-str) (= "" post-str))
            [env2 []]

            (empty? pos)
            [env2 [{:s (str pre-str post-str) :quoted? true}]]

            (= 1 (count pos))
            [env2 [{:s (str pre-str (first pos) post-str) :quoted? true}]]

            :else
            ;; Emit positionals as quoted fields with UNQUOTED IFS
            ;; separators between them. field-split sees the IFS frags
            ;; and breaks; expand-assign-value joins all :s values to
            ;; get the bash-correct "a b c" assignment semantics.
            (let [ifs (:ifs env2)
                  sep (if (seq ifs) (str (.charAt ifs 0)) " ")
                  middle (mapcat (fn [p] [{:s sep :quoted? false}
                                          {:s p :quoted? true}])
                                 (rest pos))]
              [env2 (into [{:s (str pre-str (first pos)) :quoted? true}]
                          (concat (butlast middle)
                                  ;; The last entry of `middle` was the
                                  ;; last positional itself — splice in
                                  ;; the suffix (post-str) onto it.
                                  [(let [last-frag (last middle)]
                                     (assoc last-frag :s
                                            (str (:s last-frag) post-str)))]))])))
        ;; Normal case — no $@ in this dquoted.
        (let [[env' frags]
              (reduce (fn [[env acc] sub]
                        (let [[env' fs] (expand-part env sub opts)]
                          [env' (into acc (mapv #(assoc % :quoted? true) fs))]))
                      [env []]
                      parts)]
          [env' [{:s (apply str (map :s frags)) :quoted? true}]])))

    :brace-exp
    ;; brace-exp should have been expanded out before reaching here.
    ;; If somehow it slipped through, treat as literal `{raw}`.
    [env [{:s (str "{" (:raw part) "}") :quoted? false}]]

    ;; default fallback — treat as literal
    [env [{:s (str (:value part)) :quoted? false}]]))

(defn- expand-string-in-env
  "Expand a raw bash word-text-source (e.g. the `word` part of
   `${VAR:-word}`). Re-lexes the source as a single word and runs
   expansion. Returns a string (joined, no word splitting).

   Whitespace between words is PRESERVED — bash's `${var/pat/repl}`
   keeps `a  b` (two spaces) as-is in the replacement. We do this by
   walking the raw source for whitespace and only consulting the
   tokenizer for `$`-led expansions."
  [env src opts]
  (when (and (string? src) (pos? (count src)))
    (let [tokens (lex/tokenize src)
          ;; Sort tokens by source offset so we can interleave them
          ;; with the original whitespace.
          word-toks (->> tokens
                         (filter #(= :word (:type %)))
                         (sort-by :offset))]
      (loop [acc [] pos 0 toks word-toks env env]
        (cond
          (empty? toks)
          ;; Append any trailing whitespace.
          (apply str (conj acc (subs src pos)))

          :else
          (let [t (first toks)
                t-off (or (:offset t) pos)
                gap (subs src pos t-off)
                [env' fs]
                (reduce (fn [[env acc] sub]
                          (let [[env' fs] (expand-part env sub opts)]
                            [env' (into acc fs)]))
                        [env []]
                        (:parts t))
                t-end (or (:end-offset t) (+ t-off (count gap)))]
            (recur (-> acc (conj gap) (into (map :s fs)))
                   t-end
                   (rest toks)
                   env')))))))

;; ============================================================================
;; Word splitting (pass 6) + glob (pass 7)
;; ============================================================================

(defn- split-on-ifs
  "Split `s` on any character in `ifs`. Multiple adjacent whitespace
   IFS chars collapse to one separator. Empty `ifs` disables splitting.

   Emits empty-string SENTINELS for leading/trailing IFS so callers can
   distinguish `\"b c\"` (no leading sep → first field glues to prev)
   from `\" b c\"` (leading sep → break before `b`). This matters for
   field-splitting an unquoted `${a}` inside `foo${a}bar` when `a` has
   surrounding spaces."
  [^String s ^String ifs]
  (cond
    (= "" ifs) [s]
    (= "" s) []
    :else
    (let [ws? (set (filter #(or (= % \space) (= % \tab) (= % \newline)) ifs))
          all? (set ifs)
          n (count s)
          out (volatile! [])
          cur (volatile! [])
          ;; flush! always pushes (even when cur is empty) so we record
          ;; sentinel boundaries; the caller decides what to do with
          ;; empty fields. Initial leading-sep emits an empty piece.
          flush! (fn []
                   (vswap! out conj (apply str @cur))
                   (vreset! cur []))]
      (loop [i 0]
        (if (>= i n)
          (do (flush!) @out)
          (let [c (.charAt s i)]
            (cond
              (all? c)
              (do (flush!)
                  (if (ws? c)
                    (recur (loop [j (inc i)]
                             (if (and (< j n) (ws? (.charAt s j)))
                               (recur (inc j))
                               j)))
                    (recur (inc i))))
              :else
              (do (vswap! cur conj c) (recur (inc i))))))))))

(defn- field-split
  "Apply word splitting to a fragment list. Quoted fragments are
   joined to adjacent unquoted ones; only unquoted fragments are
   split. A `:break? true` frag forces a field boundary BEFORE it —
   used to preserve the positional split of `\"$@\"` even though all
   fragments are quoted.

   `split-on-ifs` emits empty-string sentinels for leading/trailing
   IFS in the source; here we treat them as flush-markers (start /
   end a field instead of gluing)."
  [frags ifs]
  (when (seq frags)
    (let [out (volatile! [])
          cur (volatile! [])
          ;; Tracks whether the current `cur` is the start of a
          ;; meaningful field — flush! only emits if so. Prevents
          ;; spurious empty fields between two flushes in a row.
          live? (volatile! false)
          flush! (fn []
                   (when @live?
                     (vswap! out conj (apply str @cur))
                     (vreset! cur [])
                     (vreset! live? false)))
          add!   (fn [s] (vswap! cur conj s) (vreset! live? true))]
      (doseq [{:keys [s quoted? break?]} frags]
        (when break? (flush!) (vreset! live? true))
        (cond
          quoted?
          (add! s)

          (empty? s)
          nil

          :else
          (let [pieces (split-on-ifs s ifs)
                pc (count pieces)]
            (cond
              (zero? pc) nil

              (= 1 pc)
              (add! (first pieces))

              :else
              (do
                ;; First piece: empty ⇒ flush (leading IFS in s),
                ;; non-empty ⇒ glue to current cur.
                (if (empty? (first pieces))
                  (flush!)
                  (add! (first pieces)))
                ;; Boundary between first and rest forces a flush.
                (flush!)
                ;; Middle pieces are each their own field.
                (doseq [p (butlast (rest pieces))]
                  (add! p)
                  (flush!))
                ;; Last piece: empty ⇒ trailing IFS, keep cur empty so
                ;; the next frag DOESN'T glue. Non-empty ⇒ start cur
                ;; with it so a following frag CAN glue.
                (let [lp (last pieces)]
                  (if (empty? lp)
                    nil
                    (add! lp))))))))
      (flush!)
      @out)))

(defn- glob-expand
  "Apply pathname expansion to one word. Returns the original word
   wrapped in a single-element vector if no glob chars or no matches.
   Skipped when env's :noglob option is true."
  [env word]
  #?(:clj
     (let [has-glob? (re-find #"(?<!\\)[*?\[]" word)]
       (if (or (not has-glob?) (env/option env :noglob))
         [word]
         (let [matches (try (fs/glob (:cwd env) word)
                            (catch #?(:clj Throwable :cljs :default) _ nil))
               strs (mapv (fn [p] (str (fs/relativize (:cwd env) p))) matches)]
           (if (seq strs)
             (sort strs)
             [word]))))
     :cljs [word]))

;; ============================================================================
;; Public API
;; ============================================================================

(defn expand-word
  "Expand one AST :word into a list of strings (field-split, globbed).
   Returns [env' [string...]]."
  [env word & {:as opts}]
  (let [alternates (brace-expand-word word)
        [env' fields]
        (reduce
         (fn [[env acc] alt]
           (let [parts (:parts alt)
                 [env' frags]
                 (reduce (fn [[env acc] p]
                           (let [[env' fs] (expand-part env p opts)]
                             [env' (into acc fs)]))
                         [env []]
                         parts)
                 split (field-split frags (:ifs env'))
                 globbed (mapcat #(glob-expand env' %) split)]
             [env' (into acc globbed)]))
         [env []]
         alternates)]
    [env' fields]))

(defn expand-words
  "Expand a vector of AST :word values into a flat field list.
   Threads env through (any expansion can mutate via :=)."
  [env words & {:as opts}]
  (reduce (fn [[env acc] w]
            (let [[env' fs] (expand-word env w opts)]
              [env' (into acc fs)]))
          [env []]
          words))

(defn expand-assign-value
  "Expand the right-hand side of an assignment. Unlike argument
   expansion, this does NOT field-split or glob: `FOO=a b c` is
   `FOO=a` followed by argument `b` and `c`, while `FOO=\"a b\"` sets
   FOO to literal `a b`."
  [env word & {:as opts}]
  (let [parts (:parts word)
        [env' frags]
        (reduce (fn [[env acc] p]
                  (let [[env' fs] (expand-part env p opts)]
                    [env' (into acc fs)]))
                [env []]
                parts)]
    [env' (apply str (map :s frags))]))
