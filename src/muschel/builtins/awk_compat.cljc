(ns muschel.builtins.awk-compat
  "Platform shims used by muschel.builtins.awk so the same source
   compiles + runs on JVM and ClojureScript (Node, browser).

   The shims cover the parts that aren't directly portable:

     • String buffer (StringBuilder ↔ goog.string.StringBuffer)
     • Number parsing + special values (NaN, ±Infinity, parseFloat,
       parseInt for hex)
     • Regex with DOTALL (Pattern/DOTALL ↔ /…/s flag)
     • Position-aware regex matching (Matcher#find/start/end/group
       ↔ JS RegExp#exec with global flag)
     • printf-style format (clojure.core/format ↔ goog.string.format)
     • A small mutable cell for awk's CONVFMT (alter-var-root isn't a
       CLJS thing)

   Each shim has the SAME shape on both platforms so the calling
   code stays a single source. Anything that didn't need a shim
   (Math, .charAt, .indexOf, str/replace) lives directly in awk.cljc."
  (:require [clojure.string :as str]
            #?(:cljs [goog.string :as gstr])
            #?(:cljs [goog.string.format])))

;; ============================================================================
;; String buffer
;; ============================================================================

(defn sbuf
  "Make a fresh string buffer."
  []
  #?(:clj (StringBuilder.)
     :cljs (goog.string.StringBuffer.)))

(defn sappend!
  "Append `x` to `sb` and return `sb` (for chaining)."
  [sb x]
  #?(:clj (.append ^StringBuilder sb x)
     :cljs (.append sb x))
  sb)

(defn sbstr
  "Realise the buffer as a string."
  [sb]
  #?(:clj (.toString ^StringBuilder sb)
     :cljs (.toString sb)))

;; ============================================================================
;; Number parsing + special values
;; ============================================================================

(def NaN     #?(:clj Double/NaN :cljs js/NaN))
(def +inf    #?(:clj Double/POSITIVE_INFINITY :cljs js/Number.POSITIVE_INFINITY))
(def -inf    #?(:clj Double/NEGATIVE_INFINITY :cljs js/Number.NEGATIVE_INFINITY))

(defn nan?
  "True iff `n` is NaN."
  [n]
  #?(:clj (Double/isNaN (double n))
     :cljs (js/Number.isNaN n)))

(defn inf?
  "True iff `n` is ±Infinity."
  [n]
  #?(:clj (Double/isInfinite (double n))
     :cljs (or (identical? n js/Number.POSITIVE_INFINITY)
               (identical? n js/Number.NEGATIVE_INFINITY))))

(defn pdouble
  "Parse a decimal/sci-notation string into a double. Throws on
   non-numeric input."
  [^String s]
  #?(:clj (Double/parseDouble s)
     :cljs (let [n (js/parseFloat s)]
             (when (js/Number.isNaN n)
               (throw (ex-info (str "Not a number: " (pr-str s)) {:s s})))
             n)))

(defn plong-hex
  "Parse a hex digit string (no 0x prefix) into a long/number."
  [^String s]
  #?(:clj (Long/parseLong s 16)
     :cljs (js/parseInt s 16)))

(defn parse-long-radix
  "Parse `s` in the given radix. Portable wrapper around Java's
   `Long/parseLong(s, radix)` (which `clojure.core/parse-long` does
   NOT accept) — needed e.g. for `chmod 0755` octal modes."
  [^String s radix]
  #?(:clj (Long/parseLong s (int radix))
     :cljs (let [n (js/parseInt s radix)]
             (when-not (js/Number.isNaN n) n))))

(defn floor
  "Math/floor — portable."
  [x]
  #?(:clj (Math/floor x)
     :cljs (.floor js/Math x)))

(defn char-code
  "Code point of a character. On JVM (Character) this is `(int c)`;
   on CLJS strings (since `(.charAt s i)` returns a single-char string,
   not a char) we use `.charCodeAt`. Returns -1 for nil / empty."
  [c]
  (cond
    (nil? c) -1
    :else #?(:clj  (int c)
             :cljs (.charCodeAt c 0))))

;; ============================================================================
;; Regex
;; ============================================================================

(defn re-compile
  "Compile `pat` (a pattern string) with DOTALL semantics — `.` matches
   newline (awk requires this). Returns a platform regex value
   suitable for `re-find-pos`."
  [^String pat]
  #?(:clj (java.util.regex.Pattern/compile pat java.util.regex.Pattern/DOTALL)
     :cljs (js/RegExp. pat "gs")))   ;; g = global (lastIndex tracking), s = dotall

(defn re-quote
  "Return a pattern string that matches `s` as a literal."
  [^String s]
  #?(:clj (java.util.regex.Pattern/quote s)
     :cljs
     ;; Escape all regex metacharacters.
     (str/replace s #"[-\\/^$*+?.()|\[\]{}]" "\\$&")))

(defn re-find-pos
  "Find the next match of `re` in `s` starting at index `start`.
   Returns {:start :end :match} or nil. `re` must have been compiled
   via `re-compile`."
  [re ^String s start]
  #?(:clj
     (let [m (.matcher ^java.util.regex.Pattern re s)]
       (when (.find m (int start))
         {:start (.start m) :end (.end m) :match (.group m)}))
     :cljs
     (do
       (set! (.-lastIndex re) start)
       (let [r (.exec re s)]
         (when r
           (let [m0 (aget r 0)
                 idx (.-index r)]
             {:start idx
              :end (+ idx (count m0))
              :match m0}))))))

(defn re-replace
  "Walk `s` matching `re` and apply `f` to each matched substring.
   Returns [new-string match-count]. If `all?` is false, only the
   first match is replaced.

   Implements awk-style sub/gsub replacement semantics by deferring
   the literal/`&` interpolation to the caller-supplied `f`."
  [re ^String s f all?]
  (loop [pos 0
         buf (sbuf)
         n   0]
    (let [hit (re-find-pos re s pos)]
      (cond
        (nil? hit)
        [(sbstr (sappend! buf (subs s pos))) n]

        (or (zero? n) all?)
        (let [{:keys [start end match]} hit
              before (subs s pos start)
              repl (f match)
              ;; Avoid infinite loop on zero-width matches: advance
              ;; one char past `end` if we matched empty.
              next-pos (if (= start end) (inc end) end)]
          (sappend! buf before)
          (sappend! buf repl)
          (if all?
            (recur next-pos buf (inc n))
            [(sbstr (sappend! buf (subs s end))) 1]))

        :else
        [(sbstr (sappend! buf (subs s pos))) n]))))

;; ============================================================================
;; printf-style formatting — single format spec at a time
;; ============================================================================

(defn fmt1
  "Format a single value through `fmt-str` (one specifier, like
   \"%5d\" or \"%-10s\"). Returns the formatted string."
  [^String fmt-str x]
  #?(:clj  (format fmt-str x)
     :cljs (gstr/format fmt-str x)))

(defn fmt-many
  "Apply `fmt-str` (which may contain multiple specifiers) over `args`.
   Returns the formatted string. JVM uses clojure.core/format; CLJS
   uses goog.string.format (subset: %d %s %f %e %g %% — anything else,
   notably %x/%X/%o/%c, must be pre-translated to %s by the caller
   because goog.string.format silently leaves them literal)."
  [^String fmt-str args]
  #?(:clj  (apply format fmt-str args)
     :cljs (apply gstr/format fmt-str args)))

(defn to-hex
  "Return `n`'s hexadecimal representation as a string. Negative
   integers render as their 64-bit two's-complement on JVM (matching
   gawk); CLJS just uses JS Number.toString(16)."
  [n upper?]
  (let [s #?(:clj  (Long/toHexString (long n))
             :cljs (.toString n 16))]
    (if upper? (.toUpperCase s) s)))

(defn to-octal
  [n]
  #?(:clj  (Long/toOctalString (long n))
     :cljs (.toString n 8)))

(defn re-find-any?
  "True if `re` (a regex compiled via `re-compile`) finds at least
   one match in `s`."
  [re ^String s]
  (some? (re-find-pos re s 0)))

(defn re-pattern-icase
  "Build a case-insensitive `re-pattern`-style regex by prepending the
   `(?i)` inline flag. Works on both Java regex and JS regex."
  [^String pat]
  (re-pattern (str "(?i)" pat)))

(defn split-by-regex
  "Split `s` everywhere `re` matches, preserving trailing empties (so
   `split-by-regex \"a,b,\" #\",\"` yields [\"a\" \"b\" \"\"]).
   `re` must be a regex compiled via `re-compile`."
  [re ^String s]
  (let [n (count s)]
    (loop [pos 0
           acc (transient [])]
      (cond
        (> pos n) (persistent! acc)
        (= pos n) (persistent! (conj! acc ""))
        :else
        (let [hit (re-find-pos re s pos)]
          (cond
            (nil? hit)
            (persistent! (conj! acc (subs s pos)))

            ;; Zero-width match — advance one char without emitting.
            (= (:start hit) (:end hit))
            (recur (inc pos) acc)

            :else
            (recur (:end hit)
                   (conj! acc (subs s pos (:start hit))))))))))

;; ============================================================================
;; CONVFMT — mutable cell readable by fmt-num
;; ============================================================================

(def convfmt
  "Awk's CONVFMT — the format used when a number is coerced to a string
   in expression context. Defaults to %.6g; set! via `set-convfmt!`."
  (atom "%.6g"))

(defn set-convfmt! [s] (reset! convfmt s))
(defn get-convfmt [] @convfmt)
