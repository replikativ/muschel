(ns muschel.errors
  "Error infrastructure for muschel's lexer and parser.

   Mirrors the shape of `superficie.errors` so that the two
   projects produce visually-consistent diagnostic output and so
   tooling (the agentic harness) can render errors uniformly.

   Every error is an `ex-info` whose data map carries:

     :type     — namespaced keyword identifying the error family
                 (e.g. ::muschel.lex/lex-error, ::muschel.lex/refused,
                  ::muschel.parse/parse-error)
     :msg      — short human summary
     :line     — 1-indexed source line
     :col      — 1-indexed source column
     :end-col  — optional, column where the offending span ends
     :offset   — 0-indexed character offset in the source
     :source-context — the source line itself (extracted from :source
                       when available, so callers don't have to keep
                       the source around to render the error)
     :hint     — optional remediation suggestion
     :secondary — optional vector of related locations
                   ({:line :col :label} maps), for showing things like
                   the opening `\"` of an unterminated string
     :incomplete — optional bool; true if the error is from input that
                   looks intentionally truncated (lets REPLs ask for
                   more input rather than failing)

   `format-error` renders an exception (or its data map) as a
   multi-line display with a line gutter, the source line, and a
   caret underline."
  (:require [clojure.string :as str]))

;; ============================================================================
;; Source-line extraction
;; ============================================================================

(defn source-context
  "Return the source line at 1-indexed `line` from `source`, or nil if
   unavailable. Uses str/split-lines (splits on \\n and \\r\\n)."
  [source line]
  (when (and source line (not (str/blank? source)))
    (let [lines (str/split-lines source)
          i     (dec line)]
      (when (and (>= i 0) (< i (count lines)))
        (nth lines i)))))

;; ============================================================================
;; Error throwing
;; ============================================================================

(defn error!
  "Throw a muschel diagnostic.
   `data` may contain :type :line :col :end-col :offset :source :hint
   :secondary :incomplete. `:source-context` is derived from
   `:source` + `:line` when both are present; `:source` itself is
   stripped from the final ex-data (it's the whole input, often
   large)."
  [msg data]
  (let [{:keys [line col source cause]} data
        loc-suffix (when (and line col) (str " (line " line ", col " col ")"))
        full-msg   (str msg loc-suffix)
        ex-data    (cond-> (dissoc data :source :cause)
                     (and source line)
                     (assoc :source-context (source-context source line))
                     true
                     (assoc :msg msg))]
    (throw (ex-info full-msg ex-data cause))))

;; ============================================================================
;; Formatting
;; ============================================================================

(defn- gutter [n w]
  (let [s (str n) pad (- w (count s))]
    (str (apply str (repeat pad " ")) s " | ")))

(defn- blank-gutter [w] (str (apply str (repeat w " ")) " | "))

(defn- underline
  "Underline from col to end-col (exclusive). Single char → ^, multi → ~~~."
  [col end-col]
  (let [start (max 1 (or col 1))
        end   (or end-col start)
        len   (max 1 (- end start))]
    (str (apply str (repeat (dec start) " "))
         (if (= 1 len) "^" (apply str (repeat len "~"))))))

(defn format-error
  "Render an `ex-info` (or its data map) as a multi-line human display.
   Optional `source` second arg is the original input — pass it if the
   data map doesn't already carry `:source-context`."
  ([e] (format-error e nil))
  ([e source]
   (let [msg  (if (string? e) e (ex-message e))
         data (cond
                (map? e) e
                (instance? #?(:clj clojure.lang.ExceptionInfo
                              :cljs ExceptionInfo) e)
                (ex-data e))
         {:keys [line col end-col hint secondary]} data
         ctx-line (or (when (and source line) (source-context source line))
                      (:source-context data))
         all-lines (cond-> [] line (conj line)
                           secondary (into (keep :line secondary)))
         w (if (seq all-lines)
             (count (str (apply max all-lines)))
             1)
         out (volatile! [(str "Error: " msg)])]
     (when ctx-line
       (vswap! out conj (str "\n" (gutter line w) ctx-line))
       (when (and col (pos? col))
         (let [dlen (count ctx-line)
               c    (min col (inc dlen))
               ec   (when end-col (min end-col (inc dlen)))]
           (vswap! out conj (str "\n" (blank-gutter w) (underline c ec))))))
     (doseq [{sl :line sc :col label :label} secondary]
       (when-let [ctx (and source sl (source-context source sl))]
         (vswap! out conj (str "\n" (gutter sl w) ctx))
         (when (and sc (pos? sc))
           (let [csc (min sc (inc (count ctx)))]
             (vswap! out conj
                     (str "\n" (blank-gutter w)
                          (apply str (repeat (dec csc) " "))
                          "^ " label))))))
     (when hint (vswap! out conj (str "\nHint: " hint)))
     (apply str @out))))
