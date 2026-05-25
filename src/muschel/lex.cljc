(ns muschel.lex
  "Hand-written tokenizer for muschel's bash subset.

   Produces a flat vector of tokens from a source string. Each token is
   a map carrying `:type`, source-position fields, and type-specific
   fields. Word tokens carry their parts (lit/squoted/dquoted/var-ref/
   cmd-subst/arith/tilde/escape/backtick/brace-exp) already classified
   — the parser sees clean word tokens, not individual quote characters.

   The lexer is the *only* layer that knows about heredoc state: when
   it encounters `<<TAG` or `<<-TAG`, it queues a pending heredoc, then
   on the next newline drains the body up to a line matching the tag.

   Refused at lex (throws `ex-info {:type ::refused}`):
   - process substitution  `<(cmd)`, `>(cmd)`
   - extended globs        `?(`, `*(`, `+(`, `@(`, `!(`
   - locale quoting        `$\"...\"`

   Everything else from the corpus-driven support list parses cleanly:
   pipes, &&/||, ;, &, redirections (with FD prefix), heredocs (quoted/
   unquoted/strip-tabs forms), single/double/ANSI-C quoting, backslash
   escapes, parameter expansion (all standard forms), command
   substitution `$(...)` and backtick, arithmetic `$((..))`, tilde,
   brace expansion, here-strings `<<<`, `|&`, `((arith))` command form.

   No `deftype`/`definterface` — sci-safe. Uses `volatile!` for cursor
   state and a portable StringBuilder via reader conditionals."
  (:refer-clojure :exclude [peek])
  (:require [muschel.errors :as err]))

;; ============================================================================
;; Portable StringBuilder
;; ============================================================================

(defn- sb [] #?(:clj (StringBuilder.) :cljs #js []))
(defn- sb+ [b c] #?(:clj (.append ^StringBuilder b ^Character c) :cljs (.push b (str c))) b)
(defn- sb++ [b ^String s] #?(:clj (.append ^StringBuilder b s) :cljs (.push b s)) b)
(defn- sb->s [b] #?(:clj (.toString ^StringBuilder b) :cljs (.join b "")))
(defn- sb-empty? [b] #?(:clj (zero? (.length ^StringBuilder b)) :cljs (zero? (.-length b))))

;; ============================================================================
;; Character classification
;; ============================================================================

(defn- ch-code [c]
  #?(:clj  (int ^Character c)
     :cljs (.charCodeAt c 0)))

(defn- digit? [c]
  (when c
    (let [n (ch-code c)] (and (>= n 48) (<= n 57)))))

(defn- letter? [c]
  (when c
    (let [n (ch-code c)]
      (or (and (>= n 65) (<= n 90))
          (and (>= n 97) (<= n 122))))))

(defn- name-start? [c]
  (or (letter? c) (= c \_)))

(defn- name-char? [c]
  (or (name-start? c) (digit? c)))

(defn- hspace? [c]
  (or (= c \space) (= c \tab)))

(defn- nl? [c]
  (or (= c \newline) (= c \return)))

;; Characters that genuinely end a word in unquoted context: whitespace,
;; control operators, comment opener. NOT here: $, ", ', `, \\, { — those
;; ARE part of a word, the lexer just dispatches into specific readers
;; for them.
(def ^:private word-end-chars
  #{\space \tab \newline \return
    \| \& \; \< \> \( \) \#})

(defn- word-end? [c]
  (or (nil? c) (word-end-chars c)))

(defn- operator-end?
  "Predicate used by tag-readers to know when a word-like terminator
   ends. Includes EOF, whitespace, newline, and shell operators."
  [c]
  (or (nil? c)
      (hspace? c) (nl? c)
      (#{\| \& \; \< \> \( \)} c)))

;; ============================================================================
;; Scanner state
;; ============================================================================

(defn- scanner [^String src]
  {:src   src
   :len   (count src)
   :pos   (volatile! 0)
   :line  (volatile! 1)
   :col   (volatile! 1)
   ;; Queue of pending heredocs awaiting their body. Each entry is
   ;; {:tag str :strip? bool :quoted? bool :token-idx int}.
   ;; When the lexer hits a newline, it drains this queue.
   :pending-heredocs (volatile! [])})

(defn- at-end? [{:keys [pos len]}]
  (>= @pos len))

(defn- peek1 [{:keys [src pos len]}]
  (when (< @pos len) (.charAt ^String src @pos)))

(defn- peek2 [{:keys [src pos len]}]
  (let [i (inc @pos)] (when (< i len) (.charAt ^String src i))))

(defn- peek-at [{:keys [src pos len]} n]
  (let [i (+ @pos n)] (when (< i len) (.charAt ^String src i))))

(defn- starts-with? [{:keys [^String src pos len]} ^String s]
  (let [slen (.length s) p @pos]
    (and (<= (+ p slen) len)
         (loop [i 0]
           (cond
             (= i slen) true
             (not= (.charAt src (+ p i)) (.charAt s i)) false
             :else (recur (inc i)))))))

(defn- advance! [{:keys [^String src pos line col]}]
  (let [c (.charAt src @pos)]
    (if (nl? c)
      (do (vswap! line inc) (vreset! col 1))
      (vswap! col inc))
    (vswap! pos inc)
    c))

(defn- advance-n! [sc n]
  (dotimes [_ n] (advance! sc)))

(defn- mark [{:keys [pos line col]}]
  {:line @line :col @col :offset @pos})

;; ============================================================================
;; Error helpers (thin wrappers over muschel.errors)
;; ============================================================================

(defn- lex-error
  "Throw a `::lex-error` ex-info, attaching scanner-derived location.
   `extra` may add :end-col, :hint, :secondary, :incomplete."
  ([sc msg loc] (lex-error sc msg loc nil))
  ([sc msg loc extra]
   (err/error! msg
               (merge {:type   ::lex-error
                       :line   (:line loc)
                       :col    (:col loc)
                       :offset (:offset loc)
                       :source (:src sc)}
                      extra))))

(defn- refused
  ([sc msg loc] (refused sc msg loc nil))
  ([sc msg loc extra]
   (err/error! msg
               (merge {:type   ::refused
                       :line   (:line loc)
                       :col    (:col loc)
                       :offset (:offset loc)
                       :source (:src sc)}
                      extra))))

;; ============================================================================
;; Token construction
;; ============================================================================

(defn- tok
  ([type start end-mark] (tok type start end-mark {}))
  ([type start end-mark extra]
   (merge {:type type
           :line       (:line start)   :col       (:col start)   :offset     (:offset start)
           :end-line   (:line end-mark) :end-col   (:col end-mark) :end-offset (:offset end-mark)}
          extra)))

;; ============================================================================
;; Whitespace + comments
;; ============================================================================

(defn- skip-hspace-and-comments! [sc]
  "Skip horizontal whitespace and # comments. Newlines are SIGNIFICANT
   and are NOT skipped — they emit :newline tokens.
   Also skips line-continuation `\\<newline>` (the backslash + newline pair)."
  (loop []
    (cond
      (at-end? sc) nil

      (hspace? (peek1 sc))
      (do (advance! sc) (recur))

      ;; line continuation: \ then \n (or \r\n)
      (and (= (peek1 sc) \\) (nl? (peek2 sc)))
      (do (advance! sc)         ; backslash
          (advance! sc)         ; newline
          (when (= (peek1 sc) \newline) (advance! sc))   ; \r\n
          (recur))

      (= (peek1 sc) \#)
      (do (loop []
            (when (and (not (at-end? sc)) (not (nl? (peek1 sc))))
              (advance! sc) (recur)))
          (recur))

      :else nil)))

;; ============================================================================
;; Forward declarations
;; ============================================================================

(declare read-word! read-token! drain-heredocs!)

;; ============================================================================
;; Balanced readers — used by $(...), ${...}, $((..)), `...`
;; ============================================================================

(defn- skip-heredoc-into!
  "Inside a balanced reader (e.g. `$(...)` body capture), we've just
   consumed `<<` (and possibly `<<-`). Continue verbatim-capturing the
   tag, the rest of the current line, and the heredoc body up to and
   including the closing tag line.

   Crucial: body lines are captured WITHOUT quote/paren tracking, so a
   stray `'` or `)` inside a heredoc body doesn't confuse the outer
   balanced reader."
  [sc b strip? start-loc]
  ;; Capture horizontal whitespace before the tag.
  (loop [] (when (hspace? (peek1 sc)) (sb+ b (advance! sc)) (recur)))
  ;; Read the tag — same rules as read-heredoc-op!.
  (let [tag-b (sb)]
    (loop []
      (let [c (peek1 sc)]
        (cond
          (operator-end? c) nil
          (= c \')
          (do (sb+ b (advance! sc))                         ; opening '
              (loop []
                (cond
                  (at-end? sc)
                  (lex-error sc "unexpected EOF while looking for matching `''"
                             start-loc {:incomplete true})
                  (= (peek1 sc) \') (sb+ b (advance! sc))
                  :else (let [ch (advance! sc)]
                          (sb+ tag-b ch) (sb+ b ch) (recur))))
              (recur))
          (= c \")
          (do (sb+ b (advance! sc))
              (loop []
                (cond
                  (at-end? sc)
                  (lex-error sc "unexpected EOF while looking for matching `\"'"
                             start-loc {:incomplete true})
                  (= (peek1 sc) \") (sb+ b (advance! sc))
                  :else (let [ch (advance! sc)]
                          (sb+ tag-b ch) (sb+ b ch) (recur))))
              (recur))
          (= c \\)
          (do (sb+ b (advance! sc))
              (when-not (at-end? sc)
                (let [ch (advance! sc)] (sb+ tag-b ch) (sb+ b ch)))
              (recur))
          :else
          (let [ch (advance! sc)] (sb+ tag-b ch) (sb+ b ch) (recur)))))
    (let [tag (sb->s tag-b)]
      (when (clojure.core/empty? tag)
        (lex-error sc "syntax error near unexpected token `newline'"
                   start-loc {:hint "heredoc operator `<<` must be followed by a delimiter tag"}))
      ;; Capture the rest of the current line verbatim (no quote tracking).
      (loop []
        (cond
          (at-end? sc) nil
          (nl? (peek1 sc)) (sb+ b (advance! sc))
          :else (do (sb+ b (advance! sc)) (recur))))
      ;; Capture body lines until one matches the tag.
      (loop []
        (when-not (at-end? sc)
          (let [line-b (sb)]
            (loop []
              (when (and (not (at-end? sc)) (not (nl? (peek1 sc))))
                (sb+ line-b (advance! sc))
                (recur)))
            (let [raw-line (sb->s line-b)
                  cmp-line (if strip?
                             (clojure.string/replace raw-line #"^\t+" "")
                             raw-line)]
              (sb++ b raw-line)
              (when (nl? (peek1 sc)) (sb+ b (advance! sc)))
              (when-not (= cmp-line tag) (recur)))))))))

(defn- read-balanced!
  "Reads characters from `sc` until matching `close` is found, tracking
   nesting on `open`, and respecting single-quoted, double-quoted,
   backslash-escape, and heredoc regions so that characters inside
   them don't terminate the read. Returns the raw inner string
   (without the final `close`). The opening character must already be
   consumed.

   `closer-str` is the single character used in the error message
   (bash-style: `unexpected EOF while looking for matching `X'`)."
  [sc open close closer-str start-loc]
  (let [b (sb)
        depth (volatile! 1)]
    (loop []
      (cond
        (at-end? sc)
        (lex-error sc (str "unexpected EOF while looking for matching `" closer-str "'")
                   start-loc {:incomplete true})

        :else
        (let [c (peek1 sc)]
          (cond
            ;; <<TAG / <<-TAG heredoc (but not <<< here-string)
            (and (= c \<) (= (peek2 sc) \<)
                 (not= (peek-at sc 2) \<))
            (do (sb+ b (advance! sc))                  ; <
                (sb+ b (advance! sc))                  ; <
                (let [strip? (when (= (peek1 sc) \-)
                               (sb+ b (advance! sc)) true)]
                  (skip-heredoc-into! sc b strip? start-loc))
                (recur))

            (= c open)
            (do (sb+ b (advance! sc)) (vswap! depth inc) (recur))

            (= c close)
            (do (advance! sc)
                (vswap! depth dec)
                (if (zero? @depth)
                  (sb->s b)
                  (do (sb+ b close) (recur))))

            ;; backslash escapes the next char inside any of these contexts
            (= c \\)
            (do (sb+ b (advance! sc))
                (when-not (at-end? sc) (sb+ b (advance! sc)))
                (recur))

            ;; single-quoted: literal until next ' (no escapes inside)
            (= c \')
            (do (sb+ b (advance! sc))
                (loop []
                  (cond
                    (at-end? sc)
                    (lex-error sc "unexpected EOF while looking for matching `''"
                               start-loc {:incomplete true})
                    (= (peek1 sc) \') (sb+ b (advance! sc))
                    :else (do (sb+ b (advance! sc)) (recur))))
                (recur))

            ;; double-quoted: read until next unescaped "
            (= c \")
            (do (sb+ b (advance! sc))
                (loop []
                  (cond
                    (at-end? sc)
                    (lex-error sc "unexpected EOF while looking for matching `\"'"
                               start-loc {:incomplete true})
                    (= (peek1 sc) \\)
                    (do (sb+ b (advance! sc))
                        (when-not (at-end? sc) (sb+ b (advance! sc)))
                        (recur))
                    (= (peek1 sc) \") (sb+ b (advance! sc))
                    :else (do (sb+ b (advance! sc)) (recur))))
                (recur))

            :else
            (do (sb+ b (advance! sc)) (recur))))))))

(defn- read-balanced-2!
  "Reads characters until matching `close-pair` (a 2-char string like
   `))`), tracking nesting on `open-pair`. For arithmetic `$((..))`.

   The error message uses single-char `closer-str` to mirror bash,
   which reports `)` (not `))`) as the missing character."
  [sc ^String open-pair ^String close-pair closer-str start-loc]
  (let [b (sb)
        depth (volatile! 1)
        op0 (.charAt open-pair 0)
        op1 (.charAt open-pair 1)
        cl0 (.charAt close-pair 0)
        cl1 (.charAt close-pair 1)]
    (loop []
      (cond
        (at-end? sc)
        (lex-error sc (str "unexpected EOF while looking for matching `" closer-str "'")
                   start-loc {:incomplete true})

        ;; matched close
        (and (= (peek1 sc) cl0) (= (peek2 sc) cl1))
        (do (advance! sc) (advance! sc)
            (vswap! depth dec)
            (if (zero? @depth)
              (sb->s b)
              (do (sb+ b cl0) (sb+ b cl1) (recur))))

        ;; nested open
        (and (= (peek1 sc) op0) (= (peek2 sc) op1))
        (do (sb+ b (advance! sc)) (sb+ b (advance! sc))
            (vswap! depth inc) (recur))

        ;; backslash escape
        (= (peek1 sc) \\)
        (do (sb+ b (advance! sc))
            (when-not (at-end? sc) (sb+ b (advance! sc)))
            (recur))

        :else
        (do (sb+ b (advance! sc)) (recur))))))

;; ============================================================================
;; $-expansion: $VAR, ${...}, $(...), $((..)), $'...', $"..."
;; ============================================================================

(defn- read-dollar-part!
  "Called when peek1 = \\$. Returns a word-part map, or nil if $ is not
   followed by something we recognise (then $ is treated as a literal
   char by the caller)."
  [sc inside-dquoted?]
  (let [start (mark sc)
        c2 (peek2 sc)]
    (cond
      ;; $((arith))
      (and (= c2 \() (= (peek-at sc 2) \())
      (do (advance-n! sc 3)                          ; $((
          (let [body (read-balanced-2! sc "((" "))" ")" start)]
            (assoc (tok :arith start (mark sc) {:expr body}) :kind :arith)))

      ;; $(cmd)
      (= c2 \()
      (do (advance-n! sc 2)                          ; $(
          (let [body (read-balanced! sc \( \) ")" start)]
            (tok :cmd-subst start (mark sc) {:body body :form :paren})))

      ;; ${...}
      (= c2 \{)
      (do (advance-n! sc 2)                          ; ${
          (let [body (read-balanced! sc \{ \} "}" start)]
            (tok :var-ref start (mark sc) {:braced true :raw body})))

      ;; $'...' — ANSI-C quoting
      (= c2 \')
      (do (advance-n! sc 2)
          (let [b (sb)]
            (loop []
              (cond
                (at-end? sc) (lex-error sc "unexpected EOF while looking for matching `''"
                                        start {:incomplete true})
                (= (peek1 sc) \\)
                (do (sb+ b (advance! sc))
                    (when-not (at-end? sc) (sb+ b (advance! sc)))
                    (recur))
                (= (peek1 sc) \') (advance! sc)
                :else (do (sb+ b (advance! sc)) (recur))))
            (tok :ansi-c-quoted start (mark sc) {:raw (sb->s b)})))

      ;; $"..." — locale-aware quoting → REFUSED (i18n hook, nobody uses)
      (= c2 \") (refused sc "$\"...\" locale-aware quoting" start)

      ;; $NAME — simple variable
      (name-start? c2)
      (do (advance! sc)                              ; $
          (let [nb (sb)]
            (loop []
              (when (name-char? (peek1 sc))
                (sb+ nb (advance! sc))
                (recur)))
            (tok :var-ref start (mark sc) {:braced false :name (sb->s nb)})))

      ;; $? $$ $# $! $0 $- $@ $* — special parameters (single char)
      (#{\? \$ \# \! \- \@ \*} c2)
      (do (advance! sc)                              ; $
          (let [c (advance! sc)]
            (tok :var-ref start (mark sc)
                 {:braced false :name (str c) :special? true})))

      ;; $0..$9 — positional parameter (single digit)
      (digit? c2)
      (do (advance! sc)                              ; $
          (let [c (advance! sc)]
            (tok :var-ref start (mark sc)
                 {:braced false :name (str c) :positional? true})))

      :else nil)))

;; ============================================================================
;; Backtick command substitution
;; ============================================================================

(defn- read-backtick-part!
  "Called when peek1 = \\`. Reads until next unescaped backtick.
   The inner content is captured raw; per bash, backslash inside a
   backtick subst escapes only \\, `, and $ — but for the lexer we
   keep escapes raw and let downstream interpretation deal with them."
  [sc]
  (let [start (mark sc)]
    (advance! sc)                                     ; opening `
    (let [b (sb)]
      (loop []
        (cond
          (at-end? sc) (lex-error sc "unexpected EOF while looking for matching ``'"
                                  start {:incomplete true})
          (= (peek1 sc) \\)
          (do (sb+ b (advance! sc))
              (when-not (at-end? sc) (sb+ b (advance! sc)))
              (recur))
          (= (peek1 sc) \`) (advance! sc)
          :else (do (sb+ b (advance! sc)) (recur))))
      (tok :cmd-subst start (mark sc) {:body (sb->s b) :form :backtick}))))

;; ============================================================================
;; Single-quoted, double-quoted parts
;; ============================================================================

(defn- read-squoted-part!
  "'...' — no escapes, no interpolation. Newlines allowed inside."
  [sc]
  (let [start (mark sc)]
    (advance! sc)                                     ; opening '
    (let [b (sb)]
      (loop []
        (cond
          (at-end? sc) (lex-error sc "unexpected EOF while looking for matching `''"
                                  start {:incomplete true})
          (= (peek1 sc) \') (advance! sc)
          :else (do (sb+ b (advance! sc)) (recur))))
      (tok :squoted start (mark sc) {:value (sb->s b)}))))

(defn- read-dquoted-part!
  "\"...\" — interpolates $VAR / $(...) / `...`. Backslash escapes
   only $, \\, \", `, newline (line continuation)."
  [sc]
  (let [start (mark sc)]
    (advance! sc)                                     ; opening "
    (let [parts (volatile! [])
          litb  (volatile! (sb))
          flush-lit!
          (fn []
            (when-not (sb-empty? @litb)
              (vswap! parts conj
                      {:type :lit :value (sb->s @litb)})
              (vreset! litb (sb))))]
      (loop []
        (cond
          (at-end? sc) (lex-error sc "unexpected EOF while looking for matching `\"'"
                                  start {:incomplete true})

          (= (peek1 sc) \") (advance! sc)

          (= (peek1 sc) \\)
          (let [n (peek2 sc)]
            (cond
              ;; line continuation: \\\n erased
              (nl? n) (do (advance! sc)
                          (advance! sc)
                          (when (and (= n \return) (= (peek1 sc) \newline))
                            (advance! sc))
                          (recur))
              ;; \$, \\, \", \` → escape recognised
              (#{\$ \\ \" \`} n)
              (do (advance! sc)
                  (sb+ @litb (advance! sc))
                  (recur))
              ;; any other: keep backslash literal in the string
              :else
              (do (sb+ @litb (advance! sc))
                  (when-not (at-end? sc) (sb+ @litb (advance! sc)))
                  (recur))))

          (= (peek1 sc) \$)
          (let [d (read-dollar-part! sc true)]
            (if d
              (do (flush-lit!)
                  (vswap! parts conj d)
                  (recur))
              ;; literal $
              (do (sb+ @litb (advance! sc)) (recur))))

          (= (peek1 sc) \`)
          (do (flush-lit!)
              (vswap! parts conj (read-backtick-part! sc))
              (recur))

          :else
          (do (sb+ @litb (advance! sc)) (recur))))
      (flush-lit!)
      (tok :dquoted start (mark sc) {:parts @parts}))))

;; ============================================================================
;; Word: a run of word-parts until an unquoted operator/whitespace
;; ============================================================================

(defn- read-tilde-prefix!
  "If word starts with ~, capture optional username up to / or word-stop.
   Returns the tilde part and advances past it."
  [sc]
  (let [start (mark sc)]
    (advance! sc)                                     ; consume ~
    (let [b (sb)]
      (loop []
        (let [c (peek1 sc)]
          (when (and c
                     (not (word-end? c))
                     (not= c \/))
            (sb+ b (advance! sc))
            (recur))))
      (tok :tilde start (mark sc) {:user (sb->s b)}))))

(defn- read-brace-exp-part!
  "Attempt to read a brace expansion `{a,b,c}` or `{1..10}`.
   Returns the part on success; on failure, restores position and
   returns nil (the brace was a literal `{`).

   Cheap guard: if `{` is followed by whitespace/EOL it can't be a
   brace-exp (those forms always have `{a,...` content), so we return
   nil immediately without consuming. This is also what keeps a
   brace-group `{ ls; }` from triggering a slow backtrack scan.

   Tracks single- and double-quoted regions so commas/dots inside a
   quoted literal don't accidentally trigger brace-exp detection
   (e.g. `{ echo \"a, b\"; }` is a brace-group, not an expansion)."
  [sc]
  (let [start (mark sc)
        n2 (peek2 sc)]
    (if (or (nil? n2) (hspace? n2) (nl? n2))
      nil
      (let [save-pos @(:pos sc)
            save-line @(:line sc)
            save-col @(:col sc)]
        (advance! sc)                                     ; consume {
        (let [b (sb)
              depth (volatile! 1)
              has-comma? (volatile! false)
              has-range? (volatile! false)
              skip-quoted!
              (fn [^Character q]
                (sb+ b (advance! sc))                     ; opening quote
                (loop []
                  (cond
                    (at-end? sc) nil
                    (= (peek1 sc) \\)
                    (do (sb+ b (advance! sc))
                        (when-not (at-end? sc) (sb+ b (advance! sc)))
                        (recur))
                    (= (peek1 sc) q) (sb+ b (advance! sc))
                    :else (do (sb+ b (advance! sc)) (recur)))))
              fail!
              (fn []
                (vreset! (:pos sc) save-pos)
                (vreset! (:line sc) save-line)
                (vreset! (:col sc) save-col)
                nil)]
          (loop []
            (cond
              (at-end? sc) (fail!)

              (= (peek1 sc) \\)
              (do (sb+ b (advance! sc))
                  (when-not (at-end? sc) (sb+ b (advance! sc)))
                  (recur))

              (= (peek1 sc) \')
              (do (skip-quoted! \') (recur))

              (= (peek1 sc) \")
              (do (skip-quoted! \") (recur))

              (= (peek1 sc) \{)
              (do (sb+ b (advance! sc)) (vswap! depth inc) (recur))

              (= (peek1 sc) \})
              (do (advance! sc)
                  (vswap! depth dec)
                  (if (zero? @depth)
                    (if (or @has-comma? @has-range?)
                      (tok :brace-exp start (mark sc)
                           {:raw (sb->s b)
                            :kind (if @has-range? :range :list)})
                      (fail!))
                    (do (sb+ b \}) (recur))))

              (and (= @depth 1) (= (peek1 sc) \,))
              (do (vreset! has-comma? true)
                  (sb+ b (advance! sc)) (recur))

              (and (= @depth 1)
                   (= (peek1 sc) \.) (= (peek2 sc) \.))
              (do (vreset! has-range? true)
                  (sb+ b (advance! sc))
                  (sb+ b (advance! sc))
                  (recur))

              :else
              (do (sb+ b (advance! sc)) (recur)))))))))

(defn- read-word! [sc]
  (let [start (mark sc)
        parts (volatile! [])
        litb  (volatile! (sb))
        flush-lit!
        (fn []
          (when-not (sb-empty? @litb)
            (vswap! parts conj {:type :lit :value (sb->s @litb)})
            (vreset! litb (sb))))]
    ;; Handle initial tilde-prefix:
    ;;   ~          → home
    ;;   ~/path     → home + /path
    ;;   ~user      → user's home
    ;;   ~user/path → user's home + /path
    (when (and (= (peek1 sc) \~)
               (let [n (peek2 sc)]
                 (or (nil? n) (word-end? n)
                     (= n \/)
                     (name-start? n))))
      (vswap! parts conj (read-tilde-prefix! sc)))
    ;; Main word loop. NB: dispatch into special-clause readers BEFORE
    ;; checking word-end, since none of `$ ' " \\` `` ` `` are word-end.
    (loop []
      (let [c (peek1 sc)]
        (cond
          (word-end? c) nil                          ; done

          (= c \')
          (do (flush-lit!) (vswap! parts conj (read-squoted-part! sc)) (recur))

          (= c \")
          (do (flush-lit!) (vswap! parts conj (read-dquoted-part! sc)) (recur))

          (= c \$)
          (let [d (read-dollar-part! sc false)]
            (if d
              (do (flush-lit!) (vswap! parts conj d) (recur))
              (do (sb+ @litb (advance! sc)) (recur))))

          (= c \`)
          (do (flush-lit!) (vswap! parts conj (read-backtick-part! sc)) (recur))

          (= c \\)
          (let [n (peek2 sc)]
            (cond
              ;; line continuation: erased
              (nl? n) (do (advance! sc) (advance! sc)
                          (when (and (= n \return) (= (peek1 sc) \newline))
                            (advance! sc))
                          (recur))
              (nil? n) (do (sb+ @litb (advance! sc)) (recur))
              :else (do (flush-lit!)
                        (advance! sc)
                        (let [ch (advance! sc)]
                          (vswap! parts conj
                                  {:type :escape :value (str ch)}))
                        (recur))))

          (= c \{)
          (if-let [br (read-brace-exp-part! sc)]
            (do (flush-lit!) (vswap! parts conj br) (recur))
            ;; not a brace-exp; treat as literal char
            (do (sb+ @litb (advance! sc)) (recur)))

          ;; refused: process substitution <(cmd) / >(cmd)
          (and (or (= c \<) (= c \>)) (= (peek2 sc) \())
          (refused sc (str c "(cmd) process substitution") (mark sc))

          :else
          (do (sb+ @litb (advance! sc)) (recur)))))
    (flush-lit!)
    (let [end (mark sc)]
      (when (empty? @parts)
        (lex-error sc "internal: read-word produced empty parts" start))
      (tok :word start end {:parts @parts}))))

;; ============================================================================
;; Operators and redirections
;; ============================================================================

;; Reserved words — recognised at command position by the parser, not
;; the lexer. The lexer emits them as ordinary :word tokens; the parser
;; checks for the :reserved? marker we attach when a word is exactly
;; one of these names.
(def ^:private reserved-words
  #{"if" "then" "elif" "else" "fi"
    "for" "in" "do" "done"
    "while" "until"
    "case" "esac"
    "select"
    "function"
    "time"
    "!"})

(defn- maybe-mark-reserved [tok]
  (let [parts (:parts tok)]
    (if (and (= 1 (count parts))
             (= :lit (:type (first parts)))
             (reserved-words (:value (first parts))))
      (assoc tok :reserved (:value (first parts)))
      tok)))

(defn- read-heredoc-op!
  "At `<<` or `<<-`. Reads the tag (which may be quoted), registers a
   pending heredoc, returns the redirection token. The tag word itself
   is NOT consumed as a normal word — it's part of this token. After
   the operator we still need a way to look up the body once we hit
   the next newline, so we store enough state in the scanner."
  [sc fd]
  (let [start (mark sc)
        strip? (do (advance! sc)                      ; <
                   (advance! sc)                      ; <
                   (when (= (peek1 sc) \-)
                     (advance! sc)                    ; -
                     true))]
    ;; skip horizontal ws before tag (bash requires no space, but be lenient)
    (loop [] (when (hspace? (peek1 sc)) (advance! sc) (recur)))
    ;; Read tag: may be unquoted, single-quoted, or double-quoted.
    ;; Quoting any part of the tag → body is literal (no expansion).
    (let [tag-b (sb)
          quoted? (volatile! false)]
      (loop []
        (let [c (peek1 sc)]
          (cond
            ;; Tag ends at any operator/whitespace boundary (but quote
            ;; chars are NOT terminators — they're handled below).
            (operator-end? c) nil
            (= c \')
            (do (vreset! quoted? true)
                (advance! sc)
                (loop []
                  (cond
                    (at-end? sc) (lex-error sc "unexpected EOF in heredoc tag" start
                                            {:incomplete true})
                    (= (peek1 sc) \') (advance! sc)
                    :else (do (sb+ tag-b (advance! sc)) (recur))))
                (recur))
            (= c \")
            (do (vreset! quoted? true)
                (advance! sc)
                (loop []
                  (cond
                    (at-end? sc) (lex-error sc "unexpected EOF in heredoc tag" start
                                            {:incomplete true})
                    (= (peek1 sc) \") (advance! sc)
                    :else (do (sb+ tag-b (advance! sc)) (recur))))
                (recur))
            (= c \\)
            (do (vreset! quoted? true)
                (advance! sc)
                (when-not (at-end? sc) (sb+ tag-b (advance! sc)))
                (recur))
            :else
            (do (sb+ tag-b (advance! sc)) (recur)))))
      (let [tag (sb->s tag-b)]
        (when (clojure.core/empty? tag)
          (lex-error sc "syntax error near unexpected token `newline'" start
                     {:hint "heredoc operator `<<` must be followed by a delimiter tag"}))
        ;; Register pending heredoc (will be filled when newline drains).
        (let [op-tok (tok :redir-heredoc start (mark sc)
                          {:fd fd :tag tag :strip? strip? :expand? (not @quoted?)
                           :body nil})]
          (vswap! (:pending-heredocs sc) conj
                  ;; We attach a vreset on the token's :body once known.
                  ;; The actual mechanism is: we store the index in the
                  ;; output vector where this token sits, but the lexer
                  ;; main loop will fix it up after drain. Simpler:
                  ;; emit the token now without body, and the main loop
                  ;; mutates the output vector entry.
                  op-tok)
          op-tok)))))

(defn- read-heredoc-body!
  "Drain one heredoc body. Reads lines until one matches the tag.
   `strip?` true strips leading TABS from each body line AND from the
   tag-match line. `expand?` is informational; the lexer stores the raw
   body string and expand.cljc decides whether to interpret it."
  [sc {:keys [tag strip?]}]
  (let [start (mark sc)
        b (sb)]
    (loop []
      (cond
        (at-end? sc)
        ;; Bash warns but accepts EOF without delimiter; we mirror that.
        (sb->s b)

        :else
        (let [line-start-pos @(:pos sc)
              ;; read one line into a local sb
              line-b (sb)]
          (loop []
            (when (and (not (at-end? sc)) (not (nl? (peek1 sc))))
              (sb+ line-b (advance! sc))
              (recur)))
          (let [raw-line (sb->s line-b)
                cmp-line (if strip?
                           (clojure.string/replace raw-line #"^\t+" "")
                           raw-line)]
            (if (= cmp-line tag)
              (do (when (nl? (peek1 sc)) (advance! sc))
                  (sb->s b))
              (do
                (sb++ b (if strip?
                          (clojure.string/replace raw-line #"^\t+" "")
                          raw-line))
                (when (nl? (peek1 sc))
                  (sb+ b (advance! sc)))
                (recur)))))))))

(defn- drain-heredocs!
  "Called when the lexer has hit a newline at top-level. For each
   pending heredoc, read its body and update the corresponding token
   in `out-vol`."
  [sc out-vol]
  (let [pending @(:pending-heredocs sc)]
    (vreset! (:pending-heredocs sc) [])
    (doseq [op-tok pending]
      (let [body (read-heredoc-body! sc op-tok)
            ;; Find the token in out-vol by :offset and update it.
            v @out-vol
            idx (some (fn [[i t]] (when (= (:offset t) (:offset op-tok)) i))
                      (map-indexed vector v))]
        (when idx
          (vswap! out-vol assoc idx (assoc (get v idx) :body body)))))))

(defn- read-operator-token!
  "At an operator character. Recognises (longest match):
     ||  &&  ;;  ;
     |&  |
     &>  &>>  &
     >>  >|  >&  >
     <<<  <<-  <<  <&  <>  <
     ((  )  )  (
     {  }
     [[  ]]  [  ]
     !
   For redirections (with optional FD prefix), see read-redir-with-fd."
  [sc]
  (let [start (mark sc)
        c (peek1 sc)]
    (cond
      ;; ||
      (and (= c \|) (= (peek2 sc) \|))
      (do (advance-n! sc 2) (tok :or  start (mark sc)))

      ;; |&  — pipe-merging stderr+stdout
      (and (= c \|) (= (peek2 sc) \&))
      (do (advance-n! sc 2) (tok :pipe-amp start (mark sc)))

      ;; |
      (= c \|) (do (advance! sc) (tok :pipe start (mark sc)))

      ;; &&
      (and (= c \&) (= (peek2 sc) \&))
      (do (advance-n! sc 2) (tok :and start (mark sc)))

      ;; &>> &>
      (and (= c \&) (= (peek2 sc) \>))
      (cond
        (= (peek-at sc 2) \>)
        (do (advance-n! sc 3) (tok :redir-all-append start (mark sc) {:fd nil}))
        :else
        (do (advance-n! sc 2) (tok :redir-all start (mark sc) {:fd nil})))

      ;; &
      (= c \&) (do (advance! sc) (tok :amp start (mark sc)))

      ;; case clause terminators:
      ;;   ;;&  try next clause's patterns
      ;;   ;;   stop after this clause (most common)
      ;;   ;&   fall through to next clause unconditionally
      (and (= c \;) (= (peek2 sc) \;) (= (peek-at sc 2) \&))
      (do (advance-n! sc 3) (tok :semi-semi-amp start (mark sc)))

      (and (= c \;) (= (peek2 sc) \;))
      (do (advance-n! sc 2) (tok :semi-semi start (mark sc)))

      (and (= c \;) (= (peek2 sc) \&))
      (do (advance-n! sc 2) (tok :semi-amp start (mark sc)))

      ;; ;
      (= c \;) (do (advance! sc) (tok :semi start (mark sc)))

      ;; (( arith command  vs  ( subshell
      (and (= c \() (= (peek2 sc) \())
      (do (advance-n! sc 2)
          (let [body (read-balanced-2! sc "((" "))" ")" start)]
            (tok :arith-cmd start (mark sc) {:expr body})))

      (= c \() (do (advance! sc) (tok :lparen start (mark sc)))
      (= c \)) (do (advance! sc) (tok :rparen start (mark sc)))

      ;; <<<  <<-  <<  <&  <>  <(refused)  <
      (= c \<)
      (cond
        (and (= (peek2 sc) \<) (= (peek-at sc 2) \<))
        (do (advance-n! sc 3) (tok :redir-here-string start (mark sc) {:fd nil}))

        (= (peek2 sc) \<)
        (read-heredoc-op! sc nil)

        (= (peek2 sc) \&)
        (do (advance-n! sc 2) (tok :redir-dup-in start (mark sc) {:fd nil}))

        (= (peek2 sc) \>)
        (do (advance-n! sc 2) (tok :redir-rw start (mark sc) {:fd nil}))

        (= (peek2 sc) \()
        (refused sc "<(cmd) process substitution" start)

        :else
        (do (advance! sc) (tok :redir-in start (mark sc) {:fd nil})))

      ;; >>  >|  >&  >(refused)  >
      (= c \>)
      (cond
        (= (peek2 sc) \>) (do (advance-n! sc 2) (tok :redir-append start (mark sc) {:fd nil}))
        (= (peek2 sc) \|) (do (advance-n! sc 2) (tok :redir-clobber start (mark sc) {:fd nil}))
        (= (peek2 sc) \&) (do (advance-n! sc 2) (tok :redir-dup-out start (mark sc) {:fd nil}))
        (= (peek2 sc) \()  (refused sc ">(cmd) process substitution" start)
        :else (do (advance! sc) (tok :redir-out start (mark sc) {:fd nil})))

      :else
      (lex-error sc (str "syntax error near unexpected token `" c "'") start))))

(defn- maybe-read-fd-prefixed-redir!
  "If the next chars are digit(s) followed by < or >, consume the FD
   prefix, then call read-operator-token!. Otherwise return nil."
  [sc]
  (let [src (:src sc)
        save-pos @(:pos sc)
        save-line @(:line sc)
        save-col @(:col sc)
        i (volatile! save-pos)]
    (loop []
      (when (and (< @i (:len sc))
                 (digit? (.charAt ^String src @i)))
        (vswap! i inc)
        (recur)))
    (let [next-ch (when (< @i (:len sc)) (.charAt ^String src @i))]
      (if (and (not= @i save-pos) (or (= next-ch \<) (= next-ch \>)))
        (let [start (mark sc)
              fd-str (subs src save-pos @i)
              fd (#?(:clj Integer/parseInt :cljs js/parseInt) fd-str)]
          ;; advance scanner past digits
          (loop [] (when (< @(:pos sc) @i) (advance! sc) (recur)))
          (let [redir-tok (read-operator-token! sc)]
            ;; Attach :fd to the redirection token if it's a redirection.
            (if (#{:redir-out :redir-append :redir-clobber :redir-dup-out
                   :redir-in :redir-rw :redir-dup-in :redir-here-string
                   :redir-heredoc :redir-all :redir-all-append} (:type redir-tok))
              (assoc redir-tok :fd fd
                     :line (:line start) :col (:col start) :offset (:offset start))
              ;; not a redirection — restore and fail through to word
              (do (vreset! (:pos sc) save-pos)
                  (vreset! (:line sc) save-line)
                  (vreset! (:col sc) save-col)
                  nil))))
        nil))))

;; ============================================================================
;; Main loop
;; ============================================================================

(defn- read-token!
  "Reads the next token. Returns nil at EOF (caller handles)."
  [sc]
  (cond
    (at-end? sc) nil

    (nl? (peek1 sc))
    (let [start (mark sc)]
      (advance! sc)
      (when (and (= (peek-at sc -1) \return) (= (peek1 sc) \newline))
        (advance! sc))
      (tok :newline start (mark sc)))

    ;; FD-prefixed redirection? digits then < or >
    (digit? (peek1 sc))
    (or (maybe-read-fd-prefixed-redir! sc)
        ;; otherwise it's a word starting with a digit
        (read-word! sc))

    ;; operator chars
    (#{\| \& \; \< \> \( \)} (peek1 sc))
    (read-operator-token! sc)

    ;; refused: extended globs ?(  *(  +(  @(  !(   at word position
    (and (#{\? \* \+ \@ \!} (peek1 sc))
         (= (peek2 sc) \())
    (refused sc (str (peek1 sc) "(...) extended-glob") (mark sc))

    :else
    (read-word! sc)))

(defn tokenize
  "Tokenize bash source. Returns a vector of token maps ending in `:eof`.
   Throws `ex-info {:type ::lex-error}` on lexical errors, and
   `ex-info {:type ::refused}` on constructs we refuse at lex."
  [^String src]
  (let [sc (scanner src)
        out (volatile! [])]
    (loop []
      (skip-hspace-and-comments! sc)
      (if (at-end? sc)
        (do (when (seq @(:pending-heredocs sc))
              ;; EOF without seeing the heredoc-trigger newline; drain
              ;; pending heredocs against the rest (empty) input.
              (drain-heredocs! sc out))
            (vswap! out conj (tok :eof (mark sc) (mark sc)))
            @out)
        (let [t (read-token! sc)]
          (cond
            (nil? t) (recur)
            (= :word (:type t))
            (do (vswap! out conj (maybe-mark-reserved t))
                (recur))
            (= :newline (:type t))
            (do (vswap! out conj t)
                (when (seq @(:pending-heredocs sc))
                  (drain-heredocs! sc out))
                (recur))
            :else
            (do (vswap! out conj t) (recur))))))))
