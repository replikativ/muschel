(ns muschel.permit
  "Permit layer for muschel.

   `check` walks an AST and, for each `:call` node, asks the ruleset
   what to do. Possible per-call decisions are `:allow`, `:deny`,
   `:ask`. For `:ask` decisions we delegate to a caller-provided
   `prompter` function so the harness (e.g. dvergr) drives the user
   interaction.

   Heredoc bodies fed to a shell interpreter on stdin (e.g.
   `bash <<EOF rm -rf / EOF`) are re-parsed and recursively checked —
   the inner `rm` is gated even though it's wrapped in a heredoc. Data
   sinks (`cat <<EOF data EOF`) are left opaque.

   ## Rule shape

       {:tool    :bash
        :pattern <one of the matcher kinds below>
        :action  :allow | :deny | :ask
        :reason  \"optional explanation\"
        :origin  :default | :user | :session
        :id      \"rule-abc\"}                ; optional, for tooling

   ## Matcher kinds

   - `{:kind :cmd-name :name \"git\"}`
       Matches any call whose first arg is the literal `git`.

   - `{:kind :argv-glob :glob \"git status*\"}`
       Joins the call's argv with spaces, matches against a bash-style
       glob (`*` `?` `[abc]`). Practical for `Bash(git *)` style rules.

   - `{:kind :argv-vec :vec [\"git\" \"status\"]}`
       Exact PREFIX match. Vec elements can be strings or SETS, e.g.
       `[\"git\" #{\"status\" \"diff\" \"log\"}]`. A rule of length N
       matches any argv ≥ N elements whose first N agree.

   - `{:kind :argv-shape :shape [\"git\" \"push\"]}`
       Exact SHAPE match (length matters). Use this to allow `git push`
       while denying `git push --force` — under `:argv-vec` the longer
       form is a prefix-match for the same rule. Each shape element is:
       a string (must equal), a set (must be member), `:*` (any single
       arg), `:**` (zero-or-more args, only as the last element), or a
       regex (`re-find` must match). Length must match exactly unless
       the last element is `:**`.

   - `{:kind :argv-flags :head [\"rm\"] :any-of #{\"-r\" \"--recursive\"}}`
       Order-INSENSITIVE flag match. `:head` is an ordered prefix (same
       semantics as `:argv-vec`); the tail is normalised into a *set* of
       flag tokens, then tested against:
         `:any-of`  — at least one element must appear in the tail
         `:all-of`  — every element must appear in the tail
         `:none-of` — no element may appear in the tail
       At least one of those keys must be present. Tail normalisation:
       `--foo=bar` → `\"--foo\"`; `-rf` → `\"-r\"` + `\"-f\"`
       (POSIX short clusters of letters); `--` terminates option
       parsing (everything after is positional, not added to the set);
       bare `-` and non-`-` tokens are positional, ignored. Use this for
       \"flag X anywhere in argv\" rules where `:argv-shape` would have
       to enumerate every position. If any tail token is dynamic (e.g.
       `$cmd`), the matcher fails closed → the call falls through to
       the prompter, same as `:argv-vec` for dynamic args.

   - `{:kind :ast-pred :pred (fn [call-node] ...)}`
       Full predicate over the AST node. Escape hatch for anything the
       data forms can't express.

   ## Rulesets

   Rulesets are vectors of rules. `check` takes a sequence of
   rulesets (typically `[defaults user-rules session-rules]`); they
   are evaluated in order, with later rules overriding earlier ones
   for the same call (opencode-style last-match-wins).

   ## Prompter

   `prompter` is a function:

       (fn [{:keys [cmd argv ast reason matched-rule]}]
         {:result :allow-once | :allow-always | :deny-once | :deny-always
          :scope  :exact | :argv-prefix | :cmd-name})    ; for *-always only

   `:allow-always` / `:deny-always` generates a new rule whose pattern
   matches per `:scope`; the rule is returned via `:new-rules` in
   `check`'s result. The caller decides where to persist it
   (typically by appending to the user or session ruleset).

   ## Defaults

   `default-rules` loads `resources/muschel/default-permit.edn` —
   ships an auto-allow set for read-only commands, an auto-deny set
   for destructive ones, and `:ask` as the implicit fallback."
  (:require [clojure.string :as str]
            [muschel.ast :as ast]
            [muschel.expand :as expand]
            [muschel.parse :as parse]
            [muschel.permit.defaults :as defaults]))

;; ============================================================================
;; Extract command-name + argv from a :call AST when statically determinable
;; ============================================================================

(defn- word->literal
  "If the word is purely literal (no expansions), return its string
   value; else nil. (Dynamic args don't get matched by string-based
   rules; the ast-pred matcher can still inspect them.)"
  [word]
  (ast/word-literal word))

(defn- call->argv
  "Best-effort argv extraction from a `:call` node. Returns a vector
   of strings, with `nil` entries for words that aren't pure
   literals. The pattern matchers treat nil entries conservatively."
  [call]
  (mapv word->literal (:args call)))

(defn- call->cmd-name
  "First arg of a :call when it's a pure literal; else nil."
  [call]
  (when-let [a (first (:args call))]
    (word->literal a)))

;; ============================================================================
;; Matchers
;; ============================================================================

(defn- argv-matches-vec?
  "Vec-style match. Each rule element is either a string (must equal)
   or a set (call element must be a member). Rule may be shorter than
   argv (prefix match)."
  [argv rule-vec]
  (and (>= (count argv) (count rule-vec))
       (every? true?
               (map (fn [r a]
                      (cond
                        (nil? a) false                  ; dynamic arg, no match
                        (set? r) (contains? r a)
                        :else (= r a)))
                    rule-vec argv))))

(defn- argv-matches-glob?
  "Join argv with spaces, glob-match against pattern."
  [argv glob]
  (when (every? some? argv)
    (let [s (str/join " " argv)
          rx (re-pattern (str "^" (expand/glob->regex glob) "$"))]
      (boolean (re-find rx s)))))

(defn- shape-elt-matches?
  "True if shape element `el` matches a single argv element `a` (which
   may be nil for dynamic words)."
  [el a]
  (cond
    (nil? a) false                ; dynamic argv element — only :* / :** match
    (= :* el) true
    (string? el) (= el a)
    (set? el) (contains? el a)
    #?(:clj (instance? java.util.regex.Pattern el)
       :cljs (regexp? el))
    (boolean (re-find el a))
    :else false))

(defn- argv-matches-shape?
  "Exact-shape match. Last element `:**` makes the tail open-ended;
   otherwise lengths must agree. `:*` (or a string / set / regex) matches
   one slot."
  [argv shape]
  (let [open? (= :** (last shape))
        head  (if open? (butlast shape) shape)
        head-cnt (count head)
        argv-cnt (count argv)]
    (cond
      open? (and (>= argv-cnt head-cnt)
                 (every? true?
                         (map shape-elt-matches? head (take head-cnt argv))))
      :else (and (= head-cnt argv-cnt)
                 (every? true?
                         (map shape-elt-matches? head argv))))))

;; ---- :argv-flags helpers ----------------------------------------------------

(defn- letters-only? [^String s]
  (and (pos? (count s))
       #?(:clj  (every? #(Character/isLetter ^Character %) s)
          :cljs (boolean (re-matches #"[A-Za-z]+" s)))))

(defn- short-cluster->flags
  "Split a POSIX short-flag cluster like `-rf` into `[\"-r\" \"-f\"]`.
   If the cluster contains non-letters (e.g. `-2`, `-o=foo`), return
   the token unchanged — we don't have an option spec, so don't guess."
  [^String t]
  (let [rest-chars (subs t 1)]
    (if (letters-only? rest-chars)
      (mapv #(str "-" %) rest-chars)
      [t])))

(defn- token->flag-tokens
  "Normalise one argv token into the flag tokens it contributes to the
   tail flag-set. nil for positional / non-flag tokens."
  [^String t]
  (cond
    (or (= t "-") (not (str/starts-with? t "-"))) nil
    (str/starts-with? t "--") [(first (str/split t #"=" 2))]
    :else (short-cluster->flags t)))

(defn argv->flag-set
  "Normalise an argv tail into the set of flag tokens it carries.
   Stops at `--` (POSIX option terminator). Returns nil if any token
   is itself nil — caller treats that as 'can't decide, match fails'.

   Examples (the tail does NOT include the command name):
     [\"-rf\" \"/tmp\"]            => #{\"-r\" \"-f\"}
     [\"--force\" \"--prefix=/x\"] => #{\"--force\" \"--prefix\"}
     [\"--\" \"-r\"]               => #{}        ; -r is positional after --
     [nil \"-r\"]                  => nil"
  [tail]
  (loop [in tail
         acc #{}]
    (if (empty? in)
      acc
      (let [t (first in)]
        (cond
          (nil? t)   nil
          (= t "--") acc
          :else      (recur (rest in) (into acc (token->flag-tokens t))))))))

(defn- head-prefix-matches?
  "Like argv-matches-vec? but used to check just the ordered head of
   an `:argv-flags` rule."
  [argv head]
  (and (>= (count argv) (count head))
       (every? true?
               (map (fn [r a]
                      (cond
                        (nil? a) false
                        (set? r) (contains? r a)
                        :else    (= r a)))
                    head argv))))

(defn- argv-matches-flags?
  "Flag-set match. `head` is an ordered prefix; the tail (everything
   after) is normalised to a flag set and compared against any-of /
   all-of / none-of. At least one constraint must be present."
  [argv {:keys [head any-of all-of none-of]}]
  (and (head-prefix-matches? argv head)
       (or any-of all-of none-of)
       (let [tail (drop (count head) argv)
             flags (argv->flag-set tail)]
         (and (some? flags)
              (or (nil? any-of)  (boolean (some flags any-of)))
              (or (nil? all-of)  (every? flags all-of))
              (or (nil? none-of) (not-any? flags none-of))))))

(defn rule-matches?
  "True if `rule`'s pattern matches `call`."
  [rule call]
  (let [{:keys [kind name vec glob shape pred] :as pattern} (:pattern rule)
        cmd-name (call->cmd-name call)
        argv (call->argv call)]
    (case kind
      :cmd-name   (= name cmd-name)
      :argv-glob  (argv-matches-glob? argv glob)
      :argv-vec   (argv-matches-vec? argv vec)
      :argv-shape (argv-matches-shape? argv shape)
      :argv-flags (argv-matches-flags? argv pattern)
      :ast-pred   (try (boolean (pred call)) (catch #?(:clj Throwable :cljs :default) _ false))
      false)))

;; ============================================================================
;; Rule lookup
;; ============================================================================

(defn- find-matching-rule
  "Walk rulesets in order, then within each ruleset find the LAST
   matching rule (opencode-style). Across rulesets, the LATER ruleset
   wins. Returns the matching rule or nil."
  [rulesets call]
  (let [all (mapcat identity rulesets)]
    (->> all
         (filter #(rule-matches? % call))
         last)))

;; ============================================================================
;; Default rules
;; ============================================================================

(def default-rules
  "The shipped default ruleset (defined in `muschel.permit.defaults`).
   Uniform across JVM / babashka / ClojureScript — no resource I/O.
   The previous EDN form lives under `resources/muschel/default-permit.edn`
   as a human-readable reference."
  defaults/default-rules)

;; ============================================================================
;; Per-call decision (pure)
;; ============================================================================

(defn per-call-decision
  "Pure: examine a :call node against rulesets; return its decision
   `{:call <node> :decision :allow|:deny|:ask :rule <rule-or-nil>
     :reason str}`. Default for unmatched calls is `:ask`."
  [rulesets call]
  (if-let [rule (find-matching-rule rulesets call)]
    {:call call
     :decision (:action rule)
     :rule rule
     :reason (or (:reason rule) "matched rule")}
    {:call call
     :decision :ask
     :rule nil
     :reason "no matching rule (default: ask)"}))

;; ============================================================================
;; Heredoc recursion into shell interpreters
;; ============================================================================
;;
;; When `bash <<EOF … EOF` (or sh/dash/zsh) reads a heredoc as stdin, the
;; body IS a shell script — so we re-parse it and check the inner calls,
;; same idea as how exec.cljc re-runs cmd-subst bodies through the permit
;; gate. Data sinks like `cat <<EOF … EOF` keep the body opaque.

(def ^:private shell-cmd-names #{"sh" "bash" "dash" "zsh"})

(defn- shell-stdin-script?
  "True if `call`'s argv is a shell reading its script from stdin —
   i.e. first arg is a known shell and there is no `-c` flag (which
   would mean the script is the inline arg, and the heredoc is data
   for that script's own stdin)."
  [call]
  (let [argv (call->argv call)]
    (and (shell-cmd-names (first argv))
         (not (some #{"-c"} (rest argv))))))

(defn- stdin-heredoc-body
  "If `node`'s :redirs contain a heredoc whose target is stdin (fd 0
   or unspecified), return its body string; else nil. The parser
   attaches `:redirs` to the :call (not the wrapping :stmt) for
   simple commands; this helper reads it from whatever node carries
   it."
  [node]
  (some (fn [r]
          (when (and (= :heredoc (:type r))
                     (or (nil? (:fd r)) (zero? (:fd r))))
            (:body r)))
        (:redirs node)))

(defn- shell-heredoc-bodies
  "Walk `ast`. Return body strings of every heredoc that feeds a
   shell interpreter's stdin. The bodies are bash code; the caller
   re-parses and recursively permit-checks them. For simple
   commands the parser puts `:redirs` on the `:call` node itself."
  [ast]
  (let [out (volatile! [])]
    (ast/walk ast
              (fn [n]
                (when (and (map? n)
                           (ast/call? n)
                           (shell-stdin-script? n))
                  (when-let [body (stdin-heredoc-body n)]
                    (vswap! out conj body)))))
    @out))

(defn check-pure
  "Pure check (no prompter). For each :call in the AST, return a
   per-call decision. Heredoc bodies fed to shell interpreters are
   re-parsed and their inner calls are included. Overall :decision
   is the worst (deny > ask > allow)."
  [rulesets ast]
  (let [calls (ast/leaf-calls ast)
        per-call (mapv #(per-call-decision rulesets %) calls)
        inner-per-call
        (vec (mapcat (fn [body]
                       (let [inner (try (parse/parse body)
                                        (catch #?(:clj Throwable :cljs :default) _ nil))]
                         (when inner
                           (:per-call (check-pure rulesets inner)))))
                     (shell-heredoc-bodies ast)))
        all-per-call (into per-call inner-per-call)
        worst (cond
                (some #(= :deny (:decision %)) all-per-call) :deny
                (some #(= :ask  (:decision %)) all-per-call) :ask
                :else :allow)]
    {:decision worst
     :per-call all-per-call}))

;; ============================================================================
;; Promoter: prompter-result → new rule
;; ============================================================================

(defn- mk-rule-from-prompt
  "When prompter returns :allow-always or :deny-always with a :scope,
   build a rule capturing that decision so the caller can persist it."
  [{:keys [result scope]} call action-fallback]
  (let [argv (call->argv call)
        cmd (first argv)]
    (when (and cmd (#{:allow-always :deny-always} result))
      (let [pattern (case scope
                      :exact         {:kind :argv-vec :vec (vec argv)}
                      :argv-prefix   {:kind :argv-glob
                                      :glob (str (str/join " " argv) " *")}
                      :cmd-name      {:kind :cmd-name :name cmd}
                      ;; default: cmd-name
                      {:kind :cmd-name :name cmd})]
        {:tool   :bash
         :pattern pattern
         :action  (if (= result :allow-always) :allow :deny)
         :reason  "promoted from prompt"
         :origin  :session}))))

;; ============================================================================
;; Check with prompter integration
;; ============================================================================

(defn deny-all-prompter
  "Default prompter: always returns :deny-once. Use when no UI is
   available — anything not pre-allowed is denied."
  [_]
  {:result :deny-once})

(defn allow-all-prompter
  "Convenience prompter for tests and dev: always returns
   :allow-once. DO NOT use in production for untrusted agents."
  [_]
  {:result :allow-once})

(defn check
  "Walk the AST against `rulesets`. For each `:call` whose default is
   `:ask`, invoke `prompter`. Returns:

     {:decision  :allow | :deny
      :per-call  [{:call :decision :rule :reason}]
      :new-rules [<rule>]    ; from :allow-always / :deny-always
      :prompted  [{:call :result :scope}]}

   Caller should append `:new-rules` to whichever ruleset (typically
   user or session) they want to persist. The overall `:decision` is
   `:deny` if any per-call ended up `:deny`, else `:allow`."
  [{:keys [rulesets ast prompter]
    :or {prompter deny-all-prompter}}]
  (let [{:keys [per-call]} (check-pure rulesets ast)
        new-rules (volatile! [])
        prompted (volatile! [])
        resolved
        (mapv (fn [pc]
                (if (not= :ask (:decision pc))
                  pc
                  (let [ctx {:call (:call pc)
                             :cmd (call->cmd-name (:call pc))
                             :argv (call->argv (:call pc))
                             :ast (:call pc)
                             :reason (:reason pc)
                             :matched-rule (:rule pc)}
                        p-result (prompter ctx)
                        decision (case (:result p-result)
                                   :allow-once   :allow
                                   :allow-always :allow
                                   :deny-once    :deny
                                   :deny-always  :deny)]
                    (vswap! prompted conj
                            (assoc p-result :call (:call pc)))
                    (when-let [r (mk-rule-from-prompt p-result (:call pc) decision)]
                      (vswap! new-rules conj r))
                    (assoc pc :decision decision
                           :reason (str "prompter returned "
                                        (:result p-result))))))
              per-call)
        worst (if (some #(= :deny (:decision %)) resolved) :deny :allow)]
    {:decision worst
     :per-call resolved
     :new-rules @new-rules
     :prompted @prompted}))

;; ============================================================================
;; Rule validation
;; ============================================================================

(defn validate-rule
  "Sanity-check a rule. Returns nil on OK, or a string error message."
  [rule]
  (cond
    (not (map? rule))
    "rule must be a map"

    (not (#{:bash :edit :net} (:tool rule)))
    "rule :tool must be :bash :edit or :net"

    (not (#{:allow :deny :ask} (:action rule)))
    "rule :action must be :allow :deny or :ask"

    (not (map? (:pattern rule)))
    "rule :pattern must be a map"

    (not (#{:cmd-name :argv-glob :argv-vec :argv-shape :argv-flags :ast-pred}
          (:kind (:pattern rule))))
    "rule :pattern :kind must be :cmd-name :argv-glob :argv-vec :argv-shape :argv-flags or :ast-pred"

    :else nil))
