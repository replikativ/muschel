## muschel — LLM bash corpus

Empirical Phase 4 of `grammar-study.md`. Source: every `Bash` tool input
across all locally-stored Claude Code session transcripts at
`~/.claude/projects/**/*.jsonl`. **153,079 commands** from 5396 session
files (1.7 GB of transcript). All numbers below are computed by simple
substring/regex over the JSON-escaped string forms — small (<1pp)
overcounting where an operator appears inside a quoted literal.

Extraction pipeline (reproducible):

```sh
find ~/.claude/projects -name '*.jsonl' | xargs -P4 -I{} \
  sh -c 'jq -c "select(.message.content?) | .message.content[]? \
         | select(.name? == \"Bash\") | .input.command" "$1"' _ {} \
  > /tmp/muschel-corpus/all-bash-json.txt
```

### Length distribution

| pct | chars |
|----:|---:|
| p25 | 96 |
| p50 | 150 |
| p75 | 331 |
| p90 | 801 |
| p99 | 2469 |
| max | 34176 |

Half the corpus exceeds 150 characters; the long tail (heredoc-embedded
scripts, multi-command pipelines for `gh pr create` bodies, REPL
snippets) goes well into the thousands. The parser cannot assume short
inputs.

### Construct frequency

Top features by share of commands containing each construct.

| construct | count | share | tier |
|---|---:|---:|:---|
| double-quoted string | 109,179 | 72.9% | T1 |
| pipe `\|` | 89,982 | 60.1% | T1 |
| `2>` (any err redirect) | 67,346 | 45.0% | T1 |
| `2>&1` | 48,804 | 32.6% | T1 |
| single-quoted string | 48,619 | 32.5% | T1 |
| **multi-line command** | 46,667 | 31.2% | T1 |
| glob `*` | 36,524 | 24.4% | T1 |
| `&&` | 35,707 | 23.8% | T1 |
| `;` sequencing | 35,678 | 23.8% | T1 |
| `cd` (any position) | 28,384 | 19.0% | T1 |
| backslash escape | 25,552 | 17.1% | T1 |
| glob `?` | 16,197 | 10.8% | T1 |
| `>` output redirect | 14,600 | 9.8% | T1 |
| simple-command-only | 13,768 | 9.2% | T1 |
| `<` input redirect | 9,452 | 6.3% | T1 |
| `$(...)` | 8,682 | 5.8% | T1 |
| tilde `~/` | 8,072 | 5.4% | T1 |
| `if` | 7,174 | 4.8% | T2 |
| **heredoc `<<`** | 6,663 | 4.5% | T2 |
| heredoc `<<'EOF'` (quoted) | 6,528 | 4.4% | T2 |
| `$VAR` | 5,055 | 3.4% | T2 |
| `for var in ...` | 4,556 | 3.0% | T2 |
| trailing `&` (background) | 3,750 | 2.5% | T2 |
| `VAR=val cmd` (prefix) | 2,701 | 1.8% | T2 |
| `\|\|` | 2,315 | 1.5% | T2 |
| arithmetic `$((..))` / `((..))` | 2,313 | 1.5% | T2 |
| brace expansion `{a,b,...}` | 1,091 | 0.7% | T3 |
| function definition | 1,013 | 0.7% | T3 |
| `>>` append | 987 | 0.7% | T3 |
| backtick command-subst | 959 | 0.6% | T3 |
| `case` | 730 | 0.5% | T3 |
| `${VAR}` braced ref | 552 | 0.4% | T3 |
| `while` | 539 | 0.4% | T3 |
| `${VAR:-default}` (any param-op) | 207 | 0.1% | T3 |
| `export VAR=` | 103 | 0.1% | T3 |
| `arr[i]` array | 70 | 0.05% | T4 |
| process-subst `<(cmd)` | 65 | 0.04% | T4 |
| here-string `<<<` | 63 | 0.04% | T4 |
| process-subst `>(cmd)` | 33 | 0.02% | T4 |

Tier legend: **T1** ≥5% — must support; **T2** 1–5% — should support;
**T3** 0.1–1% — defensible support; **T4** <0.1% — defensible refuse.

### Baseline v0 grammar pass rate

Random 500-command sample run through `muschel.parse/parses?`:

- **passed: 394 (79.6%)**
- failed: 101 (20.4%)

Failure causes (a single failure can hit multiple buckets — they're
overlapping, not partitioning):

| cause | n |
|---|---:|
| multiline (≥1 embedded `\n`) | 72 |
| heredoc body | 19 |
| trailing `&` background | 17 |
| other (literal-word regex too narrow) | 17 |
| array | 4 |
| arithmetic | 2 |
| function def | 2 |
| history `!!` | 1 |
| backtick subst | 1 |

The single biggest fix is **multi-statement-per-line input**: 70% of
v0 failures come from agents emitting `cmd1\ncmd2\ncmd3` as a single
Bash tool input. The v0 grammar's `program = ws? command-list? ws?`
rule treats newlines as separators inside a single command-list but
chokes when surrounded by blank lines or terminating semicolons in
unfamiliar positions.

Second-biggest fix is the literal-word regex
`[A-Za-z0-9_./+:@%,?*\[\]~-]+` — too narrow. Confirmed missing
characters from "other" failures: `=` (in `--color=never`,
`--include="*.clj"`), `(` and `)` after backslash (find's `\( -name
... \)` predicate grouping), and a few more.

### Findings that override the existing grammar-study.md positions

The handoff doc placed several constructs on the "Hard no" or
"Maybe later" lists ahead of empirical data. The corpus contradicts
some of those defaults:

1. **Heredocs (`<<EOF`, `<<'EOF'`)** — doc says "Maybe later". Data:
   4.45% of all commands, dominated by the
   `git commit -m "$(cat <<'EOF' ... EOF\n)"` pattern that Claude Code
   itself prescribes for multi-line commit messages. **Promote to T2,
   ship in v1.** Both forms (quoted-delimiter "literal" body and
   unquoted-delimiter "with-expansion" body) appear; the quoted form
   is what Claude's own prompt teaches.
2. **Background `&`** — doc says "no agent need". Data: 2.5% of
   commands have a real trailing `&`, almost exclusively to spawn nREPL
   / test servers / long-running JVMs. **Promote from refuse to
   support, defer the *decision* to the permit layer** (agents that
   shouldn't background processes get told no, but the parser shouldn't
   make that call).
3. **Arithmetic `$((..))`** — doc says "Maybe later". Data: 1.5%
   share, often inside for-loops doing timing math (`ms=$(((t1-t0)/1000000))`).
   Worth supporting, but acceptable to start without.
4. **Brace expansion `{a,b,c}`** — doc says "Maybe later". Data:
   0.7%. Mostly `mkdir -p {x,y}` / `rm {a,b}`. **Keep deferred.**
5. **Backticks** — doc says "Hard no". Data: 0.6% — and inspection
   shows almost all hits are quoted strings *containing* backticks
   (e.g. `gh pr edit ... --body "...\`code\`..."`), not real
   command-substitution use. **Keep refused at parse**; agents have
   `$(...)` and use it.

Reconfirmed positions:

- **Process substitution** (`<(cmd)`, `>(cmd)`) — 0.04% / 0.02%. Keep
  refused; agents can fall back to temp files.
- **Here-strings (`<<<`)** — 0.04%. Keep refused.
- **Arrays** — 0.05%. Keep refused.
- **`case`** — 0.5%. Keep deferred; agents reach for `if`/elif chains.

### v1 grammar action list (drawn from this data)

Priority-ordered, with corpus impact:

1. **Multi-statement input** — accept `cmd1\ncmd2\ncmd3` as a sequence
   of top-level command-lists. Fixes ~70% of current failures (~14% of
   corpus uplift). *Touch: `program` rule in `grammar.bnf`.*
2. **Literal-word regex** — broaden to include `=` and escaped parens.
   Likely needs a closer look at every special character: my reading
   of failures suggests `=`, `\(`, `\)`, and possibly `!` (negation in
   `find -not`) and `^` (some regex args). *Touch: `literal-word`.*
3. **Heredocs (T2)** — both `<<EOF` and `<<'EOF'`. Critical for the
   git commit pattern; without it, agents can't commit via muschel.
   *Touch: lexer hand-off; instaparse can't track heredoc state
   purely context-freely. This is the first lexer-state construct
   that pushes us toward the hybrid option (Phase 7 Option C).*
4. **Backgrounding `&`** — emit as a flag on the pipeline node so the
   permit layer can refuse/allow per policy. *Touch: `pipeline` rule.*
5. **Arithmetic (T2)** — accept `$((expr))` as an opaque token at the
   word level for now; we don't need to evaluate the arithmetic at
   parse time, only recognise its bounds.

### Test-corpus seed

`/tmp/muschel-corpus/sample-500.txt` (random 500 commands) plus
`/tmp/muschel-corpus/v0-fail.txt` (the 101 failures) are the seeds for
a future `test/muschel/corpus_test.clj` that runs the corpus through
`parses?` and counts regressions. Once we land v1 grammar fixes, the
target is **≥95% on the random sample, with the remaining failures
falling into explicitly-refused categories** (process-subst, arrays,
backtick command-subst — all <0.1% of corpus).

### Parser implementation reference

When we do replace instaparse (Phase 7 sci-compat decision), the
template to follow is **`../superficie/src/superficie/scan/`** — a
hand-written tokenizer + grouper architecture in `.cljc`, confirmed
running in babashka. Key patterns:

- Mutable scanner state via `volatile!` (sci-safe)
- Portable StringBuilder via reader conditionals
- Map tokens `{:type :kw :value "" :line :col :offset :end-*}`
- Layered: `tokenize` → `group-tokens` → `enforest-forms`

closh's `parser_squarepeg.cljc` is *not* a reference for us — it
parses Clojure-syntax shell, not bash text. Closh's own author marked
it on hiatus in 2022 and now recommends babashka for scripting.

### Files

- `/tmp/muschel-corpus/all-bash-json.txt` — 153,079-line corpus (JSON-string per line)
- `/tmp/muschel-corpus/sample-500.txt` — random 500 for parser baseline
- `/tmp/muschel-corpus/v0-fail.txt` — the 101 failures, separator `=====`
