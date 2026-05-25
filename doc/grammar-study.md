# muschel — Grammar Study Agenda

A focused study pass to inform a proper bash parser, before committing to
a grammar. The toy grammar in `resources/muschel/grammar.bnf` is a sketch;
this study produces the understanding to replace it.

**Goal:** at the end of this study we have:
1. A defensible answer to "what subset of bash does muschel parse?"
2. A defensible answer to "what subset does muschel **refuse** to parse,
   and why?" (some constructs are inherently dangerous; some are just out
   of scope.)
3. A revised `grammar.bnf` reflecting that decision.
4. A list of subtle semantic rules (word splitting, IFS, expansion order)
   we either implement or explicitly reject.

References available locally:

- `../bash/` — GNU bash source. `parse.y` is the yacc grammar (~3300
  lines), `subst.c` does expansion, `parse.h` declares token types.
- `../mvdan-sh/` — Go bash parser. `syntax/nodes.go` is the AST, 
  `syntax/parser.go` the recursive-descent parser, `syntax/lexer.go` 
  the tokenizer.
- POSIX 2018 §2 Shell Command Language (online): the standardised subset.

---

## Phase 1 — bash's own grammar (`../bash/parse.y`)

The yacc grammar is the canonical source. Don't try to read all 3300
lines; this is a guided walk.

### 1.1 Top-level shape (~30 min)

Read lines 200–500 of `parse.y` (the production declarations + the
top-level `simple_list` / `command` rules). 

Answer:
- What's the relationship between **`command`**, **`shell_command`**, 
  **`simple_command`**, and **`pipeline`**?
- How does the grammar express precedence between `|`, `&&`, `||`, `;`, 
  `&`?
- Where do **redirections** attach in the AST — to a command, to a 
  pipeline, to a list?
- What's a **list_terminator** vs a **simple_list**?

### 1.2 The command zoo (~30 min)

Read the productions for `shell_command` (look for the `if_command`, 
`for_command`, `case_command`, `while_command`, `function_def`,
`select_command`, `arith_for_command`, `cond_command`, `coproc` rules).

Answer:
- How many distinct command-class types does bash distinguish?
- Which of these are LLM-emit-common? (rough guess; we corpus-check 
  later.)
- Which are LLM-emit-rare or never? (we may refuse to parse these.)

### 1.3 Words, expansions, and quoting (~45 min)

Read `parse.y` around the `word_list` / `WORD` productions, then jump to
**`subst.c`** for the actual expansion logic. Note that bash's parser
emits `WORD` tokens which are *then* expanded at execution time — the
parse tree itself doesn't fully encode the expansions; that's a runtime
phase.

Answer:
- Which expansions are syntactic (the parser sees) vs runtime?
- The forms `$VAR`, `${VAR}`, `${VAR:-default}`, `${VAR/pat/repl}`, 
  `${!var}`, `${var@P}` — which is what?
- How is `$(...)` parsed? Is the inner content re-parsed by the same 
  grammar?
- Where does **word splitting** (`$IFS`) happen and how does it 
  interact with quoting?
- How are **globs** (`*`, `?`, `[a-z]`) represented at parse time?

### 1.4 Redirection (~20 min)

Read `parse.y` for the `redirection` productions (~lines 700–900).

Answer:
- How many redirection operators does bash recognise? (`>`, `>>`, `<`, 
  `<<`, `<<<`, `<>`, `2>`, `&>`, `>|`, `2>&1`, `>(cmd)`, `<(cmd)` …)
- How does the grammar handle file-descriptor numbers (the `N` in `N>`)?
- Where do heredocs (`<<EOF`) live — what's the lexer state machine?

### 1.5 What to extract

Write a quick markdown table in this doc (Phase 5 below) listing every 
construct you've seen, with:
- `bash-line` (parse.y line range)
- `mvdan-node` (the corresponding Go AST node, fill in Phase 2)
- `muschel-stance` (support / refuse / later)
- `notes`

---

## Phase 2 — mvdan/sh's modern reimplementation (`../mvdan-sh/syntax/`)

The yacc grammar is dense; mvdan/sh is a clean recursive-descent take 
with a well-documented AST. Use it to triangulate.

### 2.1 The AST (~30 min)

Read `syntax/nodes.go` top-to-bottom. It's structured: each `*Stmt` /
`*Word` / `*Lit` etc. has a comment.

Answer:
- List every node type in the AST.
- Which nodes correspond to which bash productions from Phase 1?
- What's the difference between **`Stmt`** and **`Command`** in 
  mvdan/sh? (`Stmt` is the wrapper that carries redirections, comments,
  background flag.)
- How does mvdan/sh represent **`Word`**? (it's a list of `WordPart`s
  — a `Lit`, a `DblQuoted`, a `ParamExp`, a `CmdSubst`, etc.)

### 2.2 The parser (~30 min)

Skim `syntax/parser.go`. Don't read every function; focus on:
- The top-level `Parser.stmts` / `Parser.getStmt`
- The lexer-parser boundary (`Parser.next`)
- How heredocs are deferred (search for "heredoc")

Answer:
- How does mvdan/sh disambiguate `[` (test) from `[` (literal bracket)?
- How does it handle `$((arith))` vs `$(cmd)`?
- How does it handle implicit semicolons (newlines)?

### 2.3 Bash extensions worth noting (~15 min)

mvdan/sh supports POSIX + bash + zsh + mksh under flags. Look at the
constants in `syntax/lexer.go` and `syntax/printer.go` that branch on
`p.variant`.

Answer:
- What's POSIX-only vs bash-specific?
- Which bash extensions are common in agent commands? (`[[ ... ]]`, 
  `$(( ))`, `${VAR:-default}`, `&&`, `||`, process substitution.)

---

## Phase 3 — POSIX 1003.1 §2 Shell Command Language (~45 min)

Online: <https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html>

POSIX is what most agents-in-the-wild emit, even when they use bash. 
Reading the standard gives a clean "minimum viable shell".

Answer:
- What's the difference between **simple command**, **compound command**,
  and **function definition** in POSIX?
- What's the **token recognition** algorithm? (the section on 
  "Token recognition" describes lexer state quite precisely.)
- Which expansion order is mandated? (Brace → tilde → parameter → 
  command → arithmetic → word-split → pathname.)
- What does POSIX say about word splitting and IFS?

---

## Phase 4 — LLM agent corpus (~30 min)

What do real agents actually emit? We need empirical data, not 
theoretical possibility.

Sources for samples:
- Claude Code's documented bash usage (e.g. its built-in read-only set:
  `ls cat echo pwd head tail grep find wc which diff stat du cd` + 
  read-only git)
- aider/cline/codex shell tool logs if we can scrape any
- Anthropic's published agent traces (some are in their docs / examples)
- ../dvergr-playground/ session logs once we have any
- Our own intuition from using these tools

For each construct on the Phase 1.1–1.4 list, mark:
- **Common**: agents emit this regularly
- **Occasional**: agents emit this sometimes (often via cleverness)
- **Rare**: theoretically possible, almost never seen
- **Never**: not in any observed corpus

Constructs we expect to be **Common** based on intuition (verify):
- Simple commands with arguments
- Pipes (`cmd1 | cmd2`)
- Sequence operators (`;`, `&&`, `||`)
- Quoting (`"..."`, `'...'`, `\`)
- Variable expansion (`$VAR`, `${VAR}`)
- Command substitution (`$(date)`, `$(git rev-parse HEAD)`)
- Redirection (`>`, `>>`, `<`, `2>&1`)
- Globs (`*.clj`, `**/*.md`)
- Test brackets (`[ -f path ]`, `[[ -d dir ]]`)

Constructs we expect to be **Occasional**:
- `for f in ...; do ...; done` loops
- `if ... ; then ... ; fi`
- Heredocs (`cat <<EOF`)
- xargs as a pipeline partner

Constructs we expect to be **Rare** or **Never**:
- Process substitution `<(cmd)` / `>(cmd)`
- `select` / `case`
- Function definitions
- Arrays
- Coproc
- Brace expansion `{a,b,c}` (sometimes for `mkdir -p {x,y}` but agents 
  often write it out)
- `${var@P}` and other obscure parameter expansions

---

## Phase 5 — Synthesis: muschel's grammar v1

Produce a table in this doc (or split it into a sibling file 
`grammar-spec.md`) of every construct, with:

| Construct | bash parse.y | mvdan node | Corpus rank | muschel stance | Notes |
|---|---|---|---|---|---|
| Simple command | … | `*CallExpr` | Common | Support | … |
| Pipe | … | `*Stmt.Pipe` | Common | Support | … |
| Process subst | … | `*ProcSubst` | Never | Refuse | injection vector |
| … | | | | | |

Then revise `grammar.bnf` to match the "Support" rows. The grammar 
becomes ground truth for what we'll AST-build; the "Refuse" rows are 
either rejected by the grammar (preferred — fails fast) or rejected by 
the `permit` layer (when refusing requires semantic info).

### Hard "no" list (decision in advance)

Rule: refuse only when the construct is **both rare in the corpus AND
messy** (stateful, non-compositional, interactive-only, or broken-API
in unix-land). Anything cheap to implement, even if rare, gets
supported.

Refused at the **parser** (won't even parse):

- **Process substitution** `<(cmd)` / `>(cmd)` — 0.04% / 0.02%.
  Requires backgrounded inner process + named pipe / `/dev/fd/N`
  substitution. Concurrent + filesystem-pathy + bash-specific.
- **Arrays** `arr[i]`, `${arr[@]}` — 0.05%. Bash arrays are an
  afterthought-API; `${arr[@]}` interacts non-obviously with quoting
  and IFS word splitting; associative arrays are a separate
  subsystem.
- **`select var in ...`** — interactive only (prints menu, reads tty).
- **`coproc`** — stateful pseudo-co-routine with broken cleanup.
- **`trap`** — signal handlers + stateful EXIT hooks.
- **History expansion** `!!`, `!$`, `!cmd` — interactive-shell only;
  off in non-interactive bash by default.
- **Aliases** (`alias`, `unalias`) — bash itself disables aliases in
  non-interactive mode; stateful; can break lexer mid-parse.
- **Job control** (`fg`, `bg`, `jobs`, `%1`, `wait %n`) —
  interactive-only. `wait <pid>` for backgrounded jobs supported
  later.
- **Extended globs** `?(pat)`, `*(pat)`, `+(pat)`, `@(pat)`, `!(pat)`
  — requires `shopt -s extglob`; complex grammar.
- **Locale quoting** `$"..."` — i18n hook nobody uses in scripts.

Refused at the **permit** layer (parse cleanly, then deny):

- **`eval`**, **`source`**, **`.`** as command names — must
  allowlist; never default-allowed.
- **`exec`** as a command — replaces shell.
- **Backgrounding** `&` — corpus shows 2.5% real use, dominated by
  "start an nREPL server" patterns. Parser supports; permit layer is
  authoritative about whether a given agent may background.

### Promoted to v1 support (were originally on refuse/defer)

Cheap to implement → support, regardless of low corpus share:

- **Heredocs** (`<<EOF`, `<<'EOF'`, `<<-EOF`) — 4.45% corpus, driven
  by the `git commit -m "$(cat <<'EOF' ... EOF\n)"` pattern.
- **Arithmetic** (`$((...))`, `((...))` commands, `let`) — 1.5%
  corpus, mostly for-loop timing math.
- **Brace expansion** (`{a,b,c}`, `{1..10}`) — 0.7% corpus.
- **Backtick command substitution** \`cmd\` — same semantics as
  `$(...)`; cheap to lex once we already lex `$(...)`.
- **Here-strings** `<<<` — trivial (string → stdin). Cited security
  risk is about the *target* of an append-redirect, not the here-string
  itself; permit layer handles that.
- **Indirect param expansion** `${!var}` — one extra lookup step.
- **`|&`** (pipe stderr+stdout) — shorthand for `2>&1 |`.
- **ANSI-C quoting** `$'...'` — backslash-escape table for literals
  like `$'\t'`.
- **`$(<file)`** — `cat file` shorthand.
- **`time cmd`** — wraps cmd with timing.
- **Set options** (`set -e`, `set -u`, `set -o pipefail`, `shopt`) —
  modestly stateful but bounded; common in embedded scripts.

### "Maybe later" list

Still deferred — useful but real complexity:

- **`case`/`esac`** — 0.5% corpus; pattern-list grammar is its own
  subsystem.
- **Functions** (`name() { ... }`, `function name { ... }`) — 0.7%
  corpus; most hits are inside heredoc bodies for embedded scripts, not
  direct agent emit. Adds function-environment scoping.
- **C-style `for ((init; cond; update))`** — once arithmetic
  evaluator is solid.

---

## Phase 6 — Lexer / tokenisation decisions (~30 min)

Reading bash's lexer is harder than the grammar; mvdan/sh's `lexer.go`
is the friendly version.

Answer:
- Is muschel's lexer **single-pass** with shell's state machine 
  (heredoc-mode, double-quote-mode, etc.), or does instaparse handle 
  this purely through grammar?
- For `instaparse`: are there constructs where context-free parsing 
  fails and we need a hand-written tokenizer in front?
- Token-level questions: how do we recognise `2>&1` (a single token in 
  bash? or three tokens `2 > & 1`)?

---

## Phase 7 — Decision: instaparse, hand-written, or hybrid?

Once Phases 1–6 are done, decide:

**Option A — instaparse, common subset**. Use instaparse with our
revised grammar. Accept that some bash quirks (heredoc state, IFS-aware
word splitting at parse time) will not be supported. Refuse them at the
grammar level.

**Option B — hand-written recursive-descent**. More work, but matches
what mvdan/sh + bash itself do. Lets us handle heredocs and contextual
lexing.

**Option C — hybrid**. instaparse for the bulk; a small hand-written
tokenizer for the lexer hardships (heredocs, command substitution
boundaries) that feeds instaparse with well-typed tokens.

My current guess: **C**, but the study determines it.

---

## Output of this study

By the end of Phase 7 we should have, committed to muschel:

1. This document, **filled in** with the answers to each phase's 
   questions.
2. `doc/grammar-spec.md` — the constructs table (Phase 5).
3. A revised `resources/muschel/grammar.bnf` reflecting that decision 
   (or a new `src/muschel/lexer.clj` if we go down route B/C).
4. An updated `test/muschel/parse_test.clj` with the corpus from 
   Phase 4 (positive + negative cases).

Then we proceed with AST helpers, permit layer, compiler, evaluator on
a solid foundation.

---

## Working notes

> (Use this section as you read. Free-form. Stub each phase's notes 
> below as you go.)

### Phase 1 notes

- ...

### Phase 2 notes

- ...

### Phase 3 notes

- ...

### Phase 4 notes

Done — see [`doc/llm-corpus.md`](./llm-corpus.md) for the full write-up.
Source: 153,079 Bash tool inputs from local Claude Code session
transcripts (`~/.claude/projects/**/*.jsonl`, 5396 sessions, 1.7 GB).

Headline findings:

- **v0 grammar pass rate: 79.6%** on a random 500-sample. 70% of
  failures are caused by *multi-statement input* (`cmd1\ncmd2\ncmd3`),
  which the `program` rule can't represent — that's the single highest-
  leverage fix.
- **Heredocs are not "maybe later"** — 4.45% of corpus, driven by the
  `git commit -m "$(cat <<'EOF' ... EOF\n)"` pattern Claude Code's own
  prompt prescribes. Promote to v1.
- **Backgrounding `&` has real agent need** — 2.5% of corpus, used
  almost exclusively to spawn nREPL/test servers. Move from the
  "Hard no" list to "support at parse, permit layer decides".
- **Confirmed refuses**: process-substitution (0.04% / 0.02%),
  here-strings (0.04%), arrays (0.05%) — all <0.1%; the hard-no list
  stands for these. Backticks are 0.6% but almost all are inside
  quoted strings, not real command-subst; keep refused at parse.
- **literal-word regex is too narrow** — second-biggest failure class
  after multiline; needs `=` (for `--color=never`), escaped `\(`/`\)`
  (for `find . \(...\)`), and possibly `!` / `^`.

The full corpus, baseline failure set, and reproducible extraction
pipeline are documented in `doc/llm-corpus.md`. Files live under
`/tmp/muschel-corpus/`.

### Phase 5 notes

- ...

### Phase 6 notes

- ...

### Phase 7 decision

Open. The babashka/sci runtime constraint (instaparse won't load under
sci — `clojure.lang.IHashEq` is missing) and the corpus showing that
heredocs are essential (4.45% of usage) together push the answer
toward **Option C — hybrid: hand-written lexer in front, parser
behind**. The lexer is what we'd need anyway for heredoc state, and
once we have it the parser is the smaller of the two problems.

Reference implementation pattern (confirmed running in babashka):
`../superficie/src/superficie/scan/{tokenizer,grouper}.cljc`. That
codebase splits parsing into `tokenize` → `group-tokens` →
`enforest-forms`, all in `.cljc`, with `volatile!` cursor state and
portable StringBuilder via reader conditionals. Same shape works
here.

Current direction: keep instaparse for the v0 grammar (JVM-only) while
the permit / compile / eval layers land, then port to a hand-written
sci-safe parser once those stabilize and the grammar is locked.
