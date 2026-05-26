# muschel builtins — reference

This is the agent-facing reference for what each muschel builtin
supports. They're pure-Clojure reimplementations of the standard POSIX
toolset, dispatched in-process by `muschel.host.builtin/BuiltinHost`
against a `muschel.fs` handle — so every read, write, and glob respects
the FS root regardless of what the agent asks for.

Two registries live in `muschel.builtins.posix`:

- **`standard-read-only`** — strict inspection-only set (no
  filesystem mutations possible through any of these tools).
- **`standard`** — the read-only set plus write builtins and the
  text + path toolbox.

| | Builtin | Flags / forms | Notes |
|---|---|---|---|
| ✓ read | `pwd` | -L -P (accepted, no-op) | Always emits the env's logical cwd. |
| ✓ read | `echo` | -n -e -E | -e enables `\n \t \r \\` escapes. |
| ✓ read | `ls` | -a -A -l -1 -h -F | -l format is minimal: `<type> <size> <name>`. No -t/-S/-R. |
| ✓ read | `cat` | -n -b -s -E -A | Byte-correct trailing-newline preservation. |
| ✓ read | `head` | -n N, -N, -q, -v | Multi-file headers `==> name <==`. |
| ✓ read | `tail` | -n N, -N, -q, -v | No -f. |
| ✓ read | `wc` | -l -w -c -m | -l counts `\n` (GNU semantics). |
| ✓ read | `stat` | (positional only) | Output: `<size> <type> <name>`. |
| ✓ read | `which` | (positional only) | Reports muschel builtin + allowlist membership. |
| ✓ read | `sort` | -r -n -u -f | Numeric is integer-only. |
| ✓ read | `uniq` | -c -d -u -i | Adjacent-only. |
| ✓ read | `grep` | -E -F -i -n -v -c -l -L -H -h -r -q -w -e | Java regex engine (ERE-ish). -e for `-`-prefixed patterns. |
| ✓ read | `find` | -name -iname -type -maxdepth -mindepth -print -exec…\; -exec…+ -a -o -not (…) | -exec dispatches via *host*; same gates apply. |
| ✓ read | `tr` | -d -s -c with SET1 [SET2]; `[:upper:] [:lower:] [:digit:] [:alpha:] [:alnum:] [:space:] [:blank:] [:punct:] [:xdigit:] [:cntrl:] [:print:] [:graph:]` classes; `\n \t \r \\ \a \b \f \v \0` escapes. | Reads stdin only. |
| ✓ read | `cut` | -d D -f F / -c R / -s | Range spec: `n,m,n-m,n-,-m`. |
| ✓ read | `diff` | -u -q -i -w | LCS-based unified diff with `@@ -a,A +b,B @@` hunks + 3 ctx (patch / git-apply parseable). |
| ✓ read | `xargs` | -0 -n N -I R -d D -r | -I treats each line as one invocation (GNU semantics). |
| ✓ read | `sh` / `bash` / `dash` | -c SCRIPT | Re-parses through muschel — same gates apply, bounded recursion. |
| ✏ write | `touch` | -c | Create empty / bump mtime. |
| ✏ write | `mkdir` | -p [-m MODE] | -p creates parents + idempotent. |
| ✏ write | `rmdir` | -p | Refuses non-empty dirs. |
| ✏ write | `rm` | -r/-R -f -v | -r recursive; -f silences missing-file errors. |
| ✏ write | `cp` | -r/-R -f -v | -r recursive; final-arg semantics: if DST is an existing dir, each SRC → DST/basename. |
| ✏ write | `mv` | -f -v | Same final-arg semantics as cp. |
| ✏ write | `chmod` | (octal mode only) | `0644`, `755`. Symbolic modes not implemented. |
| ✏ write | `ln` | -s -f | Symbolic links only — hard links not meaningful in the sandbox. |
| ✏ write | `tee` | -a | stdin → stdout AND each file. |
| 📝 text | `sed` | `s/PAT/REPL/[gi]`, `/PAT/d`, `/PAT/p`, `Nd Np $d $p` -n -e -i | -i rewrites the file in place via the FS. |
| 📝 text | `awk` | `'{print}'`, `'{print $N, $M}'`, `'/PAT/'`, `'NR==N'`, -F SEP | Field access $0/$N, NR, NF. |
| 📝 text | `printf` | `%s %d %i %x %o %c %%` | Format string reused when args > specifiers. |
| 📝 text | `env` | (no args) | KEY=VAL lines. |
| 📝 text | `date` | `+FORMAT` | `%Y %m %d %H %M %S %j %F %T %s %%` |
| 📝 text | `seq` | `LAST` / `FIRST LAST` / `FIRST STEP LAST` -s SEP | |
| 📐 path | `basename` | PATH [SUFFIX] | |
| 📐 path | `dirname` | PATH... | |
| 📐 path | `realpath` | PATH... | Outside-root → No such file. |

## Outside the registry — handled by `muschel.exec` shell layer

These are shell-level constructs, not registry builtins:

- `test` / `[ ... ]` — file/string/int predicates. Reads `:file?` /
  `:dir?` / `:exists?` from the host's `-file-info`, which goes through
  the FS for sandboxed hosts. Supports `-z -n -e -f -d -h -L -r -w -x -s`,
  `=` / `!=` / `==`, `-eq -ne -lt -le -gt -ge`, `! -a -o`.
- `cd` — moves the env's `:cwd`, goes through the FS's `-cd!`.

## What's NOT in the registry

These are missing on purpose:

- `man` / `info` — agents read source / repo docs instead.
- `vi` / `vim` / `nano` — agents edit via `sed -i` / `cp` + `tee`.
- `curl` / `wget` / `ssh` — out-of-band; use `intake.web` from SCI.
- `top` / `htop` / `vmstat` — out-of-scope for a hermetic sandbox.
- `sudo` / `su` — auto-deny in the default permit ruleset.

## What's NOT exhaustively implemented

We aim for the agent-realistic 80% of each tool:

- `sed` doesn't support `y` (transliteration via tr instead), `r/w/N` for
  multi-line, hold-space `h/H/g/G`, `b` / `t` branching.
- `awk` runs a tiny subset of the language — `{print}`, field access,
  `/PAT/`, `NR==N`. No user-defined functions, no `for`/`while` inside
  actions, no `BEGIN`/`END`, no string-split / array operations.
- `find` doesn't implement `-size`, `-newer`, `-mtime`, `-empty`,
  `-perm`, `-user`, `-prune`.
- `chmod` is octal-only.

If an agent reaches for a real-world flag we don't cover, they get a
clean `tool: invalid option` error rather than a silent fall-through.

## System binaries

What can pass through the BuiltinHost to actual `babashka.process` exec
is controlled by its `:fallback-allowlist`. The dvergr-side default
(`dvergr.intake.bash/default-fallback-allowlist`) lets through:

  git gh clj clojure bb lein boot npm npx yarn pnpm jq make
  cargo rustc go python python3 pip pip3 uv node

Everything outside that set + the builtin registry refuses with
exit 126 — agents will see a `muschel: <cmd>: not a builtin and not
in fallback-allowlist (allowed: …)` message.

To add a tool, extend `default-fallback-allowlist` (or pass an explicit
`:fallback-allowlist` to `make-host`). Pair with a permit rule when you
want fine-grained gating of which argv shapes are allowed
(`{:kind :argv-shape :shape ["git" "push" :**]}` etc.).
