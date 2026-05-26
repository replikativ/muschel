# awk in muschel

A bounded but faithful awk implementation, portable to both JVM and
ClojureScript. Covers the ~99% of agent awk-usage that POSIX programs
actually rely on, with goawk's interp_test.go (517 entries) as the
conformance bench.

## Quick reference

| | |
|---|---|
| Namespace | `muschel.builtins.awk` |
| Reference | [github.com/benhoyt/goawk](https://github.com/benhoyt/goawk) (MIT) |
| Tests | `test/muschel/awk_test.cljc` + `test/muschel/awk_corpus.edn` (517 entries) |
| Pass rate | 498/517 (96%) on goawk corpus |
| Compat shim | `muschel.builtins.awk-compat` (handles JVM ↔ CLJS) |
| Entry point | `(awk/run {:program "…" :raw-input "…" :fs "…" :vars {…}})` |

## Supported features

```
BEGIN { … } / END { … }     pattern { action }      pat1,pat2 ranges
/regex/ patterns            expression patterns

$0 $N NR NF FNR FS OFS ORS RS RSTART RLENGTH FILENAME SUBSEP CONVFMT OFMT ARGC

+ - * / % ^ **              == != < <= > >=         && || !
~ !~ (regex match)          ?:                      string concat by juxta-
= += -= *= /= %= ^= **=     ++ --                   position
in (membership)

if / else                   while                   do / while
for ( ; ; )                 for (k in a)            break / continue
next                        exit [expr]             delete a[k]  /  delete a

print (OFS-joined, ORS-terminated)
printf (%d %i %o %x %X %c %s %e %E %f %g %G %u %%)

length()  substr()  index()  split()  sub()  gsub()  sprintf()  match()
tolower()  toupper()  int()
sqrt()  exp()  log()  sin()  cos()  atan2()

Single-dimensional associative arrays
Hex literals: 0x22 -0xa 0XABCDEF
NaN/Inf string parsing: "nan" "INF" "-infinity"

-v VAR=val   -F SEP   -f FILE
```

## Refused / parse-error (explicit)

These are not part of the muschel subset. If a script reaches one
during parsing, it surfaces as a clean error message; in some cases
(user-defined functions) the parser silently skips the definition so
the rest of the script still runs.

- `getline` (any form)
- `system(...)` — refused in sandbox context
- `gensub(...)` — gawk extension
- Redirects: `> file`, `>> file`, `| cmd`
- Multi-dim arrays via SUBSEP: `a[i,j]`
- User-defined function *calls* (definitions are tolerated but
  silently skipped)

## Known engine-difference gaps

These 19 corpus cases fail not because we want them to but because
they hit a fundamental difference between awk's POSIX-ERE / C-printf
semantics and what's portably reachable on JVM + JS.

### Regex engine

| Failing case | Why |
|---|---|
| `sub(/(#\|#!)/, "", "#!a")` → expects `"a"`, we give `"!a"` | POSIX-ERE alternation takes the *longest* match; Java regex (and JS regex) is leftmost-first. |
| `FS = "[^,]*"` / `FS = "(abc)?"` style empty-matching regex split | Different engines handle zero-width matches differently; awk's specific empty-match field rule is not portable. |
| `RS=""` paragraph mode (blank-line-separated records) | Requires special-cased multi-line record handling that we don't implement — falls back to per-line. |

### Printf

| Failing case | Why |
|---|---|
| `printf "%u", -42` → expects `"18446744073709551574"`, we give `"-42"` | JVM/CLJS lack a native unsigned 64-bit printf. |
| `printf "%c", 256` and `printf "%c", ""` | Out-of-range / empty-string `%c` is implementation-defined; gawk picks a space, we pass the value through. |
| `printf "%*s"` and `%.*s` (width-from-arg) | Rare in real awk; not implemented. |

### Numbers

| Failing case | Why |
|---|---|
| `int("0xf.fp10")` and hex floats | C99 hex-float syntax; rarely used in awk. |

### Misc

| Failing case | Why |
|---|---|
| `print ("10" < 9)` → expects numeric compare, gets string compare in a couple of edge mixes | awk's string-vs-number rule is famously twisty around literal vs. field-stored strings; differs across awk implementations. |
| `BEGIN { $0="0"; print($0<2) }` → similar tag-preservation quirk | Same root cause. |
| `BEGIN { $0=10; $$0++; print $0 }` | Uses `$$N` indirection chains — corpus expectation looks wrong. |
| Mix of `print(... 1&&x=2, ~x=2, etc)` | Allows assignment as RHS of more operators than just `&&`/`||`; could be extended, low payoff. |
| RS-as-runtime-changeable single-char per-record splitting in some inputs | A side-effect of how my record loop and FS interact; minor. |

## Portability notes

`muschel.builtins.awk.cljc` is `.cljc` and runs on both JVM and CLJS
(Node + browser). The platform differences are confined to
`muschel.builtins.awk-compat.cljc`:

| Concern | JVM | CLJS |
|---|---|---|
| String buffer | `StringBuilder` | `goog.string.StringBuffer` |
| Number parsing | `Double/parseDouble`, `Long/parseLong` | `js/parseFloat`, `js/parseInt` |
| NaN / ±Inf | `Double/NaN`, `POSITIVE_INFINITY` | `js/NaN`, `js/Infinity` |
| Regex compile | `Pattern/compile(pat, DOTALL)` | `RegExp(pat, "gs")` |
| Regex find | `Matcher#find/start/end/group` | `RegExp#exec` with `lastIndex` |
| Regex quote | `Pattern/quote` | manual escape of `-/\^$*+?.()|[]{}` |
| Replace | hand-rolled `re-replace` calling the user function | same |
| Format | `clojure.core/format` | `goog.string.format` (subset) |
| `%x %X %o` printf | platform `format` handles it | pre-translated to `%s` and `(.toString n 16)` in the coercer |
| CONVFMT mutable cell | `atom` | `atom` (same) |
| char code | `(int c)` (Character) | `(.charCodeAt c 0)` (string) |
| Exception catch | `clojure.lang.ExceptionInfo` | `:default` (via reader cond) |

## When to fall back

If you're inside the JVM and need 100% POSIX-ERE compliance or
features outside this subset (e.g. `gensub`, `getline < file`), shell
out to system awk via the `BuiltinHost` fallback. Otherwise the in-
process implementation is faster and FS-sandboxed.
