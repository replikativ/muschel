# Version-store experiment

Status: experimental Muschel proving ground. Intended extraction target:
shared replikativ library plus Datahike/konserve adapters in dvergr.

## Boundaries

`muschel.version-store` has no shell or filesystem dependency. Its two mutable
interfaces correspond directly to the intended production substrates:

- `PayloadStore`: immutable content-addressed payloads; konserve implements it.
- `versions` / `heads`: plain metadata maps currently held in atoms; Datahike
  stores these as version entities and node/head references.

`muschel.fs.versioned` is only an integration demonstrator. It decorates any
Muschel FS and records each successfully closed write as one version. Tree
identity, permissions, directories, and path containment remain responsibilities
of the wrapped FS.

## Version shape

```clojure
{:version/id             uuid-string
 :version/path           "/work/file"
 :version/content-id     sha-256-of-reconstructed-text
 :version/parent         prior-version-id
 :version/representation {:kind :full|:line-delta|:copy-insert|:chunks
                          :payload-id sha-256
                          :base-version version-id?}
 :version/delta-depth    0..N
 :version/size           utf-8-byte-count}
```

Every read reconstructs and validates `:version/content-id`. Delta chain depth is
bounded (default 8); the next write materializes a non-delta representation.
Reconstructed text uses a bounded in-memory cache (default 32 entries).

## Representations

- `:full`: complete text payload.
- `:line-delta`: structured edits from `muschel.diff`, including original line
  coordinates, inserted lines, and final-newline state. Best review/merge input.
- `:copy-insert`: Git-pack-shaped COPY ranges plus inserted strings. This
  prototype uses UTF-16 string offsets rather than Git's byte offsets; a binary
  production codec must operate on bytes.
- `:chunks`: content-defined line chunks. Natural boundaries depend on stable
  per-line fingerprints, limiting boundary disruption after an insertion.
- `:auto`: periodic chunks plus bounded deltas. It prefers COPY/INSERT when it
  is close in size to the line delta, while line edits remain independently
  computable for review.

## Current bake-off

Run:

```bash
clojure -M:version-lab
npx shadow-cljs compile version-lab
node out/version-lab.js
```

Corpus: 100 successive versions of a 1,000-line text, changing one line per
version. Representative JVM results (smoke benchmark, not JMH):

| Strategy | Stored / logical | Read all versions |
|---|---:|---:|
| Full | 100% | ~9 ms |
| Line delta, depth 8 | 13.1% | ~300–500 ms |
| COPY/INSERT, depth 8 | 13.6% | ~6 ms |
| Content-defined chunks | 13.7% | ~7 ms |
| Auto hybrid | **7.0%** | ~42 ms |

JVM and Node produce identical payload counts and stored byte totals for the
same corpus. Timing differs because hashing and persistent collection costs
differ by runtime.

## Findings

1. Line diffs should be semantic review/merge data, not necessarily the physical
   storage delta. Their reconstruction cost is dominated by splitting and
   rebuilding complete line vectors at every chain link.
2. COPY/INSERT reconstructs nearly as quickly as full/chunk storage at similar
   size to line deltas on this corpus.
3. Content-defined chunks give direct reads and cross-version deduplication with
   no base-chain dependency.
4. A hybrid wins storage because chunk snapshots periodically reset short delta
   chains. The policy remains workload-dependent and must be tested on prose,
   source code, generated files, minified files, CRLF, and non-ASCII text.
5. Atomic sink close is required. Persisting on open or on each accumulator
   mutation creates false intermediate versions; `fs/commit-sink!` is the new
   portable close-hook seam.

## Production integration still required

- Replace atom metadata with Datahike schema/transactions and stable fs-node
  identity rather than path identity.
- Implement a konserve `PayloadStore`, including immutable write metadata and
  replica-aware existence checks.
- Use a byte-oriented codec and binary fallback; current representations are
  deliberately text-only.
- Make commit atomic across payload write and Datahike head transaction, with
  orphan-payload recovery.
- Pin serialization independently of `pr-str`; payload IDs must survive Clojure
  and format upgrades.
- Define branch-aware roots and grace periods before enabling payload GC.
- Add three-way merge and conflict entities; the current parent chain is linear.
