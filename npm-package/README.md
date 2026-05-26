# muschel

Sandboxed bash shell for LLM agents — pure JavaScript, virtual or
contained filesystem, structural containment with introspection and
resource budgets.

```bash
npm install muschel
```

```js
const m = require('muschel');

const host = m.browserHost({
  files: { '/README.md': '# Hello\n', '/data.txt': '1\n2\n3\n' }
});

const r = m.run('cat /data.txt | wc -l', {
  host,
  trace: true,         // capture every tool call + FS access
  timeoutMs: 5000      // resource budget
});

console.log(r.stdout);            // → "3\n"
console.log(r.trace.reads);       // → ["/data.txt"]
console.log(r.trace.tools.map(t => t.name));  // → ["cat", "wc"]
```

## Why

LLM agents that script the shell are a real product surface, and
giving them a real shell is dangerous — `rm -rf`, `curl | sh`,
`$(< /etc/passwd)`, symlink-to-escape, etc. muschel is a bash
implementation built around two ideas:

1. **Guard all I/O.** Every builtin (~50 — `cat`, `grep`, `sed`,
   `awk`, `find`, `xargs`, `tee`, `cp`, `mv`, …) routes through a
   `FS` protocol. The agent never touches a real disk unless you
   explicitly construct a `nodeHost`. The default
   `browserHost({files})` is fully in-memory.

2. **Streaming introspection.** Every tool call, FS read/write,
   permit denial, and budget event is observable via `run().trace`
   (bounded ring-buffer) and optional `onTool`/`onFs`/`onDeny` hooks
   (unbounded, for callers that want to persist the full history —
   e.g. into a database for training data).

## API

See `index.d.ts` for full TypeScript signatures.

### Core

```js
const ast = m.parse(src);                            // AST
const result = m.check(src, { rulesets, prompter }); // permit gate
const r = m.run(src, { host, ... });                 // execute
```

### Hosts

```js
m.browserHost({ files, tools, includeStock })   // virtual fs, in-memory
m.nodeHost()                                    // real disk (unsandboxed!)
m.virtualFS({ '/a.txt': 'A' }, { cwd: '/' })    // standalone VFS handle
```

### Introspection

```js
const r = m.run(src, {
  host,
  trace: {
    cap: 1000,                                  // ring-buffer cap
    onTool: (evt) => myDb.persist(evt),         // streaming hook
    onFs:   (evt) => myDb.persist(evt),
    onDeny: (evt) => alertUI(evt),
  },
});
// r.trace.tools, r.trace.fs, r.trace.reads, r.trace.writes, r.trace.denied
```

### Resource budgets

```js
m.run(src, { host, timeoutMs: 5000 });
m.run(src, { host, interruptFn: m.budget.stepInterrupt(10000) });
```

### Permit policy

```js
const r = m.run(src, {
  host,
  permit: {
    rulesets: [m.defaultRules, myRules],
    prompter: m.denyAllPrompter
  }
});
```

## Limitations

- `nodeHost` is unsandboxed today. A real-disk **contained** Node
  host (mirroring the JVM `BuiltinHost`+`DiskFS` pair) requires a
  CLJS port and is on the roadmap.
- `m.fs.*` programmatic FS operations work on `virtualFS()` handles
  but not yet on `browserHost`'s internal filesystem — that's a
  follow-up refactor.
- bash subset is large (POSIX-ish + many common bash extensions) but
  not 100%. See `doc/builtins.md` in the source repo.

## License

EPL-1.0. Project home: https://github.com/replikativ/muschel
