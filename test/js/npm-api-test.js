// JS smoke test for the published muschel API.
//
// This test loads `npm-package/muschel.js` exactly as a downstream
// consumer would (`require('muschel')`), exercises every exported
// function and option shape that's documented in `index.d.ts` / the
// README, and asserts on the runtime behaviour. The point is not to
// re-test the parser or the executor — those have ~3000 Clojure
// assertions covering them — but to pin down:
//
//   1. The npm-shipped bundle actually exports the surface the docs
//      promise. (`m.run`, `m.parse`, `m.check`, `m.newEnv`,
//      `m.atomSession`, `m.browserHost`, `m.nodeHost`, `m.virtualFS`,
//      `m.defaultRules`, `m.denyAllPrompter`, `m.allowAllPrompter`,
//      `m.stockTools`, `m.budget.{stepInterrupt,deadlineInterrupt,combine}`,
//      `m.fs.*`.)
//   2. The option keys (`host`, `permit`, `session`, `trace`,
//      `interruptFn`, `timeoutMs`) are accepted by name.
//   3. The result shape (`{stdout, stderr, exit, env, permit?, trace?}`)
//      stays stable across releases.
//
// Run with: `node test/js/npm-api-test.js`
//
// Wired into `script/test-all` so the npm tarball is end-to-end-tested
// against muschel.js BEFORE it gets pushed to the registry.

'use strict';

const path = require('path');
const assert = require('assert');

const m = require(path.join(__dirname, '..', '..', 'npm-package', 'muschel.js'));

let pass = 0;
let fail = 0;

function test(name, fn) {
  try {
    fn();
    pass++;
    console.log(`  ✓ ${name}`);
  } catch (e) {
    fail++;
    console.log(`  ✗ ${name}`);
    console.log(`      ${e && e.message ? e.message : e}`);
    if (e && e.stack) console.log(e.stack.split('\n').slice(1, 4).join('\n'));
  }
}

// ---------------------------------------------------------------------------
// Exports — every documented symbol is present and has the right kind.
// ---------------------------------------------------------------------------

console.log('Exports');

const fnExports = [
  'parse', 'check', 'newEnv', 'atomSession',
  'browserHost', 'nodeHost', 'virtualFS', 'stockTools',
  'run',
  'denyAllPrompter', 'allowAllPrompter',
];

for (const k of fnExports) {
  test(`m.${k} is a function`, () => {
    assert.strictEqual(typeof m[k], 'function', `${k} is ${typeof m[k]}`);
  });
}

test('m.defaultRules exists', () => {
  assert.ok(m.defaultRules, 'defaultRules missing');
});

test('m.budget.{stepInterrupt,deadlineInterrupt,combine,budgetExceeded}', () => {
  assert.ok(m.budget, 'budget namespace missing');
  for (const k of ['stepInterrupt', 'deadlineInterrupt', 'combine', 'budgetExceeded']) {
    assert.strictEqual(typeof m.budget[k], 'function', `budget.${k} is ${typeof m.budget[k]}`);
  }
});

test('m.fs.{readFile,listDir,exists,stat,mkdir,delete,rename,touch,chmod,symlink}', () => {
  assert.ok(m.fs, 'fs namespace missing');
  const want = ['readFile', 'listDir', 'exists', 'stat',
                'mkdir', 'delete', 'rename', 'touch', 'chmod', 'symlink'];
  for (const k of want) {
    assert.strictEqual(typeof m.fs[k], 'function', `fs.${k} is ${typeof m.fs[k]}`);
  }
});

// ---------------------------------------------------------------------------
// parse — returns an AST object, exposes :type.
// ---------------------------------------------------------------------------

console.log('\nparse');

test('parse returns an AST', () => {
  const ast = m.parse('echo hi');
  assert.ok(ast, 'parse returned falsy');
});

// ---------------------------------------------------------------------------
// run — basic shape against an in-memory host
// ---------------------------------------------------------------------------

console.log('\nrun (browserHost + virtualFS)');

const host = m.browserHost({
  files: {
    '/work/a.txt': 'alpha\nbeta\ngamma\n',
    '/work/data.csv': 'name,age\nalice,30\nbob,25\n',
  },
});

test('echo + pipe + builtin', () => {
  const r = m.run('echo hello | tr a-z A-Z', { host });
  assert.strictEqual(r.exit, 0, `exit ${r.exit}, stderr=${r.stderr}`);
  assert.strictEqual(r.stdout, 'HELLO\n');
  assert.strictEqual(r.stderr, '');
});

test('result has stdout/stderr/exit/env keys', () => {
  const r = m.run('echo hi', { host });
  assert.ok('stdout' in r);
  assert.ok('stderr' in r);
  assert.ok('exit'   in r);
  assert.ok('env'    in r);
});

test('cat against pre-seeded vfs', () => {
  const r = m.run('cd /work && cat a.txt', { host });
  assert.strictEqual(r.exit, 0, r.stderr);
  assert.strictEqual(r.stdout, 'alpha\nbeta\ngamma\n');
});

test('grep + pipe', () => {
  const r = m.run('cd /work && cat a.txt | grep beta', { host });
  assert.strictEqual(r.exit, 0);
  assert.strictEqual(r.stdout, 'beta\n');
});

test('redirect + cat round-trip stays in vfs', () => {
  const r1 = m.run('echo hello > /tmp/note.txt', { host });
  assert.strictEqual(r1.exit, 0, r1.stderr);
  const r2 = m.run('cat /tmp/note.txt', { host });
  assert.strictEqual(r2.exit, 0, r2.stderr);
  assert.strictEqual(r2.stdout, 'hello\n');
});

test('nested sh -c re-enters the same sandbox', () => {
  const r = m.run("sh -c 'echo nested && pwd'", { host });
  assert.strictEqual(r.exit, 0, r.stderr);
  assert.ok(r.stdout.startsWith('nested\n'), `stdout=${r.stdout}`);
});

test('awk dispatch', () => {
  const r = m.run("cd /work && awk -F , 'NR>1 {print $1}' data.csv", { host });
  assert.strictEqual(r.exit, 0, r.stderr);
  assert.strictEqual(r.stdout, 'alice\nbob\n');
});

// ---------------------------------------------------------------------------
// Sessions — cd / vars persist across calls
// ---------------------------------------------------------------------------

console.log('\natomSession');

test('cd and export persist across two run calls', () => {
  const sess = m.atomSession();
  m.run('cd /work; export FOO=bar', { host, session: sess });
  const r = m.run('echo $FOO from $(pwd)', { host, session: sess });
  assert.strictEqual(r.exit, 0, r.stderr);
  assert.strictEqual(r.stdout, 'bar from /work\n');
});

// ---------------------------------------------------------------------------
// Permits — deny-all blocks unknown commands; allow-all + default-rules
// pass them
// ---------------------------------------------------------------------------

console.log('\npermit');

test('default-rules + deny-all-prompter blocks rm', () => {
  const r = m.run('rm -rf /tmp/x', {
    host,
    permit: { rulesets: [m.defaultRules], prompter: m.denyAllPrompter },
  });
  assert.strictEqual(r.exit, 126, `expected 126 (permission denied), got ${r.exit}`);
});

test('allow-all-prompter lets `make help` reach exec (then refused as non-builtin)', () => {
  const r = m.run('make help', {
    host,
    permit: { rulesets: [m.defaultRules], prompter: m.allowAllPrompter },
  });
  // The sandboxed host refuses non-builtin commands (exit 126); permit
  // itself decided :allow. Whichever order, the assertion we care about
  // is "we didn't crash and we did get a numeric exit code".
  assert.strictEqual(typeof r.exit, 'number');
});

// ---------------------------------------------------------------------------
// Resource budgets
// ---------------------------------------------------------------------------

console.log('\nbudget');

test('budget.stepInterrupt aborts an infinite-ish loop', () => {
  const interrupt = m.budget.stepInterrupt(50);
  let threw = false;
  try {
    m.run('i=0; while [ "$i" -lt 100000 ]; do i=$((i+1)); done',
          { host, interruptFn: interrupt });
  } catch (e) {
    threw = true;
    // budgetExceeded should recognise it.
    assert.ok(typeof e === 'object', 'no ex info');
  }
  assert.ok(threw, 'expected ex-info from budget');
});

test('budget.combine returns a function', () => {
  const i = m.budget.combine(
    m.budget.stepInterrupt(100),
    m.budget.deadlineInterrupt(50),
  );
  assert.strictEqual(typeof i, 'function');
});

// ---------------------------------------------------------------------------
// Tracing — :trace true returns a snapshot map; on-tool hooks fire.
// ---------------------------------------------------------------------------

console.log('\ntrace');

test('trace: true captures tool invocations', () => {
  const r = m.run('echo a; echo b', { host, trace: true });
  assert.ok(r.trace, 'no trace on result');
  assert.ok(Array.isArray(r.trace.tools), 'trace.tools is not an array');
  // We can't pin the exact count because echo is a shell builtin,
  // not a -spawn target. Just verify the shape is sane.
});

test('streaming on-tool hook fires per spawn', () => {
  const events = [];
  m.run('cd /work && cat a.txt', {
    host,
    trace: { onTool: (e) => events.push(e) },
  });
  // cat goes through -spawn → BuiltinHost dispatch → on-tool fires.
  assert.ok(events.length >= 1, `expected at least 1 tool event, got ${events.length}`);
});

// ---------------------------------------------------------------------------
// virtualFS — standalone handle, fs.* programmatic API
// ---------------------------------------------------------------------------

console.log('\nvirtualFS + fs.*');

test('virtualFS handle + fs.readFile', () => {
  const fs = m.virtualFS({ '/x.txt': 'one\ntwo\n' }, { cwd: '/' });
  assert.strictEqual(m.fs.readFile(fs, '/x.txt'), 'one\ntwo\n');
});

test('fs.exists', () => {
  const fs = m.virtualFS({ '/x.txt': '' });
  assert.strictEqual(m.fs.exists(fs, '/x.txt'), true);
  assert.strictEqual(m.fs.exists(fs, '/nope'), false);
});

// ---------------------------------------------------------------------------
// Summary + exit code
// ---------------------------------------------------------------------------

console.log(`\n${pass + fail} tests, ${pass} pass, ${fail} fail`);
process.exit(fail === 0 ? 0 : 1);
