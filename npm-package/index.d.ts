// TypeScript declarations for the `muschel` npm package.
//
// Auto-generation note: this file mirrors muschel.js-api one-to-one
// today and is hand-authored. A future muschel.api.specification
// pass will replace this with codegen (same pattern as datahike's
// npm-package/index.d.ts).

// ============================================================================
// AST + permit
// ============================================================================

/** A bash AST node. Shape varies by `:type`; treated opaquely. */
export type AST = { type: string; [k: string]: any };

/** Permit decision per call site. */
export interface PermitDecision {
  decision: "allow" | "deny" | "ask";
  reason?: string;
  ruleId?: any;
  call?: any;
}

export interface PermitResult {
  decision: "allow" | "deny" | "ask";
  perCall: PermitDecision[];
  newRules?: any[];
}

export interface PermitRule {
  tool?: string;
  pattern: any;
  action: "allow" | "deny" | "ask";
  reason?: string;
  origin?: "default" | "user" | "session";
  id?: string;
}

/** A prompter is invoked when a rule decides `:ask`. */
export type Prompter = (call: any, ruleset: PermitRule[]) => "allow" | "deny";

export function parse(src: string): AST;
export function check(src: string, opts?: { rulesets?: PermitRule[][]; prompter?: Prompter }): PermitResult;
export const defaultRules: PermitRule[];
export const denyAllPrompter: Prompter;
export const allowAllPrompter: Prompter;

// ============================================================================
// Env + session
// ============================================================================

/** Opaque env value. Use newEnv / getVar / setVar to construct + read. */
export type Env = object;
/** Opaque atom-session handle. */
export type Session = object;

export function newEnv(opts?: {
  cwd?: string;
  posArgs?: string[];
  script?: string;
  /** Inherit host process env (`process.env`). DEFAULT: false. */
  hostEnv?: boolean;
  vars?: { [k: string]: string };
}): Env;

export function getVar(env: Env, name: string): string;
export function setVar(env: Env, name: string, value: string): Env;

export function atomSession(env?: Env): Session;
export function sessionCwd(sess: Session): string;

// ============================================================================
// Hosts + filesystems
// ============================================================================

/** Opaque host handle. */
export type Host = object;

/** Tool function signature for browserHost.tools. */
export type ToolFn = (
  argv: string[],
  stdin: string,
  env: { [k: string]: string }
) => { stdout?: string; stderr?: string; exit?: number };

/**
 * Node.js host. **UNSANDBOXED by default** — uses real
 * child_process + real fs.  For a contained Node run today, use
 * `browserHost({ files, tools })` (works on Node too).
 */
export function nodeHost(opts?: object): Host;

/**
 * Sandboxed in-memory host. Works in browser AND Node.
 *
 * - `files`: initial virtual-FS contents `{path: contents}`
 * - `tools`: extra tool functions `{name: ToolFn}`
 * - `includeStock`: include `stockTools()` (cat/wc/grep/head).
 *   Default `true`.
 */
export function browserHost(opts?: {
  files?: { [path: string]: string };
  tools?: { [name: string]: ToolFn };
  includeStock?: boolean;
}): Host;

/** Opaque FS handle compatible with the `fs.*` ops below. */
export type FS = object;

/** Construct a fresh VirtualFS. */
export function virtualFS(
  files?: { [path: string]: string },
  opts?: { cwd?: string }
): FS;

/** Returns the stock tools (cat / wc / grep / head) as a JS object. */
export function stockTools(): { [name: string]: ToolFn };

/**
 * Filesystem operations against a VirtualFS handle.
 *
 * Currently works only on VirtualFS handles built via `virtualFS()`;
 * the BrowserHost has its own internal vfs that does not yet expose
 * the muschel.fs protocol (separate refactor). For BrowserHost,
 * inspect state via `run(...).trace.reads/writes` or rebuild a fresh
 * host with updated `files` between runs.
 */
export const fs: {
  readFile(fs: FS, path: string): string | null;
  listDir(fs: FS, path: string): Array<{ name: string; type: string; size: number; mtimeMs: number }>;
  exists(fs: FS, path: string): boolean;
  stat(fs: FS, path: string): { type: string; size: number; mtimeMs: number; perms?: string } | null;
  mkdir(fs: FS, path: string): boolean;
  delete(fs: FS, path: string): boolean;
  rename(fs: FS, from: string, to: string): boolean;
  touch(fs: FS, path: string): boolean;
  chmod(fs: FS, path: string, mode: number): boolean;
  symlink(fs: FS, target: string, link: string): boolean;
  sandboxRelativize(fs: FS, realPath: string): string;
  cwd(fs: FS): string;
};

// ============================================================================
// Resource budgets
// ============================================================================

/** A cooperative interrupt. Called at every shell loop boundary;
 *  throws to abort the run. */
export type InterruptFn = () => void;

export const budget: {
  /** Build an interrupt that throws once `ms` have elapsed. */
  deadlineInterrupt(ms: number): InterruptFn;
  /** Build an interrupt that throws after N invocations. */
  stepInterrupt(maxSteps: number): InterruptFn;
  /** Compose multiple interrupts into one. */
  combine(...fns: InterruptFn[]): InterruptFn;
  /** True if `err` is a budget-exceeded throw from this library. */
  budgetExceeded(err: any): boolean;
};

// ============================================================================
// Run + introspection
// ============================================================================

/** Tool-call event captured during a run. */
export interface ToolEvent {
  type: "tool";
  name: string;
  argv: string[];
  exit: number;
  stdoutBytes?: number;
  stderrBytes?: number;
  durationMs?: number;
}

/** Filesystem op event captured during a run. */
export interface FsEvent {
  type: "fs";
  op:
    | "resolve" | "read-file" | "read-bytes" | "open-source"
    | "open-sink" | "list-dir" | "stat" | "exists?"
    | "mkdir" | "delete" | "rename" | "touch"
    | "chmod" | "symlink" | "chown" | "cd";
  path: string;
  ok?: boolean;
}

/** Permit-deny event captured during a run. */
export interface DeniedEvent {
  type: "denied";
  tool: string;
  argv?: string[];
  reason?: string;
  ruleId?: any;
}

/** Trace snapshot returned by `run()` when `trace` is opted in. */
export interface TraceSnapshot {
  tools: ToolEvent[];
  fs: FsEvent[];
  reads: string[];   // distinct read paths
  writes: string[];  // distinct write paths
  denied: DeniedEvent[];
  budget: { steps: number; wallMs: number; outputBytes: number };
}

/** Trace options. `true` = default state. */
export type TraceOpt =
  | boolean
  | {
      cap?: number;
      onTool?: (e: ToolEvent) => void;
      onFs?: (e: FsEvent) => void;
      onDeny?: (e: DeniedEvent) => void;
    };

export interface RunResult {
  exit: number;
  stdout: string;
  stderr: string;
  env: any;
  session: Session;
  /** Permit check result when `permit` was passed. */
  permit?: PermitResult;
  /** Trace snapshot when `trace` was opted in. */
  trace?: TraceSnapshot;
}

export interface RunOpts {
  host?: Host;
  session?: Session;
  /** Starting env override (defaults to session env or `newEnv()`). */
  env?: Env;

  /** Optional permit policy. */
  permit?: { rulesets?: PermitRule[][]; prompter?: Prompter };

  /** Resource budgets. */
  timeoutMs?: number;
  interruptFn?: InterruptFn;

  /** Introspection. */
  trace?: TraceOpt;

  /** Optional stdin string. */
  in?: string;
}

/**
 * Execute a bash source string. By default uses a fresh
 * `browserHost()` if no `host` is provided.
 */
export function run(src: string, opts?: RunOpts): RunResult;
