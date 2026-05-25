// TypeScript definitions for muschel
// Project: https://github.com/replikativ/muschel
// SPDX-License-Identifier: Apache-2.0

declare module "muschel" {
  /** A bash AST node. Keys are bash-construct-specific; type & op
   *  fields identify the node kind. Treat as opaque unless you're
   *  walking the AST yourself. */
  export type Ast = Record<string, unknown>;

  /** Result of `parse`. On failure, `error` is set with the message
   *  and `data` carries position info from the parser. */
  export type ParseResult =
    | Ast
    | { error: string; data?: Record<string, unknown> };

  /** Per-call permit decision after walking the AST. */
  export interface PerCall {
    argv: string[];
    decision: "allow" | "deny" | "prompt";
    rule?: unknown;
  }

  /** Aggregate result of `check`. */
  export interface CheckResult {
    decision: "allow" | "deny" | "prompt";
    perCall: PerCall[];
    newRules?: unknown;
  }

  /** Tool function signature for browserHost. Receives the parsed
   *  argv, the upstream pipe contents as a string, and the
   *  environment map. Returns stdout/stderr/exit. */
  export type ToolFn = (
    argv: string[],
    stdin: string,
    env: Record<string, string>
  ) => { stdout?: string; stderr?: string; exit?: number };

  /** Opaque host handle. Pass through to `run`. */
  export type Host = object;

  /** Opaque session handle. Carries cwd / env / bg-jobs between
   *  `run` calls. */
  export type Session = object;

  export interface BrowserHostOpts {
    /** Map from command name → tool function. */
    tools?: Record<string, ToolFn>;
    /** Pre-seeded virtual filesystem (path → content). */
    files?: Record<string, string>;
  }

  export interface RunOpts {
    /** Required for the npm/cljs path. */
    host: Host;
    /** Optional — threads state across calls. */
    session?: Session;
    /** Optional — pre-flight permit check. */
    rulesets?: unknown[];
  }

  export interface RunResult {
    stdout: string;
    stderr: string;
    exit: number;
    /** Snapshot of env after the run (cwd, vars, last-exit, etc.). */
    env: Record<string, unknown>;
    /** Same session object passed in, if any (for chaining). */
    session?: Session;
  }

  /** Parse `src` into an AST. On failure returns `{error, data}`. */
  export function parse(src: string): ParseResult;

  /** Run the permit check against the default ruleset (or
   *  `opts.rulesets` if provided). */
  export function check(
    src: string,
    opts?: { rulesets?: unknown[] }
  ): CheckResult;

  /** Create a Node.js-backed host (uses `child_process.spawnSync` +
   *  `fs`). Pipelines are sequential. */
  export function nodeHost(): Host;

  /** Create a browser-backed host with a virtual fs and a virtual
   *  tool registry. No spawn capability — every external command
   *  must be registered as a tool, or it fails with exit 127. */
  export function browserHost(opts?: BrowserHostOpts): Host;

  /** Map of stock tools (cat, wc, grep, head) — usable as `tools`
   *  for `browserHost`. */
  export function stockTools(): Record<string, ToolFn>;

  /** Create a fresh AtomSession initialized from `new-env`. */
  export function session(): Session;

  /** Execute `src` against the host (and optional session). */
  export function run(src: string, opts: RunOpts): RunResult;

  /** Convenience: set a shell variable on a session. Returns the
   *  same session for chaining. */
  export function setVar(
    sess: Session,
    name: string,
    value: string
  ): Session;

  /** Read a shell variable from a session. Returns undefined if
   *  unset. */
  export function getVar(sess: Session, name: string): string | undefined;

  /** Read the current working directory from a session. */
  export function cwd(sess: Session): string;
}
