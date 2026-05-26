(ns muschel.runtime
  "Late-bound function registry that breaks the
   `muschel.exec` ⇄ `muschel.builtins.posix` cycle.

   `muschel.exec` requires `muschel.builtins.posix` (through
   `muschel.host.builtin`), and the `sh` builtin in posix needs to
   call back into exec / parse / env to evaluate its `-c SCRIPT`
   argument. On JVM the original workaround was `(require 'muschel.exec)`
   + `(resolve …)` inside `sh`; on CLJS that doesn't work — runtime
   `require` is a compile-time-only construct.

   The registry inverts the dependency. `muschel.exec` and
   `muschel.env` install themselves here at load time; `sh` reads the
   atoms when it needs to recurse. No static `:require` of exec from
   posix, and no `require/resolve` round trip — just plain function
   values stored in atoms. Works identically on JVM, ClojureScript,
   and babashka.")

(defonce ^{:doc "(fn [^String script] => AST). Set by muschel.parse."}
  parse-fn (atom nil))

(defonce ^{:doc "(fn [env ast opts]) => {:stdout :stderr :exit}.
                 Set by muschel.exec."}
  run-fn (atom nil))

(defonce ^{:doc "(fn [] => env). Set by muschel.env."}
  new-env-fn (atom nil))

(defn register!
  "Install one or more of the late-bound functions. Keys: `:parse`,
   `:run`, `:new-env`. Callers pass only the keys they own — e.g.
   muschel.parse installs `:parse`, muschel.exec installs `:run`, …"
  [{:keys [parse run new-env]}]
  (when parse   (reset! parse-fn parse))
  (when run     (reset! run-fn run))
  (when new-env (reset! new-env-fn new-env)))

(defn parse  [script] (when-let [f @parse-fn]   (f script)))
(defn run    [env ast opts] (when-let [f @run-fn] (f env ast opts)))
(defn new-env []      (when-let [f @new-env-fn] (f)))
