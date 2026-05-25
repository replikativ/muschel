(ns muschel.permit.defaults
  "Default permit ruleset, defined as Clojure data so it loads
   uniformly under JVM, babashka, and ClojureScript without resource-
   reading machinery.

   Three tiers:
     1. Auto-allow:  read-only commands safe to run without prompting.
     2. Auto-deny:   destructive commands that should never auto-run.
     3. Implicit:    no rule → `:ask` (default in `check`).

   Sources cross-referenced with codex's is_safe_command.rs and
   is_dangerous_command.rs (BSD-3) and Claude Code's documented
   read-only set.")

(def ^:private allow-cmd-names
  ;; File inspection / dir listing / navigation
  ["cat" "head" "tail" "less" "more" "wc" "stat" "file" "du" "df"
   "ls" "tree" "pwd" "cd" "realpath" "readlink" "basename" "dirname"
   ;; Searching
   "grep" "rg" "ag" "ack" "fd" "which" "type" "whereis"
   ;; Text manipulation (output-only)
   "echo" "printf" "cut" "paste" "tr" "sort" "uniq" "seq" "nl" "rev"
   "tac" "diff" "cmp" "comm" "jq" "yq"
   ;; System info / introspection
   "whoami" "id" "uname" "hostname" "date" "uptime" "free" "lscpu"
   "env" "printenv" "ps"
   ;; muschel builtins (env-only)
   "true" "false" ":" "test" "[" "[[" "let" "shift" "jobs" "wait"])

(def ^:private deny-cmd-names
  ;; Privilege escalation
  ["sudo" "su" "pkexec" "doas"
   ;; Code-as-data execution
   "eval" "source" "." "exec"
   ;; Disk-level destructive
   "dd" "fdisk" "parted" "wipefs" "cryptsetup"
   ;; Reboot / shutdown
   "reboot" "shutdown" "halt" "poweroff"])

(def ^:private allow-git-subcommands
  ["status" "log" "diff" "show" "branch" "describe" "rev-parse"
   "ls-files" "ls-tree"])

(def ^:private deny-rm-flags
  ["-rf" "-fr" "-Rf" "-r" "-R"])

(def default-rules
  (vec
   (concat
      ;; AUTO-ALLOW
    (for [n allow-cmd-names]
      {:tool :bash :pattern {:kind :cmd-name :name n}
       :action :allow :origin :default})
      ;; Read-only git
    (for [sub allow-git-subcommands]
      {:tool :bash :pattern {:kind :argv-vec :vec ["git" sub]}
       :action :allow :origin :default})
    [{:tool :bash :pattern {:kind :argv-vec :vec ["git" "remote" "-v"]}
      :action :allow :origin :default}
     {:tool :bash :pattern {:kind :argv-vec :vec ["git" "config" "--get"]}
      :action :allow :origin :default}
     {:tool :bash :pattern {:kind :argv-vec :vec ["git" "config" "--list"]}
      :action :allow :origin :default}]

      ;; AUTO-DENY
    (for [n deny-cmd-names]
      {:tool :bash :pattern {:kind :cmd-name :name n}
       :action :deny :origin :default
       :reason (case n
                 ("sudo" "su" "pkexec" "doas") "privilege escalation"
                 "eval"   "evaluates arbitrary string"
                 "source" "reads + runs a file"
                 "."      "reads + runs a file"
                 "exec"   "replaces shell"
                 "destructive command")})

      ;; rm with destructive flags
    (for [f deny-rm-flags]
      {:tool :bash :pattern {:kind :argv-vec :vec ["rm" f]}
       :action :deny :origin :default
       :reason "recursive force delete"})

      ;; Filesystem-format glob
    [{:tool :bash :pattern {:kind :argv-glob :glob "mkfs.*"}
      :action :deny :origin :default :reason "format filesystem"}
     {:tool :bash :pattern {:kind :argv-vec :vec ["chmod" #{"777" "+rwx"}]}
      :action :deny :origin :default :reason "world-writable permissions"}])))
