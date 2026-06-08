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

      ;; rm with a recursive flag in ANY position (incl. trailing flags,
      ;; long-form, or split clusters like `rm -r -f /tmp`).
    [{:tool :bash
      :pattern {:kind :argv-flags
                :head ["rm"]
                :any-of #{"-r" "-R" "--recursive"}}
      :action :deny :origin :default
      :reason "recursive delete"}]

      ;; Filesystem-format glob
    [{:tool :bash :pattern {:kind :argv-glob :glob "mkfs.*"}
      :action :deny :origin :default :reason "format filesystem"}
     {:tool :bash :pattern {:kind :argv-vec :vec ["chmod" #{"777" "+rwx"}]}
      :action :deny :origin :default :reason "world-writable permissions"}

      ;; --- argv-shape fine-grained gates ----------------------------------
      ;;
      ;; argv-shape matches exact-length argv (last `:**` makes the
      ;; tail open). Use it to allow a broad command BUT deny a
      ;; specific dangerous combination — last-match-wins layering.

      ;; git push: allow, but force-flavours deny — order-insensitive,
      ;; so `git push origin --force main` is caught too.
     {:tool :bash :pattern {:kind :argv-shape
                            :shape ["git" "push" :**]}
      :action :allow :origin :default
      :reason "publish commits"}
     {:tool :bash :pattern {:kind :argv-flags
                            :head ["git" "push"]
                            :any-of #{"-f" "--force" "--force-with-lease"}}
      :action :deny :origin :default
      :reason "force-push rewrites remote history"}

      ;; git reset --hard / git clean -f -d: history / working-tree wipes.
     {:tool :bash :pattern {:kind :argv-shape
                            :shape ["git" "reset" "--hard" :**]}
      :action :deny :origin :default
      :reason "discards uncommitted work"}
     {:tool :bash :pattern {:kind :argv-flags
                            :head ["git" "clean"]
                            :all-of #{"-f"}
                            :any-of #{"-d" "-x"}}
      :action :deny :origin :default
      :reason "deletes untracked files"}

      ;; chmod world-writable variants (octal 7 in `other` slot)
     {:tool :bash :pattern {:kind :argv-shape
                            :shape ["chmod"
                                    #{"0777" "777" "0666" "666" "0755" "0700"}
                                    :**]}
      ;; `:allow` for the well-known safe defaults, `:deny` for 0777/0666
      :action :allow :origin :default
      :reason "explicit octal modes"}
     {:tool :bash :pattern {:kind :argv-shape
                            :shape ["chmod" #{"0777" "777" "0666" "666"} :**]}
      :action :deny :origin :default
      :reason "world-writable / world-readable octal"}

      ;; rm -rf / on common dangerous paths.
     {:tool :bash :pattern {:kind :argv-shape
                            :shape ["rm" :** #{"/" "/*" "/home" "/usr" "/etc" "/var" "/bin" "/lib"}]}
      :action :deny :origin :default
      :reason "would delete a critical system path"}

      ;; curl / wget piped to sh — the install.sh pattern. The pipe
      ;; itself is fine; running anything fetched is what we gate.
      ;; The leaf `sh` / `bash` call hits the next rule.
     {:tool :bash :pattern {:kind :argv-shape
                            :shape [#{"bash" "sh" "zsh" "dash"} "-c" :**]}
      :action :allow :origin :default
      :reason "muschel re-parses through the same gates"}
     {:tool :bash :pattern {:kind :argv-shape
                            :shape [#{"bash" "sh" "zsh" "dash"}]}
      :action :ask :origin :default
      :reason "bare shell prompt — confirm intent"}])))
