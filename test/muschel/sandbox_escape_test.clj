(ns muschel.sandbox-escape-test
  "Penetration tests for muschel's DiskFS sandbox. Every test here is
   a concrete exploit attempt — if any of them succeed in escaping
   the sandbox or leaking host information, that's a regression of a
   fix from the security audit.

   These tests use **DiskFS** (real disk, contained), not VirtualFS,
   so they exercise the actual path-resolution and symlink-handling
   logic. The sandbox root is a fresh temp dir per test."
  (:require [babashka.fs :as bfs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [muschel.builtins.posix :as posix]
            [muschel.core :as m]
            [muschel.fs.disk :as disk]
            [muschel.fs :as fs]
            [muschel.host.builtin :as hb]
            [muschel.host.jvm :as jvm]))

;; ============================================================================
;; Fixture: temp sandbox root + a known outside-root file we want to NOT touch
;; ============================================================================

(defn- mk-sandbox
  "Make a fresh temp wrapper, return [wrapper agent-dir host outside-file].
   The outside file lives in a *separate* temp dir so paths from the
   sandbox to it must escape.

   DiskFS uses the default `:mount-at` `/home/agent`, so the agent's
   real-disk workspace is `<wrapper>/home/agent` (auto-created by
   `disk/make`). Tests that inspect the filesystem state should use
   `agent-dir`, not the bare wrapper path."
  []
  (let [wrapper (str (bfs/create-temp-dir))
        agent-dir (str wrapper "/home/agent")
        outside-dir (str (bfs/create-temp-dir))
        outside-file (str outside-dir "/SECRET")
        _ (spit outside-file "TOP-SECRET-DO-NOT-LEAK\n")
        fs (disk/make wrapper)
        host (hb/make {:fs fs
                       :fallback-host (jvm/make)
                       :builtins posix/standard})]
    [wrapper agent-dir host outside-file]))

(defn- cleanup [root]
  (try (bfs/delete-tree root) (catch Throwable _ nil)))

(defn- run [host cmd]
  (m/run-and-capture (m/new-env) cmd {:host host}))

(defn- outside-was-touched?
  "True if the outside file got modified or read in a way that proves
   escape. We probe two things: contents changed (write escaped), or
   contents reachable (read escaped)."
  [outside-file expected-content]
  (not= expected-content (slurp outside-file)))

;; ============================================================================
;; F1 — symlink-parent write escape (the critical finding)
;; ============================================================================

(deftest f1-symlink-parent-write-escape-rejected
  (testing "ln -s + write through link's parent cannot escape"
    (let [[wrapper _agent-dir host outside-file] (mk-sandbox)
          outside-dir (str/replace outside-file #"/SECRET$" "")]
      (try
        ;; Try the exploit: create a symlink whose target is the
        ;; outside dir, then write through it.
        (let [r1 (run host (str "ln -s " outside-dir " evil"))
              r2 (run host "echo PWNED > evil/SECRET")]
          ;; We don't care whether the symlink creation itself was
          ;; refused (it should be, per F2) or whether the write was
          ;; refused — but the outside file MUST be untouched.
          (is (= "TOP-SECRET-DO-NOT-LEAK\n" (slurp outside-file))
              (str "outside SECRET file was modified — ESCAPE\n"
                   "  ln exit: " (:exit r1) "\n"
                   "  write exit: " (:exit r2) "\n"
                   "  contents: " (pr-str (slurp outside-file)))))
        (finally (cleanup wrapper))))))

(deftest f1-symlink-parent-create-newfile-rejected
  (testing "writing a brand-new file through a sandbox-internal symlink whose target escapes is rejected"
    (let [[wrapper _agent-dir host outside-file] (mk-sandbox)
          outside-dir (str/replace outside-file #"/SECRET$" "")]
      (try
        (run host (str "ln -s " outside-dir " evil"))
        (run host "echo PWNED > evil/newfile.txt")
        ;; Either the symlink was refused or the write resolves to
        ;; nothing. Either way no new file should appear outside.
        (is (not (bfs/exists? (str outside-dir "/newfile.txt")))
            "wrote outside-root through symlink-parent — ESCAPE")
        (finally (cleanup wrapper))))))

;; ============================================================================
;; F2 — direct symlink-to-outside is refused at create time
;; ============================================================================

(deftest f2-symlink-to-outside-target-refused
  (testing "ln -s /etc evil refuses to create the link at all"
    (let [[wrapper agent-dir host outside-file] (mk-sandbox)
          outside-dir (str/replace outside-file #"/SECRET$" "")]
      (try
        (run host (str "ln -s " outside-dir " evil"))
        ;; The link itself shouldn't exist inside the agent workspace.
        (is (not (bfs/exists? (str agent-dir "/evil")))
            (str "symlink with outside-root target was CREATED at " agent-dir "/evil — "
                 "this widens F1's attack surface"))
        (finally (cleanup wrapper))))))

;; ============================================================================
;; .. traversal (covered by inside?-check + lex-normalize)
;; ============================================================================

(deftest dot-dot-traversal-cannot-read
  (testing "cat ../../etc/passwd cannot escape root"
    (let [[wrapper _agent-dir host _] (mk-sandbox)]
      (try
        (let [r (run host "cat ../../../../../../../etc/passwd")]
          (is (not (str/includes? (:stdout r) "root:"))
              "passed real /etc/passwd content through ..-traversal"))
        (finally (cleanup wrapper))))))

(deftest absolute-path-cannot-read
  (testing "cat /etc/passwd cannot escape root"
    (let [[wrapper _agent-dir host _] (mk-sandbox)]
      (try
        (let [r (run host "cat /etc/passwd")]
          (is (not (str/includes? (:stdout r) "root:"))
              "absolute outside-root path returned real /etc/passwd"))
        (finally (cleanup wrapper))))))

;; ============================================================================
;; Redirect tricks
;; ============================================================================

(deftest redirect-to-outside-cannot-write
  (testing "echo X > /etc/passwd cannot write outside root"
    (let [[wrapper _agent-dir host _] (mk-sandbox)]
      (try
        (let [_r (run host "echo X > /etc/MUSCHEL_PROBE")]
          ;; The host shell may print an error or silently no-op,
          ;; but the file MUST NOT appear on real /etc.
          (is (not (bfs/exists? "/etc/MUSCHEL_PROBE"))
              "redirect created real /etc/MUSCHEL_PROBE"))
        (finally (cleanup wrapper))))))

(deftest redirect-via-dotdot-cannot-write
  (testing "echo X > ../escape cannot land outside the agent workspace"
    (let [[wrapper agent-dir host _] (mk-sandbox)
          ;; Real-disk parent of the agent workspace is <wrapper>/home;
          ;; real-disk parent of the wrapper itself is /tmp (or similar).
          ;; Neither must receive the file.
          agent-parent (.getParent (java.io.File. ^String agent-dir))
          wrapper-parent (.getParent (java.io.File. ^String wrapper))]
      (try
        (run host "echo X > ../escape")
        (is (not (bfs/exists? (str agent-parent "/escape")))
            "redirect via ..-traversal wrote into <wrapper>/home")
        (is (not (bfs/exists? (str wrapper-parent "/escape")))
            "redirect via ..-traversal wrote above the wrapper")
        (finally (cleanup wrapper))))))

;; ============================================================================
;; $(< file) command-substitution short-circuit
;; ============================================================================

(deftest dollar-lt-cannot-read-outside
  (testing "$(< /etc/passwd) cannot leak real file via cmd-substitution"
    (let [[wrapper _agent-dir host _] (mk-sandbox)]
      (try
        (let [r (run host "echo \"$(< /etc/passwd)\"")]
          (is (not (str/includes? (:stdout r) "root:"))
              "$(< /etc/passwd) leaked real file content"))
        (finally (cleanup wrapper))))))

;; ============================================================================
;; F6 — pwd / realpath surface the sandbox-relative mount, not the host prefix
;; ============================================================================

(deftest realpath-hides-host-prefix
  (testing "realpath . returns /home/agent (the mount), not /tmp/muschel-xyz/home/agent"
    (let [[wrapper _agent-dir host _] (mk-sandbox)]
      (try
        (let [r (run host "realpath .")]
          (is (zero? (:exit r)))
          (is (= "/home/agent" (str/trim (:stdout r)))
              (str "realpath leaked host mount path: " (pr-str (:stdout r)))))
        (finally (cleanup wrapper))))))

(deftest realpath-inside-hides-host-prefix
  (testing "realpath sub returns /home/agent/sub, not /tmp/muschel-xyz/home/agent/sub"
    (let [[wrapper agent-dir host _] (mk-sandbox)]
      (try
        (bfs/create-dirs (str agent-dir "/sub"))
        (let [r (run host "realpath sub")]
          (is (zero? (:exit r)))
          (is (= "/home/agent/sub" (str/trim (:stdout r)))
              (str "realpath leaked host mount path: " (pr-str (:stdout r)))))
        (finally (cleanup wrapper))))))

(deftest pwd-hides-host-prefix
  (testing "pwd inside sandbox returns the mount path, not the host mount"
    (let [[wrapper _agent-dir host _] (mk-sandbox)]
      (try
        (let [r (run host "pwd")]
          (is (zero? (:exit r)))
          (is (= "/home/agent" (str/trim (:stdout r)))
              (str "pwd leaked host mount path: " (pr-str (:stdout r)))))
        (finally (cleanup wrapper))))))

;; ============================================================================
;; Glob doesn't leak when used in a sandboxed run
;; ============================================================================

(deftest glob-stays-inside-sandbox
  (testing "echo * inside sandbox doesn't list outside-root files"
    (let [[wrapper agent-dir host outside-file] (mk-sandbox)
          outside-dir (str/replace outside-file #"/SECRET$" "")]
      (try
        (spit (str agent-dir "/inside.txt") "")
        (let [r (run host "echo *")]
          (is (= "inside.txt\n" (:stdout r))
              "glob listed agent workspace files only")
          (is (not (str/includes? (:stdout r) outside-dir))
              "glob mentioned outside path")
          (is (not (str/includes? (:stdout r) "SECRET"))
              "glob mentioned the secret file"))
        (finally (cleanup wrapper))))))

(deftest glob-with-dotdot-cannot-escape
  (testing "echo ../* doesn't list parent-of-root contents"
    (let [[wrapper _agent-dir host _] (mk-sandbox)]
      (try
        (let [r (run host "echo ../*")]
          ;; The glob should expand to literal `../*` (no matches in
          ;; the sandbox) or resolve to nothing — definitely not to
          ;; the host's /tmp listing.
          (is (not (str/includes? (:stdout r) "/tmp"))
              (str "glob ../* leaked host paths: " (pr-str (:stdout r)))))
        (finally (cleanup wrapper))))))

;; ============================================================================
;; Host env leak — the re-audit critical finding. `new-env` must NOT
;; inherit (System/getenv) by default; secrets in the launching
;; process must not be visible from inside a sandboxed run.
;; ============================================================================

(deftest host-env-not-leaked-by-default
  (testing "echo $PATH inside sandbox returns empty (host PATH must NOT leak)"
    (let [[wrapper _agent-dir host _] (mk-sandbox)]
      (try
        ;; Set a known canary in the JVM process env via Java reflection
        ;; — alas can't really do that, so instead read a var we KNOW
        ;; exists on the host. PATH is always there on Linux.
        (let [host-path (System/getenv "PATH")
              r (run host "echo $PATH")]
          (is (some? host-path) "test prerequisite: host has $PATH")
          (is (not= (str host-path "\n") (:stdout r))
              (str "host $PATH leaked into sandbox: " (pr-str (:stdout r)))))
        (finally (cleanup wrapper))))))

(deftest env-builtin-does-not-dump-host-vars
  (testing "the `env` builtin inside sandbox returns only sandbox-set vars"
    (let [[wrapper _agent-dir host _] (mk-sandbox)]
      (try
        (let [r (run host "env")
              lines (str/split-lines (:stdout r))]
          ;; With no host-env inheritance, env should print PWD (and
          ;; maybe a couple of muschel-set vars) but NOT 90+ host
          ;; vars like SSH_AUTH_SOCK / DBUS_SESSION_BUS_ADDRESS / etc.
          (is (< (count lines) 10)
              (str "env dumped " (count lines) " vars — looks like host env leaked")))
        (finally (cleanup wrapper))))))
