(ns muschel.write-builtins-test
  "Tests for the write-side builtins: touch, mkdir, rmdir, rm, cp, mv,
   chmod, ln, tee. Each goes through the FS protocol so containment
   is enforced — verified at the bottom."
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.builtins.posix :as posix]
            [muschel.core :as m]
            [muschel.fs :as fs]
            [muschel.fs.virtual :as vfs]
            [muschel.host.builtin :as hb]
            [muschel.host.jvm :as jvm]))

(defn- fs+host
  ([] (fs+host {"/work" :dir "/work/a.txt" "alpha\n"}))
  ([entries]
   (let [fs   (vfs/make entries {:cwd "/work"})
         host (hb/make {:fs fs
                        :fallback-host (jvm/make)
                        :builtins posix/standard})]
     [fs host])))

(defn- run [host cmd]
  (m/run-and-capture (m/new-env) cmd {:host host}))

;; ============================================================================
;; touch
;; ============================================================================

(deftest touch-creates-empty
  (let [[fs host] (fs+host)
        r (run host "touch new.txt")]
    (is (= 0 (:exit r)))
    (is (fs/exists? fs "/work/new.txt"))
    (is (= "" (fs/read-file fs "/work/new.txt")))))

(deftest touch-updates-mtime
  (let [[fs host] (fs+host)
        before    (:mtime-ms (fs/stat fs "/work/a.txt"))]
    (Thread/sleep 3)
    (run host "touch a.txt")
    (let [after (:mtime-ms (fs/stat fs "/work/a.txt"))]
      (is (> after before)))))

(deftest touch-c-skips-creation
  (let [[fs host] (fs+host)
        r (run host "touch -c missing.txt")]
    (is (= 0 (:exit r)))
    (is (not (fs/exists? fs "/work/missing.txt")))))

;; ============================================================================
;; mkdir / rmdir
;; ============================================================================

(deftest mkdir-creates-dir
  (let [[fs host] (fs+host)
        r (run host "mkdir sub")]
    (is (= 0 (:exit r)))
    (is (= :dir (:type (fs/stat fs "/work/sub"))))))

(deftest mkdir-refuses-existing
  (let [[fs host] (fs+host {"/work" :dir "/work/sub" :dir})
        r (run host "mkdir sub")]
    (is (= 1 (:exit r)))
    (is (re-find #"File exists" (:stderr r)))))

(deftest mkdir-p-creates-parents
  (let [[fs host] (fs+host)
        r (run host "mkdir -p a/b/c")]
    (is (= 0 (:exit r)))
    (is (= :dir (:type (fs/stat fs "/work/a/b/c"))))))

(deftest mkdir-p-idempotent
  (let [[fs host] (fs+host)]
    (run host "mkdir -p sub")
    (let [r (run host "mkdir -p sub")]
      (is (= 0 (:exit r))))))

(deftest rmdir-empty
  (let [[fs host] (fs+host {"/work" :dir "/work/empty" :dir})
        r (run host "rmdir empty")]
    (is (= 0 (:exit r)))
    (is (not (fs/exists? fs "/work/empty")))))

(deftest rmdir-refuses-non-empty
  (let [[fs host] (fs+host {"/work" :dir
                            "/work/sub" :dir
                            "/work/sub/x" "1"})
        r (run host "rmdir sub")]
    (is (= 1 (:exit r)))
    (is (re-find #"not empty" (:stderr r)))
    (is (fs/exists? fs "/work/sub"))))

;; ============================================================================
;; rm
;; ============================================================================

(deftest rm-file
  (let [[fs host] (fs+host)
        r (run host "rm a.txt")]
    (is (= 0 (:exit r)))
    (is (not (fs/exists? fs "/work/a.txt")))))

(deftest rm-refuses-directory-without-r
  (let [[fs host] (fs+host {"/work" :dir "/work/sub" :dir})
        r (run host "rm sub")]
    (is (= 1 (:exit r)))
    (is (re-find #"Is a directory" (:stderr r)))))

(deftest rm-r-recursive
  (let [[fs host] (fs+host {"/work" :dir
                            "/work/sub" :dir
                            "/work/sub/a" "1"
                            "/work/sub/b" "2"
                            "/work/sub/inner" :dir
                            "/work/sub/inner/c" "3"})
        r (run host "rm -r sub")]
    (is (= 0 (:exit r)))
    (is (not (fs/exists? fs "/work/sub")))))

(deftest rm-f-silences-missing
  (let [[_ host] (fs+host)
        r (run host "rm -f no-such-file")]
    (is (= 0 (:exit r)))
    (is (= "" (:stderr r)))))

;; ============================================================================
;; cp / mv
;; ============================================================================

(deftest cp-file
  (let [[fs host] (fs+host)
        r (run host "cp a.txt b.txt")]
    (is (= 0 (:exit r)))
    (is (= "alpha\n" (fs/read-file fs "/work/b.txt")))))

(deftest cp-into-existing-dir
  (let [[fs host] (fs+host {"/work" :dir
                            "/work/a.txt" "alpha\n"
                            "/work/dst" :dir})
        r (run host "cp a.txt dst")]
    (is (= 0 (:exit r)))
    (is (= "alpha\n" (fs/read-file fs "/work/dst/a.txt")))))

(deftest cp-r-tree
  (let [[fs host] (fs+host {"/work" :dir
                            "/work/src" :dir
                            "/work/src/x" "X"
                            "/work/src/sub" :dir
                            "/work/src/sub/y" "Y"})
        r (run host "cp -r src dst")]
    (is (= 0 (:exit r)))
    (is (= "X" (fs/read-file fs "/work/dst/x")))
    (is (= "Y" (fs/read-file fs "/work/dst/sub/y")))))

(deftest cp-dir-without-r-refuses
  (let [[_ host] (fs+host {"/work" :dir "/work/sub" :dir})
        r (run host "cp sub dst")]
    (is (= 1 (:exit r)))
    (is (re-find #"omitting directory" (:stderr r)))))

(deftest mv-file
  (let [[fs host] (fs+host)
        r (run host "mv a.txt b.txt")]
    (is (= 0 (:exit r)))
    (is (not (fs/exists? fs "/work/a.txt")))
    (is (= "alpha\n" (fs/read-file fs "/work/b.txt")))))

(deftest mv-into-existing-dir
  (let [[fs host] (fs+host {"/work" :dir
                            "/work/a.txt" "alpha\n"
                            "/work/dst" :dir})
        r (run host "mv a.txt dst")]
    (is (= 0 (:exit r)))
    (is (not (fs/exists? fs "/work/a.txt")))
    (is (= "alpha\n" (fs/read-file fs "/work/dst/a.txt")))))

;; ============================================================================
;; chmod / ln / tee
;; ============================================================================

(deftest chmod-stores-mode
  (let [[fs host] (fs+host)
        r (run host "chmod 0644 a.txt")]
    (is (= 0 (:exit r)))
    ;; vfs stores opaque mode; check via internal entry shape.
    (is (= 0644 (-> (fs/stat fs "/work/a.txt") :perms-mode (or 0))))))

(deftest ln-s-creates-symlink
  (let [[fs host] (fs+host)
        r (run host "ln -s a.txt link.txt")]
    (is (= 0 (:exit r)))
    (is (= :symlink (:type (fs/stat fs "/work/link.txt"))))))

(deftest ln-hard-link-refused
  (let [[_ host] (fs+host)
        r (run host "ln a.txt b.txt")]
    (is (= 1 (:exit r)))
    (is (re-find #"only -s" (:stderr r)))))

(deftest tee-writes-file-and-stdout
  (let [[fs host] (fs+host)
        r (run host "echo hello | tee out.txt")]
    (is (= 0 (:exit r)))
    (is (.contains ^String (:stdout r) "hello"))
    (is (.contains ^String (fs/read-file fs "/work/out.txt") "hello"))))

(deftest tee-append
  (let [[fs host] (fs+host {"/work" :dir "/work/log" "init\n"})]
    (run host "echo more | tee -a log")
    (is (= "init\nmore\n" (fs/read-file fs "/work/log")))))

;; ============================================================================
;; Audit-bug regressions: write-side
;; ============================================================================

(deftest mkdir-m-applies-mode
  ;; Previously parsed but silently ignored.
  (let [[fs host] (fs+host)
        r (run host "mkdir -m 0700 secret")]
    (is (= 0 (:exit r)))
    (is (= 0700 (-> (fs/stat fs "/work/secret") :perms-mode (or 0))))))

(deftest rmdir-p-walks-parents
  (let [[fs host] (fs+host {"/work" :dir
                            "/work/a" :dir
                            "/work/a/b" :dir
                            "/work/a/b/c" :dir})
        r (run host "rmdir -p a/b/c")]
    (is (= 0 (:exit r)))
    (is (not (fs/exists? fs "/work/a/b/c")))
    (is (not (fs/exists? fs "/work/a/b")))
    (is (not (fs/exists? fs "/work/a")))))

(deftest chmod-symbolic-u-plus-x
  (let [[fs host] (fs+host)
        ;; Start at 0644
        _ (run host "chmod 0644 a.txt")
        r (run host "chmod u+x a.txt")]
    (is (= 0 (:exit r)))
    (is (= 0744 (-> (fs/stat fs "/work/a.txt") :perms-mode (or 0))))))

(deftest chmod-symbolic-a-equals-r
  (let [[fs host] (fs+host)
        r (run host "chmod a=r a.txt")]
    (is (= 0 (:exit r)))
    (is (= 0444 (-> (fs/stat fs "/work/a.txt") :perms-mode (or 0))))))

(deftest chmod-symbolic-go-minus-w
  (let [[fs host] (fs+host)
        _ (run host "chmod 0666 a.txt")
        r (run host "chmod go-w a.txt")]
    (is (= 0 (:exit r)))
    (is (= 0644 (-> (fs/stat fs "/work/a.txt") :perms-mode (or 0))))))

(deftest chown-records-owner-group
  (let [[fs host] (fs+host)
        r (run host "chown alice:dev a.txt")]
    (is (= 0 (:exit r)))
    (let [s (fs/stat fs "/work/a.txt")]
      (is (= "alice" (:owner s)))
      (is (= "dev"   (:group s))))))

(deftest chown-owner-only
  (let [[fs host] (fs+host)
        r (run host "chown alice a.txt")]
    (is (= 0 (:exit r)))
    (is (= "alice" (:owner (fs/stat fs "/work/a.txt"))))))

;; ============================================================================
;; Write builtins respect FS containment
;; ============================================================================

(deftest write-builtins-cannot-touch-real-disk
  ;; Whatever the virtual FS chooses to do internally (the implicit
  ;; root is `/`, so absolute paths like `/etc/passwd` end up as vfs
  ;; entries), no operation reaches real disk. The contract under
  ;; test: the real filesystem is invariant across any sequence of
  ;; write-builtin invocations.
  (let [marker (str "/tmp/muschel-write-sandbox-marker-"
                    (System/currentTimeMillis))]
    (try
      (let [[_ host] (fs+host)]
        (doseq [cmd [(str "touch " marker)
                     (str "mkdir " marker)
                     (str "mkdir -p " marker "/a/b/c")
                     (str "echo data > " marker)
                     (str "cp a.txt " marker)
                     (str "rm -rf /etc")]]
          (run host cmd))
        (is (not (.exists (java.io.File. marker)))
            "no real /tmp file may exist after all the writes")
        (is (.exists (java.io.File. "/etc/passwd"))
            "real /etc/passwd must still exist"))
      (finally
        (try (.delete (java.io.File. marker)) (catch Throwable _))))))
