(ns muschel.test-helpers
  "Shared helpers used by .cljc test files. Provides a portable
   `fallback-host` constructor and a `mk-host` shortcut so the same
   test source runs against `host.jvm` on JVM and `host.browser` on
   Node / browser."
  (:require [muschel.builtins.posix :as posix]
            [muschel.fs.virtual :as vfs]
            [muschel.host.builtin :as hb]
            #?(:clj  [muschel.host.jvm :as host.jvm]
               :cljs [muschel.host.browser :as host.browser])))

(defn fallback-host
  "Construct a platform-appropriate fallback host. The BuiltinHost
   wraps this for FS-aware builtin dispatch; the fallback handles
   buffers, pipes, async, and allowlisted (rare) external spawns."
  []
  #?(:clj  (host.jvm/make)
     :cljs (host.browser/make)))

(defn mk-host
  "Build a BuiltinHost over a virtual FS. Options:
     :files     — vfs seed map {path → content}. Defaults to `{}`.
     :cwd       — vfs cwd. Defaults to `\"/\"`.
     :builtins  — builtin map. Defaults to `posix/standard` (full set).
     :allowlist — set of cmds the fallback may run. Defaults to `#{}`."
  ([] (mk-host {}))
  ([{:keys [files cwd builtins allowlist]
     :or {files {} cwd "/" builtins posix/standard allowlist #{}}}]
   (hb/make {:fs (vfs/make files {:cwd cwd})
             :fallback-host (fallback-host)
             :builtins builtins
             :fallback-allowlist allowlist})))

(defn ex-info? [e]
  (instance? #?(:clj clojure.lang.ExceptionInfo
                :cljs cljs.core/ExceptionInfo) e))
