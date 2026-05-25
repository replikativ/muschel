(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.string :as str]
            [clojure.tools.build.api :as b]
            [borkdude.gh-release-artifact :as gh]
            [deps-deploy.deps-deploy :as dd])
  (:import [clojure.lang ExceptionInfo]))

(def org "replikativ")
(def lib 'org.replikativ/muschel)
(def current-commit (b/git-process {:git-args "rev-parse HEAD"}))
(def version (format "0.1.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src"]
                :scm {:url "https://github.com/replikativ/muschel"
                      :connection "scm:git:git://github.com/replikativ/muschel.git"
                      :developerConnection "scm:git:ssh://git@github.com/replikativ/muschel.git"
                      :tag (str "v" version)}
                :pom-data [[:description "Parse bash, gate it with an allow/deny permit, run it through a pluggable host (JVM, Node, browser)."]
                           [:url "https://github.com/replikativ/muschel"]
                           [:licenses
                            [:license
                             [:name "Apache License 2.0"]
                             [:url "https://www.apache.org/licenses/LICENSE-2.0"]]]
                           [:developers
                            [:developer
                             [:id "whilo"]
                             [:name "Christian Weilbach"]
                             [:email "ch_weil@topiq.es"]]]]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn deploy
  "Push the jar to Clojars. Requires CLOJARS_USERNAME / CLOJARS_PASSWORD."
  [_]
  (jar nil)
  (dd/deploy {:installer :remote :artifact jar-file
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))

(defn fib [a b]
  (lazy-seq (cons a (fib b (+ a b)))))

(defn retry-with-fib-backoff [retries exec-fn test-fn]
  (loop [idle-times (take retries (fib 1 2))]
    (let [result (exec-fn)]
      (if (test-fn result)
        (do (println "Returned: " result)
            (if-let [sleep-ms (first idle-times)]
              (do (println "Retrying with remaining back-off times (in s): " idle-times)
                  (Thread/sleep (* 1000 sleep-ms))
                  (recur (rest idle-times)))
              result))
        result))))

(defn try-release []
  (try (gh/overwrite-asset {:org org
                            :repo (name lib)
                            :tag version
                            :commit current-commit
                            :file jar-file
                            :content-type "application/java-archive"
                            :draft false})
       (catch ExceptionInfo e
         (assoc (ex-data e) :failure? true))))

(defn release
  "Attach the built jar to a GitHub release named after `version`.
   Requires a GITHUB_TOKEN env var."
  [_]
  (jar nil)
  (println "Trying to release artifact...")
  (let [ret (retry-with-fib-backoff 10 try-release :failure?)]
    (if (:failure? ret)
      (do (println "GitHub release failed!")
          (System/exit 1))
      (println (:url ret)))))

(defn install
  "Install the jar into the local Maven repo so other local projects
   can pull `org.replikativ/muschel` by version."
  [_]
  (clean nil)
  (jar nil)
  (b/install {:basis (b/create-basis {})
              :lib lib
              :version version
              :jar-file jar-file
              :class-dir class-dir}))

(defn npm-version
  "Stamp the current git-derived version into the root package.json."
  [_]
  (let [path "package.json"
        pkg (slurp path)
        updated (str/replace pkg
                             #"\"version\"\s*:\s*\"[^\"]+\""
                             (str "\"version\": \"" version "\""))]
    (spit path updated)
    (println (str path " version set to " version))))

(defn build-npm
  "Build the shadow-cljs :npm target into dist/muschel.js."
  [_]
  (println "Building :npm via shadow-cljs...")
  (let [ret (b/process {:command-args ["npx" "shadow-cljs" "release" "npm"]})]
    (when (not= 0 (:exit ret))
      (throw (ex-info "shadow-cljs build failed" {:exit (:exit ret)})))))

(defn build-playground
  "Build the shadow-cljs :playground target into dist/playground/.
   Clears any prior dev-build artifacts (cljs-runtime/, manifest.edn)
   so what ships to npm is just the minified bundle."
  [_]
  (println "Building :playground via shadow-cljs...")
  (b/delete {:path "dist/playground"})
  (let [ret (b/process {:command-args ["npx" "shadow-cljs" "release" "playground"]})]
    (when (not= 0 (:exit ret))
      (throw (ex-info "shadow-cljs playground build failed" {:exit (:exit ret)}))))
  (b/delete {:path "dist/playground/cljs-runtime"})
  (b/delete {:path "dist/playground/manifest.edn"}))

(defn npm-publish
  "Bump version, build the npm bundle + browser playground, then
   `npm publish` from the repo root. Requires npx + npm on PATH and
   an authenticated npm session (with --otp if 2FA is enabled)."
  [_]
  (npm-version nil)
  (build-npm nil)
  (build-playground nil)
  (println "Publishing to npm...")
  (let [ret (b/process {:command-args ["npm" "publish" "--access" "public"]})]
    (when (not= 0 (:exit ret))
      (throw (ex-info "npm publish failed" {:exit (:exit ret)}))))
  (println (str "Published muschel " version " to npm.")))
