(ns muschel.fs-versioned-test
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.builtins.posix :as posix]
            [muschel.core :as muschel]
            [muschel.fs :as fs]
            [muschel.fs.versioned :as versioned-fs]
            [muschel.fs.virtual :as virtual-fs]
            [muschel.host.builtin :as builtin-host]
            [muschel.test-helpers :as test-helpers]
            [muschel.version-store :as version-store]))

(defn- fixture []
  (let [store (version-store/make-store {:representation :line-delta})
        inner (virtual-fs/make {"/work" :dir} {:cwd "/work"})
        filesystem (versioned-fs/make inner store)]
    {:store store :inner inner :fs filesystem}))

(deftest map-sink-commits-exactly-once-at-close
  (let [{:keys [store fs]} (fixture)
        sink (fs/open-sink fs "/work/doc" false)]
    (swap! (:acc sink) str "one")
    (swap! (:acc sink) str " two")
    (is (nil? (version-store/head store "/work/doc")))
    (is (fs/commit-sink! sink))
    (is (= "one two" (fs/read-file fs "/work/doc")))
    (is (= 1 (count (version-store/versions store))))
    (is (nil? (fs/commit-sink! sink)))
    (is (= 1 (count (version-store/versions store))))))

(deftest portable-write-helper-commits-a-version
  (let [{:keys [store fs]} (fixture)]
    (is (fs/write-string! fs "/work/doc" "first\n" false))
    (is (fs/write-string! fs "/work/doc" "second\n" false))
    (is (= "second\n" (version-store/read-head store "/work/doc")))
    (is (= 2 (count (version-store/versions store))))))

(deftest shell-redirection-is-one-logical-version
  (let [{:keys [store fs]} (fixture)
        host (builtin-host/make {:fs fs
                                 :fallback-host (test-helpers/fallback-host)
                                 :builtins posix/standard
                                 :fallback-allowlist #{}})
        run #(muschel/run-and-capture (muschel/new-env) % {:host host})]
    (is (= 0 (:exit (run "echo first > /work/doc"))))
    (is (= 1 (count (version-store/versions store))))
    (is (= 0 (:exit (run "echo second >> /work/doc"))))
    (is (= "first\nsecond\n" (fs/read-file fs "/work/doc")))
    (is (= 2 (count (version-store/versions store))))))

(deftest rename-and-delete-move-version-heads
  (let [{:keys [store fs]} (fixture)]
    (fs/write-string! fs "/work/old" "history" false)
    (is (fs/rename fs "/work/old" "/work/new"))
    (is (nil? (version-store/head store "/work/old")))
    (is (= "history" (version-store/read-head store "/work/new")))
    (is (fs/delete fs "/work/new"))
    (is (nil? (version-store/head store "/work/new")))
    (testing "unreferenced history remains until explicit GC"
      (is (= 1 (count (version-store/versions store))))
      (is (= 1 (:deleted-versions (version-store/gc! store)))))))
