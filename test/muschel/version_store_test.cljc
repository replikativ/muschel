(ns muschel.version-store-test
  (:require [clojure.test :refer [deftest is testing]]
            [muschel.version-store :as version-store]))

(defn- lines-text [n]
  (apply str (map #(str "line-" % " some stable text\n") (range n))))

(deftest portable-sha256
  (is (= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
         (version-store/sha256 "abc"))))

(deftest full-payloads-content-deduplicate
  (let [store (version-store/make-store {:representation :full})
        v1 (version-store/commit-text! store "/a" "same")
        v2 (version-store/commit-text! store "/b" "same")]
    (is (not= (:version/id v1) (:version/id v2)))
    (is (= (:version/content-id v1) (:version/content-id v2)))
    (is (= 1 (:payloads (version-store/storage-stats store))))
    (is (= "same" (version-store/read-head store "/a")))))

(deftest line-delta-roundtrip-and-bounded-depth
  (let [store (version-store/make-store {:representation :line-delta
                                         :max-delta-depth 2})
        base (lines-text 100)
        changed-1 (str base "one\n")
        changed-2 (str changed-1 "two\n")
        changed-3 (str changed-2 "three\n")
        versions (mapv #(version-store/commit-text! store "/doc" %)
                       [base changed-1 changed-2 changed-3])]
    (is (= [0 1 2 0] (mapv :version/delta-depth versions)))
    (is (= [:full :line-delta :line-delta :full]
           (mapv #(get-in % [:version/representation :kind]) versions)))
    (is (= changed-3 (version-store/read-head store "/doc")))
    (doseq [[v expected] (map vector versions [base changed-1 changed-2 changed-3])]
      (is (= expected (version-store/read-version store (:version/id v)))))))

(deftest line-delta-preserves-newline-only-change
  (let [store (version-store/make-store {:representation :line-delta})
        v1 (version-store/commit-text! store "/x" "last")
        v2 (version-store/commit-text! store "/x" "last\n")]
    (is (= :line-delta (get-in v2 [:version/representation :kind])))
    (is (= "last" (version-store/read-version store (:version/id v1))))
    (is (= "last\n" (version-store/read-version store (:version/id v2))))))

(deftest copy-insert-delta-roundtrip
  (let [store (version-store/make-store {:representation :copy-insert})
        base (lines-text 100)
        changed (str "new header\n" base "new footer\n")
        v1 (version-store/commit-text! store "/x" base)
        v2 (version-store/commit-text! store "/x" changed)]
    (is (= :full (get-in v1 [:version/representation :kind])))
    (is (= :copy-insert (get-in v2 [:version/representation :kind])))
    (is (= changed (version-store/read-version store (:version/id v2))))
    (is (< (:stored-bytes (version-store/storage-stats store))
           (:logical-bytes (version-store/storage-stats store))))))

(deftest content-defined-chunks-reuse-unchanged-regions
  (let [store (version-store/make-store {:representation :chunks})
        base (lines-text 300)
        changed (str (lines-text 151) "inserted\n" (apply str (map #(str "line-" % " some stable text\n")
                                                                   (range 151 300))))
        v1 (version-store/commit-text! store "/doc" base)
        v2 (version-store/commit-text! store "/doc" changed)
        payload-store (:payload-store store)
        manifest-1 (version-store/-payload-get
                    payload-store (get-in v1 [:version/representation :payload-id]))
        manifest-2 (version-store/-payload-get
                    payload-store (get-in v2 [:version/representation :payload-id]))
        shared (set (filter (set (:chunks manifest-1)) (:chunks manifest-2)))]
    (is (= base (version-store/read-version store (:version/id v1))))
    (is (= changed (version-store/read-version store (:version/id v2))))
    (is (seq shared))
    (is (< (:stored-bytes (version-store/storage-stats store))
           (:logical-bytes (version-store/storage-stats store))))))

(deftest heads-moves-and-garbage-collection
  (let [store (version-store/make-store {:representation :line-delta})]
    (version-store/commit-text! store "/old" (lines-text 20))
    (version-store/commit-text! store "/old" (str (lines-text 20) "new\n"))
    (is (version-store/move-head! store "/old" "/new"))
    (is (nil? (version-store/head store "/old")))
    (is (string? (version-store/read-head store "/new")))
    (testing "the parent chain is retained from a live head"
      (is (= 2 (count (version-store/reachable-version-ids store))))
      (is (= {:deleted-versions 0 :deleted-payloads 0}
             (version-store/gc! store))))
    (version-store/remove-head! store "/new")
    (let [result (version-store/gc! store)]
      (is (= 2 (:deleted-versions result)))
      (is (zero? (:versions (version-store/storage-stats store))))
      (is (zero? (:payloads (version-store/storage-stats store)))))))
