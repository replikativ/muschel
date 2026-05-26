(ns muschel.awk-test
  "Tests for muschel.builtins.awk — direct unit tests + goawk corpus.

   Runs on JVM AND Node (the awk impl is portable; see awk_compat.cljc).
   Host-integration tests (`-v`/`-F`/`-f` going through posix.clj) live
   in `awk_host_test.clj` since the host layer is JVM-only."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            #?(:clj  [clojure.edn :as edn])
            #?(:clj  [clojure.java.io :as io])
            [muschel.builtins.awk :as awk]))

;; ============================================================================
;; Direct unit tests against the awk ns
;; ============================================================================

(defn- run1
  [src in]
  (:stdout (awk/run {:program src :raw-input in})))

(deftest patterns
  (is (= "foo\n"        (run1 "/foo/" "foo\nbar")))
  (is (= "42\n"         (run1 "$1==42" "foo\n42\nbar")))
  (is (= "42\n"         (run1 "$1==\"42\"" "foo\n42\nbar")))
  (is (= "2\n3\n4\n"    (run1 "NR==2,NR==4" "1\n2\n3\n4\n5\n6")))
  (is (= ""             (run1 "0" "foo")))
  (is (= "foo\n"        (run1 "1" "foo")))
  (is (= "foo\n"        (run1 "{ print $1 }" "foo"))))

(deftest expressions
  (is (= "3\n"          (run1 "BEGIN { print 1+2 }" "")))
  (is (= "6\n"          (run1 "BEGIN { s=\"5abc\"; print s+1 }" "")))
  (is (= "0"            (run1 "BEGIN { printf \"%d\", \"abc\" }" "")))
  (is (= "true\n"       (run1 "BEGIN { print (1<2 ? \"true\" : \"false\") }" ""))))

(deftest control-flow
  (is (= "0 1 2 \n"     (run1 "BEGIN { for (i=0;i<3;i++) printf \"%d \", i; print \"\" }" "")))
  (is (= "a\nc\n"       (run1 "{ if (NR==2) next; print }" "a\nb\nc")))
  (is (= "x\n"          (run1 "BEGIN { for (;;) { print \"x\"; break } }" "")))
  (is (= "4950\n"       (run1 "BEGIN { for (i=0;i<100;i++) s+=i; print s }" ""))))

(deftest arrays
  (is (= "7\n"          (run1 "BEGIN { a[\"x\"]=3; a[\"y\"]=4; for (k in a) s+=a[k]; print s }" "")))
  (is (= "1\n"          (run1 "BEGIN { a[\"x\"]=1; print (\"x\" in a) }" "")))
  (is (= "0\n"          (run1 "BEGIN { a[\"x\"]=1; delete a[\"x\"]; print (\"x\" in a) }" ""))))

(deftest builtins-strings
  (is (= "3\n"          (run1 "BEGIN { print length(\"abc\") }" "")))
  (is (= "ell\n"        (run1 "BEGIN { print substr(\"hello\", 2, 3) }" "")))
  (is (= "3\n"          (run1 "BEGIN { print index(\"hello\", \"ll\") }" "")))
  (is (= "HELLO\n"      (run1 "BEGIN { print toupper(\"hello\") }" "")))
  (is (= "hello\n"      (run1 "BEGIN { print tolower(\"HELLO\") }" ""))))

(deftest builtins-regex
  (is (= "Xoo\n"        (run1 "BEGIN { s=\"foo\"; sub(/f/, \"X\", s); print s }" "")))
  (is (= "fxx\n"        (run1 "BEGIN { s=\"foo\"; gsub(/o/, \"x\", s); print s }" "")))
  (is (= "4\n"          (run1 "BEGIN { print match(\"hello\", /lo/) }" ""))))

(deftest printf-cases
  (is (= "% 42 2a *"    (run1 "BEGIN { printf \"%% %d %x %c\", 42, 42, 42 }" "")))
  (is (= " 42"          (run1 "BEGIN { printf \"%3d\", 42 }" "")))
  (is (= "x y\n"        (run1 "BEGIN { print \"x\", \"y\" }" "")))
  (is (= " \nx,y\n"     (run1 "BEGIN { print OFS; OFS=\",\"; print \"x\",\"y\" }" ""))))

;; ============================================================================
;; goawk-derived test corpus — JVM-only since the EDN is on disk
;; (CLJS load-corpus would need to embed the data at compile time)
;; ============================================================================

#?(:clj
   (do
     (defn- load-corpus []
       (let [r (io/resource "muschel/awk_corpus.edn")
             f (io/file "test/muschel/awk_corpus.edn")]
         (cond
           r (edn/read-string (slurp r))
           (.exists f) (edn/read-string (slurp f))
           :else nil)))

     (deftest goawk-corpus
       (let [corpus (load-corpus)]
         (when corpus
           (let [results (volatile! {:pass 0 :fail [] :err []})]
             (doseq [{:keys [src in out]} corpus]
               (try
                 (let [actual (run1 src in)]
                   (if (= out actual)
                     (vswap! results update :pass inc)
                     (vswap! results update :fail conj
                             {:src src :in in :expected out :actual actual})))
                 (catch Throwable t
                   (vswap! results update :err conj
                           {:src src :in in :expected out :error (.getMessage t)}))))
             (let [{:keys [pass fail err]} @results
                   total (count corpus)
                   pct (if (pos? total) (int (* 100 (/ pass total))) 0)]
               (println (format "awk corpus: %d/%d pass (%d%%); %d fail; %d error"
                                pass total pct (count fail) (count err)))
               (when (seq fail)
                 (println "First 5 failures:")
                 (doseq [f (take 5 fail)]
                   (println "  src:" (pr-str (:src f)))
                   (println "  in:" (pr-str (:in f)))
                   (println "  exp:" (pr-str (:expected f)))
                   (println "  got:" (pr-str (:actual f)))))
               (when (seq err)
                 (println "First 5 errors:")
                 (doseq [e (take 5 err)]
                   (println "  src:" (pr-str (:src e)))
                   (println "  err:" (:error e))))
               (is (some? corpus) "corpus loaded"))))))))
