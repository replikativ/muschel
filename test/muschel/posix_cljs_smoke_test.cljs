(ns muschel.posix-cljs-smoke-test
  "Force the CLJS build to compile muschel.builtins.posix. Once the
   port is complete, the build succeeds and a handful of common
   builtins are smoke-tested against a virtual host on Node."
  (:require [clojure.test :refer [deftest is]]
            [muschel.builtins.posix :as posix]))

(deftest posix-namespace-loads
  (is (some? posix/standard) "posix/standard map exists")
  (is (some? posix/standard-read-only) "posix/standard-read-only exists"))
