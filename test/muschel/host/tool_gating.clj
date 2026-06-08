(ns muschel.host.tool-gating
  "Shared helpers for integration tests that depend on external OS
   sandbox tools (bubblewrap, gVisor's runsc, future runtimes).

   Each `tool-available?` probe runs the tool with a cheap `--version`
   (or equivalent) and caches the result. `deftest-when-tool` skips
   the body — printing a SKIP line — when the probe fails, so CI
   environments without the tool stay green.

   Adding a new backend (e.g. a Firecracker sandbox): add a probe
   here and a `deftest-when-firecracker` macro. The integration
   tests themselves stay separate per backend (one ns per tool)
   and just use the macro.")

(defn- exits-zero?
  "Run `argv` (vector of strings) and return true iff exit code 0."
  [argv]
  (try
    (let [p (.exec (Runtime/getRuntime)
                   ^"[Ljava.lang.String;" (into-array String argv))]
      (.waitFor p)
      (zero? (.exitValue p)))
    (catch Throwable _ false)))

(def ^:private cache (atom {}))

(defn tool-available?
  "Cached probe: does running `argv` exit 0? Cached on the argv vec."
  [argv]
  (if-let [hit (find @cache argv)]
    (val hit)
    (let [r (exits-zero? argv)]
      (swap! cache assoc argv r)
      r)))

(defn bwrap-available? [] (tool-available? ["bwrap" "--version"]))
(defn runsc-available? [] (tool-available? ["runsc" "--version"]))

(defmacro deftest-when-tool
  "Like deftest, but the body short-circuits with a SKIP message when
   `(tool-available?-fn)` returns falsy. `tool-name` is a string used
   in the SKIP message."
  [test-name tool-name tool-available?-fn & body]
  `(clojure.test/deftest ~test-name
     (if (~tool-available?-fn)
       (do ~@body)
       (println (str "SKIP " '~test-name " — " ~tool-name " not available")))))

(defmacro deftest-bwrap
  "Skip the test body when bwrap isn't on PATH."
  [test-name & body]
  `(deftest-when-tool ~test-name "bwrap" bwrap-available? ~@body))

(defmacro deftest-runsc
  "Skip the test body when runsc (gVisor) isn't on PATH. For
   forthcoming gVisor-backed sandbox tests."
  [test-name & body]
  `(deftest-when-tool ~test-name "runsc" runsc-available? ~@body))
