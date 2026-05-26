(ns muschel.host.jvm
  "JVM impl of `muschel.host/Host`. Uses `babashka.process` for spawn,
   `java.io.*` for streams + files, `clojure.core/future` for async.

   This is the host you get from `(muschel.host.jvm/make)` on JVM
   and babashka. Tests verify exec's behavior here.

   Sinks/sources here are bare `java.io.OutputStream`/`InputStream`.
   `-string-sink` returns a `ByteArrayOutputStream`; `-sink->string`
   calls `.toString` on it."
  (:require [babashka.process :as bp]
            [clojure.java.io :as io]
            [muschel.host :as host]))

(deftype JvmHost []
  host/Host
  (-write-string! [_ sink s]
    (when sink
      (let [^java.io.OutputStream out sink
            ^bytes bs (.getBytes ^String s "UTF-8")]
        (.write out bs 0 (count bs))
        (.flush out))))

  (-read-all-string [_ source]
    ;; Do NOT `with-open` close the source here. When `source` is the
    ;; read-end of a muschel pipe, babashka.process has a background
    ;; copy thread that also reads it; closing under their feet emits
    ;; \"ERROR while copying :in option:  Stream closed\" stderr noise.
    ;; The caller (typically a builtin) owns the lifetime — they will
    ;; close once they're done extracting whatever bytes they need.
    (slurp source))

  (-close! [_ io]
    (when (instance? java.io.Closeable io)
      (try (.close ^java.io.Closeable io) (catch Throwable _ nil))))

  (-string-sink [_]
    (java.io.ByteArrayOutputStream.))

  (-sink->string [_ sink]
    (.toString ^java.io.ByteArrayOutputStream sink "UTF-8"))

  (-string-source [_ s]
    (java.io.ByteArrayInputStream. (.getBytes ^String s "UTF-8")))

  (-open-file-sink [_ path append?]
    (io/output-stream (io/file path) :append append?))

  (-open-file-source [_ path]
    (io/input-stream (io/file path)))

  (-file-info [_ path]
    (let [f (java.io.File. ^String path)]
      {:exists?     (.exists f)
       :file?       (.isFile f)
       :dir?        (.isDirectory f)
       :readable?   (.canRead f)
       :writable?   (.canWrite f)
       :executable? (.canExecute f)
       :symlink?    (try (java.nio.file.Files/isSymbolicLink (.toPath f))
                         (catch Throwable _ false))
       :size        (when (.exists f) (.length f))
       :mtime-ms    (when (.exists f) (.lastModified f))}))

  (-read-file [_ path]
    (slurp (io/file path)))

  (-make-pipe [_]
    (let [out (java.io.PipedOutputStream.)
          in  (java.io.PipedInputStream. out)]
      [in out]))

  (-spawn [_ {:keys [cmd args dir extra-env in out err]}]
    (let [proc-opts (cond-> {:dir dir
                             :extra-env extra-env}
                      in  (assoc :in  in)
                      out (assoc :out out)
                      err (assoc :err err))
          proc (bp/process (into [cmd] args) proc-opts)]
      {:handle proc
       :wait (fn []
               (try (let [done @proc] (or (:exit done) 0))
                    (catch Throwable _ 1)))}))

  (-async [_ thunk]
    (future (thunk)))

  (-await [_ h]
    (deref h)))

(defn make
  "Construct a JVM host."
  []
  (->JvmHost))
