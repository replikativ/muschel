(ns muschel.fs.traced
  "Optional FS decorator that records every protocol op into a
   muschel.trace state. Wrapped around the inner FS by
   `muschel.exec/run-and-capture` when `:trace` is opted in. The
   inner FS sees no change — all containment logic stays where it
   was."
  (:require [muschel.fs :as fs]
            [muschel.trace :as trace]))

(defn- record!
  ([state op path]      (record! state op path true))
  ([state op path ok?]  (trace/record-fs! state {:type :fs :op op :path path :ok? (boolean ok?)})))

(defrecord TracedFS [inner state]
  fs/FS
  (-resolve [_ path]
    (let [r (fs/-resolve inner path)]
      (record! state :resolve path (some? r))
      r))

  (-cwd [_] (fs/-cwd inner))

  (-cd! [_ path]
    (let [r (fs/-cd! inner path)]
      (record! state :cd path (some? r))
      r))

  (-exists? [_ path]
    (let [r (fs/-exists? inner path)]
      (record! state :exists? path (boolean r))
      r))

  (-stat [_ path]
    (let [r (fs/-stat inner path)]
      (record! state :stat path (some? r))
      r))

  (-list-dir [_ path]
    (let [r (fs/-list-dir inner path)]
      (record! state :list-dir path (some? r))
      r))

  (-read-file [_ path]
    (let [r (fs/-read-file inner path)]
      (record! state :read-file path (some? r))
      r))

  (-read-bytes [_ path]
    (let [r (fs/-read-bytes inner path)]
      (record! state :read-bytes path (some? r))
      r))

  (-open-source [_ path]
    (let [r (fs/-open-source inner path)]
      (record! state :open-source path (some? r))
      r))

  (-open-sink [_ path append?]
    (let [r (fs/-open-sink inner path append?)]
      (record! state :open-sink path (some? r))
      r))

  (-mkdir [_ path]
    (let [r (fs/-mkdir inner path)]
      (record! state :mkdir path (boolean r))
      r))

  (-delete [_ path]
    (let [r (fs/-delete inner path)]
      (record! state :delete path (boolean r))
      r))

  (-rename [_ from to]
    (let [r (fs/-rename inner from to)]
      (record! state :rename (str from "→" to) (boolean r))
      r))

  (-touch [_ path]
    (let [r (fs/-touch inner path)]
      (record! state :touch path (boolean r))
      r))

  (-chmod [_ path mode]
    (let [r (fs/-chmod inner path mode)]
      (record! state :chmod path (boolean r))
      r))

  (-symlink [_ target link-path]
    (let [r (fs/-symlink inner target link-path)]
      (record! state :symlink link-path (boolean r))
      r))

  (-chown [_ path owner group]
    (let [r (fs/-chown inner path owner group)]
      (record! state :chown path (boolean r))
      r))

  (-sandbox-relativize [_ p]
    (fs/-sandbox-relativize inner p)))

(defn wrap
  "Wrap `inner-fs` so every protocol call records an event into
   `state` (a muschel.trace state)."
  [inner state]
  (->TracedFS inner state))
