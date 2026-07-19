(ns muschel.fs.mount
  "Composing FS: a base filesystem plus child filesystems mounted at
   absolute sandbox paths — the union-mount muschel was missing.

     (mount/make base-fs {\"/drive\" drive-fs
                          \"/mnt/shared\" other-fs})

   Semantics (deliberately POSIX-mount-like):

   - Path routing: a canonicalised sandbox path is served by the FS
     with the LONGEST matching mount prefix; everything else falls
     through to the base. A mount shadows whatever the base has at
     and below that path.
   - Rebasing: children are self-rooted (their `/` is the mount
     point). `/drive/a/b` reaches the child as `/a/b`. Children are
     always called with absolute rebased paths, so their own cwd
     state is never consulted.
   - cwd lives at the mount layer (one shell, one cwd — even when it
     points inside a child).
   - Mount points and their ancestors always exist as directories:
     `-stat`/`-exists?` synthesize them when the base lacks them, and
     `-list-dir` unions mount-point names into parent listings.
   - Mount points are not writable as entries: `-delete`/`-rename`
     OF a mount point (or an ancestor of one) returns nil, as does
     `-rename` ACROSS filesystems (callers see plain failure — same
     contract as every other refused op).

   Containment composes: the mount layer canonicalises lexically
   (cwd + `..` collapse) before routing, and the routed child still
   runs its own `-resolve` on the rebased path — a symlink escaping a
   child is caught by that child exactly as if it were the only FS."
  (:refer-clojure :exclude [resolve])
  (:require [muschel.fs :as fs]
            [clojure.string :as str]))

(defn- canonicalize
  "Resolve `path` against `cwd`, collapse `.`/`..`. Nil on escape past /."
  [cwd path]
  (when (string? path)
    (let [combined (if (str/starts-with? path "/")
                     path
                     (str cwd "/" path))
          normalized (fs/normalize-segments (fs/split-path combined))]
      (when normalized
        (let [p (fs/join-path "" normalized)]
          (if (= "" p) "/" p))))))

(defn- rebase
  "Sandbox path `p` under mount point `m` → the child's own path."
  [p m]
  (let [tail (subs p (count m))]
    (if (str/blank? tail) "/" tail)))

(defn- unbase
  "Child path back into sandbox space under mount `m`."
  [m child-path]
  (if (or (nil? child-path) (= "/" child-path))
    m
    (str m child-path)))

(defn- under?
  "Is canonical path `p` at or below prefix `m`?"
  [p m]
  (or (= p m) (str/starts-with? p (str m "/"))))

(defn- mount-table
  "Return a stable routing snapshot. Older callers may still construct a
   MountFS with a plain map; `make` uses an atom so mounts can be installed at
   shell command boundaries."
  [mounts]
  (if #?(:clj  (instance? clojure.lang.IDeref mounts)
         :cljs (satisfies? IDeref mounts))
    @mounts
    mounts))

(defn- mount-point? [mounts p]
  (contains? (mount-table mounts) p))

(defn- route
  "Canonical sandbox path → [fs child-path mount-point|nil]."
  [base mounts p]
  (let [mounts (mount-table mounts)]
    (if-let [m (->> (keys mounts)
                    (filter #(under? p %))
                    (sort-by count >)
                    first)]
      [(get mounts m) (rebase p m) m]
      [base p nil])))

(defn- ancestor-of-mount?
  "Is `p` a strict ancestor of some mount point?"
  [mounts p]
  (boolean (some (fn [m] (and (not= m p) (under? m p)))
                 (keys (mount-table mounts)))))

(defn- mount-children
  "Names of mount points (or their next path segment) directly under
   dir `p` — the synthetic entries unioned into listings."
  [mounts p]
  (let [prefix (if (= "/" p) "/" (str p "/"))]
    (->> (keys (mount-table mounts))
         (keep (fn [m]
                 (when (and (str/starts-with? m prefix)
                            (> (count m) (count prefix)))
                   (first (str/split (subs m (count prefix)) #"/")))))
         set)))

(defn- synth-dir-stat []
  {:type :dir :size 0 :mtime-ms 0 :perms nil})

(defrecord MountFS [base mounts cwd-atom]
  fs/FS
  (-resolve [_ path]
    (when-let [p (canonicalize @cwd-atom path)]
      (let [[f cp m] (route base mounts p)]
        (cond
          ;; mount points/ancestors exist by construction
          (or (mount-point? mounts p) (ancestor-of-mount? mounts p)) p
          ;; child (or base) must accept the rebased path
          (some? (fs/-resolve f cp)) p
          ;; non-existent paths still resolve lexically (needed for
          ;; write targets) as long as the owning FS resolves them
          :else nil))))

  (-cwd [_] @cwd-atom)

  (-cd! [this path]
    (when-let [p (fs/-resolve this path)]
      (let [st (fs/-stat this p)]
        (when (= :dir (:type st))
          (reset! cwd-atom p)
          p))))

  (-exists? [this path]
    (when-let [p (canonicalize @cwd-atom path)]
      (or (mount-point? mounts p)
          (ancestor-of-mount? mounts p)
          (let [[f cp _] (route base mounts p)]
            (boolean (fs/-exists? f cp))))))

  (-stat [this path]
    (when-let [p (canonicalize @cwd-atom path)]
      (let [[f cp m] (route base mounts p)]
        (or (fs/-stat f cp)
            (when (or (mount-point? mounts p)
                      (ancestor-of-mount? mounts p))
              (synth-dir-stat))))))

  (-list-dir [this path]
    (when-let [p (canonicalize @cwd-atom path)]
      (let [[f cp _] (route base mounts p)
            own (fs/-list-dir f cp)
            synth (mount-children mounts p)
            ;; a dir "exists" for listing if the owning FS lists it OR
            ;; it's an ancestor/point of a mount (synth dirs)
            listable? (or (some? own)
                          (mount-point? mounts p)
                          (ancestor-of-mount? mounts p))]
        (when listable?
          (let [own (or own [])
                names (into #{} (map :name) own)
                extra (->> synth
                           (remove names)
                           (map (fn [n] {:name n :type :dir :size 0 :mtime-ms 0})))]
            (->> (concat own extra)
                 (sort-by :name)
                 vec))))))

  (-read-file [this path]
    (when-let [p (canonicalize @cwd-atom path)]
      (let [[f cp _] (route base mounts p)]
        (fs/-read-file f cp))))

  (-read-bytes [this path]
    (when-let [p (canonicalize @cwd-atom path)]
      (let [[f cp _] (route base mounts p)]
        (fs/-read-bytes f cp))))

  (-open-source [this path]
    (when-let [p (canonicalize @cwd-atom path)]
      (let [[f cp _] (route base mounts p)]
        (fs/-open-source f cp))))

  (-open-sink [this path append?]
    (when-let [p (canonicalize @cwd-atom path)]
      ;; refuse writing the mount point itself
      (when-not (or (mount-point? mounts p) (ancestor-of-mount? mounts p))
        (let [[f cp _] (route base mounts p)]
          (fs/-open-sink f cp append?)))))

  (-mkdir [this path]
    (when-let [p (canonicalize @cwd-atom path)]
      (if (or (mount-point? mounts p) (ancestor-of-mount? mounts p))
        nil ;; already exists by construction — POSIX mkdir on existing → fail
        (let [[f cp m] (route base mounts p)]
          ;; inside a child whose parent is the mount root: child sees
          ;; parent "/" which always exists — plain delegation works
          (fs/-mkdir f cp)))))

  (-delete [this path]
    (when-let [p (canonicalize @cwd-atom path)]
      (when-not (or (mount-point? mounts p) (ancestor-of-mount? mounts p))
        (let [[f cp _] (route base mounts p)]
          (fs/-delete f cp)))))

  (-rename [this from to]
    (let [pf (canonicalize @cwd-atom from)
          pt (canonicalize @cwd-atom to)]
      (when (and pf pt
                 (not (mount-point? mounts pf))
                 (not (ancestor-of-mount? mounts pf))
                 (not (mount-point? mounts pt))
                 (not (ancestor-of-mount? mounts pt)))
        (let [[ff cpf mf] (route base mounts pf)
              [ft cpt mt] (route base mounts pt)]
          ;; cross-FS rename refused (callers copy+delete)
          (when (= mf mt)
            (fs/-rename ff cpf cpt))))))

  (-touch [this path]
    (when-let [p (canonicalize @cwd-atom path)]
      (when-not (or (mount-point? mounts p) (ancestor-of-mount? mounts p))
        (let [[f cp _] (route base mounts p)]
          (fs/-touch f cp)))))

  (-chmod [this path mode]
    (when-let [p (canonicalize @cwd-atom path)]
      (let [[f cp _] (route base mounts p)]
        (fs/-chmod f cp mode))))

  (-symlink [this target link-path]
    (when-let [p (canonicalize @cwd-atom link-path)]
      (when-not (or (mount-point? mounts p) (ancestor-of-mount? mounts p))
        (let [[f cp _] (route base mounts p)]
          (fs/-symlink f target cp)))))

  (-chown [this path owner group]
    (when-let [p (canonicalize @cwd-atom path)]
      (let [[f cp _] (route base mounts p)]
        (fs/-chown f cp owner group))))

  (-sandbox-relativize [_ path] path)

  (-physical-path [this sandbox-path]
    (when-let [p (canonicalize @cwd-atom sandbox-path)]
      (let [[f cp _] (route base mounts p)]
        (fs/-physical-path f cp)))))

(defn make
  "Compose `base-fs` with `mounts` — a map of absolute sandbox path →
   child FS. Mount paths must be absolute, normalized, and non-root.
   cwd starts at \"/\"."
  ([base-fs mounts] (make base-fs mounts {}))
  ([base-fs mounts {:keys [cwd] :or {cwd "/"}}]
   {:pre [(every? #(and (string? %)
                        (str/starts-with? % "/")
                        (not= "/" %)
                        (not (str/ends-with? % "/")))
                  (keys mounts))]}
   (->MountFS base-fs (atom (into {} mounts)) (atom cwd))))

(defn mount-points
  "Current absolute mount points, in deterministic order."
  [mount-fs]
  (->> (mount-table (:mounts mount-fs)) keys sort vec))

(defn mounted-at
  "Return the child filesystem mounted exactly at `path`, if any."
  [mount-fs path]
  (when-let [path (canonicalize "/" path)]
    (get (mount-table (:mounts mount-fs)) path)))

(defn owning-mount
  "Return `[mount-point child-fs]` for the longest mount owning `path`."
  [mount-fs path]
  (when-let [path (canonicalize (fs/cwd mount-fs) path)]
    (let [[child _ mount-point] (route (:base mount-fs)
                                       (:mounts mount-fs) path)]
      (when mount-point [mount-point child]))))

(defn mount!
  "Atomically install `child-fs` at an absolute, normalized non-root path.
   Refuses replacement and parent/child overlap by default; pass
   `{:allow-nested? true}` for generic POSIX-style nested mounts."
  ([mount-fs path child-fs] (mount! mount-fs path child-fs nil))
  ([mount-fs path child-fs {:keys [allow-nested?]}]
   (let [path (canonicalize "/" path)]
     (when (or (nil? path) (= "/" path))
       (throw (ex-info "Invalid mount point" {:path path})))
     (swap! (:mounts mount-fs)
            (fn [current]
              (when (contains? current path)
                (throw (ex-info "Mount point already occupied" {:path path})))
              (when (and (not allow-nested?)
                         (some #(or (under? path %) (under? % path))
                               (keys current)))
                (throw (ex-info "Nested mounts are disabled"
                                {:path path :mounts (sort (keys current))})))
              (assoc current path child-fs)))
     child-fs)))

(defn unmount!
  "Atomically remove and return the child mounted exactly at `path`."
  [mount-fs path]
  (let [path (canonicalize "/" path)
        removed (volatile! nil)]
    (swap! (:mounts mount-fs)
           (fn [current]
             (vreset! removed (get current path))
             (dissoc current path)))
    @removed))
