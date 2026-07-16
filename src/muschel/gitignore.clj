(ns muschel.gitignore
  "Gitignore interpretation for Geschichte worktrees.

   Rules are loaded from every tracked or untracked `.gitignore` currently in
   the worktree. Later rules override earlier rules, including `!` negation.
   The matcher is path based and does not load blob payloads except for the
   ignore files themselves."
  (:require [clojure.string :as str]
            [geschichte.bytes :as bytes]
            [geschichte.repo :as repo]))

(defn- parent [path]
  (let [i (.lastIndexOf ^String path "/")]
    (if (neg? i) "" (subs path 0 i))))

(defn- unescaped-trailing-spaces [line]
  ;; Git discards unescaped trailing spaces. A backslash immediately before a
  ;; trailing space quotes it and is itself removed.
  (loop [line line]
    (cond
      (not (str/ends-with? line " ")) line
      (str/ends-with? line "\\ ") (str (subs line 0 (- (count line) 2)) " ")
      :else (recur (subs line 0 (dec (count line)))))))

(defn- regex-quote [character]
  (if (contains? #{\. \+ \( \) \^ \$ \| \{ \} \[ \] \\} character)
    (str "\\" character)
    (str character)))

(defn- glob-regex [glob]
  (loop [characters (seq glob), result ""]
    (if-let [character (first characters)]
      (cond
        (= character \*)
        (if (= \* (second characters))
          (if (= \/ (nth characters 2 nil))
            (recur (drop 3 characters) (str result "(?:.*/)?"))
            (recur (drop 2 characters) (str result ".*")))
          (recur (next characters) (str result "[^/]*")))

        (= character \?)
        (recur (next characters) (str result "[^/]"))

        (= character \\)
        (if-let [quoted (second characters)]
          (recur (nnext characters) (str result (regex-quote quoted)))
          (recur (next characters) (str result "\\\\")))

        :else
        (recur (next characters) (str result (regex-quote character))))
      result)))

(defn- parse-line [base order raw-line]
  (let [line (-> raw-line (str/replace #"\r$" "") unescaped-trailing-spaces)]
    (when-not (or (str/blank? line)
                  (and (str/starts-with? line "#")
                       (not (str/starts-with? line "\\#"))))
      (let [escaped-prefix? (or (str/starts-with? line "\\#")
                                (str/starts-with? line "\\!"))
            negated? (and (not escaped-prefix?) (str/starts-with? line "!"))
            pattern (cond-> line
                      negated? (subs 1)
                      escaped-prefix? (subs 1))
            directory? (str/ends-with? pattern "/")
            pattern (str/replace pattern #"/+$" "")
            anchored? (str/starts-with? pattern "/")
            pattern (str/replace pattern #"^/+" "")
            slash? (str/includes? pattern "/")
            prefix (if (str/blank? base) "" (str (glob-regex base) "/"))
            body (glob-regex pattern)
            expression
            (cond
              (or anchored? slash?)
              (str "^" prefix body "(?:/.*)?$")

              :else
              (str "^" prefix "(?:.*/)?" body "(?:/.*)?$"))]
        {:base base
         :order order
         :pattern raw-line
         :negated? negated?
         :directory? directory?
         :regex (re-pattern expression)}))))

(defn rules
  "Load ordered ignore rules from all worktree `.gitignore` files. Parent
   ignore files are evaluated before nested files."
  [conn]
  (->> (repo/files conn)
       (filter #(or (= % ".gitignore") (str/ends-with? % "/.gitignore")))
       (sort-by (juxt #(count (str/split % #"/")) identity))
       (mapcat (fn [path]
                 (let [base (parent path)
                       content (bytes/decode-utf8 (repo/read conn path))]
                   (keep-indexed #(parse-line base %1 %2)
                                 (str/split content #"\n" -1)))))
       vec))

(defn ignored?
  "True when the final matching rule excludes repository-relative `path`."
  [rules path]
  (reduce (fn [ignored {:keys [regex negated?]}]
            (if (re-matches regex path) (not negated?) ignored))
          false rules))

(defn filter-visible [rules paths]
  (remove #(ignored? rules %) paths))
