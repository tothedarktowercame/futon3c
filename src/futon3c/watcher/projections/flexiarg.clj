(ns futon3c.watcher.projections.flexiarg
  "Substrate-2 projector for .flexiarg pattern files. Reads the
   `@flexiarg <ns>/<name>` header to derive the pattern's qname,
   plus optional `@title`, `! conclusion:`, and the Toulmin-style
   slot fields (IF, HOWEVER, THEN, BECAUSE) as props. v0 emits one
   :var (kind=flexiarg) per file.

   Ported from /home/joe/code/futon3/scripts/flexiarg_projection.clj
   (bb script). babashka.fs was imported but not used; dropped the
   require during the port."
  (:require [clojure.string :as str]))

(def src-exts #{"flexiarg"})

(defn- read-text [path]
  (try (slurp path) (catch Exception _ "")))

(defn- parse-header
  "Reads the @flexiarg header. Returns {:ns :name :qname} or nil."
  [text]
  (when-let [m (re-find #"(?m)^@flexiarg\s+([\w./_-]+)/([\w._-]+)\s*$" text)]
    (let [[_ ns nm] m]
      {:ns (str "flexiarg." (str/replace ns "/" "."))
       :name nm
       :qname (str "flexiarg." (str/replace ns "/" ".") "/" nm)})))

(defn- extract-directive [text key]
  (when-let [m (re-find (re-pattern (str "(?m)^@" key "\\s+(.+?)\\s*$")) text)]
    (str/trim (second m))))

(defn- extract-conclusion [text]
  (when-let [m (re-find #"(?m)^!\s*conclusion:\s*(.+?)\s*$" text)]
    (str/trim (second m))))

(defn- has-toulmin-slots? [text]
  (and (re-find #"(?m)^\s*\+\s*IF:" text)
       (re-find #"(?m)^\s*\+\s*HOWEVER:" text)
       (re-find #"(?m)^\s*\+\s*THEN:" text)
       (re-find #"(?m)^\s*\+\s*BECAUSE:" text)))

(defn- collect-symbols
  "v0: collect explicit pattern references from `library/<ns>/<name>`
   path-shaped strings inside the body."
  [text]
  (->> (re-seq #"library/[\w./_-]+/[\w._-]+" text)
       (map (fn [s]
              (let [s (str/replace s #"^library/" "flexiarg.")
                    [ns nm] (let [i (str/last-index-of s "/")]
                              [(subs s 0 i) (subs s (inc i))])
                    qname (str (str/replace ns "/" ".") "/" nm)]
                (symbol qname))))
       (into #{})))

(defn collect-file
  "Project one .flexiarg file into the substrate-2 metadata shape."
  [path]
  (let [text (read-text path)
        header (parse-header text)]
    (when header
      (let [title (extract-directive text "title")
            conclusion (extract-conclusion text)
            doc-quality (has-toulmin-slots? text)
            body-syms (collect-symbols text)]
        {:ns (:ns header)
         :aliases {}
         :is-test? false
         :tests []
         :vars [{:vertex/type :var
                 :var/ns (:ns header)
                 :var/name (:name header)
                 :var/qname (:qname header)
                 :var/kind "flexiarg"
                 :var/has-doc (or (some? title)
                                  (some? conclusion)
                                  doc-quality)
                 :var/syms body-syms}]}))))
