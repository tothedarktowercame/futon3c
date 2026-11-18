(ns futon3c.giraffe.run-data
  "Content-addressed persistence for Giraffe input/pass maps.

  Legacy files contain the logical map directly. Version-1 index files keep
  scalar metadata inline and refer to canonical immutable EDN objects."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp])
  (:import [java.nio.file AtomicMoveNotSupportedException CopyOption Files
            StandardCopyOption]))

(def index-schema [:giraffe/run-index 1])
(def ref-key :giraffe/object-ref)

(defn- sorted-value [x]
  (cond
    (map? x) (into (sorted-map) (map (fn [[k v]] [(sorted-value k) (sorted-value v)])) x)
    (set? x) (into (sorted-set) (map sorted-value) x)
    (sequential? x) (mapv sorted-value x)
    :else x))

(defn canonical-edn [x]
  (with-out-str (pp/pprint (sorted-value x))))

(defn sha256-bytes [^bytes bs]
  (let [md (doto (java.security.MessageDigest/getInstance "SHA-256") (.update bs))]
    (apply str (map #(format "%02x" %) (.digest md)))))

(defn logical-sha [x]
  (sha256-bytes (.getBytes ^String (canonical-edn x) "UTF-8")))

(defn object-file [root sha]
  (io/file root "objects" (subs sha 0 2) (str sha ".edn")))

(defn- atomic-move! [from to]
  (try
    (Files/move (.toPath from) (.toPath to)
                (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
    (catch AtomicMoveNotSupportedException _
      (Files/move (.toPath from) (.toPath to)
                  (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING])))))

(defn- write-object! [root value written]
  (let [text (canonical-edn value)
        bytes (.getBytes ^String text "UTF-8")
        sha (sha256-bytes bytes)
        target (object-file root sha)]
    (when-not (.isFile target)
      (io/make-parents target)
      (let [tmp (io/file (.getParentFile target)
                         (str "." sha "." (java.util.UUID/randomUUID) ".tmp"))]
        (spit tmp text)
        (if (.isFile target)
          (.delete tmp)
          (atomic-move! tmp target))))
    (swap! written conj sha)
    {ref-key sha}))

(defn object-ref? [x]
  (and (map? x) (= #{ref-key} (set (keys x))) (string? (get x ref-key))))

(defn- normalize-row [root row written]
  (cond-> row
    (map? (:dependency-reading row))
    (assoc :dependency-reading (write-object! root (:dependency-reading row) written))
    (map? (:graph-terms row))
    (assoc :graph-terms (write-object! root (:graph-terms row) written))))

(defn- row-refs [root rows written]
  (mapv #(write-object! root (normalize-row root % written) written) rows))

(defn- map-entry-refs [root m written]
  (mapv (fn [[k v]] [k (write-object! root (if (map? v)
                                              (normalize-row root v written) v)
                                          written)])
        (sort-by (comp pr-str key) m)))

(defn- input-content [root input written]
  (let [edges (row-refs root (:edges input) written)
        coverage (:graph-coverage input)
        member-refs (row-refs root (:members coverage) written)
        coverage-ref (write-object! root (assoc coverage :members member-refs) written)
        evidence (:reading-evidence input)
        evidence-ref
        (write-object!
         root
         (cond-> evidence
           (contains? evidence :coverage) (assoc :coverage (write-object! root (:coverage evidence) written))
           (contains? evidence :edges) (assoc :edges edges)
           (contains? evidence :links) (assoc :links (row-refs root (:links evidence) written))
           (contains? evidence :unresolved) (assoc :unresolved (row-refs root (:unresolved evidence) written))
           (contains? evidence :out-of-scope) (assoc :out-of-scope (row-refs root (:out-of-scope evidence) written))
           (contains? evidence :rejected) (assoc :rejected (row-refs root (:rejected evidence) written)))
         written)]
    (into (sorted-map)
          (for [[k v] input]
            [k (case k
                 :items (write-object! root (row-refs root v written) written)
                 :edges (write-object! root edges written)
                 :graph-coverage coverage-ref
                 :reading-evidence evidence-ref
                 v)]))))

(defn- pass-content [root pass input-index written]
  (let [input-content (:content input-index)]
    (into (sorted-map)
          (for [[k v] pass]
            [k (case k
                 :graph-coverage (:graph-coverage input-content)
                 :reading-evidence (:reading-evidence input-content)
                 :order (write-object! root (row-refs root v written) written)
                 :readiness (write-object! root (map-entry-refs root v written) written)
                 :cones (write-object! root (map-entry-refs root v written) written)
                 :unblocks (write-object! root (map-entry-refs root v written) written)
                 :work-clusters (write-object! root (row-refs root v written) written)
                 (if (coll? v) (write-object! root v written) v))]))))

(defn make-indexes!
  "Write immutable objects below ROOT and return input/pass index maps."
  [root input pass]
  (let [written (atom #{})
        input-index {:schema index-schema :kind :input
                     :logical-sha256 (logical-sha input)
                     :content (input-content root input written)}
        pass-index {:schema index-schema :kind :pass
                    :logical-sha256 (logical-sha pass)
                    :content (pass-content root pass input-index written)}]
    {:input-index input-index :pass-index pass-index :object-shas @written}))

(defn index? [x]
  (= index-schema (:schema x)))

(defn- read-object [root sha cache]
  (if-let [v (get @cache sha)]
    v
    (let [f (object-file root sha)
          value (edn/read-string (slurp f))]
      (swap! cache assoc sha value)
      value)))

(declare resolve-value)

(defn- resolve-value [root x cache]
  (cond
    (object-ref? x) (resolve-value root (read-object root (get x ref-key) cache) cache)
    (map? x) (into (empty x) (map (fn [[k v]] [k (resolve-value root v cache)])) x)
    (vector? x) (mapv #(resolve-value root % cache) x)
    (set? x) (set (map #(resolve-value root % cache) x))
    (sequential? x) (mapv #(resolve-value root % cache) x)
    :else x))

(defn- restore-entry-maps [kind logical]
  (if (= :pass kind)
    (reduce (fn [m k]
              (if (vector? (get m k)) (update m k #(into {} %)) m))
            logical [:readiness :cones :unblocks])
    logical))

(defn read-run-file
  "Read logical input/pass data from legacy full EDN or a version-1 index."
  [run-dir filename]
  (let [value (edn/read-string (slurp (io/file run-dir filename)))]
    (if-not (index? value)
      value
      (let [root (.getParentFile (io/file run-dir))
            logical (resolve-value root (:content value) (atom {}))]
        (restore-entry-maps (:kind value) logical)))))

(defn referenced-shas [x]
  (->> (tree-seq coll? seq x)
       (keep #(when (object-ref? %) (get % ref-key)))
       set))

(defn verify-index
  "Verify every transitively referenced object. Returns typed mismatches."
  [run-dir filename]
  (let [index-file (io/file run-dir filename)]
    (if-not (.isFile index-file)
      {:mismatches [] :object-count 0}
      (let [index (edn/read-string (slurp index-file))]
    (if-not (index? index)
      {:mismatches [] :object-count 0}
      (let [root (.getParentFile (io/file run-dir))]
        (loop [pending (vec (referenced-shas index)) seen #{} mismatches []]
          (if-let [sha (peek pending)]
            (if (seen sha)
              (recur (pop pending) seen mismatches)
              (let [f (object-file root sha)]
                (if-not (.isFile f)
                  (recur (pop pending) (conj seen sha)
                         (conj mismatches {:file filename :reason :object-missing :object-sha256 sha}))
                  (let [bytes (Files/readAllBytes (.toPath f))
                        actual (sha256-bytes bytes)
                        value (try (edn/read-string (String. bytes "UTF-8"))
                                   (catch Exception _ nil))]
                    (recur (into (pop pending) (referenced-shas value))
                           (conj seen sha)
                           (cond-> mismatches
                             (not= sha actual)
                             (conj {:file filename :reason :object-sha-mismatch
                                    :object-sha256 sha :actual-sha256 actual})))))))
            {:mismatches mismatches :object-count (count seen)}))))))))
