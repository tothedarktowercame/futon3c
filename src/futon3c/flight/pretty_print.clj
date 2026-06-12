(ns futon3c.flight.pretty-print
  "Pure renderer from schema-v0.4 .flight.edn to canonical-organ-order text.

   This namespace does not score, repair, or mutate flight records. It renders
   the organ cells already present in the EDN: term cells carry
   :judgment/:ground; sorry cells carry typed :sorry maps. Missing canonical
   organs render as explicit structural holes so detector consumers can see the
   absence instead of losing the section."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def default-mapping-path
  (str (System/getProperty "user.dir") "/data/flight-pretty-print-mapping.edn"))

(defn read-edn-file [path]
  (edn/read-string (slurp path)))

(defn load-mapping
  ([] (load-mapping default-mapping-path))
  ([path] (read-edn-file path)))

(defn term-cell? [cell]
  (and (map? cell)
       (contains? cell :judgment)
       (contains? cell :ground)))

(defn sorry-cell? [cell]
  (and (map? cell)
       (map? (:sorry cell))
       (keyword? (get-in cell [:sorry :kind]))))

(defn- key-sort [a b]
  (compare (pr-str a) (pr-str b)))

(declare stable-value)

(defn- stable-map [m]
  (into (sorted-map-by key-sort)
        (map (fn [[k v]] [k (stable-value v)]) m)))

(defn stable-value [x]
  (cond
    (map? x) (stable-map x)
    (set? x) (vec (sort-by pr-str (map stable-value x)))
    (vector? x) (mapv stable-value x)
    (seq? x) (mapv stable-value x)
    :else x))

(defn stable-pr-str [x]
  (binding [*print-namespace-maps* false]
    (pr-str (stable-value x))))

(defn- emit-block [title body]
  (str title "\n" body "\n"))

(defn- measurement-class-key [record cell]
  (let [class (get-in cell [:judgment :class])]
    (cond
      (keyword? class) class
      (= :thin (:flight/derivation record)) :class-absent-derivation-thin
      :else :class-not-yet-judged)))

(defn- organ-atlas-line [mapping record organ-key cell]
  (let [organ-vocab (get-in mapping [:organ-vocabulary organ-key])
        base-role (:atlas-role organ-vocab)
        role (if (= organ-key :measurement)
               (get-in mapping [:measurement-class-vocabulary
                                (measurement-class-key record cell)
                                :certificate]
                       base-role)
               base-role)]
    (str "Atlas-role: " (stable-pr-str role) "\n"
         "Wire: " (stable-pr-str (:wire organ-vocab)) "\n"
         "Aliases: " (stable-pr-str (or (:aliases organ-vocab) [])) "\n")))

(defn- sorry-lines [mapping sorry]
  (let [kind (:kind sorry)
        vocab (get-in mapping [:sorry-vocabulary kind])]
    (str "State: typed-hole\n"
         "Hole-kind: " (stable-pr-str kind) "\n"
         "Hole-vocabulary: " (stable-pr-str (or vocab {:hole-type kind})) "\n"
         "Sorry:\n" (stable-pr-str sorry) "\n")))

(defn- term-lines [cell]
  (str "State: term\n"
       "Ground:\n" (stable-pr-str (:ground cell)) "\n"
       "Judgment:\n" (stable-pr-str (:judgment cell)) "\n"))

(defn- missing-lines [mapping organ-key]
  (sorry-lines mapping
               {:kind :not-yet
                :blocked-by :missing-organ
                :note (str "canonical organ " (name organ-key)
                           " absent from this flight record")}))

(defn render-organ [mapping record organ-key]
  (let [cell (get-in record [:organs organ-key])
        header (str "## " (name organ-key) "\n")
        atlas (organ-atlas-line mapping record organ-key cell)
        body (cond
               (term-cell? cell) (term-lines cell)
               (sorry-cell? cell) (sorry-lines mapping (:sorry cell))
               (nil? cell) (missing-lines mapping organ-key)
               :else (str "State: invalid-cell\n"
                          "Cell:\n" (stable-pr-str cell) "\n"))]
    (emit-block header (str atlas body))))

(defn render-flight
  "Return deterministic markdown-ish text for one flight record map."
  ([record] (render-flight (load-mapping) record))
  ([mapping record]
   (let [order (:canonical-organ-order mapping)
         organs (:organs record)
         extra-organs (->> (keys organs)
                           (remove (set order))
                           (sort-by name))
         all-organs (vec (concat order extra-organs))]
     (str "# Flight " (:flight/id record) "\n\n"
          "Derivation: " (stable-pr-str (:flight/derivation record)) "\n"
          "Mapping-version: " (stable-pr-str (:version mapping)) "\n"
          "Morphism-trace: forward field-read -> prediction -> velocity -> act; backward measurement -> self-record\n"
          "Links:\n" (stable-pr-str (or (:flight/links record) [])) "\n\n"
          (apply str (map #(render-organ mapping record %) all-organs))))))

(defn render-file
  ([path] (render-file default-mapping-path path))
  ([mapping-path path]
   (render-flight (load-mapping mapping-path) (read-edn-file path))))

(defn flight-path-for-run-id
  ([run-id] (flight-path-for-run-id (str (System/getProperty "user.dir")
                                         "/data/repl-traces")
                                    run-id))
  ([traces-dir run-id]
   (str (.getPath (io/file traces-dir (str run-id ".flight.edn"))))))

(defn latest-flight-path
  ([] (latest-flight-path (str (System/getProperty "user.dir")
                               "/data/repl-traces")))
  ([traces-dir]
   (some->> (file-seq (io/file traces-dir))
            (filter #(.isFile ^java.io.File %))
            (filter #(str/ends-with? (.getName ^java.io.File %) ".flight.edn"))
            (sort-by #(.lastModified ^java.io.File %))
            last
            .getPath)))
