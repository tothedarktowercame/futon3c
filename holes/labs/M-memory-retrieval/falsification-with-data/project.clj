(ns project
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.diagramprover.causal.admg :as admg]
            [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.dsep :as dsep]))

(def here "holes/labs/M-memory-retrieval/falsification-with-data")
(def spec-path "holes/labs/M-memory-retrieval/retrieval-stage-causal-spec.json")
(def receipt-path "holes/labs/M-memory-retrieval/receipts-export-20260731-all-authors.edn")
(def measured #{:query-cardinality :query-vocabulary :pollution
                :surfaced-set :offered-set :used-set})

(defn- mark-unmeasured-latent [causal-dag]
  (update causal-dag :variables
          (fn [variables]
            (into (sorted-map)
                  (map (fn [[id variable]]
                         [id (assoc variable :kind
                                    (if (measured id) :observed :latent-unobserved))]))
                  variables))))

(defn- admg-as-dag [{:keys [nodes directed bidirected]}]
  (let [latent-edges
        (map-indexed
         (fn [index edge]
           (let [[a b] (sort edge)
                 latent (keyword (str "projected-confounder-" (inc index)))]
             {:latent latent :edges [[latent a] [latent b]]}))
         (sort-by #(vec (sort %)) bidirected))
        variables (concat (map #(hash-map :id % :kind :observed) nodes)
                          (map #(hash-map :id (:latent %) :kind :latent-unobserved)
                               latent-edges))
        arrows (concat directed (mapcat :edges latent-edges))]
    (dag/validate
     {:variables (into (sorted-map) (map (juxt :id identity)) variables)
      :arrows (mapv (fn [[from to]] {:from from :to to}) arrows)})))

(defn- minimal-cis [projection]
  (let [expanded (admg-as-dag projection)
        nodes (vec (sort (:nodes projection)))
        all-cis (dsep/implied-independencies
                 expanded {:max-conditioning (max 0 (- (count nodes) 2))})]
    (->> all-cis
         (filter #(and (measured (:x %)) (measured (:y %))
                       (every? measured (:given %))))
         (map (fn [{:keys [x y given]}]
                {:x (name x) :y (name y) :given (mapv name (sort given))}))
         (sort-by (juxt :x :y :given))
         vec)))

(def tex-fragment-pattern #"[\\\\{}_^]")

(defn- pollution-proxy [terms]
  (if (seq terms)
    (/ (double (count (filter #(re-find tex-fragment-pattern (str %)) terms)))
       (count terms))
    0.0))

(defn- csv-cell [x]
  (if (nil? x) "" (str x)))

(defn- write-csv! [rows]
  (let [columns [:job-id :query-cardinality :query-vocabulary :pollution
                 :surfaced-set :offered-set :used-set]
        line (fn [row] (str/join "," (map #(csv-cell (get row %)) columns)))]
    (spit (io/file here "data.csv")
          (str (str/join "," (map name columns)) "\n"
               (str/join "\n" (map line rows)) "\n"))))

(defn -main []
  (let [causal-dag (dag/load-spec spec-path)
        projection (admg/latent-project (mark-unmeasured-latent causal-dag))
        corpus (edn/read-string (slurp receipt-path))
        bodies (map #(get-in % [:evidence/body]) (:entries corpus))
        offered (sort-by :job-id (filter #(= :offered (:phase %)) bodies))
        outcomes (reduce (fn [result body]
                           ;; A few jobs have more than one outcome receipt.
                           ;; Preserve the outcome carrying the instrumented
                           ;; memory-use block; never let a narrative-only
                           ;; duplicate erase it.
                           (if (or (:memory-use body)
                                   (not (contains? result (:job-id body))))
                             (assoc result (:job-id body) body)
                             result))
                         {}
                         (filter #(= :outcome (:phase %)) bodies))
        rows
        (mapv
         (fn [body]
           (let [job-id (:job-id body)
                 terms (get-in body [:recall-query :terms])
                 surfaced (get-in body [:memory-use :memory-use/surfaced-ids])
                 inclusion (get-in body [:memory-use :memory-use/inclusion-reasons])
                 outcome-use (get-in outcomes [job-id :memory-use])]
             {:job-id job-id
              :query-cardinality (count terms)
              ;; All 129 are observational runs of a shipped builder. E8 arms
              ;; are supplementary and deliberately not pooled into this panel.
              :query-vocabulary 1
              :pollution (pollution-proxy terms)
              :surfaced-set (count (distinct surfaced))
              :offered-set (count (distinct (map :memory-id inclusion)))
              :used-set (when outcome-use
                          (count (distinct (:memory-use/used-ids outcome-use))))}))
         offered)
        result {:sample-size (count rows)
                :complete-used-set (count (filter (comp some? :used-set) rows))
                :measured (mapv name (sort measured))
                :projection {:nodes (mapv name (sort (:nodes projection)))
                             :directed (mapv (fn [[a b]] [(name a) (name b)])
                                             (sort (:directed projection)))
                             :bidirected (mapv #(mapv name (sort %))
                                               (sort-by #(vec (sort %))
                                                        (:bidirected projection)))}
                :implied-cis (minimal-cis projection)
                :column-support
                (into (sorted-map)
                      (for [column (disj measured :query-vocabulary)]
                        [(name column)
                         (count (distinct (keep column rows)))]))}]
    (when-not (= 129 (count rows))
      (throw (ex-info "Frozen corpus does not contain 129 offered dispatches"
                      {:actual (count rows)})))
    (write-csv! rows)
    (spit (io/file here "engine.json")
          (str (json/generate-string result {:pretty true}) "\n"))))
