(ns export
  "Side-effecting export harness for the otherwise pure causal layer."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [futon3c.diagramprover.causal.admg :as admg]
            [futon3c.diagramprover.causal.bow :as bow]
            [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.dsep :as dsep]
            [futon3c.diagramprover.causal.receipts :as receipts]))

(def output-directory "holes/labs/M-diagramprover/oracle-pass")

(defn- plain
  "Recursively make keyword-rich EDN deterministic and JSON-friendly."
  [value]
  (cond
    (keyword? value) (name value)
    (map? value) (into (sorted-map)
                       (map (fn [[key item]] [(plain key) (plain item)]))
                       value)
    (set? value) (->> value (map plain) sort vec)
    (sequential? value) (mapv plain value)
    :else value))

(defn- graph-export [causal-dag]
  {:variables (->> (:variables causal-dag) keys sort vec)
   :arrows (->> (:arrows causal-dag)
                (map #(select-keys % [:from :to]))
                (sort-by (juxt :from :to))
                vec)})

(defn- induced-dag [causal-dag nodes]
  (let [node-set (set nodes)]
    (dag/validate
     (-> causal-dag
         (update :variables select-keys node-set)
         (update :arrows
                 #(into [] (filter (fn [{:keys [from to]}]
                                     (and (node-set from) (node-set to))) %)))))))

(defn- mediation-projection [causal-dag kept]
  (let [ancestral-nodes (into kept (dag/ancestors causal-dag kept))
        ancestral-dag (induced-dag causal-dag ancestral-nodes)
        projected-away (set/difference ancestral-nodes kept)
        marked-dag
        (reduce (fn [result node]
                  (assoc-in result [:variables node :kind] :latent-unobserved))
                ancestral-dag projected-away)
        projection (admg/latent-project marked-dag)]
    {:variables (vec (sort (:nodes projection)))
     :arrows (->> (:directed projection)
                  (map (fn [[from to]] {:from from :to to}))
                  (sort-by (juxt :from :to))
                  vec)
     :bidirected (->> (:bidirected projection)
                      (map (comp vec sort))
                      sort vec)
     :kept-set (vec (sort kept))
     :ancestral-nodes (vec (sort ancestral-nodes))
     :projected-away (vec (sort projected-away))}))

(defn build-export []
  (let [memory (dag/load-spec receipts/memory-spec-path)
        lean (dag/load-spec receipts/lean-spec-path)
        q3 (receipts/q3-variants memory)
        r2 (receipts/r2-variants lean)
        r3 (receipts/r3-variants lean)
        mediation-query-nodes #{:V07 :V13 :V14 :V18}
        mediation-projection-nodes #{:V07 :V12 :V13 :V14 :V16 :V17 :V18}
        bow-graphs (into (sorted-map)
                         (map (fn [[id path]] [id (dag/load-spec path)]))
                         bow/fixture-paths)
        mediation-nodes (into mediation-query-nodes
                              (dag/ancestors memory mediation-query-nodes))]
    {:schema-version 2
     :receipts (receipts/all-receipts)
     :bow-receipts (bow/all-bow-receipts)
     :bow-graphs (into (sorted-map)
                       (map (fn [[id causal-dag]]
                              [id (assoc (graph-export causal-dag)
                                         :latent-variables
                                         (->> (:variables causal-dag)
                                              (keep (fn [[node variable]]
                                                      (when (= :latent-unobserved
                                                               (keyword (:kind variable)))
                                                        node)))
                                              sort vec))]))
                       bow-graphs)
     :memory-graph (graph-export memory)
     :lean-graph (graph-export lean)
     :memory-mediation-graph
     (graph-export (induced-dag memory mediation-nodes))
     :memory-mediation-projection
     (mediation-projection memory mediation-projection-nodes)
     :implied-independencies
     (dsep/implied-independencies memory {:max-conditioning 2})
     :lean-implied-independencies
     (dsep/implied-independencies lean {:max-conditioning 2})
     :q3-variants
     {:star-forest (graph-export (:star-forest q3))
      :populated-graph (graph-export (:populated-graph q3))}
     :r1-selection (graph-export (receipts/r1-selection-variant lean))
     :r2-variants
     {:copied-class (graph-export (:copied-class r2))
      :extracted-class (graph-export (:extracted-class r2))}
     :r3-variants
     {:current-sensors (graph-export (:current-sensors r3))
      :with-hypothetical-t05
      (graph-export (:with-hypothetical-t05 r3))}}))

(defn -main [& _]
  (let [payload (build-export)
        json-path (io/file output-directory "engine-export.json")
        edn-path (io/file output-directory "engine-export.edn")]
    (io/make-parents json-path)
    (spit json-path (str (json/generate-string (plain payload)
                                               {:pretty true}) "\n"))
    (spit edn-path (str (pr-str payload) "\n"))
    (println "exported memory/Lean implications"
             (count (:implied-independencies payload)) "/"
             (count (:lean-implied-independencies payload))
             "to" (.getPath json-path))))

(defn- numbered-values [value]
  (if (map? value)
    (->> value
         (sort-by (comp parse-long name key))
         (mapv val))
    value))

(defn- converse-result [causal-dag basis]
  (let [verdicts
        (mapv (fn [{:keys [x y given] :as ci}]
                (assoc ci :holds
                       (dsep/d-separated?
                        causal-dag (keyword x) (keyword y)
                        (into #{} (map keyword)
                              (cond
                                (nil? given) []
                                (string? given) [given]
                                :else given)))))
              (numbered-values basis))
        disagreements (into [] (remove :holds) verdicts)]
    {:checked (count verdicts)
     :agreements (- (count verdicts) (count disagreements))
     :disagreements disagreements}))

(defn verify-converse
  "Evaluate dagitty's emitted CI basis with the Clojure engine itself."
  [& _]
  (let [memory (dag/load-spec receipts/memory-spec-path)
        lean (dag/load-spec receipts/lean-spec-path)
        basis-path (io/file output-directory "dagitty-basis.json")
        output-path (io/file output-directory "engine-converse.json")
        parsed-basis (json/parse-string (slurp basis-path) true)
        result {:memory (converse-result memory (:memory parsed-basis))
                :lean (converse-result lean (:lean parsed-basis))}
        disagreements (mapcat :disagreements (vals result))]
    (spit output-path
          (str (json/generate-string (plain result) {:pretty true}) "\n"))
    (println "engine converse: memory/Lean"
             (get-in result [:memory :agreements]) "/"
             (get-in result [:lean :agreements]) "agreements,"
             (count disagreements) "disagreements")
    (when (seq disagreements)
      (throw (ex-info "Engine disagrees with dagitty CI basis"
                      {:disagreements disagreements})))))
