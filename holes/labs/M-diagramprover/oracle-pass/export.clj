(ns export
  "Side-effecting export harness for the otherwise pure causal layer."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
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

(defn build-export []
  (let [memory (dag/load-spec receipts/memory-spec-path)
        lean (dag/load-spec receipts/lean-spec-path)
        q3 (receipts/q3-variants memory)]
    {:schema-version 1
     :receipts (receipts/all-receipts)
     :memory-graph (graph-export memory)
     :lean-graph (graph-export lean)
     :implied-independencies
     (dsep/implied-independencies memory {:max-conditioning 2})
     :q3-variants
     {:star-forest (graph-export (:star-forest q3))
      :populated-graph (graph-export (:populated-graph q3))}
     :r1-selection (graph-export (receipts/r1-selection-variant lean))}))

(defn -main [& _]
  (let [payload (build-export)
        json-path (io/file output-directory "engine-export.json")
        edn-path (io/file output-directory "engine-export.edn")]
    (io/make-parents json-path)
    (spit json-path (str (json/generate-string (plain payload)
                                               {:pretty true}) "\n"))
    (spit edn-path (str (pr-str payload) "\n"))
    (println "exported" (count (:implied-independencies payload))
             "engine implications to" (.getPath json-path))))

(defn verify-converse
  "Evaluate dagitty's emitted CI basis with the Clojure engine itself."
  [& _]
  (let [memory (dag/load-spec receipts/memory-spec-path)
        basis-path (io/file output-directory "dagitty-basis.json")
        output-path (io/file output-directory "engine-converse.json")
        parsed-basis (json/parse-string (slurp basis-path) true)
        basis (if (map? parsed-basis)
                (->> parsed-basis
                     (sort-by (comp parse-long name key))
                     (mapv val))
                parsed-basis)
        verdicts
        (mapv (fn [{:keys [x y given] :as ci}]
                (assoc ci :holds
                       (dsep/d-separated?
                        memory (keyword x) (keyword y)
                        (into #{} (map keyword)
                              (cond
                                (nil? given) []
                                (string? given) [given]
                                :else given)))))
              basis)
        disagreements (into [] (remove :holds) verdicts)
        result {:checked (count verdicts)
                :agreements (- (count verdicts) (count disagreements))
                :disagreements disagreements}]
    (spit output-path
          (str (json/generate-string (plain result) {:pretty true}) "\n"))
    (println "engine converse:" (:agreements result) "agreements,"
             (count disagreements) "disagreements")
    (when (seq disagreements)
      (throw (ex-info "Engine disagrees with dagitty CI basis"
                      {:disagreements disagreements})))))
