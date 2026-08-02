(ns futon3c.diagramprover.causal.surgery
  "Deterministic DAG surgery. Diagrams are always re-rendered by callers."
  (:require [futon3c.diagramprover.causal.dag :as dag]))

(defn- require-node [causal-dag node]
  (when-not (contains? (:variables causal-dag) node)
    (throw (ex-info "Unknown intervention variable" {:variable node})))
  causal-dag)

(defn do-intervention [causal-dag node]
  (-> (require-node causal-dag node)
      (update :arrows #(into [] (remove (fn [arrow] (= node (:to arrow))) %)))
      dag/validate))

(defn cut-outgoing [causal-dag node]
  (-> (require-node causal-dag node)
      (update :arrows #(into [] (remove (fn [arrow] (= node (:from arrow))) %)))
      dag/validate))

(defn remove-node [causal-dag node]
  (-> (require-node causal-dag node)
      (update :variables dissoc node)
      (update :arrows #(into [] (remove (fn [{:keys [from to]}]
                                          (or (= node from) (= node to))) %)))
      (update :leak-edges #(into [] (remove (fn [{:keys [id to]}]
                                              (or (= node id) (= node to))) %)))
      dag/validate))

(def with-leaks dag/with-leaks)
(def without-leaks dag/without-leaks)
