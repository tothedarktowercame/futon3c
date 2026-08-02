(ns futon3c.diagramprover.causal.diagram-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.diagram :as diagram]
            [futon3c.diagramprover.graph :as graph]
            [futon3c.diagramprover.matcher :as matcher]))

(def memory-path "docs/memory-causal-graph-spec.json")
(def lean-path "docs/lean-proof-pipeline-causal-spec.json")

(deftest real-spec-round-trips
  (doseq [causal-dag [(dag/load-spec memory-path) (dag/load-spec lean-path)]]
    (let [rendered (diagram/dag->diagram causal-dag)]
      (is (diagram/canonical? rendered))
      (is (= causal-dag (diagram/diagram->dag rendered))))))

(deftest canonical-copy-comb-order
  (let [causal-dag {:variables {:p {:id :p} :a {:id :a}
                                :b {:id :b} :c {:id :c}}
                    :arrows [{:from :p :to :c} {:from :p :to :a}
                             {:from :p :to :b}]
                    :leak-edges [] :interventions [] :sensors [] :metadata {}}
        rendered (diagram/dag->diagram causal-dag)
        copies (filter #(= :copy (:value (graph/edge-data rendered %)))
                       (graph/edges rendered))]
    (is (diagram/canonical? rendered))
    (is (= 2 (count copies)))
    (is (= #{:a :b :c}
           (into #{} (keep :causal/consumer) (vals (:vdata rendered)))))))

(deftest pinned-cocommutativity-gap
  (let [causal-dag {:variables {:p {:id :p} :a {:id :a} :b {:id :b}}
                    :arrows [{:from :p :to :a} {:from :p :to :b}]
                    :leak-edges [] :interventions [] :sensors [] :metadata {}}
        canonical (diagram/dag->diagram causal-dag)
        copy-edge (first (filter #(= :copy (:value (graph/edge-data canonical %)))
                                 (graph/edges canonical)))
        reversed (update-in canonical [:edata copy-edge :target]
                            #(vec (reverse %)))]
    (is (= causal-dag (diagram/diagram->dag reversed)))
    (is (not (diagram/canonical? reversed)))
    ;; Quotient gap, WS-B.0 route (b). This failure is definitional, not a bug.
    ;; MPZ extension (arXiv:2204.04274) acceptance = this test flips.
    (is (nil? (matcher/find-iso canonical reversed)))))
