(ns futon3c.diagramprover.rmdiagram-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.diagram :as plain-diagram]
            [futon3c.diagramprover.causal.receipts :as receipts]
            [futon3c.diagramprover.graph :as graph]
            [futon3c.diagramprover.rmdiagram :as rmdiagram]
            [futon3c.diagramprover.rmgraph :as rm]))

(def memory-path "docs/memory-causal-graph-spec.json")
(def lean-path "docs/lean-proof-pipeline-causal-spec.json")

(deftest real-spec-round-trips
  (doseq [causal-dag [(dag/load-spec memory-path) (dag/load-spec lean-path)]]
    (let [rendered (rmdiagram/dag->rmdiagram causal-dag)]
      (is (rm/rm-valid? rendered))
      (is (rmdiagram/canonical? rendered))
      (is (= causal-dag (rmdiagram/rmdiagram->dag rendered)))
      (is (= rendered (rmdiagram/dag->rmdiagram causal-dag))))))

(deftest q3-variant-round-trips
  (let [memory (dag/load-spec memory-path)]
    (doseq [[_ causal-dag] (receipts/q3-variants memory)]
      (let [rendered (rmdiagram/dag->rmdiagram causal-dag)]
        (is (rm/rm-valid? rendered))
        (is (rmdiagram/canonical? rendered))
        (is (= causal-dag (rmdiagram/rmdiagram->dag rendered)))))))

(deftest rm-cocommutativity-flip
  (let [causal-dag {:variables {:p {:id :p} :a {:id :a} :b {:id :b}}
                    :arrows [{:from :p :to :a} {:from :p :to :b}]
                    :leak-edges [] :interventions [] :sensors [] :metadata {}}
        canonical-plain (plain-diagram/dag->diagram causal-dag)
        copy-edge (first
                   (filter #(= :copy
                               (:value (graph/edge-data canonical-plain %)))
                           (graph/edges canonical-plain)))
        cocommuted-plain (update-in canonical-plain [:edata copy-edge :target]
                                    #(vec (reverse %)))
        left-rm (rmdiagram/dag->rmdiagram
                 (plain-diagram/diagram->dag canonical-plain))
        right-rm (rmdiagram/dag->rmdiagram
                  (plain-diagram/diagram->dag cocommuted-plain))]
    ;; Flip of causal.diagram-test/pinned-cocommutativity-gap: the plain-kernel
    ;; pinned test stays green and untouched; absorbed copy has no branch order.
    (is (= left-rm right-rm))
    (is (rm/rm-valid? left-rm))
    (is (rmdiagram/canonical? left-rm))))
