(ns futon3c.diagramprover.rmdiagram-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.receipts :as receipts]
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

(defn- build-pinned-rm-variant [consumer-order]
  (let [[graph p] (rm/add-vertex (rm/make-graph) {:value :p-wire})
        [graph a] (rm/add-vertex graph {:value :a-wire})
        [graph b] (rm/add-vertex graph {:value :b-wire})
        wires {:a a :b b}
        graph (first (rm/add-edge graph [] [p] {:value :p}))]
    (reduce (fn [result consumer]
              (first (rm/add-edge result [p] [(wires consumer)]
                                  {:value consumer})))
            graph consumer-order)))

(deftest rm-cocommutativity-flip
  (let [left-build (build-pinned-rm-variant [:a :b])
        right-build (build-pinned-rm-variant [:b :a])
        left-rm (rm/canonicalize left-build)
        right-rm (rm/canonicalize right-build)]
    ;; Flip of causal.diagram-test/pinned-cocommutativity-gap: the plain-kernel
    ;; pinned test stays green and untouched. Here its two consumer orders reach
    ;; the RM constructor, then canonicalization erases the non-semantic order.
    (is (not= left-build right-build))
    (is (= left-rm right-rm))
    (is (rm/rm-valid? left-rm))
    (is (rmdiagram/canonical? left-rm))))
