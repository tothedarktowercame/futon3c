(ns futon3c.diagramprover.graph-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.graph :as graph]))

(deftest graph-construction-round-trip
  (testing "a 2->1 generator composed with a 1->1 generator"
    (let [f (graph/generator :f [:wire :wire] [:wire])
          g (graph/generator :g [:wire] [:wire])
          composed (graph/compose f g)]
      (is (= 4 (graph/num-vertices composed)))
      (is (= 2 (graph/num-edges composed)))
      (is (= [[:wire 1] [:wire 1]] (graph/domain composed)))
      (is (= [[:wire 1]] (graph/codomain composed)))
      (is (= #{:f :g} (set (map #(-> (graph/edge-data composed %) :value)
                                  (graph/edges composed))))))))

(deftest operations-are-immutable
  (let [[graph vertex] (graph/add-vertex (graph/make-graph) {:vtype :wire})]
    (is (empty? (:vdata (graph/make-graph))))
    (is (= #{vertex} (set (graph/vertices graph))))))
