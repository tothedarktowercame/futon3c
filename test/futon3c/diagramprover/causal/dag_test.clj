(ns futon3c.diagramprover.causal.dag-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.surgery :as surgery]))

(def memory-path "docs/memory-causal-graph-spec.json")
(def lean-path "docs/lean-proof-pipeline-causal-spec.json")

(deftest real-spec-ingest
  (let [memory (dag/load-spec memory-path)
        lean (dag/load-spec lean-path)]
    (is (= [20 34 4] [(count (:variables memory))
                      (count (:arrows memory))
                      (count (:leak-edges memory))]))
    (is (= [20 31] [(count (:variables lean)) (count (:arrows lean))]))
    (is (= 20 (count (dag/topological-sort memory))))
    (is (= 20 (count (dag/topological-sort lean))))
    (is (every? keyword? (keys (:variables memory))))
    (is (every? keyword? (map :target (:interventions memory))))
    (is (every? keyword? (map :id (:sensors memory))))))

(deftest validation-rejects-corruption
  (let [memory (dag/load-spec memory-path)]
    (testing "unknown arrow references"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"unknown variable"
           (dag/validate (update memory :arrows conj {:from :V01 :to :NOPE})))))
    (testing "cycle members are named"
      (let [error (try
                    (dag/validate (update memory :arrows conj
                                          {:from :V20 :to :V01}))
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (is (seq (:cycle (ex-data error))))))))

(deftest adjacency-and-leak-variants
  (let [base (dag/load-spec memory-path)
        leaked (dag/with-leaks base)
        restored (dag/without-leaks leaked)]
    (is (= 4 (- (count (:variables leaked)) (count (:variables base)))))
    (is (= 4 (- (count (:arrows leaked)) (count (:arrows base)))))
    (is (= base restored))
    (is (dag/exogenous? leaked :L1))
    (is (= #{(:to (first (:leak-edges base)))}
           (dag/children leaked :L1)))
    (is (contains? (dag/ancestors base :V18) :V01))
    (is (contains? (dag/descendants base :V01) :V18))))

(deftest intervention-and-removal
  (let [motif {:variables {:u {:id :u} :x {:id :x} :y {:id :y}}
               :arrows [{:from :u :to :x} {:from :u :to :y}
                        {:from :x :to :y}]
               :leak-edges [] :interventions [] :sensors [] :metadata {}}]
    (is (= #{} (dag/parents (surgery/do-intervention motif :x) :x)))
    (is (= #{:u} (dag/parents (surgery/cut-outgoing motif :x) :x)))
    (is (= #{:u :y} (set (keys (:variables (surgery/remove-node motif :x))))))))
