(ns futon3c.diagramprover.critical-pairs-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.critical-pairs :as cp]
            [futon3c.diagramprover.paper-choice :as paper]
            [futon3c.diagramprover.graph :as graph]
            [futon3c.diagramprover.matcher :as matcher]
            [futon3c.diagramprover.rule :as rule]))

(defn- chain [& xs]
  (reduce graph/compose (map #(graph/generator % [:wire] [:wire]) xs)))

(deftest choice-conflict-is-enumerated-and-normalised
  (let [pairs (cp/critical-pairs paper/r1 paper/r2)
        pair (first pairs)]
    (is (= 2 (count pairs)))
    (is (every? #(= :non-joining (:status %)) pairs))
    (is (matcher/find-iso (:overlap pair) (paper/term-graph paper/overlap-term)))
    (is (matcher/find-iso (first (:results pair)) (paper/term-graph paper/beta-result)))
    (is (matcher/find-iso (second (:results pair)) (paper/term-graph paper/indexed-result)))
    (is (= [1 1] (mapv #(count (:normal-forms %)) (:normalisations pair))))))

(deftest choose-exemption-removes-the-overlap
  (is (empty? (cp/critical-pairs paper/r1-guarded paper/r2)))
  (is (= 1 (count (cp/applications paper/r2 (:lhs paper/r2))))))

(deftest disjoint-heads-have-no-overlap
  (is (empty? (cp/critical-pairs (rule/make-rule (chain :f) (chain :a))
                                 (rule/make-rule (chain :g) (chain :b))))))

(deftest proper-subdiagram-and-joining-descendants
  (let [a (rule/make-rule (chain :f :g) (chain :h))
        b (rule/make-rule (chain :f) (chain :k))
        c (rule/make-rule (chain :k :g) (chain :h))
        pairs (cp/critical-pairs a b [a b c] {})]
    (is (= 1 (count pairs)))
    (is (= :joining (:status (first pairs))))))

(deftest exploration-bound-is-not-a-nonjoining-verdict
  (is (every? #(= :inconclusive (:status %))
              (cp/critical-pairs paper/r1 paper/r2 [paper/r1 paper/r2]
                                 {:max-states 0}))))

(deftest exemption-is-not-a-blanket-beta-disable
  (let [ordinary (assoc paper/r1-guarded
                        :lhs (paper/term-graph [:app [:lam [:spec [:var]]] [:index]])
                        :rhs (paper/term-graph [:spec [:index]]))]
    (is (= 1 (count (cp/applications ordinary (:lhs ordinary)))))))

(deftest cyclic-descendants-are-compared-modulo-isomorphism
  (let [a (rule/make-rule (chain :f) (chain :g))
        b (rule/make-rule (chain :g) (chain :f))
        result (cp/descendants [a b] (chain :f) 10)]
    (is (= :complete (:status result)))
    (is (= 2 (count (:states result))))
    (is (empty? (:normal-forms result)))))
