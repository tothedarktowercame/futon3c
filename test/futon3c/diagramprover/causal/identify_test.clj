(ns futon3c.diagramprover.causal.identify-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.identify :as identify]))

(defn graph [arrows & [latent]]
  (let [nodes (set (mapcat (juxt :from :to) arrows))]
    (dag/validate
     {:variables (into (sorted-map)
                       (map (fn [node]
                              [node {:id node :kind
                                     (if (contains? (set latent) node)
                                       :latent-unobserved :observed)}]))
                       nodes)
      :arrows arrows})))

(def fd-base
  (graph [{:from :X :to :Z} {:from :Z :to :Y}]))

(deftest fd1-bypass-is-named
  (let [g (update fd-base :arrows conj {:from :X :to :Y})
        conditions (identify/front-door-conditions g :X :Y #{:Z})]
    (is (= [false true true] (mapv :holds? conditions)))
    (is (= [[:X :Y]] (:offending-paths (first conditions))))))

(deftest fd2-confounder-is-named
  (let [g (graph [{:from :X :to :Z} {:from :Z :to :Y}
                  {:from :U :to :X} {:from :U :to :Z}] [:U])
        conditions (identify/front-door-conditions g :X :Y #{:Z})]
    (is (= [true false true] (mapv :holds? conditions)))
    (is (= [[:X :U :Z]] (:offending-paths (second conditions))))))

(deftest fd3-confounder-is-named
  (let [g (graph [{:from :X :to :Z} {:from :Z :to :Y}
                  {:from :U :to :Z} {:from :U :to :Y}] [:U])
        conditions (identify/front-door-conditions g :X :Y #{:Z})]
    (is (= [true true false] (mapv :holds? conditions)))
    (is (= [[:Z :U :Y]] (:offending-paths (nth conditions 2))))))

(deftest front-door-known-answer
  (let [g (graph [{:from :X :to :Z} {:from :Z :to :Y}
                  {:from :U :to :X} {:from :U :to :Y}] [:U])
        result (identify/identify g :X :Y)]
    (is (= :front-door (:method result)))
    (is (= #{:Z} (:mediators result)))
    (is (every? :holds? (:conditions result)))
    (is (= result (identify/identify g :X :Y)))))
