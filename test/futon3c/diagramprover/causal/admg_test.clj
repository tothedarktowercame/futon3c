(ns futon3c.diagramprover.causal.admg-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.causal.admg :as admg]
            [futon3c.diagramprover.causal.dag :as dag]))

(defn graph [variables arrows]
  (dag/validate {:variables (into (sorted-map) (map (juxt :id identity)) variables)
                 :arrows arrows}))

(deftest latent-chain-projects-directed-edge
  (let [g (graph [{:id :A :kind :observed} {:id :L :kind :latent-unobserved}
                  {:id :B :kind :observed}]
                 [{:from :A :to :L} {:from :L :to :B}])]
    (is (contains? (:directed (admg/latent-project g)) [:A :B]))))

(deftest latent-common-cause-projects-bidirected-edge
  (let [g (graph [{:id :L :kind :latent-unobserved} {:id :A :kind :observed}
                  {:id :B :kind :observed}]
                 [{:from :L :to :A} {:from :L :to :B}])]
    (is (contains? (:bidirected (admg/latent-project g)) #{:A :B}))))

(deftest observed-interior-is-not-projected
  (let [g (graph [{:id :A :kind :observed} {:id :M :kind :observed}
                  {:id :B :kind :observed}]
                 [{:from :A :to :M} {:from :M :to :B}])
        projected (admg/latent-project g)]
    (is (not (contains? (:directed projected) [:A :B])))
    (is (= #{[:A :M] [:M :B]} (set (:directed projected))))))
