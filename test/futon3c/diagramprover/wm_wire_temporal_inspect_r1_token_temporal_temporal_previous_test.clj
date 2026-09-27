(ns futon3c.diagramprover.wm-wire-temporal-inspect-r1-token-temporal-temporal-previous-test
  (:require [futon3c.diagramprover.wm-wire-temporal-previous-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as support]))

(def wire-id [:temporal-inspect :r1-token-temporal [:temporal-previous {:record :temporal-inspection}]])
(defn check [] (support/observe wire-id :none))
(def wire {:wire wire-id :second-layer {:test `temporal-previous-reader-product
                          :kind :value-varying
                          :product [:continuation-belief]
                          :intervention :before-reader}
           :kind :witnessed-hermetically
           :test `real-courier-reaches-reader :check check
           :live-records-read support/live-records-read
           :note "MAP-2B-TEMPORAL: real writer and reader with isolated publication; no live temporal record claimed."})

(deftest real-courier-reaches-reader
  (let [r (check)]
    (is (some? (:writer r)))
    (is (some? (:product r)))
    (is (w/received? r) (pr-str r))))

(deftest carrier-intervention-is-detected
  (doseq [mode [:absent :different]]
    (let [r (support/observe wire-id mode)]
      (is (some? (:writer r)))
      (is (not (w/received? r)) (pr-str r)))))

(deftest historical-records-have-no-temporal-pair
  (is (support/live-absent?)))

(deftest temporal-previous-reader-product
  (let [{:keys [products initial replayed]} (products/products :consume)
        [a b bad] products]
    (is (every? some? replayed))
    (is (= replayed (mapv :continuation-belief [a b])))
    (is (not= (:continuation-belief a) (:continuation-belief b)))
    (is (= :temporal-posterior (:conditioning-status a) (:conditioning-status b)))
    (is (= :posterior (:basis a) (:basis b)))
    (is (= :domain-changed (:conditioning-status bad)))
    (is (= :declared-initialization (:basis bad)))
    (is (= initial (:continuation-belief bad)))
    (println :temporal-reader :consume
             (pr-str (mapv #(select-keys % [:continuation-belief :conditioning-status :basis]) products)))))
