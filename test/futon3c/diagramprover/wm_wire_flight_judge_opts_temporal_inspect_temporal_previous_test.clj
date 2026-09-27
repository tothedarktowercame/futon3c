(ns futon3c.diagramprover.wm-wire-flight-judge-opts-temporal-inspect-temporal-previous-test
  (:require [futon3c.diagramprover.wm-wire-temporal-previous-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as support]))

(def wire-id [:flight-judge-opts :temporal-inspect [:temporal-previous {:record :flight}]])
(defn check [] (support/observe wire-id :none))
(def wire {:wire wire-id :second-layer {:test `temporal-previous-reader-product
                          :kind :record
                          :product [:temporal-previous]
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
  ;; inspect-trace:35-39 only carries the flight envelope; no IO/replay here.
  (let [{:keys [carriers products]} (products/products :inspect)
        [a b bad] products]
    (is (= carriers (mapv :temporal-previous products)))
    (is (not= (:temporal-previous a) (:temporal-previous b)))
    (is (= (dissoc a :temporal-previous) (dissoc b :temporal-previous)
           (dissoc bad :temporal-previous)))
    (println :inspection-previous
             (pr-str (mapv #(get-in % [:temporal-previous :record :posterior]) [a b]))
             :candidate-classifications-unchanged true)))
