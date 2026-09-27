(ns futon3c.diagramprover.wm-wire-temporal-finalize-temporal-envelope-temporal-receipt-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-envelope-products-13a :as products]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as support]))

(def wire-id [:temporal-finalize :temporal-envelope [:temporal-receipt {:record :enactment}]])
(defn check [] (support/observe wire-id :none))
(def wire {:wire wire-id :kind :witnessed-hermetically
           :test `real-courier-reaches-reader :check check
           :second-layer {:test `envelope-product-under-intervention :kind :value-varying
                          :product [:envelope] :intervention :before-reader}
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

(deftest finalized-and-direct-refusal-publications-reach-reader
  (let [r (support/publication-paths)]
    (is (= :posterior (:published r)))
    (is (= :no-previous-posterior (:refused r)))
    (is (= (:persisted r) (select-keys (:read r) [:status :reason :detail])))))

(deftest envelope-product-under-intervention
  (let [{:keys [original changed a b]} (products/products :temporal-receipt)]
    (is (= (dissoc original :temporal-receipt) (dissoc changed :temporal-receipt)))
    (is (= :posterior (get-in a [:envelope :basis])))
    (is (nil? (:envelope b)) "Unpublished status yields nil, not a typed envelope refusal.")
    (is (= {:status :absent :reason :no-previous-posterior}
           (select-keys (:read b) [:status :reason])))
    (is (= (:publication b) (:read b)) "Published absence retains its path/digest citation.")
    (is (= :no-previous-posterior (get-in b [:consumed :conditioning-status])))
    (is (not (contains? (:consumed b) :continuation-belief)))
    (println :receipt :envelope-before (get-in a [:envelope :basis])
             :envelope-after (:envelope b) :read-back (:read b))))
