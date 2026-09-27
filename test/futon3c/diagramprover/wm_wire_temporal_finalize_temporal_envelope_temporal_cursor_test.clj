(ns futon3c.diagramprover.wm-wire-temporal-finalize-temporal-envelope-temporal-cursor-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-envelope-products-13a :as products]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as support]))

(def wire-id [:temporal-finalize :temporal-envelope [:temporal-cursor {:record :enactment}]])
(defn check [] (support/observe wire-id :none))
(def wire {:wire wire-id :kind :witnessed-hermetically
           :test `real-courier-reaches-reader :check check
           :second-layer {:test `envelope-product-under-intervention :kind :record
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

(deftest envelope-product-under-intervention
  (let [{:keys [original changed a b]} (products/products :temporal-cursor)]
    (is (= (dissoc original :temporal-cursor) (dissoc changed :temporal-cursor)))
    (doseq [[r product] [[original a] [changed b]]]
      (is (= (:temporal-cursor r)
             (select-keys (:envelope product) [:initial-event-id :consumed-event-ids]))))
    (is (not= (:envelope a) (:envelope b)))
    (is (= (dissoc (:envelope a) :initial-event-id :consumed-event-ids)
           (dissoc (:envelope b) :initial-event-id :consumed-event-ids)))
    (is (= :verified-temporal-posterior (get-in b [:consumed :reason])))
    (is (= (get-in a [:consumed :continuation-belief])
           (get-in b [:consumed :continuation-belief])))
    (println :cursor :before (:temporal-cursor original) :after (:temporal-cursor changed)
             :consumption (get-in b [:consumed :reason]))))
