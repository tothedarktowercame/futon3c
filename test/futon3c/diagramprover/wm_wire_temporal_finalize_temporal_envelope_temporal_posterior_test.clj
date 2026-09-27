(ns futon3c.diagramprover.wm-wire-temporal-finalize-temporal-envelope-temporal-posterior-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-envelope-products-13a :as products]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as support]))

(def wire-id [:temporal-finalize :temporal-envelope [:temporal-posterior {:record :enactment}]])
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
  (let [{:keys [original changed a b]} (products/products :temporal-posterior)]
    (is (= (dissoc original :temporal-posterior) (dissoc changed :temporal-posterior)))
    (is (= :ok (get-in changed [:temporal-posterior :status])))
    (doseq [[r product] [[original a] [changed b]]]
      (is (= (:temporal-posterior r) (get-in product [:envelope :record])))
      (is (= :verified-temporal-posterior (get-in product [:consumed :reason])))
      (is (= (get-in r [:temporal-posterior :posterior])
             (get-in product [:consumed :continuation-belief])))
      (is (= (:envelope product) (dissoc (:read product) :publication))))
    (is (not= (get-in a [:publication :digest]) (get-in b [:publication :digest])))
    (is (not= (get-in a [:consumed :continuation-belief])
              (get-in b [:consumed :continuation-belief])))
    (is (= (dissoc (:envelope a) :record) (dissoc (:envelope b) :record)))
    (println :posterior :beliefs (mapv #(get-in % [:consumed :continuation-belief]) [a b])
             :digests (mapv #(get-in % [:publication :digest]) [a b]))))
