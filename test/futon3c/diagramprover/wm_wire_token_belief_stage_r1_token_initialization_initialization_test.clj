(ns futon3c.diagramprover.wm-wire-token-belief-stage-r1-token-initialization-initialization-test
  (:require [futon3c.diagramprover.wm-wire-initialization-hash-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as support]))

(def wire-id [:token-belief-stage :r1-token-initialization [:initialization {:record :token-belief-stage}]])
(defn check [] (support/observe wire-id :none))
(def wire {:wire wire-id :kind :witnessed-hermetically
           :test `real-courier-reaches-reader :check check
           :second-layer {:test `initialization-changes-observation-fold
                          :kind :value-varying :product [:continuation-belief]
                          :intervention :before-reader}
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

(deftest initialization-changes-observation-fold
  (let [{:keys [stages initial products no-updates token]}
        (products/initialization-products :receipt)
        [a b] products
        beliefs (mapv :continuation-belief products)]
    (is (= (dissoc (first stages) :initialization) (dissoc (second stages) :initialization)))
    (is (not= (first initial) (second initial)))
    (is (some #(= :updated (:status %)) (:observation-updates a)))
    (is (= (:observation-updates a) (:observation-updates b)))
    (is (some #(and (= token (:token %)) (= :not-updated (:status %))) (:observation-updates a)))
    (is (not= (first beliefs) (second beliefs)))
    (is (= initial (mapv :continuation-belief no-updates)))
    (is (every? #(not-any? (fn [u] (= :updated (:status u))) (:observation-updates %)) no-updates))
    (println "initialization :receipt" (pr-str {:initial initial :beliefs beliefs
                                                 :no-updates (mapv :continuation-belief no-updates)}))))
