(ns futon3c.diagramprover.wm-wire-flight-run-flight-judge-opts-temporal-receipt-test
  (:require [futon3c.diagramprover.wm-wire-temporal-run-products-14b :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as support]))

(def wire-id [:flight-run :flight-judge-opts [:temporal-receipt {:record :enactment-entry}]])
(defn check [] (support/observe wire-id :none))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-flight-run-flight-judge-opts-temporal-receipt-test/last-receipt-determines-next-posterior
                          :kind :value-varying :product [:flight :temporal-previous]
                          :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically
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

(deftest finalized-and-direct-refusal-publications-reach-reader
  (let [r (support/publication-paths)]
    (is (= :posterior (:published r)))
    (is (= :no-previous-posterior (:refused r)))
    (is (= (:persisted r) (select-keys (:read r) [:status :reason :detail])))))

(deftest last-receipt-determines-next-posterior
  (let [{:keys [inputs products expected envelopes absence]} (products/judge-products)
        posteriors (mapv #(get-in % [:record :posterior]) envelopes)]
    (prn :wire-2l-14b :judge-opts :posteriors posteriors :absent (last envelopes))
    (is (= (first expected) (first envelopes)))
    (is (= (second expected) (second envelopes)))
    (is (= [1 0] (mapv #(get % #{[:courier :done]} 0) (take 2 posteriors))))
    (is (not= (first posteriors) (second posteriors)))
    (is (= absence (last envelopes)))
    (is (nil? (last posteriors)))
    (is (apply = (map #(update-in % [:enactments 1] dissoc :temporal-receipt) inputs)))
    (is (apply = (map #(update % :flight dissoc :temporal-previous) products)))))
