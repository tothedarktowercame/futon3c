(ns futon3c.diagramprover.wm-wire-temporal-finalize-temporal-write-once-temporal-receipt-test
  (:require [futon3c.diagramprover.wm-wire-temporal-storage-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as support]))

(def wire-id [:temporal-finalize :temporal-write-once [:temporal-receipt {:record :enactment}]])
(defn check [] (support/observe wire-id :none))
(def wire {:wire wire-id :second-layer {:test `temporal-storage-reader-product :kind :record
                          :product [:record :temporal-receipt] :intervention :before-reader}
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

(deftest finalized-and-direct-refusal-publications-reach-reader
  (let [r (support/publication-paths)]
    (is (= :posterior (:published r)))
    (is (= :no-previous-posterior (:refused r)))
    (is (= (:persisted r) (select-keys (:read r) [:status :reason :detail])))))

(deftest temporal-storage-reader-product
  (let [{:keys [final absent a b repeat event-repeat disk-a disk-b]} (products/products)]
    (is (= (dissoc final :temporal-receipt) (dissoc absent :temporal-receipt)))
    (is (= final disk-a (:record a) (:record repeat) (:record event-repeat)))
    (is (= absent disk-b (:record b)))
    (is (= [:published :absent] (mapv #(get-in % [:temporal-receipt :status]) [disk-a disk-b])))
    (is (= (:temporal-posterior disk-a) (:temporal-posterior disk-b)))
    (is (= (:temporal-receipt disk-a) (dissoc (:receipt a) :record-path :digest)))
    (is (= (:temporal-receipt disk-b) (dissoc (:receipt b) :record-path :digest)))
    (is (not= (get-in a [:receipt :digest]) (get-in b [:receipt :digest])))
    (is (= :temporal-record-already-exists (get-in repeat [:receipt :reason])))
    (is (= :event-already-consumed (get-in event-repeat [:receipt :reason])))
    (println :write-once-receipts (pr-str (mapv :temporal-receipt [disk-a disk-b]))
             :second-write (get-in repeat [:receipt :reason])
             :replayed-event (get-in event-repeat [:receipt :reason]))))
