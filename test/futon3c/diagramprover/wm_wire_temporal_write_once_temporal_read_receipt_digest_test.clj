(ns futon3c.diagramprover.wm-wire-temporal-write-once-temporal-read-receipt-digest-test
  (:require [futon3c.diagramprover.wm-wire-temporal-storage-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as support]))

(def wire-id [:temporal-write-once :temporal-read-receipt [:digest {:record :temporal-publication}]])
(defn check [] (support/observe wire-id :none))
(def wire {:wire wire-id :second-layer {:test `temporal-storage-reader-product :kind :refusal
                          :product [:reason] :intervention :before-reader :expected :temporal-record-digest-mismatch}
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

(deftest temporal-storage-reader-product
  (let [{:keys [receipt bad-digest good digest-result]} (products/products)]
    (is (= (dissoc receipt :digest) (dissoc bad-digest :digest)))
    (is (= :posterior (:basis good)))
    (is (= :ok (get-in good [:record :status])))
    (is (= :absent (:status digest-result)))
    (is (= :temporal-record-digest-mismatch (:reason digest-result)))
    (is (nil? (:record digest-result)))
    (is (nil? (:basis digest-result)))
    (println :digest-read [:posterior (:reason digest-result)])))
