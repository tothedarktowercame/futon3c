(ns futon3c.diagramprover.wm-wire-temporal-write-once-temporal-read-receipt-record-path-test
  (:require [futon3c.diagramprover.wm-wire-temporal-storage-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-temporal-courier-support :as support]))

(def wire-id [:temporal-write-once :temporal-read-receipt [:record-path {:record :temporal-publication}]])
(defn check [] (support/observe wire-id :none))
(def wire {:wire wire-id :second-layer {:test `temporal-storage-reader-product :kind :refusal
                          :product [:reason] :intervention :before-reader :expected :temporal-record-unreadable}
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
  (let [{:keys [receipt missing-path wrong-path good missing-result wrong-result other-result]} (products/products)]
    (is (= (dissoc receipt :record-path) (dissoc missing-path :record-path)
           (dissoc wrong-path :record-path)))
    (is (= :posterior (:basis good)))
    (is (= :temporal-record-unreadable (:reason missing-result)))
    (is (= :temporal-record-digest-mismatch (:reason wrong-result)))
    (is (= :no-previous-posterior (:reason other-result)))
    (doseq [r [missing-result wrong-result other-result]]
      (is (= :absent (:status r)))
      (is (nil? (:record r)))
      (is (nil? (:basis r))))
    (println :path-read [:posterior (:reason missing-result) (:reason wrong-result)]
             :other-file-with-own-receipt (:reason other-result))))
