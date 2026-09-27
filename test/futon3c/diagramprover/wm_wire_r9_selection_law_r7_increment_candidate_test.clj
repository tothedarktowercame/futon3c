(ns futon3c.diagramprover.wm-wire-r9-selection-law-r7-increment-candidate-test
  (:require [futon3c.diagramprover.wm-wire-selection-handoff-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-selection-out-support :as support]))
(def positive (delay (support/observe :candidate-increment :none)))
(defn check [] @positive)
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r9-selection-law-r7-increment-candidate-test/changed-selection-product :kind :record
                   :product [:receipt :record-id] :intervention :before-reader}
   :wire [:r9-selection-law :r7-increment :candidate]
           :kind :witnessed-hermetically :test `the-real-reader-handoff :check check
           :live-records-read support/live-records-read
           :note "Real selector candidate into increment record-id. Policy-key is a separate argument and stays unchanged; a missing candidate can still count delta 1 with passing W_c, source unfixed."})
(deftest the-real-reader-handoff
  (is (seq (support/census)))
  (let [o (check)] (is (w/received? o))
    (is (= 1 (get-in o [:result :delta])))))
(deftest absence-before-reader
  (let [o (support/observe :candidate-increment :absent)]
    (is (not (w/received? o)))
    (is (nil? (:reader o)))
    (is (= 1 (get-in o [:result :delta])))))
(deftest different-value-before-reader
  (let [o (support/observe :candidate-increment :different)]
    (is (not (w/received? o)))
    (is (= :different-candidate (:reader o)))
    (is (= (get-in (check) [:result :policy-key]) (get-in o [:result :policy-key])))))

(deftest changed-selection-product
  (let [a (products/increment-product false) b (products/increment-product true)]
    (prn :wire-2l-5b :increment :before a :after b)
    (is (= (:other-inputs a) (:other-inputs b)))
    (is (not= (:candidate a) (:candidate b)))
    (is (not= (:posterior a) (:posterior b)))
    (is (= (:candidate a) (get-in a [:receipt :record-id 1])))
    (is (= (:candidate b) (get-in b [:receipt :record-id 1])))
    ;; enactment_habit.clj:74: candidate is the record-id component only.
    (is (= 1 (get-in a [:receipt :delta]) (get-in b [:receipt :delta])))
    (is (= (dissoc (:receipt a) :record-id) (dissoc (:receipt b) :record-id)))))
