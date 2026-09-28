(ns futon3c.diagramprover.wm-wire-r9-selection-law-r7-increment-candidate-test
  (:require [clojure.test :refer [deftest is testing]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:r9-selection-law :r7-increment :candidate])
(def producer (delay (producer-record/record "selection-out-observe")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:note "Real selector candidate into increment record-id. Policy-key is a separate argument and stays unchanged; a missing candidate can still count delta 1 with passing W_c, source unfixed. The values are read from the producer record `selection-out-observe`."
           :second-layer {:test `changed-selection-product :kind :record :product [:receipt :record-id]
                          :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically :test `the-real-reader-handoff
           :check check :live-records-read []})
(deftest the-real-reader-handoff
  (is (seq (:census @producer)))
  (let [o (check)] (is (w/received? o) (str "writer-reader " (pr-str o))) (is (= 1 (:delta o)))))
(deftest absence-before-reader
  (let [o (get-in (fields) [:interventions :absent])]
    (is (not (w/received? o))) (is (nil? (:reader o))) (is (= 1 (:delta o)))))
(deftest different-value-before-reader
  (let [o (get-in (fields) [:interventions :different])]
    (is (not (w/received? o))) (is (= :different-candidate (:reader o)))
    (is (= (:policy-key (check)) (:policy-key o)))))
(deftest changed-selection-product
  (doseq [[k v] (dissoc (:second-layer (fields)) :before :after)]
    (testing (name k) (is (true? v) (str k " relation failed"))))
  (let [r (:second-layer (fields))]
    (is (not= (get-in r [:before :candidate]) (get-in r [:after :candidate])))
    (is (= (get-in r [:before :candidate]) (get-in r [:before :receipt-record-id 1])))
    (is (= (get-in r [:after :candidate]) (get-in r [:after :receipt-record-id 1])))))
