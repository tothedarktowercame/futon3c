(ns futon3c.diagramprover.wm-wire-temporal-finalize-temporal-envelope-temporal-cursor-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def wire-id [:temporal-finalize :temporal-envelope [:temporal-cursor {:record :enactment}]])
(def producer (delay (producer-record/record "temporal-courier")))
(defn- wire-fields [] (get-in @producer [:fields :wires wire-id]))
(defn check []
  (let [fields (wire-fields)]
    {:writer (:writer fields) :reader (:reader fields)
      :product (when (:product-present? fields) {:recorded true})}))
(def wire {:wire wire-id :kind :witnessed-hermetically
           :test `real-courier-reaches-reader :check check
           :second-layer {:test `envelope-product-under-intervention :kind :record
                          :product [:envelope] :intervention :before-reader}
           :live-records-read []
           :note "MAP-2B-TEMPORAL: real writer and reader with isolated publication; no live temporal record claimed. The values are read from the producer record `temporal-courier`."})

(deftest real-courier-reaches-reader
  (let [r (check)]
    (is (some? (:writer r)) "writer")
    (is (some? (:product r)) "product")
    (is (w/received? r) (str "writer-reader " (pr-str r)))))

(deftest carrier-intervention-is-detected
  (doseq [mode [:absent :different]
          :let [result (get-in (wire-fields) [:interventions mode])]]
    (testing (name mode)
      (is (:writer-present? result) (str mode " writer-present?"))
      (is (false? (:received? result)) (str mode " received?")))))

(deftest historical-records-have-no-temporal-pair
  (is (true? (get-in @producer [:fields :live-absent?]))))

(deftest envelope-product-under-intervention
  (doseq [[field passed?] (get-in @producer [:fields :second-layer wire-id])]
    (testing (name field)
      (is (true? passed?) (str field " relation failed")))))
