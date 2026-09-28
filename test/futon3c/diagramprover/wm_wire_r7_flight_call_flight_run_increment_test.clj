(ns futon3c.diagramprover.wm-wire-r7-flight-call-flight-run-increment-test
  (:require [clojure.test :refer [deftest is testing]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn" :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
    :why "its one enactment is {:absent :no-dispatch-configured}: no wc call ran, no :increment on the entry; the writer's return was never persisted apart from the reader's copy"}
   {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/flight-ada87008.edn" :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"
    :why "same: no :increment or :publication-observed on any enactments entry in any spike flight record"}])
(def producer (delay (producer-record/record "publication-increment-observe")))
(defn- fields [] (:fields @producer))
(defn check [] (select-keys (fields) [:writer :reader]))
(def wire {:wire [:r7-flight-call :flight-run :increment]
           :second-layer {:test `policy-key-selects-the-continued-posterior :kind :value-varying :product [:enactments 1 :step :p-o] :intervention :before-reader}
           :kind :witnessed-hermetically :test `the-writers-increment-reaches-the-enactments-entry :check check :live-records-read live-records-read})
(deftest the-writers-increment-reaches-the-enactments-entry
  (let [f (fields) o (check)] (is (:writer-present? f)) (is (false? (:writer-typed-absence? f))) (is (:record-id? f)) (is (:delta-present? f)) (is (w/received? o))))
(deftest a-typed-absence-at-the-field-does-not-witness-the-wire (let [a (:absent (fields))] (is (= {:absent :not-carried} (:reader a))) (is (false? (:received? a)))))
(deftest a-different-increment-than-the-writers-does-not-witness-the-wire (let [d (:different (fields))] (is (:reader-present? d)) (is (false? (:typed? d))) (is (false? (:received? d)))))
(deftest the-live-records-carry-no-increment (doseq [[k v] (:live (fields))] (testing (name k) (is (true? v)))))
(deftest increment-policy-key-is-carried-into-the-step (doseq [[k v] (:conditioning (fields))] (testing (name k) (is (true? v)))))
(deftest policy-key-selects-the-continued-posterior (doseq [[k v] (:continued (fields))] (testing (name k) (is (true? v)))))
