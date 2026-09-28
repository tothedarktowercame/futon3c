(ns futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-measurement-test
  (:require [clojure.test :refer [deftest is testing]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def producer (delay (producer-record/record "rates-products-measured-product")))
(defn- fields [] (:fields @producer))
(defn check [] (select-keys (fields) [:writer :reader]))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-measurement-test/reader-product-changes-at-the-carrier :kind :refusal :product [:reason] :intervention :before-reader :expected :no-measured-rates}
           :wire [:r6-sourced-rates :r9-measured-a-version [:measurement {:record :sourced-rates}]] :kind :witnessed-hermetically
           :test `the-writers-measurement-reaches-the-reader :check check :live-records-read []})
(deftest the-writers-measurement-reaches-the-reader
  (let [f (fields) o (check)] (is (:writer-present? f)) (is (false? (:writer-typed-absence? f)))
    (is (:schema? f)) (is (:false-neg-measured? f)) (is (:false-pos-measured? f)) (is (w/received? o) (pr-str o))))
(deftest a-typed-absence-at-the-field-does-not-witness-the-wire
  (let [a (:absent (fields))] (is (= {:absent :not-carried} (:reader a))) (is (false? (:received? a)))))
(deftest a-different-measurement-than-the-writers-does-not-witness-the-wire
  (let [d (:different (fields))] (is (:reader-present? d)) (is (false? (:reader-typed-absence? d))) (is (false? (:received? d)))))
(deftest the-live-records-carry-neither-end
  (doseq [[k v] (:live (fields))] (testing (name k) (is (true? v)))))
(deftest reader-product-changes-at-the-carrier
  (doseq [[k v] (:second-layer (fields))] (testing (name k) (is (true? v)))))
