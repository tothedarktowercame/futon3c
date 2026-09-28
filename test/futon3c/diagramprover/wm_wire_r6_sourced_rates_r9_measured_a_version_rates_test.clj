(ns futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-rates-test
  (:require [clojure.test :refer [deftest is testing]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def producer (delay (producer-record/record "rates-products-measured-record")))
(defn- fields [] (:fields @producer))
(defn check [] (select-keys (fields) [:writer :reader]))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-rates-test/reader-record-retains-the-intervened-carrier :kind :record :product [:rates] :intervention :before-reader}
           :wire [:r6-sourced-rates :r9-measured-a-version [:rates {:record :sourced-rates}]] :kind :witnessed-hermetically
           :test `the-sourced-rates-reach-measured-a-version :check check :live-records-read []})
(deftest the-sourced-rates-reach-measured-a-version
  (let [f (fields) o (check)] (is (:writer-present? f)) (is (false? (:writer-typed-absence? f)))
    (is (:writer-rate? f)) (is (:schema? f)) (is (w/received? o) (pr-str o))))
(deftest absent-rates-before-reader
  (doseq [[k v] (:absent (fields))] (testing (name k) (is (if (= k :received?) (false? v) (true? v))))))
(deftest different-rates-before-reader
  (doseq [[k v] (:different (fields))] (testing (name k) (is (if (= k :received?) (false? v) (true? v))))))
(deftest live-records-lack-both-ends (is (:live-records-lack? (fields))))
(deftest reader-record-retains-the-intervened-carrier
  (doseq [[k v] (:second-layer (fields))] (testing (name k) (is (true? v)))))
