(ns futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-rates-test
  "WIRE-23-C2 wire 3: [:r6-sourced-rates :r9-measured-a-version [:rates {:record :sourced-rates}]].
  Same harness as the status wire; the reader's produced value is the
  target-qualified rate [:rates [target :t]] on the measured-A record.
  WITNESSED-HERMETICALLY: no live record carries either end
  (support/measured-live-records-read).

  The values are read from the producer record `rates-products-measured-record`."
  (:require [clojure.test :refer [deftest is testing]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def producer (delay (producer-record/record "rates-products-measured-record")))
(defn- fields [] (:fields @producer))
(defn check [] (select-keys (fields) [:writer :reader]))
(def wire {:note "Writer: observation_rates.clj sourced-rates (:rates {token {:false-neg r :false-pos r}}). Reader: war_machine.clj measured-a-version qualifies them to [target token] on the measured-A record. Hermetic: the producer's return is persisted nowhere and no tick record here carries :measured-a. The values are read from the producer record `rates-products-measured-record`."
           :second-layer {:test 'futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-rates-test/reader-record-retains-the-intervened-carrier :kind :record :product [:rates] :intervention :before-reader}
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
