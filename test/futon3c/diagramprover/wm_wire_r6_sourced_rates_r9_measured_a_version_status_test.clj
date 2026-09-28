(ns futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-status-test
  "WIRE-23-C2 wire 2: [:r6-sourced-rates :r9-measured-a-version [:status {:record :sourced-rates}]].
  The REAL observation-rates/sourced-rates over ten admitted :C4 labels,
  reaching the REAL war-machine/measured-a-version exactly as
  flight-conditioning-step-test's produced-measured-a drives it; the
  producer's return is tampered by with-redefs around the real var. A
  :sourced status reads as the measured-A record; any other status reads as
  the typed absence {:status :absent :reason :sourcing-refused :refusals …}
  carrying the producer's status. WITNESSED-HERMETICALLY: no live record
  carries either end (support/measured-live-records-read).

  The values are read from the producer record `c2-measured-live-records-read`."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def producer (delay (producer-record/record "c2-measured-live-records-read")))
(defn- fields [] (:fields @producer))
(defn check [] (select-keys (fields) [:writer :reader]))
(def wire {:note "Writer: observation_rates.clj sourced-rates (:status :sourced). Reader: war_machine.clj measured-a-version (:6361): a :sourced status produces the measured-A record, anything else {:status :absent :reason :sourcing-refused}. Hermetic: the producer's return is persisted nowhere and no tick record here carries :measured-a. The values are read from the producer record `c2-measured-live-records-read`."
           :second-layer {:test 'futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-status-test/absent-status-before-reader :kind :refusal :product [:result :reason] :intervention :before-reader :expected :sourcing-refused}
           :wire [:r6-sourced-rates :r9-measured-a-version [:status {:record :sourced-rates}]]
           :kind :witnessed-hermetically :test `the-sourced-status-reaches-measured-a-version :check check
           :live-records-read []})
(deftest the-sourced-status-reaches-measured-a-version
  (let [f (fields) o (check)]
    (is (:writer-present? f)) (is (false? (:writer-typed-absence? f)))
    (is (= :sourced (:writer o))) (is (:schema? f)) (is (:rates-correct? f))
    (is (w/received? o) (pr-str o))))
(deftest absent-status-before-reader
  (doseq [[k v] (:absent (fields))]
    (testing (name k) (is (if (= k :received?) (false? v) (true? v))))))
(deftest different-status-before-reader
  (doseq [[k v] (:different (fields))]
    (testing (name k) (is (if (= k :received?) (false? v) (true? v))))))
(deftest live-records-lack-both-ends (is (:live-records-lack-measured-a? (fields))))
