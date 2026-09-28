(ns futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-status-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def producer (delay (producer-record/record "c2-measured-live-records-read")))
(defn- fields [] (:fields @producer))
(defn check [] (select-keys (fields) [:writer :reader]))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-status-test/absent-status-before-reader :kind :refusal :product [:result :reason] :intervention :before-reader :expected :sourcing-refused}
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
