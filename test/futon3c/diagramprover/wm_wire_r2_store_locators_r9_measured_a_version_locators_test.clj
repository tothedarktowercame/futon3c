(ns futon3c.diagramprover.wm-wire-r2-store-locators-r9-measured-a-version-locators-test
  "Wire [:r2-store-locators :r9-measured-a-version :locators]: a locator
  published into the store by mission-reading/publish-locator! reaching
  measured-a-version, which reads it as the assembled problem's
  [:cascade-problem :locators].

  No live record carries both ends: the tick records under spike/ carry
  [:decision :selection-certificate :candidate-derivations …
  :observation-locators] (the store's locators, keyed [target token] — the
  writer's end) but no [:decision :measured-a] at all (the reader's end
  was never persisted), so no single record pins both.
  WITNESSED-HERMETICALLY: a real locator published into a temp store by
  the real writer chain (issue! → validate-locator with the real C3
  observation → publish-locator!), assembled by the real
  cascade-problems/assemble, read by the real measured-a-version.

  The values are read from the producer record `publication-locators-observe`."
  (:require [clojure.test :refer [deftest is testing]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def producer (delay (producer-record/record "publication-locators-observe")))
(defn- fields [] (:fields @producer))
(defn check [] (select-keys (fields) [:writer :reader]))
(def wire {:wire [:r2-store-locators :r9-measured-a-version :locators] :kind :witnessed-hermetically
           :test `the-stores-locator-reaches-the-reader :check check :live-records-read []})
(deftest the-stores-locator-reaches-the-reader
  (let [f (fields) o (check)] (is (:writer-present? f)) (is (false? (:writer-typed-absence? f)))
    (is (:published-locator? f)) (is (:class-c3? f)) (is (:measured? f)) (is (w/received? o) (pr-str o))))
(deftest a-typed-absence-at-the-field-does-not-witness-the-wire
  (let [a (:absent (fields))] (is (= {:absent :not-carried} (:reader a))) (is (false? (:received? a)))))
(deftest a-different-locator-than-the-writers-does-not-witness-the-wire
  (let [d (:different (fields))] (is (:reader-present? d)) (is (false? (:reader-typed-absence? d))) (is (false? (:received? d)))))
(deftest the-live-records-carry-only-the-writers-end
  (doseq [[k v] (:live (fields))] (testing (name k) (is (true? v)))))
