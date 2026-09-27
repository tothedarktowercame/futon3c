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
  cascade-problems/assemble, read by the real measured-a-version."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-publication-support :as support]))

(def live-records-read
  [(assoc support/tick-278b6988
          :why "carries the writer's end ([:decision :selection-certificate :candidate-derivations … :observation-locators], the store's locators keyed [target token]) but no [:decision :measured-a]: the reader's end was never persisted")
   (assoc support/tick-e70b4baf
          :why "same: :observation-locators present on the decision, :measured-a absent everywhere")])

(defn check [] (support/locators-observe identity))

(def wire
  {:wire [:r2-store-locators :r9-measured-a-version :locators]
   :kind :witnessed-hermetically
   :test `the-stores-locator-reaches-the-reader
   :check check
   :live-records-read live-records-read})

(deftest the-stores-locator-reaches-the-reader
  (let [{:keys [writer classes measurement] :as o} (check)]
    (is (= support/flight-source-locator (get writer support/locator-token)))
    (is (some #{:C3} classes) "the reader's own product names the locator's class for the token")
    (is (pos? (get-in measurement [:false-neg :denominator] 0))
        "and measured the token through it")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-field-does-not-witness-the-wire
  (let [o (support/locators-observe
           (fn [p] (assoc-in p [:cascade-problem :locators] {:absent :not-carried})))]
    (is (= {:absent :not-carried} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-locator-than-the-writers-does-not-witness-the-wire
  (let [o (support/locators-observe
           (fn [p] (assoc-in p [:cascade-problem :locators support/locator-token :path]
                             "src/futon2/aif/flight_runner.clj")))]
    (is (some? (:reader o)))
    (is (not (w/typed-absence? (:reader o))))
    (is (not (w/received? o)) "present, not absent, but not the locator the writer published")))

(deftest the-live-records-carry-only-the-writers-end
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (let [record (w/read-record (:path (first live-records-read)))]
    (is (some #(and (map? %) (contains? % :observation-locators))
              (tree-seq coll? seq record))
        "the writer's end is on the record")
    (is (not-any? #(and (map? %) (contains? % :measured-a))
                  (tree-seq coll? seq record))
        "the reader's end is not")))
