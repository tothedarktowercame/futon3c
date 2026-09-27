(ns futon3c.diagramprover.wm-wire-r6-sourced-rates-r9-measured-a-version-measurement-test
  "Wire [:r6-sourced-rates :r9-measured-a-version [:measurement {:record :sourced-rates}]]: the
  measurement provenance on sourced-rates' :sourced return reaching
  measured-a-version's own :measurement, keyed [target token].

  No live record carries either end: the tick records under spike/ carry
  no :measured-a at all (they predate measured-A persistence — the brief's
  expectation of [:decision :measured-a :measurement] on the tick records
  is stale), and sourced-rates' return is never persisted separately.
  live-records-read names the records read, each pinned. So the wire is
  WITNESSED-HERMETICALLY: the real sourced-rates driven through the real
  measured-a-version over admitted labels, as
  flight_conditioning_step_test's produced-measured-a does."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-publication-support :as support]))

(def live-records-read
  [(assoc support/tick-278b6988
          :why "carries no :measured-a anywhere (grep count 0): the tick predates measured-A persistence, so the reader's end is absent; the writer's sourced-rates return is never persisted")
   (assoc support/tick-e70b4baf
          :why "same: no :measured-a on the tick record; neither end of this wire is persisted on any spike record")])

(defn check [] (support/measurement-observe identity))

(def wire
  {:wire [:r6-sourced-rates :r9-measured-a-version [:measurement {:record :sourced-rates}]]
   :kind :witnessed-hermetically
   :test `the-writers-measurement-reaches-the-reader
   :check check
   :live-records-read live-records-read})

(deftest the-writers-measurement-reaches-the-reader
  (let [{:keys [writer measured-a] :as o} (check)]
    (is (= :wm/measured-a-v1 (:schema measured-a)))
    (is (pos? (get-in writer [:false-neg :denominator] 0)))
    (is (pos? (get-in writer [:false-pos :denominator] 0))
        "both cells measured, never an :absent default")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-field-does-not-witness-the-wire
  (let [o (support/measurement-observe
           (fn [m] (assoc m support/measurement-token {:absent :not-carried})))]
    (is (= {:absent :not-carried} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-measurement-than-the-writers-does-not-witness-the-wire
  (let [o (support/measurement-observe
           (fn [m] (update-in m [support/measurement-token :false-neg :numerator] inc)))]
    (is (some? (:reader o)))
    (is (not (w/typed-absence? (:reader o))))
    (is (not (w/received? o)) "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-neither-end
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (doseq [{:keys [path]} live-records-read]
    (is (not-any? #(and (map? %) (contains? % :measured-a))
                  (tree-seq coll? seq (w/read-record path)))
        (str path " has no [:decision :measured-a]"))))
