(ns futon3c.diagramprover.wm-wire-run-record-publication-r10-observe-publication-repair-publication-test
  "Wire [:run-record-publication :r10-observe-publication
  :repair/publication]: the :repair/publication that
  full-loop-runner/persist-run-record! writes on the run record reaching
  flight-runner/observe-publication-fn, which reads
  (:repair/publication record) off the click's run record (typed
  {:absent :no-publication-observation-source} when it is not a sequence).

  No live record carries both ends: the tick run records under spike/
  carry :repair/publication (the writer's end), but every flight record's
  one enactment is a typed absence, so no [:enactments
  i :publication-observed] derived from it exists anywhere — the reader's
  product is not persisted. WITNESSED-HERMETICALLY: a real run record
  written by the real persist-run-record! into a temp dir, read by the
  real observe-publication-fn."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-publication-support :as support]))

(def live-records-read
  [(assoc support/tick-278b6988
          :why "carries the writer's end (:repair/publication on the run record) but the reader's product is not persisted on it")
   (assoc support/flight-278b6988
          :why "its one enactment is {:absent :no-dispatch-configured}: no [:enactments i :publication-observed] exists on any spike flight record, so the reader's end is not persisted")])

(defn check [] (support/publication-observe identity))

(def wire
  {:wire [:run-record-publication :r10-observe-publication :repair/publication]
   :kind :witnessed-hermetically
   :test `the-run-records-publication-reaches-the-reader
   :check check
   :live-records-read live-records-read})

(deftest the-run-records-publication-reaches-the-reader
  (let [{:keys [writer observation] :as o} (check)]
    (is (= support/publication-entries writer)
        "persist-run-record! carried the entries verbatim")
    (is (true? (:observed observation)))
    (is (= "occ-wire" (get-in observation [:evidence :repair/id]))
        "the reader's own product is derived from the entries it received")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-field-does-not-witness-the-wire
  (let [o (support/publication-observe #(dissoc % :repair/publication))]
    (is (nil? (:reader o)))
    (is (= :no-publication-observation-source (:absent (:observation o)))
        "the reader types the absence itself")
    (is (not (w/received? o)))))

(deftest a-different-publication-than-the-writers-does-not-witness-the-wire
  (let [o (support/publication-observe
           #(assoc % :repair/publication
                   [{:status :publication-refused :repair/id "occ-wire" :reason :publication-error}]))]
    (is (some? (:reader o)))
    (is (false? (get-in o [:observation :observed])))
    (is (not (w/received? o)) "present, not absent, but not the entries the writer wrote")))

(deftest the-live-records-carry-only-the-writers-end
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (let [record (w/read-record (:path (first live-records-read)))]
    (is (sequential? (:repair/publication record)) "the writer's end is on the run record"))
  (let [f (:flight (w/read-record (:path (second live-records-read))))]
    (is (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments f)))
        "no enactment entry, so no publication-observed derived from a run record")))
