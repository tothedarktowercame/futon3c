(ns futon3c.diagramprover.wm-wire-r10-observe-publication-r0-enact-step-publication-observed-test
  "Wire [:r10-observe-publication :r0-enact-step :publication-observed]:
  observe-publication-fn's {:publication-observed v} (flight_runner.clj
  :711-727) reaching enact-fn, which reads it at :881 and carries it onto
  the enactment record at :903.

  No live record carries either end: every flight record's one enactment
  is a typed absence, so no enactment record was ever written live, and
  the hand-authored exemplar enactment predates H-publish (it carries no
  :publication-observed). WITNESSED-HERMETICALLY: the real enact-fn built
  with no :publication-observation override, so its step-12 read is the
  real observe-publication-fn; the writer's value is captured by wrapping
  the real var (with-redefs around it), the reader's value is the
  enactment's :publication-observed."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-publication-support :as support]))

(def live-records-read
  [(assoc support/flight-278b6988
          :why "its one enactment is {:absent :no-dispatch-configured}: no enactment record was written, so no :publication-observed on either end")
   (assoc support/click-001-enactment
          :why "the hand-authored exemplar enactment (claude-10, 2026-09-24) predates H-publish: it carries no :publication-observed")])

(defn check [] (support/enact-observe identity))

(def wire
  {:wire [:r10-observe-publication :r0-enact-step :publication-observed]
   :kind :witnessed-hermetically
   :test `the-observation-reaches-the-enactment
   :check check
   :live-records-read live-records-read})

(deftest the-observation-reaches-the-enactment
  (let [{:keys [writer] :as o} (check)]
    (is (true? (:observed writer)))
    (is (= "occ-wire" (get-in writer [:evidence :repair/id])))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-field-does-not-witness-the-wire
  (let [o (support/enact-observe (constantly {:absent :not-carried}))]
    (is (= {:absent :not-carried} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-observation-than-the-writers-does-not-witness-the-wire
  (let [o (support/enact-observe (constantly {:observed false :checked {:repair/id "occ-wire"}}))]
    (is (some? (:reader o)))
    (is (not (w/typed-absence? (:reader o))))
    (is (not (w/received? o)) "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-observation
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (let [f (:flight (w/read-record (:path (first live-records-read))))]
    (is (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments f)))))
  (is (not (contains? (w/read-record (:path (second live-records-read))) :publication-observed))))
