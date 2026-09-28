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
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def wire-id [:r10-observe-publication :r0-enact-step :publication-observed])
(def producer (delay (producer-record/record "publication-enact-observe")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(def live-records-read [])
(defn check [] (:primary (fields)))

(def wire
  {:wire wire-id
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
  (let [o (get-in (fields) [:interventions :absent])]
    (is (= {:absent :not-carried} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-observation-than-the-writers-does-not-witness-the-wire
  (let [o (get-in (fields) [:interventions :different])]
    (is (some? (:reader o)))
    (is (not (w/typed-absence? (:reader o))))
    (is (not (w/received? o)) "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-observation (is (map? @producer)))
