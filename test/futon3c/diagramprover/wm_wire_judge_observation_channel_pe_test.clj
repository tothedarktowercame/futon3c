(ns futon3c.diagramprover.wm-wire-judge-observation-channel-pe-test
  "Wire [:r2-judge-observation :r3a-channel-prediction-error :tick-observation]:
  the tick's observation reaching the channel prediction error.

  The writer is war-machine/judge's observation, (obs/observe scan-data),
  passed as the first argument of fe/channel-prediction-error (the map's
  :passes names obs/observe's return). The reader is
  free-energy/channel-prediction-error, which reads the channel through the
  observation envelope (channel-source-status) and, when the channel was
  observed, reports the value it read as the record's :observed.

  No live record carries either end: the run records under the spike
  directory carry no channel-prediction record and no observation envelope
  for the judge's observation (the one :observation on the pinned record,
  under the token-belief stage, is a status envelope, not the channel map;
  see live-records-read). So the wire is WITNESSED-HERMETICALLY:
  observation/observe is called over a fixture scan and its result handed to
  free-energy/channel-prediction-error; the writer's value is the
  observation's :annotation-health, the reader's value is the :observed the
  reader's record carries (the record itself when it is a typed absence)."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.free-energy :as fe]
            [futon2.aif.observation :as obs]
            [futon3c.diagramprover.wm-wire :as w]))

(defn- scan [health] {:annotation-graph {:health health}})

(defn observe
  "obs/observe's observation through fe/channel-prediction-error for the
  :annotation-health channel. OBS-FN rewrites the observation between writer
  and reader (the bad cases). {:writer the observation's channel value,
  :reader the record's :observed (the record when a typed absence)}."
  ([] (observe identity))
  ([obs-fn]
   (let [o (obs-fn (obs/observe (scan 0.7)))
         r (fe/channel-prediction-error o :annotation-health {:mean 0.5 :variance 0.2})]
     {:writer (get o :annotation-health)
      :reader (if (w/typed-absence? r) r (:observed r))
      :record r})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "carries neither end: no channel-prediction record (no :prediction-errors, no :observed) and no judge observation; the one :observation key, under [:decision :selection-certificate :token-belief-stage], is a status envelope {:status ... :reason ...}, not the channel map"}
     {:path (p "tick-run-record-2026-09-26-flight-7f89646a-click-1.edn")
      :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
      :why "the same: no channel-prediction record, no judge observation"}
     {:paths ["holes/labs/M-futon-seams/exemplar/click-001.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-outcome.edn"]
      :why "hand-authored exemplar records; no observation, no prediction error"}]))

(def wire
  {:wire [:r2-judge-observation :r3a-channel-prediction-error :tick-observation]
   :kind :witnessed-hermetically
   :test `the-tick-observation-reaches-the-channel-prediction-error
   :check check
   :live-records-read live-records-read})

(deftest the-tick-observation-reaches-the-channel-prediction-error
  (let [o (check)]
    (is (= 0.7 (:writer o)) "obs/observe projected the fixture's annotation health")
    (is (= :present (get-in o [:record :status])))
    (is (w/received? o) "the reader's :observed is the writer's channel value")))

(deftest an-unobserved-channel-is-a-typed-absence-and-fails-the-wire
  (let [o (observe #(dissoc % :annotation-health))]
    (is (= :absent (get-in o [:record :status])) (pr-str (:record o)))
    (is (= :absent (:status (:reader o))))
    (is (not (w/received? o)))))

(deftest a-different-observation-fails-the-wire
  ;; a second real observation (health 0.5) handed to the reader: present,
  ;; not absent, but not the writer's 0.7
  (let [writer-obs (obs/observe (scan 0.7))
        r (fe/channel-prediction-error (obs/observe (scan 0.5))
                                       :annotation-health {:mean 0.5 :variance 0.2})
        o {:writer (get writer-obs :annotation-health)
           :reader (if (w/typed-absence? r) r (:observed r))}]
    (is (= 0.5 (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-pinned-record-carries-neither-end
  (doseq [{:keys [path sha256]} (filter :path live-records-read)]
    (is (= sha256 (w/sha256-file path)) path))
  (let [r (w/read-record (:path (first live-records-read)))
        obs (get-in r [:decision :selection-certificate :token-belief-stage :observation])]
    (is (contains? obs :status) "a status envelope, not the judge's channel observation")
    (is (not (contains? obs :annotation-health)))))
