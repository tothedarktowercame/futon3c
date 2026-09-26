(ns futon3c.diagramprover.wm-wire-weighted-error-aggregate-driver-test
  "Wire [:r7-weighted-error :r3-aggregate-driver :weighted-error]: the R7
  precision-weighted error reaching the R3d driver aggregation.

  The writer is precision/weighted-error, whose returned map carries
  :weighted-error (:error × the history-tracked precision). The reader is
  belief/r3d-aggregate-driver, which reads (:weighted-error %) off each
  channel's entry (classify-channel-entry: a finite :weighted-error makes
  the channel contributing). With the multichannel flag OFF (the production
  default, bound explicitly here) the driver is the :annotation-health
  weighted-error alone — on a present, finite entry byte-identical to it —
  so the driver record's :driver is the value the reader read.

  No live record carries either end: the run records under the spike
  directory carry no prediction-error record and no driver record (see
  live-records-read). So the wire is WITNESSED-HERMETICALLY: a real
  compute-prediction-error record re-weighted by precision/weighted-error
  is handed to belief/r3d-aggregate-driver; the writer's value is the
  re-weighted map's :weighted-error, the reader's value is the driver
  record's :driver (nil when no channel contributed, the reader's typed
  :unknown)."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.belief :as belief]
            [futon2.aif.free-energy :as fe]
            [futon2.aif.precision :as precision]
            [futon3c.diagramprover.wm-wire :as w]))

(defn- weighted [observed]
  (precision/weighted-error {} :annotation-health
                            (fe/compute-prediction-error observed {:mean 0.5 :variance 0.2})))

(defn observe
  "precision/weighted-error's map through belief/r3d-aggregate-driver.
  ENTRY-FN rewrites the weighted-errors map between writer and reader (the
  bad cases). {:writer the map's :weighted-error, :reader the driver
  record's :driver (nil on the reader's :unknown)}."
  ([] (observe identity))
  ([entry-fn]
   (let [wmap (weighted 0.8)
         dr (binding [belief/*r3d-multichannel?* false]
              (belief/r3d-aggregate-driver (entry-fn {:annotation-health wmap})))]
     {:writer (:weighted-error wmap)
      :reader (:driver dr)
      :driver-record dr})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "carries neither end: no re-weighted error map (no :weighted-error, no :per-call-precision) and no driver record (:driver absent)"}
     {:path (p "tick-run-record-2026-09-26-flight-7f89646a-click-1.edn")
      :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
      :why "the same: no re-weighted error map, no driver record"}
     {:path (p "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn")
      :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
      :why "the same: no re-weighted error map, no driver record"}
     {:paths ["holes/labs/M-futon-seams/exemplar/click-001.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-outcome.edn"]
      :why "hand-authored exemplar records; no weighted error, no driver record"}]))

(def wire
  {:wire [:r7-weighted-error :r3-aggregate-driver :weighted-error]
   :kind :witnessed-hermetically
   :test `the-weighted-error-reaches-the-driver-aggregation
   :check check
   :live-records-read live-records-read})

(deftest the-weighted-error-reaches-the-driver-aggregation
  (let [o (check)]
    (is (= 0.30000000000000004 (:writer o)) "(0.8 − 0.5) × the empty state's precision 1.0")
    (is (= :present (get-in o [:driver-record :status])) (pr-str (:driver-record o)))
    (is (= :annotation-health-only (get-in o [:driver-record :mode])))
    (is (w/received? o)
        "the single-channel driver is the writer's weighted-error, byte-identical")))

(deftest an-absent-channel-contributes-no-driver-and-fails-the-wire
  ;; the writer's entry replaced by an upstream typed absence: the reader
  ;; omits the channel, the record is :unknown, and no driver value exists
  (let [absent (fe/compute-prediction-error nil {:mean 0.5 :variance 0.2}
                                            {:observation-status {:reason :no-observation}})
        o (observe (fn [_] {:annotation-health absent}))]
    (is (= :absent (:status absent)))
    (is (= :unknown (get-in o [:driver-record :status])) (pr-str (:driver-record o)))
    (is (nil? (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-weighted-error-fails-the-wire
  ;; a second real weighted error (observed 0.9): present, not absent, but
  ;; not the writer's
  (let [o (observe (fn [_] {:annotation-health (weighted 0.9)}))]
    (is (= 0.4 (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-neither-end
  (doseq [{:keys [path sha256]} (filter :path live-records-read)]
    (is (= sha256 (w/sha256-file path)) path)))
