(ns futon3c.diagramprover.wm-wire-prediction-error-weighted-error-test
  "Wire [:r3a-prediction-error :r7-weighted-error [:error {:record :prediction-error}]]:
  the prediction-error record's :error reaching the R7 re-weighting.

  The writer is free-energy/compute-prediction-error: its :present record
  carries :error (observed − predicted-mean), scoped to the
  :prediction-error record. The reader is precision/weighted-error, which
  reads (:error error-map 0.0) — error-map aliased to :prediction-error —
  and returns the record with :precision and :weighted-error replaced by
  the R7 values, :error carried through unchanged.

  No live record carries either end: the run records under the spike
  directory carry no prediction-error record at all (see
  live-records-read). So the wire is WITNESSED-HERMETICALLY:
  compute-prediction-error's :present record is handed to
  precision/weighted-error; the writer's value is the record's :error, the
  reader's value is the :error on the map the reader returns, and the
  reader's :weighted-error = :error × :precision shows the read happened."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.free-energy :as fe]
            [futon2.aif.precision :as precision]
            [futon3c.diagramprover.wm-wire :as w]))

(defn- present-record [observed]
  (fe/compute-prediction-error observed {:mean 0.5 :variance 0.2}))

(defn observe
  "compute-prediction-error's record through precision/weighted-error.
  REC-FN rewrites the record between writer and reader (the bad cases).
  {:writer the record's :error, :reader the returned map's :error (nil when
  the reader's record carries no :error)}."
  ([] (observe identity))
  ([rec-fn]
   (let [rec (rec-fn (present-record 0.8))
         wmap (precision/weighted-error {} :annotation-health rec)]
     {:writer (:error (present-record 0.8))
      :reader (:error wmap)
      :weighted-error (:weighted-error wmap)
      :precision (:precision wmap)})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "carries no prediction-error record: no :prediction-errors, no :weighted-error, no :per-call-precision anywhere in the record"}
     {:path (p "tick-run-record-2026-09-26-flight-7f89646a-click-1.edn")
      :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
      :why "the same: no prediction-error record"}
     {:path (p "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn")
      :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
      :why "the same: no prediction-error record"}
     {:paths ["holes/labs/M-futon-seams/exemplar/click-001.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-outcome.edn"]
      :why "hand-authored exemplar records; no prediction-error record"}]))

(def wire
  {:wire [:r3a-prediction-error :r7-weighted-error [:error {:record :prediction-error}]]
   :kind :witnessed-hermetically
   :test `the-prediction-error-reaches-the-reweighting
   :check check
   :live-records-read live-records-read})

(deftest the-prediction-error-reaches-the-reweighting
  (let [o (check)]
    (is (= 0.30000000000000004 (:writer o)) "0.8 − 0.5, the writer's :error")
    (is (= (* (:writer o) (:precision o)) (:weighted-error o))
        "the reader computed :weighted-error from the :error it read")
    (is (w/received? o))))

(deftest an-absent-observation-carries-no-error-and-fails-the-wire
  ;; the writer's :absent branch (observation not taken) has no :error;
  ;; the reader's own default (:error error-map 0.0) yields 0.0, and the
  ;; returned map carries no :error — the reader has no value under the field
  (let [o (observe (fn [_] (fe/compute-prediction-error
                            nil {:mean 0.5 :variance 0.2}
                            {:observation-status {:reason :no-observation}})))]
    (is (nil? (:reader o)))
    (is (= 0.0 (:weighted-error o)) "the reader's documented 0.0 default for a missing :error")
    (is (not (w/received? o)))))

(deftest a-different-error-fails-the-wire
  ;; a second real record (observed 0.9): present, not absent, but not the
  ;; writer's :error
  (let [o (observe (fn [_] (present-record 0.9)))]
    (is (= 0.4 (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-prediction-error
  (doseq [{:keys [path sha256]} (filter :path live-records-read)]
    (is (= sha256 (w/sha256-file path)) path)))
