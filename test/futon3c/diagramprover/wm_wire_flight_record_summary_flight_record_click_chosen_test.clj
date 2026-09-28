(ns futon3c.diagramprover.wm-wire-flight-record-summary-flight-record-click-chosen-test
  "Wire [:flight-record-summary :flight-record-click :chosen]: the click's
  selection as record-summary reads it from the run record ([:decision
  :chosen], select-keys [:id :candidate :precedence], only when the
  chosen's target is the flight's) reaching the flight record's click
  entry (record-click keeps it: `(:chosen click) (assoc :chosen ...)`,
  WM-CAST-I 2).

  No live record carries both ends: the seventh flight's tick run record
  carries the writer's source ([:decision :chosen] with :candidate :C1)
  but its flight record's click entry predates WM-CAST-I 2 and has no
  :chosen; the eighth flight's run record chose nothing ([:decision
  :chosen] {:status :absent :reason :no-chosen-action}) and its click
  entry carries none (see live-records-read, each pinned). So the wire is
  WITNESSED-HERMETICALLY: a real select-action-cascades decision is run
  through full-loop-runner/run-opportunity! in hermetic stores (the r9
  wire's seam), and the run record it writes is read by record-summary
  (the writer's var) and kept by record-click (the reader's var). The values are read from the producer record."
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w] [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def live-records-read [{:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn" :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa" :why "the writer's source only: [:decision :chosen] is present with :candidate :C1; a run record is not the flight record, and the matching flight record's click entry predates WM-CAST-I 2"} {:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn" :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212" :why "the reader's end absent: the seventh flight's click entry has no :chosen (written before WM-CAST-I 2 kept it)"} {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn" :sha256 "df01831c24a7042d66b6ef2c38d82cdfbd0994a03b5539f3112db7dc41894970" :why "the eighth flight chose nothing: [:decision :chosen] {:status :absent :reason :no-chosen-action}"} {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/flight-ada87008.edn" :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de" :why "and its click entry accordingly carries no :chosen"}])
(defn data [] (:fields (producer-record/record "wm-wire-flight-record-summary-flight-record-click-chosen-tes")))
(defn check [] (:positive (data)))
(def wire {:wire [:flight-record-summary :flight-record-click :chosen] :second-layer {:test `summary-field-is-recorded-without-changing-progress :kind :record :product [:clicks 0 :chosen] :intervention :before-reader} :kind :witnessed-hermetically :test `the-clicks-selection-reaches-the-click-entry :check check :live-records-read live-records-read})
(deftest the-clicks-selection-reaches-the-click-entry (let [o (check)] (is (= :cas/b (get-in o [:record-chosen :candidate]))) (is (= (select-keys (:record-chosen o) [:id :candidate :precedence]) (:writer o))) (is (w/received? o))))
(deftest another-targets-selection-is-not-kept-and-fails-the-wire (let [o (:absent (data))] (is (some? (:record-chosen o))) (is (nil? (:writer o))) (is (= {:absent :field-not-carried} (:reader o))) (is (not (w/received? o)))) (is (not (w/received? (assoc (check) :reader {:absent :no-selection})))) (is (not (w/received? (assoc (check) :reader {:status :absent :reason :no-selection})))))
(deftest a-different-selection-fails-the-wire (let [o (check) other (:different (data))] (is (= :cas/a (get-in other [:writer :candidate]))) (is (not= (:writer o) (:reader other))) (is (not (w/received? (assoc o :reader (:reader other)))))))
(deftest the-live-records-carry-one-end-at-most (let [[run flight eighth-run eighth-flight] live-records-read] (doseq [{:keys [path sha256]} live-records-read] (is (= sha256 (w/sha256-file path)) path)) (is (= :C1 (get-in (w/read-record (:path run)) [:decision :chosen :candidate]))) (is (not-any? :chosen (:clicks (:flight (w/read-record (:path flight)))))) (is (= {:status :absent :reason :no-chosen-action} (get-in (w/read-record (:path eighth-run)) [:decision :chosen]))) (is (not-any? :chosen (:clicks (:flight (w/read-record (:path eighth-flight))))))))
(deftest summary-field-is-recorded-without-changing-progress (let [s (:summary (data))] (is (:carriers-without-field-equal? s)) (is (:a-copied? s)) (is (:b-copied? s)) (is (:values-differ? s)) (is (:records-without-field-equal? s)) (is (= [:no-progress :no-progress] (:statuses s))) (is (= [1 1] (:click-counts s)))))
