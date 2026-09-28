(ns futon3c.diagramprover.wm-wire-flight-record-summary-flight-click-close-test-chosen-test
  "Wire [:flight-record-summary :flight-click-close-test :chosen]: the click's
  selection as record-summary reads it from the run record ([:decision
  :chosen], select-keys [:id :candidate :precedence], only when the
  chosen's target is the flight's) reaching the component's own test
  (futon2/test/futon2/aif/flight_click_close_test.clj, the :box/kind :test
  box), whose read of this field is

    (is (= :C1 (get-in e [:chosen :candidate])))
    (is (= (get-in seventh-run [:decision :chosen :precedence])
           (get-in e [:chosen :precedence])))

  in the-seventh-flights-selection-and-close-are-kept — e being the click
  entry record-click wrote over record-summary of the run record. A test
  box has no runtime var to drive through, so the hermetic witness
  performs exactly that read over a real call of the writer's var
  (record-summary of a run record run-opportunity! wrote from a real
  select-action-cascades decision, the r9 wire's seam), and observes the
  entry's :chosen beside the run record's [:decision :chosen] — the box's
  own comparisons.

  No live record carries the reader's end (the reader is a test, its read
  on no record); the live records read say what the field's live presence
  is (the seventh flight's run record chose :C1; its click entry, written
  before WM-CAST-I 2, kept no :chosen — the defect the reader box pins).
  So the wire is WITNESSED-HERMETICALLY. The values are read from the producer record."
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def live-records-read [{:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn" :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa" :why "the writer's source only: [:decision :chosen] is present with :candidate :C1; a run record is not the flight record, and the matching flight record's click entry predates WM-CAST-I 2"} {:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn" :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212" :why "the reader's end absent: the seventh flight's click entry has no :chosen (written before WM-CAST-I 2 kept it)"} {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn" :sha256 "df01831c24a7042d66b6ef2c38d82cdfbd0994a03b5539f3112db7dc41894970" :why "the eighth flight chose nothing: [:decision :chosen] {:status :absent :reason :no-chosen-action}"} {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/flight-ada87008.edn" :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de" :why "and its click entry accordingly carries no :chosen"}])
(defn data [] (:fields (producer-record/record "wm-wire-flight-record-summary-flight-click-close-test-chosen")))
(defn check [] (:positive (data)))
(def wire {:wire [:flight-record-summary :flight-click-close-test :chosen] :kind :witnessed-hermetically :test `the-clicks-selection-reaches-the-components-own-test :check check :live-records-read live-records-read})
(deftest the-clicks-selection-reaches-the-components-own-test (let [o (check)] (is (= :cas/b (get-in o [:record-chosen :candidate]))) (is (= (select-keys (:record-chosen o) [:id :candidate :precedence]) (:writer o))) (is (:reader-agrees? o)) (is (w/received? o))))
(deftest another-targets-selection-is-not-kept-and-fails-the-wire (let [o (:absent (data))] (is (some? (:record-chosen o))) (is (nil? (:writer o))) (is (= {:absent :field-not-carried} (:reader o))) (is (not (w/received? o)))) (is (not (w/received? (assoc (check) :reader {:absent :no-selection})))) (is (not (w/received? (assoc (check) :reader {:status :absent :reason :no-selection})))))
(deftest a-different-selection-fails-the-wire (let [o (check) other (:different (data))] (is (= :cas/a (get-in other [:writer :candidate]))) (is (not= (:writer o) (:reader other))) (is (not (w/received? (assoc o :reader (:reader other)))))))
(deftest the-live-records-carry-one-end-at-most (let [[run flight eighth-run eighth-flight] live-records-read] (doseq [{:keys [path sha256]} live-records-read] (is (= sha256 (w/sha256-file path)) path)) (is (= :C1 (get-in (w/read-record (:path run)) [:decision :chosen :candidate]))) (is (not-any? :chosen (:clicks (:flight (w/read-record (:path flight)))))) (is (= {:status :absent :reason :no-chosen-action} (get-in (w/read-record (:path eighth-run)) [:decision :chosen]))) (is (not-any? :chosen (:clicks (:flight (w/read-record (:path eighth-flight))))))))
