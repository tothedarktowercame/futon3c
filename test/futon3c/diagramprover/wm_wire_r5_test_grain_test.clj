(ns futon3c.diagramprover.wm-wire-r5-test-grain-test
  "Wire [:r5-flight-call :r5-test :grain]: the candidate's grain, as the
  flight call wrote it, reaching the grain-gate test.

  The writer is the flight's enactment call, flight-runner/enact-fn
  (:r5-flight-call's site): it hands the candidate's grain to grain-gate
  and writes the same grain on the enactment record under :grain. The
  reader is futon2.aif.flight-grain-gate-test (:r5-test's site), which
  asserts (= role-grain (:grain enactment)).

  No live record carries the writer's end (live-records-read), so the wire
  is WITNESSED-HERMETICALLY: enact-fn runs with a fixture seat over the
  flight-grain-gate-test shape, the writer's value is (:grain
  (:enactment out)), and the reader's is (:grain record) read back from
  the record file the writer wrote. Both are the role grain from the
  pinned exemplar record, and the captured gate call confirms the record's
  :grain is the value the flight call handed on."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-enact-driver :as d]))

(defn observe
  "The writer's :grain and the reader's, TAMPER editing the record file
  before the reader reads it (the different-value bad case). MODE :r0 runs
  the no-grain-pattern shape (the typed-absence bad case)."
  ([] (observe identity :r5))
  ([tamper mode]
   (let [{:keys [enactment record-path gate-calls]} (if (= :r0 mode) (d/enact-r0) (d/enact-r5))]
     (d/rewrite record-path tamper)
     {:writer (:grain enactment)
      :reader (:grain (w/read-record record-path))
      :gate-received (get-in (first gate-calls) [:candidate :grain])})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "its one enactment is {:absent :no-dispatch-configured}: the flight call never wrote a :grain"}
     {:path (p "flight-e70b4baf.edn")
      :sha256 "10ac7cd76b2b18b40c4044bb55f0e54bc210cd908d9c4deef74875fc9d2bc88c"
      :why "its one enactment is {:absent :no-decision}: no record written"}
     {:path (:path d/click-001-enactment)
      :sha256 (:sha256 d/click-001-enactment)
      :why "hand-authored (claude-10, 2026-09-24): its :grain is the role grain this test pins, but it was not written by enact-fn"}
     {:path (:path d/click-001-outcome)
      :sha256 (:sha256 d/click-001-outcome)
      :why "hand-authored: the provider grain, the different real value the second bad case puts at the reader"}]))

(def wire
  {:wire [:r5-flight-call :r5-test :grain]
   :kind :witnessed-hermetically
   :test `the-candidates-grain-reaches-the-grain-gate-test
   :check check
   :live-records-read live-records-read})

(deftest the-candidates-grain-reaches-the-grain-gate-test
  (let [o (check)]
    (is (= :role (get-in o [:writer :keyed-by])))
    (is (= (:writer o) (:gate-received o))
        "the record's :grain is the grain the flight call handed to the gate")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  ;; the production no-grain-pattern shape: enact-fn writes the typed
  ;; absence itself, and a typed absence never crosses the wire
  (let [o (observe identity :r0)]
    (is (= {:absent :candidate-names-no-grain-pattern} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-grain-at-the-reader-fails-the-wire
  (let [o (observe #(assoc % :grain (d/provider-grain)) :r5)]
    (is (= :agent-id (get-in o [:reader :keyed-by])))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-machine-grain
  (doseq [{:keys [path sha256 why]} live-records-read]
    (is (= sha256 (w/sha256-file path)) why))
  (let [[f278 fe70 _ex _out] (map #(w/read-record (:path %)) live-records-read)]
    (is (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments (:flight f278)))))
    (is (= [{:absent :no-decision}] (mapv :enactment (:enactments (:flight fe70)))))))
