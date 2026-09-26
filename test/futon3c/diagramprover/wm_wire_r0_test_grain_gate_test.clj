(ns futon3c.diagramprover.wm-wire-r0-test-grain-gate-test
  "Wire [:r0-enact-step :r0-test :grain-gate]: the enactment step's grain
  gate result reaching the row-0 test.

  The writer is flight-runner/enact-fn (:r0-enact-step's site). When the
  chosen candidate names no grain pattern, enact-fn runs the gate on two
  nil grains and records its typed refusal at the record's top level under
  :grain-gate (with a grain pattern the gate's result rides on the grain
  attempt instead; that shape is the r5 wires). The reader is
  futon2.aif.flight-enact-test (:r0-test's site), which reads
  (get-in enactment [:grain-gate :reason]) and asserts :grain-not-declared.

  No live record carries the writer's end (live-records-read), so the wire
  is WITNESSED-HERMETICALLY: enact-fn runs with a fixture seat over the
  flight-enact-test shape (two patterns, no grain), the writer's value is
  (:grain-gate (:enactment out)) and the reader's is (:grain-gate record)
  read back from the record file the writer wrote."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-enact-driver :as d]))

(defn observe
  "The writer's :grain-gate and the reader's, TAMPER editing the record
  file before the reader reads it (the bad cases)."
  ([] (observe identity))
  ([tamper]
   (let [{:keys [enactment record-path]} (d/enact-r0)]
     (d/rewrite record-path tamper)
     {:writer (:grain-gate enactment)
      :reader (:grain-gate (w/read-record record-path))})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "its one enactment is {:absent :no-dispatch-configured}: enact-fn never ran, so no :grain-gate was written"}
     {:path (p "flight-d00574c8.edn")
      :sha256 "68e531b462d535983ea5114a8da370daec943a3b4283b6b87831e63ecf9eadd8"
      :why "its one enactment is {:absent :no-decision}: no record written"}
     {:path (:path d/click-001-enactment)
      :sha256 (:sha256 d/click-001-enactment)
      :why "hand-authored (claude-10, 2026-09-24): carries no :grain-gate key at all (its grain story is told in prose and :observations), and is not enact-fn's output"}]))

(def wire
  {:wire [:r0-enact-step :r0-test :grain-gate]
   :kind :witnessed-hermetically
   :test `the-gate-refusal-reaches-the-row-0-test
   :check check
   :live-records-read live-records-read})

(deftest the-gate-refusal-reaches-the-row-0-test
  (let [o (check)]
    (is (= :grain-not-declared (get-in o [:writer :reason]))
        "the gate's own refusal, as flight-enact-test asserts it")
    (is (= :refuse (get-in o [:reader :status])))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(assoc % :grain-gate {:absent :gate-not-run}))]
    (is (= {:absent :gate-not-run} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-gate-result-at-the-reader-fails-the-wire
  ;; {:status :pass} is a real gate result (the grain-mode run of the r5
  ;; wires), not an invented constant; it is not what this writer wrote
  (let [pass (:grain-gate (first (:attempts (:enactment (d/enact-r5)))))
        o (observe #(assoc % :grain-gate pass))]
    (is (= {:status :pass} pass))
    (is (= pass (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-grain-gate
  (doseq [{:keys [path sha256 why]} live-records-read]
    (is (= sha256 (w/sha256-file path)) why))
  (let [[f278 f005 ex] (map #(w/read-record (:path %)) live-records-read)]
    (is (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments (:flight f278)))))
    (is (= [{:absent :no-decision}] (mapv :enactment (:enactments (:flight f005)))))
    (is (not (contains? ex :grain-gate)))))
