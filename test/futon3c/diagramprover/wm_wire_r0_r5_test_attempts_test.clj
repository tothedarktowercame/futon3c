(ns futon3c.diagramprover.wm-wire-r0-r5-test-attempts-test
  "Wire [:r0-enact-step :r5-test :attempts]: the enactment step's attempts
  reaching the grain-gate test.

  The writer is flight-runner/enact-fn (:r0-enact-step's site), writing the
  attempts vector on the enactment record. The reader is
  futon2.aif.flight-grain-gate-test (:r5-test's site), which reads
  (first (:attempts enactment)) for the grain attempt and counts
  (:attempts enactment) to show the flight continues past a refusal.

  No live record carries the writer's end (live-records-read), so the wire
  is WITNESSED-HERMETICALLY: enact-fn runs with a fixture seat over the
  flight-grain-gate-test shape (a grain pattern whose interpretation
  declares the role grain from the pinned exemplar record, plus a second
  pattern), the writer's value is (:attempts (:enactment out)) and the
  reader's is (:attempts record) read back from the record file."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-enact-driver :as d]))

(defn observe
  "The writer's attempts and the reader's, TAMPER editing the record file
  before the reader reads it (the bad cases)."
  ([] (observe identity))
  ([tamper]
   (let [{:keys [enactment record-path]} (d/enact-r5)]
     (d/rewrite record-path tamper)
     {:writer (:attempts enactment)
      :reader (:attempts (w/read-record record-path))})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "its one enactment is {:absent :no-dispatch-configured}: enact-fn never wrote an :attempts"}
     {:path (p "flight-e70b4baf.edn")
      :sha256 "10ac7cd76b2b18b40c4044bb55f0e54bc210cd908d9c4deef74875fc9d2bc88c"
      :why "its one enactment is {:absent :no-decision}: no record written"}
     {:path (:path d/click-001-enactment)
      :sha256 (:sha256 d/click-001-enactment)
      :why "hand-authored (claude-10, 2026-09-24): schema :m-futon-seams/proof2a-enactment-v1, not enact-fn's :wm/enactment-v1; supplies the role grain the hermetic run plans, but its :attempts were not written by the writer"}]))

(def wire
  {:wire [:r0-enact-step :r5-test :attempts]
   :kind :witnessed-hermetically
   :test `the-attempts-reach-the-grain-gate-test
   :check check
   :live-records-read live-records-read})

(deftest the-attempts-reach-the-grain-gate-test
  (let [o (check)]
    (is (= [:p/grain :p/after] (mapv :pattern (:writer o))))
    (is (= {:status :pass} (:grain-gate (first (:writer o))))
        "the grain attempt carries the gate's pass, as the r5-test asserts")
    (is (true? (:success (first (:writer o)))))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(assoc % :attempts {:absent :no-attempts}))]
    (is (= {:absent :no-attempts} (:reader o)))
    (is (not (w/received? o)))))

(deftest different-attempts-at-the-reader-fail-the-wire
  ;; the record without its grain attempt is a real prefix of the writer's
  ;; vector, and it is not the value the writer wrote
  (let [o (observe (fn [r] (update r :attempts (fn [as] (vec (rest as))))))]
    (is (= [:p/after] (mapv :pattern (:reader o))))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-enactment-attempts
  (doseq [{:keys [path sha256 why]} live-records-read]
    (is (= sha256 (w/sha256-file path)) why))
  (let [[f278 fe70 ex] (map #(w/read-record (:path %)) live-records-read)]
    (is (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments (:flight f278)))))
    (is (= [{:absent :no-decision}] (mapv :enactment (:enactments (:flight fe70)))))
    (is (= :m-futon-seams/proof2a-enactment-v1 (:schema ex)))))
